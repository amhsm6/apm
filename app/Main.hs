{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Main where

import Control.Monad
import Control.Monad.Except
import Control.Monad.Trans
import Control.Lens
import Text.Printf
import System.FilePath.Lens
import System.Exit
import System.FilePath
import System.Directory
import Options.Applicative

import Tree

db :: FilePath
db = "/var/lib/apm"

data Error = ErrorNoPath String
           | ErrorAbsoluteSymlink FilePath
           | ErrorPackageExists
           | ErrorNoPackage
           | ErrorCorruptedFile FilePath

report :: Error -> IO a
report (ErrorNoPath path) = putStrLn (printf "error: no such file or directory: %s" path) >> exitFailure
report (ErrorAbsoluteSymlink path) = putStrLn (printf "error: symlinks must have relative path to the target: %s" path) >> exitFailure
report ErrorPackageExists = putStrLn "error: package with such name is already installed" >> exitFailure
report ErrorNoPackage = putStrLn "error: package with such name is not installed" >> exitFailure
report (ErrorCorruptedFile path) = putStrLn (printf "error: package configuration file corrupted: %s" path) >> exitFailure

build :: FilePath -> IO Tree
build src = normalize <$> go src
    where go path = do
              doesPathExist path >>= \x -> unless x $ report $ ErrorNoPath path

              let name = path ^. filename

              dir <- doesDirectoryExist path
              if dir then do
                  symlink <- pathIsSymbolicLink path
                  if symlink then do
                      pure $ Dir name path []
                  else do
                      files <- listDirectory path
                      Dir name path <$> mapM (go . (path</>)) files
              else do
                  pure $ File name path

          normalize (File name path)  = Root [File name path]
          normalize (Dir _ _ children) = Root children
          normalize _ = undefined

prefix :: FilePath -> Tree -> Tree
prefix path (Root children) = Root $ foldr (\x acc -> [Dir x "" acc]) children dirs
    where dirs = dropWhile (=="/") $ splitDirectories path
prefix _ _ = undefined

copy :: Tree -> FilePath -> IO ()

copy (Root children) odir = do
    putStrLn $ printf "ROOT %s" odir
    forM_ children $ \x -> copy x odir

copy (Dir name src children) odir = do
    let dst = odir </> name

    symlink <- handleError (const $ pure False) $ pathIsSymbolicLink src
    if symlink then do
        target <- getSymbolicLinkTarget src
        unless (isRelative target) $ report $ ErrorAbsoluteSymlink src

        putStrLn $ printf "DIRLINK %s -> %s" dst target
        createDirectoryLink target dst
    else do
        putStrLn $ printf "DIR %s (%s)" dst src
        createDirectoryIfMissing False dst
        forM_ children $ \x -> copy x dst

copy (File name src) odir = do
    let dst = odir </> name

    symlink <- pathIsSymbolicLink src
    if symlink then do
        target <- getSymbolicLinkTarget src
        unless (isRelative target) $ report $ ErrorAbsoluteSymlink src

        putStrLn $ printf "SYMLINK %s -> %s" dst target
        createFileLink target dst
    else do
        putStrLn $ printf "CP %s -> %s" src dst
        copyFile src dst

data InstallOpts = InstallOpts { __packageName :: String
                               , __sourceDir :: String
                               , __prefix :: String
                               }

makeFieldsNoPrefix ''InstallOpts

install :: InstallOpts -> IO ()
install opts = do
    let file = db </> opts ^. _packageName

    doesPathExist file >>= \x -> when x $ report ErrorPackageExists

    tree <- prefix (opts ^. _prefix) <$> build (opts ^. _sourceDir)
    writeFile file $ show tree
    copy tree "/"

installOpts :: Parser InstallOpts
installOpts = InstallOpts <$> strArgument ( metavar "package_name" <>
                                            help "The name of the package"
                                          )
                          <*> strArgument ( metavar "source_directory" <>
                                            value "." <>
                                            showDefaultWith (const "Current directory") <>
                                            help "The package source location"
                                          )
                          <*> strOption ( long "prefix" <>
                                          metavar "PREFIX" <>
                                          value "/" <>
                                          showDefault <>
                                          help "Treat all files in the source directory as prefixed with PREFIX"
                                        )

delete :: Tree -> FilePath -> IO ()

delete (Root children) odir = do
    putStrLn $ printf "ROOT %s" odir
    forM_ children $ \x -> delete x odir

delete (Dir name _ children) odir = do
    let dst = odir </> name

    forM_ children $ \x -> delete x dst

    symlink <- handleError (const $ pure False) $ pathIsSymbolicLink dst
    if symlink then do
        putStrLn $ printf "RM %s" dst
        removeDirectoryLink dst
    else do
        exists <- doesPathExist dst
        if exists then do
            empty <- (==0) . length <$> listDirectory dst
            if empty then do
                putStrLn $ printf "RM %s" dst
                removeDirectory dst
            else do
                putStrLn $ printf "NONEMPTY %s" dst
        else do
            putStrLn $ printf "NO %s" dst

delete (File name _) odir = do
    let dst = odir </> name

    exists <- doesPathExist dst
    symlink <- handleError (const $ pure False) $ pathIsSymbolicLink dst
    if exists || symlink then do
        putStrLn $ printf "RM %s" dst
        removeFile dst
    else do
        putStrLn $ printf "NO %s" dst

data RemoveOpts = RemoveOpts { __packageName :: String }

makeFieldsNoPrefix ''RemoveOpts

remove :: RemoveOpts -> IO ()
remove opts = do
    let file = db </> opts ^. _packageName

    doesPathExist file >>= \x -> unless x $ report ErrorNoPackage

    tree <- parse <$> readFile file
    case tree of
        Just tree -> delete tree "/"
        Nothing -> report $ ErrorCorruptedFile file

    removeFile file

removeOpts :: Parser RemoveOpts
removeOpts = RemoveOpts <$> strArgument ( metavar "package_name" <>
                                          help "The name of the package"
                                        )

data Opts = IOpts InstallOpts
          | ROpts RemoveOpts

opts :: Parser Opts
opts = hsubparser ( command "install" (info (IOpts <$> installOpts) $ progDesc "Install a package") <>
                    command "remove" (info (ROpts <$> removeOpts) $ progDesc "Remove a package")
                  )

main :: IO ()
main = do
    createDirectoryIfMissing True db

    x <- execParser $ info (helper <*> opts) $ fullDesc <> progDesc "A simple package manager"
    case x of
        IOpts x -> install x
        ROpts x -> remove x
