# A Simple Package Manager
This is a **simple** package manager, capable only of copying files from one place to another and logging that (*aka* installing a package) and deleting those copied files (*aka* removing a package).

## Example
For example:
```bash
apm install foo bar --prefix=/usr/local
```
will install a package named *foo* whose contents are files inside *bar* directory into /usr/local.
