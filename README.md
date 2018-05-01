# make-package

Starting new Haskell is a hassle. You have to:

* Create a new project directory
* Populate it with source and build infrastructure files
* Create a git repository
* Commit the boilerplate you just created
* Install dependencies (e.g. run »stack build«)
* Create a new github repository
* Link the github repository

This package automates these steps.

## Configuration

User configuration files are expected in either of
* `~/.make-package.conf`
* `~/.make-package/make-package.conf`
* `$XDG_CONFIG_HOME/make-package/make-package.conf.` (where `$XDG_CONFIG_HOME` defaults to `~/.config` if unset)

## Templates

* Templates are expected in `$XDG_DATA_HOME/make-package/templates` (where `$XDG_DATA_HOME` defaults to `~/.local/share` if unset).
* The directory will be copied, files ending in `.mustache` are treated as mustache templates

The following variables are set for template expansion:

* `name`: Name of the package
* `desc`: Description of the package
* `email`: Author's email address
* `author`: Author's full name
* `year`: Current year
* `license`: Name of the selected license
* `stack-resolver`: Configured stack resolver
* `category`: Chosen category (if set)

## Licenses

License files can be stored in `$XDG_DATA_HOME/macke-package/licenses`. If this directory does not exist, the licenses provided
