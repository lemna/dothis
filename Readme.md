# dothis [![Travis-CI Build Status](https://travis-ci.org/lemna/dothis.svg?branch=master)](https://travis-ci.org/lemna/dothis) [![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/apixoxrhv382cpst?svg=true)](https://ci.appveyor.com/project/lemna/dothis)


**dothis** - a very basic "To Do" List for Rstudio.

Eclipse has a handy task view - it lists comments starting with words like 'TODO' or 'FIXME' in a pane, providing a basic offline issue tracker. This package provides a barebones implementation for Rstudio. It adds two entries in the addin menu, one for the active file, one for all R and Rmd files in the active project. 

Comments starting with TODO:, TODO, FIXME: or FIXME will be listed in the marker pane. Clicking a line leads to the right place in the file.

## Installation
Use the [devtools](https://github.com/hadley/devtools) package:
```R
devtools::install_github('pkoki/dothis')
```
Alternatively there is a binary for windows available at [AppVeyor](https://ci.appveyor.com/project/pkoki/dothis/build/artifacts). Download the zip file and install using `install.packages()`.

