# xcode-project

[![License: GPL v3](https://img.shields.io/badge/License-GPL%20v3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)

A package for Emacs which parses Xcode project files.

## Features

- Parse Xcode project file (project.pbxproj)
- Extract information about targets, build configurations, build phases, build settings and files etc.

## Usage

To obtain the parsed project object (alist):

    (xcode-project-read PATH-TO-XCODEPROJ)

Helper to locate the Xcode project (.xcodeproj) path for any given source file:

    (xcode-project-find-xcodeproj PATH-TO-FILE)

Then extract information such as targets, build phases, configurations and files.

Most functions return an object (alist), unless otherwise described (e.g. `xcode-project-target-names').

Some functions have optional arguments for further filtering (e.g. filter targets by type or name).

### Targets:

    (xcode-project-targets PROJ)
    (xcode-project-target-names PROJ)

### Build Phases:

    (xcode-project-build-phases PROJ TARGET-NAME)

### Build Configurations:

    (xcode-project-build-config-names PROJ)
    (xcode-project-build-config PROJ CONFIG-NAME TARGET-NAME)

### Build Settings:

    (xcode-build-config-setings' CONFIG)

### Build files - as objects:

    (xcode-project-build-files' PROJ TARGET-NAME)

### Build files - as paths (relative to project):

    (xcode-project-build-file-paths' PROJ TARGET-NAME)

## Installation

If you're an Emacs 24 user or you have a recent version of `package.el`, you can install `xcode-project.el` from the [MELPA](https://melpa.org/) or the [MELPA Stable](https://stable.melpa.org/) repository.

## License

This software is licensed under the [GNU GPLv3 License](http://www.gnu.org/licenses/)

See the LICENSE file for details.
