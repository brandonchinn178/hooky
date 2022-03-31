# Contributing

Thank you for contributing to Hooky! This document will go over some of the basic commands to do development tasks.

Pre-requirements:
* [Stack](https://docs.haskellstack.org/)

## Build

```bash
stack build
```

## Test

```bash
stack test
```

## Install pre-commit hooks

To install hooky on your hooky repo itself, you can either do the normal `hooky install` command with an already-installed version of Hooky, or do the following steps to install with a newly built Hooky:

1. Run the build as usual
2. `stack exec -- hooky install`

After this, git commits will run hooky as usual! You can also run any usual Hooky commands with the built Hooky with `stack exec -- hooky ...`.

## Lint

Linters are installed + run via Hooky, so follow the instructions in "Install pre-commit hooks" and run the usual hooky commands with `stack exec`; e.g.

```bash
stack exec -- hooky run --all-files
```

## Release

TODO
