# Hooky

Minimal git hooks manager.

Features:
* Installs as a compiled executable; no need to have Python/Node/whatever installed to bootstrap.
* Configure checks to run in git's pre-commit hook via a TOML configuration file
    * Allow checks to define the files to run the command for
* Only runs on staged files
    * Supports partially staged files
* Allow pulling checks from third-party GitHub repos

## Installation

TODO

## Usage

TODO

## Comparison with other tools

### pre-commit

[`pre-commit`](https://pre-commit.com) is a rather popular package, and actually serves as the inspiration for this tool! It has a really solid ecosystem and is implemented well, and if it fits your needs, I'd definitely recommend it.

However, the maintainer has expressed in multiple issues ([ref 1](https://github.com/pre-commit/pre-commit/issues/1453#issuecomment-967743197), [ref2](https://github.com/pre-commit/pre-commit/issues/2316#issuecomment-1083643390)) that its primary purpose is to be a tool/environment manager, not a git hooks manager. So if you just need a tool that sets up git hooks properly and don't care about pre-commit setting up isolated environments, you might find that pre-commit doesn't work for your use-case exactly, and there's no upstream desire to make it work.

Hooky's primary philosophy is to do one thing well, running git hooks, which means Hooky will probably be more accepting of improvements and features related to configuring and running git hooks.

### husky

TODO: https://typicode.github.io/husky/#/
