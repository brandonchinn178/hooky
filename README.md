# Hooky

Minimal git hooks manager.

Features:
* Installs as a compiled executable; no need to have Python/Node/whatever installed to bootstrap.
* Only runs on staged files
    * Supports partially staged files
* Built-in support for auto-fixing files

## Installation

Download the appropriate binary from the GitHub release.

## Usage

* `hooky install [--mode=check|fix|fix-add]`
    * Registers `hooky` with git hooks.
    * `--mode` determines what to do when hooks modify files:
        * `check`: Run the commands in `check` mode, which shouldn't modify files
        * `fix`: Run the commands in `fix` mode, which may modify files
        * `fix-add`: Same as `fix`, except add the changes to the stage and continue the commit

* `hooky run hook1 hook2 -- [FILES ...]`
    * Manually runs the given hooks in `check` mode
    * Files may be specified as `@foo.txt`, where `foo.txt` contains one file per line.
    * Shortcuts:
        * `--modified` = `@<(git diff --name-only)`
        * `--staged` = `@<(git diff --staged --name-only)`
        * `--all` = `@<(git ls-files)`

* `hooky fix hook1 hook2 -- [FILES ...]`
    * Same as `hooky run`, except runs in `fix` mode

### `.hooky.toml` configuration

Each hook is specified in a `[hook.foo]` section containing the following keys:

* `command`: TODO (`"foo"` => `["/bin/sh", "-c", "foo \"$@\"", "/bin/sh"]`)
* `arg_mode`: TODO (xargs/file/none)
* `files`: TODO
    * `"*.hs" => "**/*.hs"`
    * `"/*.hs" => "*.hs"`
    * `"foo/*.hs" => "**/foo/*.hs"`
    * `"/foo/*.hs" => "foo/*.hs"`
    * `"!foo.hs" => "!**/foo.hs"`

### Built-in hooks

For convenience, Hooky also ships with some general purpose hooks, which can be specified as:

```toml
[hook.hooky]
command = "hooky lint --fix"
arg_mode = "file"
files = ["*"]
```

`hooky lint` runs the rules specified under the `[lint]` section, e.g.

```toml
[lint]
rules = [
    "no_commit_to_branch"
    "trailing_whitespace",
]
no_commit_to_branch.branches = ["main"]
trailing_whitespace.files = ["!foo.txt"]
```

Rules are enabled with the `rules` key, with per-rule configuration defined separately. All rules have the following configuration:

* `files`: Same as `files` in `[hook.*]` sections. If not specified, defaults to `["*"]`

Available rules and rule-specific configuration:

* `check_case_conflict` - Check if any files differ only by case, which would cause issues on case-insensitive filesystems like macOS
* `check_merge_conflict` - Check if any files contain merge conflict strings
* `check_broken_symlinks` - Check if any symlinks which do not point to anything
* `end_of_file_fixer` - Ensure files end with exactly one newline
* `no_commit_to_branch` - Prevent commits to specific branches
    * `branches` - Branches to prevent commits to, as a pattern; e.g. `["main", "release-*"]`
* `trailing_whitespace` - Remove trailing whitespace

## Comparison with other tools

### pre-commit

[`pre-commit`](https://pre-commit.com) is primarily a tool/environment manager ([ref](https://github.com/pre-commit/pre-commit/pull/3577), [ref2](https://github.com/pre-commit/pre-commit/issues/2316#issuecomment-1083643390)), not a command runner. IMO this is the wrong direction; most of the time, you've already configured the linter in your own environment; e.g. `eslint` in `package.json` or `ruff` in `pyproject.toml`. With `pre-commit`, you have to re-configure the linter in `.pre-commit-config.yaml`.

At the end of the day, git pre-commit hooks should just be a matter of registering commands you can already run in your repo with git hooks. That is the only thing Hooky cares about, and how you want to manage your tools is up to you.

### husky

TODO: https://typicode.github.io/husky/#/
