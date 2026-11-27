# v1.0.10

* Start publishing to Hackage
* Add `hooky uninstall`
* Fix installing hooky in worktrees

# v1.0.9

* Support specifying files as a list:

    ```kdl
    files {
        - *.hs
        - *.txt
    }
    ```

# v1.0.8

* Output `--verbose` logs to stderr
* Log all subprocess calls
* Fix "Argument list too long" error in large repos
* Fix `check_broken_symlinks` failing when a symlink points to a directory that only has nested files
  * e.g. `foo-link -> foo/bar`, but only `foo/bar/a/b/c/baz.txt` exists
* Show all successful `hooky lint` linters
* Include hidden files/directories in `*` and `**` file globs

# v1.0.7

* Fix regression loading files with whitespace in name

# v1.0.6

* Fix regression running after `git mv`

# v1.0.5

* Support brace expansion in file globs ([#37](https://github.com/brandonchinn178/hooky/pull/37))
* Various symlink fixes ([#39](https://github.com/brandonchinn178/hooky/pull/39), [#40](https://github.com/brandonchinn178/hooky/pull/40), [#41](https://github.com/brandonchinn178/hooky/pull/41))
  * Fix errors when repo contains symlinks that point to a directory
  * Fix errors when repo contains symlinks in subdirectories with relative paths

# v1.0.4

* Filter out deleted files ([#32](https://github.com/brandonchinn178/hooky/pull/32))

# v1.0.3

* Force utf-8 encoding ([#23](https://github.com/brandonchinn178/hooky/issues/23))
* Fix stashing untracked renamed files ([#27](https://github.com/brandonchinn178/hooky/issues/27))
* Fix mangled output on long command ([#26](https://github.com/brandonchinn178/hooky/issues/26))

# v1.0.2

* Change `hooky install` to use any `hooky` on PATH, since absolute path may be version-specific and break on upgrade
* Add `--absolute` flag to `hooky install` to force using the absolute path to `hooky` in the pre-commit hook
* Improve performance of `check_case_conflict` lint rule
* Respect `files` config for lint rules

# v1.0.1

* Fix `end_of_file_fixer` failing on empty files
* Add extra newline in output, to improve the spacing

# v1.0.0

* Initial release
