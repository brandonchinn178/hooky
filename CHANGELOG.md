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
