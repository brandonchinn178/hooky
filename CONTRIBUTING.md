# Contributing

## Development

```shell
stack build
stack test

stack exec -- hooky run ...
```

## Release

1. Bump version in `hooky.cabal`
1. Update `CHANGELOG.md`
1. Make + merge a PR
1. Trigger the release workflow
    * https://github.com/brandonchinn178/hooky/actions/workflows/release.yml
1. Publish GitHub release
    * https://github.com/brandonchinn178/hooky/releases
1. Update homebrew formula
    * `brew bump-formula-pr brandonchinn178/tap/hooky --version $VERSION`
