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
    * `brew bump-formula-pr brandonchinn178/tap/hooky --version 0.0.0 --write-only`
    * Commit changes on a new branch + push
    * Make PR
    * After CI passes, add the `pr-pull` label
1. Update nixpkgs
    * Update hash with `./nix_hash.py https://github.com/brandonchinn178/hooky/archive/v0.0.0.tar.gz --strip-components=1`
        * https://github.com/brandonchinn178/nix-hash
