# Contributing

## Development

```shell
stack build
stack test

stack exec -- hooky run ...
```

## Release

1. Create a new branch
    1. Bump version in `hooky.cabal`
    1. Update `CHANGELOG.md`
    1. Make + merge a PR

1. Ensure your Hackage token is set in Settings > Secrets > Actions as `HACKAGE_TOKEN_<github_username>` (replace any non alphanumeric characters in username with `_`).
    * Generate a token from `https://hackage.haskell.org/user/<hackage_username>/manage`

1. Trigger the release workflow
    * https://github.com/brandonchinn178/hooky/actions/workflows/release.yml

1. Go to the GitHub Actions page, click on the "Release" workflow, and click "Run workflow" on the main branch

1. Publish the candidate: https://hackage.haskell.org/package/hooky/candidates

1. Publish GitHub release
    * https://github.com/brandonchinn178/hooky/releases

1. Update homebrew formula
    * `brew bump-formula-pr brandonchinn178/tap/hooky --write-only --version 0.0.0`
    * Commit changes on a new branch + push
    * Make PR
    * After CI passes, add the `pr-pull` label

1. Make changes to nixpkgs, if necessary
    * `https://github.com/NixOS/nixpkgs/tree/master/pkgs/by-name/ho/hooky/package.nix`
