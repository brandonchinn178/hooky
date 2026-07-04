#!/usr/bin/env python3

import argparse
import itertools
from pathlib import Path

TOP = Path(__file__).resolve().parent.parent


def main() -> None:
    parser = argparse.ArgumentParser()
    subparsers = parser.add_subparsers(required=True)

    parser_validate_changelog = subparsers.add_parser("validate-changelog")
    parser_validate_changelog.add_argument("--release-version", required=True)
    parser_validate_changelog.set_defaults(
        run=lambda args: validate_changelog(
            version=args.release_version,
        )
    )

    parser_generate = subparsers.add_parser("generate")
    parser_generate.add_argument("--release-version", required=True)
    parser_generate.set_defaults(
        run=lambda args: generate(
            version=args.release_version,
        )
    )

    args = parser.parse_args()
    args.run(args)


def validate_changelog(version: str) -> None:
    _ = get_changelog_for(version)


def get_changelog_for(version: str) -> str:
    changelog = (TOP / "CHANGELOG.md").read_text().splitlines()

    if changelog[0] != f"# v{version}":
        raise Exception(
            f"""
            CHANGELOG doesn't look updated.
            Expected version: {version!r}
            Got header: {changelog[0]!r}
            """
        )

    return "\n".join(
        itertools.takewhile(
            lambda line: not line.startswith("#"),
            changelog[1:],
        )
    )


def generate(version: str) -> None:
    release_notes = [
        get_changelog_for(version),
    ]
    print("\n".join(release_notes))


if __name__ == "__main__":
    main()
