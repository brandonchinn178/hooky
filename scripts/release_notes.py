#!/usr/bin/env python3

import argparse
import itertools
from pathlib import Path
from typing import Mapping

TOP = Path(__file__).resolve().parent.parent

BASE_DOWNLOAD_URL = "https://github.com/brandonchinn178/hooky/releases/download"
BIN_NAME = "hooky"


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
    parser_generate.add_argument("--bin", required=True, nargs=2, action="append")
    parser_generate.set_defaults(
        run=lambda args: generate(
            version=args.release_version,
            binaries={
                label: Path(bin_path)
                for label, bin_path in args.bin
            },
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


def generate(version: str, binaries: Mapping[str, Path]) -> None:
    release_notes = [
        get_changelog_for(version),
    ]
    print("\n".join(release_notes))


if __name__ == "__main__":
    main()
