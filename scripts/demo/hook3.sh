#!/usr/bin/env bash

set -eu -o pipefail

source hook_impl.sh

main() {
    progress_output hook3
}

main "$@"
