#!/usr/bin/env bash

set -eu -o pipefail

source hook_impl.sh

main() {
    sleep 2
    check "ADDED BY HOOK 1" "$@"
}

main "$@"
