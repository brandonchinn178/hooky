#!/usr/bin/env bash

set -eu -o pipefail

progress_output() {
    local name="$1"; shift

    local max=20
    local delay=0.1

    for i in $(seq 1 $max); do
        echo "$name: running... ($i/$max)"
        sleep $delay
    done
}

check() {
    local msg="$1"; shift
    local fix=false; if [[ $1 == --fix ]]; then fix=true; shift; fi

    fail=false
    for file in "$@"; do
        if ! grep -q "$msg" "$file"; then
            if [[ $fix == true ]]; then
                echo "$msg" >> "$file"
            else
                echo >&2 "File is not valid: $file"
                fail=true
            fi
        fi
    done
    if [[ $fail == true ]]; then
        return 1
    fi
}
