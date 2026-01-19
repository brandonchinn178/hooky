#!/usr/bin/env bash

set -eux -o pipefail

here="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "${here}"

export PATH=$(stack path --local-install-root)/bin:$PATH

exec vhs demo.tape
