#!/usr/bin/env bash
cd "$(dirname "$0")"
[[ -f cooma-component.jar ]] && rm cooma-component.jar || echo "cooma-component.jar not found"
