#!/usr/bin/env bash
SCRIPT_HOME="$(cd "$(dirname "$0")" && pwd -P)"
if hash gu 2>/dev/null; then
  gu install -L -r -f $SCRIPT_HOME/cooma-component.jar
else
  echo "You need to have gu (Graal Updater) on the path. Add GraalVM to the path."
fi

