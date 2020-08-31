#!/bin/bash -l
set -e
echo "Synchronizing ${1} in ${PWD}"
Rscript -e "sync::sync_from_registry('${1}')"
echo "Action complete!"
