#!/bin/bash -l
set -e
echo "Synchronizing ${1} in ${PWD}"
if [ "${1}" == "" ]; then
Rscript -e "sync::update_local()"
else
Rscript -e "sync::sync_from_registry('${1}')"
fi
echo "Action complete!"
