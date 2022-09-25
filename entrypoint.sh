#!/bin/bash -l
set -e
echo "Synchronizing ${1} in ${PWD}"
if [ "$1" == "all" ]; then
Rscript -e "sync::trigger_syncs()"
else
Rscript -e "sync::sync_from_registry('${1}')"
fi
echo "Action complete!"
