#!/bin/bash -l
set -e
echo "Synchronizing ${1} in ${PWD}"
if [ "$1" == "all" ]; then
Rscript -e "sync::trigger_syncs()"
elif [ "$1" == "https://github.com/r-universe/cran" ] || [ "$2" == "true" ]; then
Rscript -e "sync::update_remote('${1}')"
else
Rscript -e "sync::sync_from_registry('${1}')"
fi
echo "Action complete!"
