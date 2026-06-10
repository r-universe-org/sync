#!/bin/bash -l
set -e

# Authenticating gives better error messages for git
export GIT_TERMINAL_PROMPT=0
if [ "$GITHUB_PAT" ]; then
AUTH=$(printf "x-access-token:%s" "$GITHUB_PAT" | base64 -w0)
git config --global http.https://github.com/.extraheader "AUTHORIZATION: basic $AUTH"
fi

echo "Synchronizing ${1} in ${PWD}"
if [ "$1" == "all" ]; then
Rscript -e "sync::trigger_syncs()"
else
Rscript -e "sync::sync_from_registry('${1}')"
fi
echo "Action complete!"
