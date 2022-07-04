# Action: sync

This action is used to update all workflows and submodules within a monorepo to the latest version. It is used both within each monorepo, and globally from the [control-room](https://github.com/r-universe-org/control-room). It is one of the most complex pieces in r-universe.


## Run within a monorepo

When this action runs in a monorepo (such as https://github.com/r-universe/ropensci), it roughly performs the following steps:

 1. Update the `.github/workflows` from [r-universe-org/workflows](https://github.com/r-universe-org/workflows) if needed.
 2. Check if the user has a custom `packages.json` in `https://github.com/{user}/universe`. Switch if needed.
 3. Update and check for changes in the `.registry` submodule and if any, add/remove corresponding package submodules
 4. For each of the package submodules in the monorepo, lookup upstream sha from the package git url/branch and test which of the package submodules should be updated.
 5. For each package submodule that can be updated do:
    - clone the upstream package git-url and checkout to the new sha.
    - read the package version and maintainer details from `DESCRIPTION` file (this also serves as a minimal check that the url actually contains an R package).
    - commit the new sha of the package submodule to the monorepo using the maintainer and version from the description as commit author/message.
    - push this new commit to the monorepo. This will trigger the [build workflow](https://github.com/r-universe-org/workflows/blob/master/build.yml) for this package.
    - also check for "Remotes" in the package description. If any remotes have changed, update the `.remotes` list and perform the same steps for the remotes packages.
    
## Global: meta sync

Before June 2022 we would run the above action as an hourly cron-job action in each of the monorepos. However after ingesting most of CRAN, we now have almost [5000 monorepos](https://github.com/r-universe). Running 5000 cronjobs every hour is very expensive and painful to monitor.

Therefore we changed the sync action above into a "dispatch" action (i.e. manually triggered). We now use a single global cron action that runs in the [control-room](https://github.com/r-universe-org/control-room) to selectively trigger dispatches in monorepos when needed.

When this action runs from the [control-room](https://github.com/r-universe-org/control-room) it does the following:

 1. List all monorepos under `https://github.com/r-universe` from the GitHub API.
 2. For each of these monorepos:
    - shallow clone the monorepo
    - quick check if either the registry or any packages can be updated
    - if so, trigger a dispatch of the sync action in the given monorepo.

This is not perfect yet, but it is already much more efficient, and we only need to monitor one CRONjob: https://github.com/r-universe-org/control-room/actions/workflows/sync.yml
