README-release.md
========

Release Procedure for Caffeine
-------------
1. Nominate a Release Manager with primary responsibility for ensuring each step in this
   procedure is followed
2. Ensure there are no open issues marked with `release-blocker` label
3. Validate correctness testing has been performed across all supported systems and supported
   versions of external dependencies
4. Complete release related chores in repository files
    1. Update all instances of the copyright year embedded in: [LICENSE.txt](../LICENSE.txt),
       [manifest/fpm.toml.template](../manifest/fpm.toml.template)
    2. Update all instances of the release package version number embedded in:
       [manifest/fpm.toml.template](../manifest/fpm.toml.template), [install.sh](../install.sh)
    3. Update the author list embedded in: [manifest/fpm.toml.template](../manifest/fpm.toml.template)
    4. Review top-level [README.md](../README.md) and other user-facing documentation for any
       necessary changes
    5. Update [docs/implementation-status.md](../docs/implementation-status.md) with current status
    6. If the PRIF specification revision is changing, search and update all instances of the old revision,
       including `PRIF_VERSION_{MAJOR,MINOR}` in [prif.F90](../src/prif.F90)
    7. Temporarily hardcode version of gasnet installer in [install.sh](../install.sh) as the
       last commit in the release. Set GASNET_VERSION flag to the latest gasnet release
5. Produce the ChangeLog
    1. Create draft release on GitHub
    2. Review/edit the automated ChangeLog
    3. Add/update list of supported features/platforms
    4. Add/update list of high-level changes since last release
    5. Add/update list of known defects/limitations
    6. Spell-check and proofread
6. Tag a release candidate. For example `git tag #.#.#-rc1`, then `git push origin #.#.#-rc1`
7. Compel several people to manually validate the release candidate on systems of interest
   and with compilers and compiler versions listed in README
8. Create annotated tag (only after release candidate has been checked by team members)
    For example `git tag -a #.#.# -m "release version #.#.#"`, then `git push origin #.#.#`
9. Publish the release
10. Post release chores
    1. Git revert the commit that hardcoded the gasnet version or manually edit
    2. Update patch number of the version number embedded in:
       [manifest/fpm.toml.template](../manifest/fpm.toml.template), [install.sh](../install.sh)
       Update to an odd number to indicate that the `main` branch is currently a snapshot of something
       that is beyond the offical release
    3. Update the release procedure with any new steps or changes
