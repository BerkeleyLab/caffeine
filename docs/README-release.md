README-release.md
========

Conventions for Caffeine Releases
-------------
1. Nominate a Release Manager with primary responsibility for ensuring each step in this
   procedure is followed
2. Ensure there are no open issues marked with `complete-before-release` label
3. Validate correctness testing has been performed across all supported systems and supported
   versions of external dependencies
4. Update all instances of the copyright year embedded in: [LICENSE.txt](../LICENSE.txt),
   [fpm.toml (created by install.sh)](../install.sh)
5. Update all instances of the release package version number embedded in: [fpm.toml (created by install.sh)](../install.sh),
   [install.sh](../install.sh)
6. Update the author list embedded in: [fpm.toml(created by install.sh)](../install.sh)
7. Review top-level [README.md](../README.md) and other user-facing documentation for any necessary
   changes
8. Produce the ChangeLog
    1. Create draft release on GitHub
    2. Review/edit the automated ChangeLog
    3. Add/update list of supported features/platforms
    4. Add/update list of high-level changes since last release
    5. Add/update list of known defects/limitations
    6. spell-check and proofread
9. Produce a release candidate tarball and compel several people to manually validate it on
   systems of interest
10. Publish the release
11. Update the release procedure with any new steps or changes
