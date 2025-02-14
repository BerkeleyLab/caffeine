Contributing to Caffeine
--------------------------

## Reporting Defects or Suggesting Features

If you encounter problems or limitations when installing or using Caffeine, please do the following:

 1. Search the Caffeine [issues](https://github.com/berkeleylab/caffeine/issues), including [closed issues].
    If your search finds a report of the same problem, please post a comment in the issue.
 2. Email the Caffeine [mailing list](mailto:fortran@lbl.gov) for advice.
 3. If steps 1 or 2 do not resolve the problem, please file a [new issue] including
    - [ ] The Fortran compiler and compiler version used with Caffeine,
    - [ ] The complete output of the build command,
    - [ ] The Caffeine version number or commit hash,
    - [ ] Any conditions required to reproduce the problem such as
      - [ ] The output of `uname -a` showing the operating system (OS), OS version, and processor architecture,
      - [ ] The number of images executed: the output of `echo $GASNET_PSHM_NODES`),
      - [ ] The command used to run your program (e.g., `./build/run-fpm.sh run`), and
      - [ ] A minimal reproducer: if possible, fewer than 50 lines demonstrating an issue.

## Contributing Code or Documentation

We welcome help with diagnosing, isolating and fixing problems or adding features!
All contributions are governed by the Caffeine [LICENSE.txt](./LICENSE.txt).
To contribute, please follow these steps:

- [ ] First please follow the [above steps](#reporting-defects-or-suggesting-features) and include a description of your proposed contribution.
- [ ] Fork Caffeine.
- [ ] Create a new local branch for your work. 
- [ ] Name your branch according to the issue created.  For example `fix-issue-53` or `issue-53-feature`.
- [ ] Follow the coding conventions in [docs/README-maintainers.md](./docs/README-maintainers.md).
- [ ] Write [conventional commits](https://www.conventionalcommits.org).
- [ ] Make your commits logically atomic, self-consistent, and cohesive.
- [ ] Add one or more unit tests in the `test` subdirectory to verify your fix or feature.
- [ ] Ensure that your branch passes all tests (via `./build/run-fpm.sh test` with appropriate flags).
- [ ] Update the [README.md](./README.md) if your branch affects anything described there.
- [ ] Push your branch to your fork.
- [ ] Open a [Pull Request](https://github.com/berkeleylab/caffeine/pulls) (PR) against an existing branch of the Berkeley Lab [Caffeine repository](https://go.lbl.gov). 
- [ ] Please include the corresponding issue number in the PR title.
- [ ] If your PR is not ready for merging, please click the downward arrow next to the "Create pull request" button and select the "Create draft pull request" option before submitting.
- [ ] Please be patient and responsive to comments on your PR.

---

[Long or Frequently Used URLs]: #
[closed issues]: https://github.com/berkeleylab/caffeine/issues?q=is%3Aissue+is%3Aclosed
[new issue]: https://github.com/berkeleylab/caffeine/issues/new
