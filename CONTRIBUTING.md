Contributing to Caffeine
--------------------------

## Reporting Defects or Suggesting Features

If you encounter problems or limitations when installing or using Caffeine, please do the following:

 1. Search the Caffeine [issues](https://github.com/berkeleylab/caffeine/issues), including [closed issues].
    If your search finds a report of the same problem, please post a comment in the issue.
 2. Email the Caffeine [mailing list](mailto:fortran@lbl.gov) for advice.
 3. If steps 1 or 2 do not resolve the problem, please file a [new issue] including
    - [ ] The Fortran compiler and compiler version used with Caffeine,
    - [ ] The complete output of the install and build commands run with `--verbose` argument,
    - [ ] The Caffeine version number or commit hash,
    - [ ] Any conditions required to reproduce the problem such as
      - [ ] The output of `uname -a` showing the operating system (OS), OS version, and processor architecture,
      - [ ] The number of images executed (e.g., the output of `echo $CAF_IMAGES`),
      - [ ] The command used to run your program (e.g., `./build/run-fpm.sh run`), and
      - [ ] A minimal reproducer: if possible, fewer than 50 lines demonstrating an issue.

## Contributing Code or Documentation

We welcome help with diagnosing, isolating and fixing problems or adding features!
All contributions are governed by the Caffeine [LICENSE.txt](./LICENSE.txt).
To contribute, please follow these steps:

- [ ] First please follow the [above steps](#reporting-defects-or-suggesting-features) and include a description of your proposed contribution.
- [ ] Fork the Caffeine repository into your GitHub account
- [ ] Create a new local branch for your work. 
- [ ] Name your branch according to the issue created.  For example `fix-issue-53` or `issue-53-feature`.
- [ ] Follow the coding conventions in [docs/README-maintainers.md](./docs/README-maintainers.md).
- [ ] Make your commits logically atomic, self-consistent, and cohesive.
- [ ] Add one or more unit tests in the `test` subdirectory to verify your fix or feature.
- [ ] Ensure that your branch passes all tests (via `./build/run-fpm.sh test` with appropriate flags).
- [ ] Update the [README.md](./README.md) if your branch affects anything described there.
- [ ] Push your branch to your fork.
- [ ] Open a [Pull Request](https://github.com/berkeleylab/caffeine/pulls) (PR) against an existing branch of the Berkeley Lab [Caffeine repository](https://github.com/berkeleylab/caffeine). 
- [ ] Please include the corresponding issue number in the PR title.
- [ ] If your PR is not ready for merging, please click the downward arrow next to the "Create pull request" button and select the "Create draft pull request" option before submitting.
- [ ] Watch for CI results on your PR and address any failures.
- [ ] Please be patient and responsive to comments on your PR.

## Current and Past Contributors

Caffeine is an open-source project and welcomes community participation in the development process.
Notable current and past contributors include:

* [Dan Bonachea](https://go.lbl.gov/dan-bonachea)
  [@bonachea](https://github.com/bonachea)
  [![View ORCID record] 0000-0002-0724-9349](https://orcid.org/0000-0002-0724-9349)

* [Katherine Rasmussen](https://go.lbl.gov/katherine-rasmussen)
  [@ktras](https://github.com/ktras)
  [![View ORCID record] 0000-0001-7974-1853](https://orcid.org/0000-0001-7974-1853)

* [Brad Richardson](https://everythingfunctional.com/)
  [@everythingfunctional](https://github.com/everythingfunctional)
  [![View ORCID record] 0000-0002-3205-2169](https://orcid.org/0000-0002-3205-2169)

* [Damian Rouson](https://go.lbl.gov/damian-rouson) 
  [@rouson](https://github.com/rouson)
  [![View ORCID record] 0000-0002-2344-868X](https://orcid.org/0000-0002-2344-868X)


You can also browse the [full list of repository contributors](https://github.com/BerkeleyLab/caffeine/graphs/contributors).

---

[Long or Frequently Used URLs]: #
[View ORCID record]: https://github.com/BerkeleyLab/caffeine/wiki/img/ORCID-small.png
[closed issues]: https://github.com/berkeleylab/caffeine/issues?q=is%3Aissue+is%3Aclosed
[new issue]: https://github.com/berkeleylab/caffeine/issues/new
