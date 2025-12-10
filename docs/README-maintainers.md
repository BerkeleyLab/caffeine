README-maintainers.md
========


Conventions for Git and Pull Requests
-------------

This repository follows the [fork-and-pull](https://docs.github.com/en/pull-requests/collaborating-with-pull-requests/getting-started/about-collaborative-development-models#fork-and-pull-model) model of development.
If you would like to contribute some changes, please
[fork](https://docs.github.com/en/pull-requests/collaborating-with-pull-requests/working-with-forks) this repository,
push your proposed edits to a feature branch in your fork, and then
[open a pull request](https://docs.github.com/en/pull-requests/collaborating-with-pull-requests/proposing-changes-to-your-work-with-pull-requests) against this repo when the changes are ready for review.


This repository aims to maintain a strictly linear git history. In order to achieve this, please
observe the following workflow:

* After your PR has been approved, your feature branch will be rebased onto `origin/main` before merge.
* In general you should try to avoid rebasing a non-draft PR with pending approvals until the
  last step before merge, because it complicates iterative review.
* Only after your feature branch is up-to-date with `origin/main` may it then be merged
  into `main` with a merge commit.

Additional git policies for the primary Caffeine repository:

* Never force push to `main`
* All code changes and non-trivial documentation changes use a pull request
* No stray branches, except for rare cases of long-lived parallel development

Conventions for code and commits in Caffeine
-------------
[TODO: Improve wording of below bullets]
* All Fortran filenames must have a `*.F90` suffix
* Procedures with `prif_` prefix are public facing procedures
* Procedures with `caf_` prefix are private procedures,
  internal to Caffeine, which are implemented in C
* Procedures with neither of the above two prefixes are private procedures,
  internal to Caffeine, which are implemented in Fortran
* C functions and global variables lacking a `caf_` prefix must be `static`
* Identifiers named `image` and `rank` refer to processes. Any identifier named `image` represents
  the process as 1-based number (Fortran-style), while `rank` represents the process as 0-based
  number (C-style) (i.e. rank == image - 1)
* When writing or making changes to BIND(C) interfaces, be vigilant when checking that the types
  and attributes of the arguments and return values are equivalent across the Fortran and
  C declarations
* Avoid committing whitespace-only changes to source lines distant from meaningful PR changes. In
  particular disable source editor features that automatically reformat entire files.
* If you absolutely must make whitespace-only changes to otherwise unmodified lines
  (for example, rewrapping the lines in documentation), please isolate those changes
  in a separate commit with a commit message explaining the lack of meaningful change.
* Similarly if you need to move blocks of lines unchanged between distant locations or rename files,
  please also isolate those changes in a separate commit with a commit message 
  explaining the lack of meaningful change.
* Tab characters should NEVER appear in source code

Conventions for test code
-------------------------
* All significant features should have non-trivial correctness tests in test/
* Every PRIF procedure must be invoked by at least one test
* Correctness tests should aim to achieve complete code coverage of internal paths,
  and exercise any important corner-cases.
* Test functions should return a Julienne `test_diagnosis_t` named `diag`
  - If a complete diagnosis can be computed using a single Fortran expression `expr`,
    then the statement `diag = expr` should appear near the end of the procedure.
  - Otherwise, the statement `diag = .true.` must appear as the first executable
    statement in the procedure, and the `ALSO/ALSO2` macros defined by 
    [test-utils.F90](../test/test-utils.F90) should be invoked to build an incremental diagnosis.

