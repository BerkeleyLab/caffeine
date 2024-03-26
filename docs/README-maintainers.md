README-maintainers.md
========


Conventions for Git and Pull Requests
-------------
This repository aims to maintain a mostly linear history. In order to achieve this, please
observe the following workflow:
* Checkout a feature branch and open a PR when the changes are ready for review
* After your PR has been approved, make sure to rebase your feature branch with `origin/main`.
* In general you should try to avoid rebasing a non-draft PR with pending approvals until the
  last step before merge, because it complicates iterative review.
* Only after your feature branch is up-to-date with `origin/main`, then you may merge the branch
  into `main` with a merge commit.

Additional git policies:
* Never force push to `main`


Conventions for code and commits in Caffeine
-------------
[TODO: Improve wording of below bullets]
* Procedures with `prif_` prefix are public facing procedures
* Procedures with `caf_` prefix are private procedures internal to Caffeine
* C functions and global variables lacking a `caf_` prefix must be `static`
* Identifiers named `image` and `rank` refer to processes. Any identifier named `image` represents
  the process as 1-based number (Fortran-style), while `rank` represents the process as 0-based
  number (C-style) (i.e. rank = image -1)
* When writing or making changes to BIND(C) interfaces, be vigilant when checking that the types
  and attributes of the arguments and return values are equivalent across the Fortran and
  C declarations
* Avoid committing whitespace-only changes to source lines distant from meaningful PR changes. In
  particular disable source editor features that automatically reformat entire files.
* If you absolutely must make whitespace-only changes to otherwise unmodified lines
  (for example, rewrapping the lines in documentation), please isolate those changes
  in a separate commit with a commit message explaining the lack of meaningful change.
* Similarly if you need to move blocks of lines unchanged between distant locations,
  please also isolate those changes in a separate commit with a commit message 
  explaining the lack of meaningful change.
* Tab characters should NOT be used in source code
