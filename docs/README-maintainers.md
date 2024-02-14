README-maintainers.md
========

Conventions for code in Caffeine
-------------
[TODO: Improve wording of below bullets]
* Procedures with `prif_` prefix are public facing procedures
* Procedures with `caf_` prefix are private procedures internal to Caffeine
* Identifiers named `image` and `rank` refer to processes. Any identifier named `image` represents
the process as 1-based number (Fortran-style), while `rank` represents the process as 0-based
number (C-style) (i.e. rank = image -1)
* When writing or making changes to BIND(C) interfaces, be vigilant when checking that the types
and attributes of the arguments and return values are equivalent across the Fortran and
C declarations
