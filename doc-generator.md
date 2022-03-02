---
project: Caffeine
summary: Library functions and derived types implementing the parallel features of Fortran 2018.
src_dir: src/
exclude_dir: doc
output_dir: doc/html
preprocess: true
macro: FORD
preprocessor: gfortran -E
display: public
         protected
         private
source: true
graph: true
css: ford-docs.css
md_extensions: markdown.extensions.toc
coloured_edges: true
sort: permission-alpha
extra_mods: iso_fortran_env:https://gcc.gnu.org/onlinedocs/gfortran/ISO_005fFORTRAN_005fENV.html
            iso_c_binding:https://gcc.gnu.org/onlinedocs/gfortran/ISO_005fC_005fBINDING.html#ISO_005fC_005fBINDING
lower: true
print_creation_date: true
creation_date: %Y-%m-%d %H:%M %z
project_github: https://go.lbl.gov/caffeine
project_download: https://github.com/berkeleylab/caffeine/releases
author: Lawrence Berkeley National Laboratory
email: rouson@lbl.gov
author_description: A U.S. Department of Energy National Laboratory Managed by the University of California
author_pic: https://avatars.githubusercontent.com/u/18706005?s=200&v=4
github: https://github.com/berkeleylab
website: https://go.lbl.gov/class

[TOC]
 
@warning Caffeine is early-stage developmental software with evolving interfaces and functionality.

Documentation
=============

Welcome to the Caffeine documentation.
[FORD] generates this documentation from inline comments, static analysis, and Markdown files.
The target audience for Caffeine is Fortran compiler developers.
With a fully Caffeinated compiler, Fortran programmers can produce parallel executable files from standard Fortran with no need to directly reference Caffeine or any lower-level communication software.

Philosophy and Motivations
--------------------------
* Write as much of Caffeine as possible in Fortran:
    - Writing the runtime library in the language of the users increases the likelihood of community contributions.
    - Writing the runtime library in Fortran obviates the need to directly manipulate compiler descriptors throughout much of Caffeine and allows Caffeine's underlying C layer to receive the Fortran-standard `CFI_cdesc_t` desriptor, which imwill make it easier to support multiple compilers.
    - Writing most of Caffeine in Fortran offers the potential exploiting Fortran's rich array syntax, concurrent loop iterations (`do concurrent`), `pure` procedures and related features. Currently, these play a role only in one place: C callbacks to user-provided, `pure` functions that can be invoked inside a `do concurrent` block during the execution of `co_reduce`.
* Define an interface that remains agnostic about the back-end communication library:
    - Once multiple back ends are supported, Fortran developers would not have to rewrite or even recompile their programs in order to change back ends. Switching from GASNet-EX to MPI, for example, could become a link-time decision.

Organization
------------
The tree below depicts a skeletal overview of Caffeine's directory structure.
```
build/ - build directory created by running ./install.sh
  |-- run-fpm.sh - shell script for rebuilding Caffeine or running examples or tests
  |-- * - temporary build files
example/
  |-- hello.f90 - a Caffeinated "Hello, world!" program
  |-- support-test/ - programs invoked by the test suite
src/
  |-- caffeine_m.f90 - the Big Kahuna: one module that exports all Caffeine functionality
  |-- caffeine/
        |-- *_m.f90 - modules containing procedure interface bodies
        |-- *_s.f90 - submodules containing procedure definitions
        |-- *.c - thin C functions wrapping networking middleware
        |-- *.h - corresponding C header files
        |-- *_h_m.f90 - a Fortran translation of a C header file
        |-- collective_subroutines/
              |-- co_*_s.f90 - submodules containing procedure definitions
test/
  |-- main.f90 - test suite driver created by make_vegetable_driver
  |-- *_test.f90 - unit tests

```

[FORD]: https://github.com/Fortran-FOSS-Programmers/ford#readme
