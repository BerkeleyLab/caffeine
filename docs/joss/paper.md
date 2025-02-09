---
title: 'Caffeine: A parallel runtime library for supporting modern Fortran compilers'
tags:
  - high-performance computing
  - parallel programming
  - Fortran
  - compilers
  - coarrays
authors:
  - name: Dan Bonachea
    orcid: 0000-0002-0724-9349
    equal-contrib: true # (This is how you can denote equal contributions between multiple authors)
    affiliation: 1
  - name: Katherine Rasmussen
    orcid: 0000-0001-7974-1853
    equal-contrib: true
    affiliation: 1
  - name: Brad Richardson
    orcid: 0000-0002-3205-2169
    equal-contrib: true
    affiliation: 1
  - name: Damian Rouson
    orcid: 0000-0002-2344-868X
    equal-contrib: true
    affiliation: 1
affiliations:
 - name: Lawrence Berkeley National Laboratory, United States
   index: 1
date: 5 February 2025
bibliography: paper.bib

---

# Summary

The Fortran programming language standard added features supporting
single-program, multiple-data (SPMD) parallel programming and loop
parallelism beginning with [@fortran2008].  In Fortran, SPMD programming
involves the creation of a fixed number of images (instances) of a
program that execute asynchronously in shared or distributed memory except
where a program uses specific synchronization mechanisms.  Fortran's
``coarray'' distributed data structures offer a subscripted,
multidimensional array notation defining a partitioned global address space
(PGAS).  One image can use this notation for one-sided access to another
image's slice of a coarray. The CoArray Fortran Framework of Efficient
Interfaces to Network Environments (Caffeine) provides a runtime library
that supports Fortran's SPMD features by building atop the GASNet-EX exascale
networking middleware [@rouson2022caffeine; @caffeine-site; @gasnet-lcpc18;
@gasnetex-spec].  Caffeine is the first implementation of the compiler- and
runtime-agnostic Parallel Runtime Interface for Fortran (PRIF) specification
[@bonachea2024prif].  Any compiler that targets PRIF can use any runtime that
supports PRIF.  Caffeine supports researching the novel approach of writing
most of a compiler's parallel runtime library in the language being compiled:
Caffeine uses Fortran's non-parallel features to wrap a thin C-language layer
that invokes GASNet-EX functions.  Exploring this approach in open source
lowers a barrier to contributions from the compiler's users: Fortran
programmers.  Future research will include investigating optimization
opportunities afforded by specific hardware such as shared memory or 
or specific interconnects.

# Statement of need

The Coarray Fortran domain-specific language pioneered a parallel programming
approach designed as a syntactically small extension to Fortran 95
[@numrich1998coarray].  Fortran 2008 incorporated Coarray Fortran features, 
including multi-image execution, synchronization statements, coarrays, and more.
Fortran 2018 greatly expanded this feature set to include such concepts as 
teams (groupings) of images, events (counting semaphores), and collective
subroutines, failed-image detection (fault tolerance). Fortran 2023 provided
additional, minor mult-image extensions, including notified remote data access.

Caffeine's initial target compilers include LLVM `flang` and LFortran, both of
which have no existing parallel runtime and thus will need one to reach full
compliance with the Fortran 2008, 2018, or 2023 versions of the Fortran standard.
The Caffeine project team has submitted the PRIF specification as a pull request
on the `llvm-project` `git` repository and have confirmed through private
correspondence the primary LFortran developer's interest in adopting PRIF when
LFortran begins work on enabling mult-image execution.

# Comparisons

At least six Fortran compilers have released support for multi-image execution.
These include production compilers from Hewlett Packard Enterprise (HPE),
Intel, GNU Compiler Collection (GCC), and the Numerical Algorithms Group (NAG);
research compilers developed primarily at Rice University and the University of
Houston; and the dormant `g95` compiler project.  Each compiler required programs
using multi-image features to rely a parallel runtime developed to support only
the chosen compiler.  Caffeine improves upon the prior approaches by facilitating
use with any compiler that targets PRIF.  

Caffeine also differs from the alternatives in its use of GASNet-EX...

# Figures

Figures \autoref{fig:prif-stack} depicts a Fortran software stack in which a parallel runtime library such as Caffeine supportes compiled Fortran code by implementing PRIF.

![The parallel Fortran software stack enabled by the Caffeine parallel runtime PRIF implementation.\label{fig:prif-stack}](PRIF-software-stack-with-more.pdf){ width=100% }

# Citations

Citations to entries in paper.bib should be in
[rMarkdown](http://rmarkdown.rstudio.com/authoring_bibliographies_and_citations.html)
format.

For a quick reference, the following citation commands can be used:
- `@author:2001`  ->  "Author et al. (2001)"
- `[@author:2001]` -> "(Author et al., 2001)"
- `[@author1:2001; @author2:2001]` -> "(Author1 et al., 2001; Author2 et al., 2002)"

Test citations:

* [@rouson2022caffeine]
* [@bonachea2024prif]
* [@prif-0.5]
* [@open-coarrays]
* [@gasnet-lcpc18]
* [@gasnetex-spec]
* [@caffeine-site]
* [@fortran2023]
* [@fortran2008]


# Acknowledgements

The Computer Languages and Systems Software (CLaSS) Group at Berkeley Lab [@class-site]
has developed Caffeine on funding from the Exascale Computing Project (ECP) and the Stewardship for Programming Systems and Tools (S4PST) project.

# References
