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
GNU Compiler Collection (GCC), Intel, and the Numerical Algorithms Group (NAG);
research compilers developed at the University of Houston and Rice University [@caf-2-site];
and the dormant `g95` compiler project.  One can view each of
these along several axes from open- to closed-source, from portable to hardware-specfic
from research artifact to production-ready, and from dormant to actively developed.
One can also categorize each runtime in terms of the choice of communication
substrate and the ability to switch subtrates.  In each measure, Caffeine stands
apart from some or all the other compilers.

## Openness and portability

As commercial products with proprietary, hardware-specific optimizations, the
HPE, Intel, and NAG compilers all have closed source. Caffeine differs from these
compilers in its open-source development practices.  

The openness of a given project's source impacts other dimensions of comparison.
The closed-source projects publishs limited detail about their intrnal design
choices.  For example, openness also impacts portability.  Whereas the HPE, Intel,
and NAG compilers are all offered only in precompiled binary formats, one can
recompile the open-source runtimes for a variety of hardware.

The lack of publicly available design details also limits the utility of the
closed-source projects for research studies on those design decisions.  One
obviously impactful decision is the choice of communication substracts, the
impact of which is discussed in the next subsection.

## Communication substrates
One of Caffeine's most unique traits lies in its use of the GASNet-EX networking
middleware.  Developed as part of the Exascale Computing Project funded by the
United States Department of Energy (DOE), GASNet-EX facilitates communication 
for PGAS programming models on supercomputes at DOE leadership computing
facilities.  GASNet-EX often outperforms the widely used Message Passing
Interface (MPI) communication library as \autoref{fig:Frontier-bw}
shows for the Frontier supercomputer that currently ranked first on the Top 500
list of the fastest general-purpose computers from November 2022 through
November 2024.

![Comparison of GASNet-EX and MPI bandwidth on the Frontier supercomputer.\label{fig:Frontier-bw}](Frontier-bw){ width=100% }

The GCC Fortran compiler (`gfortran`) uses the OpenCoarrays runtime library 
to support mult-image execution.  The only maintained runtime in OpenCoarrays
uses the MPI for inter-image communication.

The Intel compiler offers a runtime that works only in shared memory along with 
a second runtime that works in distributed memory.  Implementation details of the
shared-memory runtime are not public.  Like GCC/OpenCoarrays, the Intel
compiler's distributed-memory runtime uses MPI.

HPE's Cray Program Environment Fortran compiler uses a proprietary runtime.
The proprietary runtime uses HPE's DMAP library for communication between images.
The remaining compilers that have supported multi-image execution appear to be
dormant.

## Portability

## Research
OpenCoarrays and Caffeine are the only two actively maintained open-source runtimes
and thus the only two suitable for supporting research...

## Activity level

Every other tof parallel runtime library has been developed to support only one
compiler.  Caffeine improves upon the prior approaches by facilitating use with
any compiler that targets PRIF...

# Figures

\autoref{fig:prif-stack} depicts a Fortran software stack in which a parallel runtime library such as Caffeine supportes compiled Fortran code by implementing PRIF.

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
* 
* [@gasnet-lcpc18]
* [@gasnetex-spec]
* [@caffeine-site]
* [@fortran2023]
* [@fortran2008]


# Acknowledgements

The Computer Languages and Systems Software (CLaSS) Group at Berkeley Lab [@class-site]
has developed Caffeine on funding from the Exascale Computing Project (ECP) and the Stewardship for Programming Systems and Tools (S4PST) project.

# References
