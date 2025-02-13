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
program that execute asynchronously in shared or distributed memory, except
where a program uses specific synchronization mechanisms.  Fortran's
``coarray'' distributed data structures offer a subscripted,
multidimensional array notation defining a partitioned global address space
(PGAS).  One image can use this notation for one-sided access to another
image's slice of a coarray. 

The CoArray Fortran Framework of Efficient
Interfaces to Network Environments (Caffeine) provides a runtime library
that supports Fortran's SPMD features [@rouson2022caffeine; @caffeine-site].
Caffeine implements cross-process communication by building atop the 
GASNet-EX exascale networking middleware library [@gasnet-lcpc18; @gasnetex-spec].
Caffeine is the first implementation of the compiler- and
runtime-agnostic Parallel Runtime Interface for Fortran (PRIF) specification
[@bonachea2024prif; @prif-0.5].  Any compiler that targets PRIF can use any
runtime that supports PRIF.  Caffeine supports researching the novel approach
of writing most of a compiler's parallel runtime library in the language being
compiled: Caffeine is primarily implemented using Fortran's non-parallel features,
with a thin C-language layer that invokes the external GASNet-EX communication library. 
Exploring this approach in
open source lowers a barrier to contributions from the compiler's users: Fortran
programmers.  Caffeine also facilitates research such as investigating various
optimization opportunities that exploit specific hardware such as shared memory
or specific interconnects.

\autoref{fig:prif-stack} depicts a software stack in which a parallel
runtime library, such as Caffeine, supports compiled Fortran code by
implementing PRIF.

![The parallel Fortran software stack enabled by the Caffeine parallel runtime's implementation of PRIF.\label{fig:prif-stack}](PRIF-software-stack-with-more.pdf){ width=100% }

# Statement of need

The Coarray Fortran domain-specific language pioneered a parallel programming
approach designed as a syntactically small extension to Fortran 95
[@numrich1998coarray].  Fortran 2008 incorporated Coarray Fortran features, 
including multi-image execution, synchronization statements, coarrays, and more.
Fortran 2018 greatly expanded this feature set to include such concepts as 
teams (groupings) of images, events (counting semaphores), collective
subroutines and failed-image detection (fault tolerance). Fortran 2023 provided
additional, minor multi-image extensions, including notified remote data access.
[@fortran2023]

Caffeine's initial target compilers include LLVM `flang` and LFortran, both of
which have no existing multi-image parallel runtime and thus will need one to reach full
compliance with the Fortran 2008, 2018, or 2023 versions of the Fortran standard.
The Caffeine project team has submitted the PRIF specification as a pull request
on the `llvm-project` `git` repository and have confirmed through private
correspondence the lead LFortran developer's interest in adopting PRIF when
LFortran begins work on enabling multi-image execution.

# Comparisons

At least eight implementations of Fortran's multi-image features have been
developed:

1. Caffeine,
2. g95,
3. Hewlett Packard Enterprise (HPE),
4. Intel,
5. Numerical Algorithms Group (NAG),
6. OpenCoarrays and the GNU Compiler Collection (GCC) [@fanfarillo2014opencoarrays],
7. Rice University [@rice-caf-compiler], and
8. University of Houston [@chapman2013experiences; @ge-2016].

One can view each of these along several axes from open- to closed-source, from
portable to hardware-specific from research artifact to production-ready, and from
dormant to actively developed.  One can also categorize each runtime in terms of
the choice of communication substrate and the ability to switch substrates.  In
each measure, Caffeine stands apart from some or all the other compilers.

## Activity level

Of the eight aforementioned parallel runtimes, three appear to be dormant: g95,
Rice University, and the University of Houston.  The rest remain under active
development.

## Openness and portability

As commercial products with proprietary, hardware-specific optimizations, the
HPE, Intel, and NAG compilers all have closed-source runtime libraries.  Caffeine
differs from such runtimes in its open-source development practices.  The
openness of a project's source impacts other dimensions of comparison.  For
example, openness impacts portability: compilers and runtimes delivered as
precompiled binary files have limited portability across hardware architectures.

In addition to portability across hardware, one can compare a parallel runtime
library's  portability across compilers.  With the exception of Caffeine, each
of the eight aforementioned runtimes is designed to support only one compiler.
In contrast, any compiler that targets PRIF can use the Caffeine runtime library.

## Communication substrates
One of Caffeine's most unique traits lies in its use of the GASNet-EX networking
middleware.  Developed as part of the Exascale Computing Project funded by the
United States Department of Energy (DOE), GASNet-EX facilitates communication 
for PGAS programming models on supercomputers at DOE leadership computing
facilities.  GASNet-EX often outperforms the widely used Message Passing
Interface (MPI) communication library as \autoref{fig:Frontier-bw} shows for
the Frontier supercomputer that ranked first from November 2022 through November
2024 on the Top 500 list of the fastest general-purpose computers 

![Comparison of GASNet-EX and MPI bandwidth on the Frontier supercomputer, reproduced with permission from [@gasnet-perf-2023].\label{fig:Frontier-bw}](Frontier-bw){ width=100% }
 
By contrast, the GCC Fortran compiler (`gfortran`) supports multi-image execution
by linking programs to the OpenCoarrays runtime library, which in turn uses MPI
for inter-image communication. Similarly, the Intel compiler uses MPI in the
version compiled for execution in distributed-memory.  A different version of the
Intel compiler produces executable programs that work only in shared memory, but
details of the corresponding communication substrate are not public.

HPE's Cray Compilation Environment (CCE) Fortran compiler uses a proprietary 
closed-source runtime.  
The CCE Fortran runtime targets HPE's proprietary DMAPP communication library.

## Research suitability
Open-source development expands the ways in which software can support research.
For example, the ability to recompile a runtime library facilitates studying
performance portability or studying the impact of different build configurations.
Although the University of Houston and Rice University compilers are research
compilers, both appear to be dormant. [@caf-2-site; @chapman2013experiences]
OpenCoarrays and Caffeine are the only two actively maintained open-source
runtimes and thus the two most suitable for research.  

# Recent research and scholarly publications
Caffeine has supported researching whether and how one can write a compiler's
parallel runtime library in the language being compiled.  This research spawned
a peer-reviewed publication describing the approach developed for Caffeine
[@rouson2022caffeine]. Caffeine also inspired and supported researching whether
and how one can define an interface to a compiler's parallel runtime in a
compiler- and runtime-agnostic manner. The latter research produced a second
peer-reviewed publication motivating and describing PRIF [@bonachea2024prif].
Future research will include investigating avenues for supporting the
optimization of communications on specific categories of hardware.

# Acknowledgements

The Computer Languages and Systems Software (CLaSS) Group at Berkeley Lab [@class-site]
has developed Caffeine on funding from the Exascale Computing Project (ECP) and the
Stewardship for Programming Systems and Tools (S4PST) project.

# References
