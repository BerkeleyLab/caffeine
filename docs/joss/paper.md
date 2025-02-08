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
parallelism beginning with [@fortran2008].  The SPMD features involve the
creation of a fixed number of images (instances) of a program that execute
asynchronously in shared or distributed memory except where a programmer
explicitly imposes synchronization.  Coarrays employ a subscripted
multidimensional array notation to define a partitioned global address 
space (PGAS) that images use to communicate data with each other. The
CoArray Fortran Framework of Efficient Interfaces to Network Environments
(Caffeine) provides a runtime library that supports Fortran's SPMD features
by building atop the GASNet-EX exascale networking middleware
[@rouson2022caffeine; @caffeine-site; @gasnet-lcpc18; @gasnetex-spec].
Caffeine is the first implementation of the compiler- and runtime-agnostic
Parallel Runtime Interface for Fortran (PRIF) specification [@bonachea2024prif].
Any compiler that targets PRIF can use any runtime that supports PRIF.
As a platform for researching approaches to mapping Fortran's SPMD/PGAS
features onto lower-level communication primitives, Caffeine facilitates
exploring the novel approach of writing most of a compiler's parallel runtime
library in the language being compiled: Caffeine uses Fortran's non-parallel
features to support Fortran's parallel features.  Doing so in open source
lowers a barrier to contributions from the compiler's users: Fortran
programmers.  Future research will include investigating optimization
opportunities related to specific hardware such as shared memory or 
specific interconnects.

# Statement of need

`Gala` is an Astropy-affiliated Python package for galactic dynamics. Python
enables wrapping low-level languages (e.g., C) for speed without losing
flexibility or ease-of-use in the user-interface. The API for `Gala` was
designed to provide a class-based and user-friendly interface to fast (C or
Cython-optimized) implementations of common operations such as gravitational
potential and force evaluation, orbit integration, dynamical transformations,
and chaos indicators for nonlinear dynamics. `Gala` also relies heavily on and
interfaces well with the implementations of physical units and astronomical
coordinate systems in the `Astropy` package

`Gala` was designed to be used by both astronomical researchers and by
students in courses on gravitational dynamics or astronomy. It has already been
used in a number of scientific publications  and has also been
used in graduate courses on Galactic dynamics to, e.g., provide interactive
visualizations of textbook material . The combination of speed,
design, and support for Astropy functionality in `Gala` will enable exciting
scientific explorations of forthcoming data releases from the *Gaia* mission
by students and experts alike.

# Mathematics

Single dollars ($) are required for inline mathematics e.g. $f(x) = e^{\pi/x}$

Double dollars make self-standing equations:

$$\Theta(x) = \left\{\begin{array}{l}
0\textrm{ if } x < 0\cr
1\textrm{ else}
\end{array}\right.$$

You can also use plain \LaTeX for equations
\begin{equation}\label{eq:fourier}
\hat f(\omega) = \int_{-\infty}^{\infty} f(x) e^{i\omega x} dx
\end{equation}
and refer to \autoref{eq:fourier} from text.

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


# Figures

Figures \autoref{fig:prif-stack} depicts a Fortran software stack in which a parallel runtime library such as Caffeine supportes compiled Fortran code by implementing PRIF.

![The parallel Fortran software stack enabled by the Caffeine parallel runtime PRIF implementation.\label{fig:prif-stack}](PRIF-software-stack-with-more.pdf){ width=100% }

# Acknowledgements

The Computer Languages and Systems Software (CLaSS) Group at Berkeley Lab [@class-site]
has developed Caffeine on funding from the Exascale Computing Project (ECP) and the Stewardship for Programming Systems and Tools (S4PST) project.

# References
