Caffeine
========

**CoArray Fortran Framework of Efficient Interfaces to Network Environments**

Caffeine is a parallel runtime library that aims to support Fortran compilers with a programming-model-agnostic application interface to various communication libraries.  Current work is on supporting the Parallel Runtime Interface for Fortran (PRIF) with the [GASNet-EX] exascale-ready networking middleware.  Future plans include support for an alternative Message Passing Interface ([MPI]) back end.

![Caffeine system stack diagram](https://github.com/BerkeleyLab/caffeine/wiki/img/caffeine-stack.gif)

Prerequisites
-------------
### Build prerequisites
The `install.sh` script uses the following packages:
* Fortran and C compilers
* [GASNet-EX] exascale networking middleware
* [Fortran package manager] `fpm`
* [pkg-config]
* [realpath]
* [make]

The script will invoke these if present in a user's `PATH`.
If not present, the script will ask permission to use [Homebrew] to install the relevant package.

Caffeine leverages the following non-parallel features of Fortran to simplify the writing of a portable, compact runtime-library that supports Fortran's parallel features:

| Feature                                   | Introduced in |
|-------------------------------------------|---------------|
| The `iso_c_binding` module                | Fortran 2003  |
| The `contiguous` attribute                | Fortran 2008  |
| Submodule support [1]                     | Fortran 2008  |
| `do concurrent` [2]                       | Fortran 2008  |
| The `ISO_Fortran_binding.h` C header file | Fortran 2018  |
| Assumed-type dummy arguments: `type(*)`   | Fortran 2018  |
| Assumed-rank dummy arguments: `array(..)` | Fortran 2018  |


[1] This feature simplifies development but is not essential to the package

[2] This feature is used to support only `co_reduce` and might become optional in a future release.

Download, build, and run an example
-----------------------------------
```
git clone https://github.com/BerkeleyLab/caffeine.git
cd caffeine
./install.sh
export GASNET_PSHM_NODES=8
FC=<Fortran-compiler-path> CC=<C-compiler-path> CXX=<C++-compiler-path> ./build/run-fpm.sh run --example hello
```

Run tests
---------
```
./build/run-fpm.sh test
```

Implementation Status
--------------------

### Caffeine is an implementation of the [Parallel Runtime Interface for Fortran (PRIF)](#citing-prif-please-use-the-following-publication)

![PRIF system stack diagram](https://github.com/BerkeleyLab/caffeine/wiki/img/prif-stack.gif)

For details on the PRIF features that are implemented, please see the [Implementation Status doc](docs/implementation-status.md).

Publications
------------

### Citing Caffeine? Please use the following publication:

Damian Rouson, Dan Bonachea.   
"[**Caffeine: CoArray Fortran Framework of Efficient Interfaces to Network Environments**](https://github.com/BerkeleyLab/caffeine/wiki/pubs/Caffeine_for_LLVM-2022.pdf)",     
Proceedings of the [Eighth Annual Workshop on the LLVM Compiler Infrastructure in HPC (LLVM-HPC2022)](https://llvm-hpc-2022-workshop.github.io), November 2022.    
Paper: <https://doi.org/10.25344/S4459B>     
[Talk Slides](https://github.com/BerkeleyLab/caffeine/wiki/pubs/Caffeine_for_LLVM-2022-Slides.pdf)

### Citing PRIF? Please use the following publication:

Dan Bonachea, Katherine Rasmussen, Brad Richardson, Damian Rouson.    
"[**Parallel Runtime Interface for Fortran (PRIF): A Multi-Image Solution for LLVM Flang**](https://github.com/BerkeleyLab/caffeine/wiki/pubs/LLVM-HPC24_PRIF.pdf)",     
Proceedings of the [Tenth Annual Workshop on the LLVM Compiler Infrastructure in HPC (LLVM-HPC2024)](https://llvm-hpc-2024-workshop.github.io/), November 2024.    
Paper: <https://doi.org/10.25344/S4N017>    
[Talk Slides](https://github.com/BerkeleyLab/caffeine/wiki/pubs/LLVM-HPC24_PRIF_Slides.pdf)

### PRIF Specification:

Dan Bonachea, Katherine Rasmussen, Brad Richardson, Damian Rouson.    
"[**Parallel Runtime Interface for Fortran (PRIF) Specification, Revision 0.5**](https://github.com/BerkeleyLab/caffeine/wiki/pubs/PRIF_0.5.pdf)",     
Lawrence Berkeley National Laboratory Technical Report (LBNL-2001636), Dec 2024.    
<https://doi.org/10.25344/S4CG6G>

Funding
-------
The Computer Languages and Systems Software ([CLaSS]) Group at [Berkeley Lab] has developed Caffeine development on funding from the Exascale Computing Project ([ECP]) and the Stewardship for Programming Systems and Tools ([S4PST]) project. 

License
-------
See [LICENSE.txt](LICENSE.txt) for usage terms and conditions.

[GASNet-EX]: https://gasnet.lbl.gov
[CLaSS]: https://go.lbl.gov/class
[Berkeley Lab]: https://lbl.gov
[ECP]: https://www.exascaleproject.org
[MPI]: https://www.mpi-forum.org
[S4PST]: https://ornl.github.io/events/s4pst2023/
[Homebrew]: https://brew.sh
[GASNet-EX]: https://gasnet.lbl.gov
[Fortran package manager]: https://github.com/fortran-lang/fpm
[pkg-config]: https://www.freedesktop.org/wiki/Software/pkg-config/
[realpath]: https://man7.org/linux/man-pages/man3/realpath.3.html
[make]: https://www.gnu.org/software/make/
