Caffeine
========

**CoArray Fortran Framework of Efficient Interfaces to Network Environments**

Caffeine is a parallel runtime library that aims to support Fortran compilers
with a programming-model-agnostic application interface to various
communication libraries.  Current work is on supporting the Parallel Runtime
Interface for Fortran (PRIF) with the [GASNet-EX] exascale-ready networking
middleware.  Future plans include support for an alternative Message Passing
Interface ([MPI]) back end.

![Caffeine system stack diagram](https://github.com/BerkeleyLab/caffeine/wiki/img/caffeine-stack.gif)

Statement of need
-----------------

The Fortran programming language standard added features supporting
single-program, multiple-data (SPMD) parallel programming and loop
parallelism beginning with Fortran 2008.  In Fortran, SPMD programming
involves the creation of a fixed number of images (instances) of a
program that execute asynchronously in shared or distributed memory, except
where a program uses specific synchronization mechanisms.  Fortran's
coarray feature provides distributed data structures that offer a subscripted,
multidimensional array notation defining a partitioned global address space
(PGAS). One image can use the cosubscript notation to perform one-sided access
of coarray data associated with another image.

Fortran 2018 greatly expanded this feature set to include such concepts as
teams (groupings) of images, events (counting semaphores), collective
subroutines and failed-image detection (fault tolerance). Fortran 2023 provided
additional, minor multi-image extensions, including notified remote data access.

Several popular Fortran compilers, including LLVM Flang and LFortran, currently
lack complete support for multi-image parallel execution. These features are a mandatory
part of Fortran, and thus are an important part of reaching full compliance with
the 2008, 2018, or 2023 versions of the Fortran standard. Thanks to PRIF and Caffeine,
the forthcoming LLVM Flang 22 release is expected to support a meaningful subset of
multi-image Fortran features. For more details, see
[LLVM-HPC2025 paper](#Additional-Publications) below.

Caffeine provides a portable, high-performance and open-source parallel
runtime library that such compilers can target in code generation as part of
their solution to support Fortran's multi-image parallel features.

Prerequisites & Dependencies
-------------
### Build prerequisites
The `install.sh` script uses the following packages:
* Fortran and C compilers
    * We regularly test with: GNU Fortran versions 13, 14 and LLVM Flang versions 19, 20, 21
    * On macOS the Homebrew-installed `llvm` and `flang` packages may require some
      additional settings, see [issue #228](https://github.com/BerkeleyLab/caffeine/issues/228) for the latest information.
* [Fortran package manager] `fpm`
* [pkg-config]
* [realpath]
* [make]
* [git]
* [curl]

The script will invoke these if present in a user's `PATH`.
If not present, the script will ask permission to use [Homebrew] to install the relevant package
or, in some cases, ask the user to install the package.

### Build dependencies

Caffeine also depends on the following packages that will be automatically installed as part
of the build process.

* [GASNet-EX] exascale networking middleware
* [assert](https://github.com/BerkeleyLab/assert)
* [veggies](https://github.com/everythingfunctional/veggies)
* [iso_varying_string](https://github.com/everythingfunctional/iso_varying_string)

Caffeine leverages the following non-parallel features of Fortran to simplify the writing of a portable, compact runtime-library that supports Fortran's parallel features:

| Feature                                   | Introduced in |
|-------------------------------------------|---------------|
| The `iso_c_binding` module                | Fortran 2003  |
| The `contiguous` attribute                | Fortran 2008  |
| Submodule support [1]                     | Fortran 2008  |
| The `ISO_Fortran_binding.h` C header file | Fortran 2018  |
| Assumed-type dummy arguments: `type(*)`   | Fortran 2018  |
| Assumed-rank dummy arguments: `array(..)` | Fortran 2018  |


[1] This feature simplifies development but is not essential to the package

Download, build, and run an example
-----------------------------------
Here is an outline of the basic commands used to build Caffeine and run an example:

```
git clone https://github.com/BerkeleyLab/caffeine.git
cd caffeine
env FC=<Fortran-compiler> CC=<C-compiler> CXX=<C++-compiler> ./install.sh <options>
env CAF_IMAGES=8 ./build/run-fpm.sh run --example hello
```

The provided compilers MUST be "compatible": for the best experience you are
HIGHLY recommended to specify the language frontends provided by a single version
of a given compiler suite installation. The C++ compiler is optional for
single-node deployments (and can be disabled using command-line option `--without-cxx`), 
but C++ is required for some network backends.

The `install.sh` recognizes a number of command-line options and environment variables to
customize behavior for your system. See the output of `./install.sh --help` for full documentation,
including options for how to build for a distributed-memory platform.


Example Usage
-------------
The Caffeine parallel runtime is intended as an embedded compilation target
library, to provide multi-image parallel runtime support to a Fortran compiler.
As such, real usage of Caffeine is specific to the host Fortran compiler, and
one should consult compiler-provided documentation regarding the use of Caffeine
to back multi-image features.

However we provide an [example hello world program](example/hello.F90), 
written in Fortran, simulating the PRIF calls that a theoretical
source-to-source Fortran compiler might generate for a simple program written
using Fortran's multi-image features to print a message from each image.

Run tests
---------
```
./build/run-fpm.sh test
```

Recognized Environment Variables
--------------------------------

The following environment variables control the execution of the `fpm`-driven Caffeine unit test suite:

* `CAF_IMAGES`: integer that indicates the number of images to run
* `SUBJOB_PREFIX`: command prefix to use for recursive `fpm` invocations in the test suite. 
   Set `SUBJOB_PREFIX=skip` to disable such invocations (recommended for distributed-memory systems).

The following environment variables control the behavior of the Caffeine library:

* `CAF_HEAP_SIZE=128MB`: set the size of the shared-memory heap used for coarray storage, defaults to 128 MiB
* `CAF_COMP_FRAC=0.10`: set the fraction of the shared-memory heap reserved for non-symmetric allocation, defaults to 10%

Caffeine is built atop the [GASNet-EX] exascale networking middleware, which has its own
set of environment variable knobs to control network-level behavior. 
Here are *a few* of the most useful GASNet knobs:

* `GASNET_VERBOSEENV=1`: enable console output of all the envvar settings affecting GASNet operation
* `GASNET_SPAWN_VERBOSE=1`: enable verbose console output of parallel job-spawning steps
* `GASNET_BACKTRACE=1`: enable automatic backtrace upon fatal errors
* `GASNET_SSH_SERVERS="host1 host2"`: space-deliminted list of hostnames for distributed-memory job launch using the ssh-spawner

See [GASNet documentation](https://gasnet.lbl.gov/dist-ex/README) for full details on all settings.

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
Proceedings of the [Eighth Annual Workshop on the LLVM Compiler Infrastructure in HPC (LLVM-HPC2022)](https://web.archive.org/web/20230605003029/https://llvm-hpc-2022-workshop.github.io/), November 2022.    
Paper: <https://doi.org/10.25344/S4459B>     
[Talk Slides](https://github.com/BerkeleyLab/caffeine/wiki/pubs/Caffeine_for_LLVM-2022-Slides.pdf)

### Citing PRIF? Please use the following publication:

Dan Bonachea, Katherine Rasmussen, Brad Richardson, Damian Rouson.    
"[**Parallel Runtime Interface for Fortran (PRIF): A Multi-Image Solution for LLVM Flang**](https://github.com/BerkeleyLab/caffeine/wiki/pubs/LLVM-HPC24_PRIF.pdf)",     
Proceedings of the [Tenth Annual Workshop on the LLVM Compiler Infrastructure in HPC (LLVM-HPC2024)](https://web.archive.org/web/20241006163246/https://llvm-hpc-2024-workshop.github.io/), November 2024.    
Paper: <https://doi.org/10.25344/S4N017>    
[Talk Slides](https://github.com/BerkeleyLab/caffeine/wiki/pubs/LLVM-HPC24_PRIF_Slides.pdf)

### PRIF Specification:

PRIF Committee.    
"[**Parallel Runtime Interface for Fortran (PRIF) Specification, Revision 0.6**](https://github.com/BerkeleyLab/caffeine/wiki/pubs/PRIF_0.6.pdf)",     
Lawrence Berkeley National Laboratory Technical Report (LBNL-2001698), Sept 2025.    
<https://doi.org/10.25344/S4M01X>

### Additional Publications:

Dan Bonachea, Katherine Rasmussen, Damian Rouson, Jean-Didier Pailleux, Etienne Renault, Brad Richardson.    
"[**Lowering and Runtime Support for Fortran’s Multi-Image Parallel Features using LLVM Flang, PRIF, and Caffeine**](https://doi.org/10.25344/S4G883)",    
Proceedings of the [Eleventh Annual Workshop on the LLVM Compiler Infrastructure in HPC (LLVM-HPC2025)](https://llvm-in-hpc-workshop.github.io/LLVM-HPC-2025-Workshop.github.io/), November 2025.    
Paper: <https://doi.org/10.25344/S4G883>

Funding
-------
The Computer Languages and Systems Software ([CLaSS]) Group at [Berkeley Lab] has developed Caffeine 
on funding from the Exascale Computing Project ([ECP](https://www.exascaleproject.org)) 
and the Stewardship for Programming Systems and Tools ([S4PST](https://ornl.github.io/events/s4pst2023/)) project,
part of the Consortium for the Advancement of Scientific Software ([CASS](https://cass.community/)).

Support and Licensing
---------------------
See [CONTRIBUTING.md](CONTRIBUTING.md) for guidelines on reporting defects, feature requests and contributing to Caffeine.

See [LICENSE.txt](LICENSE.txt) for usage terms and conditions.

[GASNet-EX]: https://gasnet.lbl.gov
[CLaSS]: https://go.lbl.gov/class
[Berkeley Lab]: https://lbl.gov
[MPI]: https://www.mpi-forum.org
[Homebrew]: https://brew.sh
[Fortran package manager]: https://github.com/fortran-lang/fpm
[pkg-config]: https://www.freedesktop.org/wiki/Software/pkg-config/
[realpath]: https://man7.org/linux/man-pages/man3/realpath.3.html
[make]: https://www.gnu.org/software/make/
[git]: https://git-scm.com
[curl]: https://curl.se

