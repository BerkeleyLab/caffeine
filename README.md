Caffeine
========

**CoArray Fortran Framework of Efficient Interfaces to Network Environments**

Caffeine is a parallel runtime library that aims to support Fortran compilers with a programming-model-agnostic application binary interface (ABI) to various communication libraries.  Current work is on supporting the ABI with the [GASNet-EX] or with POSIX processes.  Future plans include support for an alternative MPI back end.

```

                    .
                        `:.
                          `:.
                  .:'     ,::
                 .:'      ;:'
                 ::      ;:'
                  :    .:'
                   `.  :.
          _________________________
         : _ _ _ _ _ _ _ _ _ _ _ _ :
     ,---:".".".".".".".".".".".".":
    : ,'"`::.:.:.:.:.:.:.:.:.:.:.::'
    `.`.  `:-===-===-===-===-===-:'
      `.`-._:                   :
        `-.__`.               ,' 
    ,--------`"`-------------'--------.
     `"--.__                   __.--"'
            `""-------------""'
```

Download, build, and run an example
-----------------------------------
```
git clone https://gitlab.lbl.gov/rouson/caffeine
cd caffeine
./install.sh
export GASNET_PSHM_NODES=8
./build/run-fpm.sh run --example hello
```

Run tests
---------
```
./build/run-fpm.sh test
```

Generate documentation
----------------------
```
ford doc-generator.md
```
Open `doc/html/index.htmtl` in a web browser.

Art from [ascii.co.uk](https://ascii.co.uk/art/cup).

[GASNet-EX]: https://gasnet.lbl.gov

Copyright Notice
----------------

Caffeine Copyright (c) 2021-2022, The Regents of the University of California,
through Lawrence Berkeley National Laboratory (subject to receipt of
any required approvals from the U.S. Dept. of Energy), Archaeologic Inc., 
and Harris Snyder. All rights reserved.

If you have questions about your rights to use or distribute this software,
please contact Berkeley Lab's Intellectual Property Office at
IPO@lbl.gov.

NOTICE.  This Software was developed under funding from the U.S. Department
of Energy and the U.S. Government consequently retains certain rights.  As
such, the U.S. Government has been granted for itself and others acting on
its behalf a paid-up, nonexclusive, irrevocable, worldwide license in the
Software to reproduce, distribute copies to the public, prepare derivative 
works, and perform publicly and display publicly, and to permit others to do so.
