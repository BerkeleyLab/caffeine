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

See [LICENSE.txt](LICENSE.txt) for usage terms and conditions.

