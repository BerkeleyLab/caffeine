/*   $Source: bitbucket.org:berkeleylab/gasnet.git/tests/testhello.c $
 * Description: GASNet "Hello, World" test/example
 * Copyright 2010, The Regents of the University of California
 * Terms of use are as specified in license.txt
 */

#include <gasnetex.h>
#include <stdio.h>
#include "hello.h"

#ifndef _GASNET_SAFE_
#include "gasnet_safe.h"
#endif

gex_Client_t myclient;
gex_EP_t myep;
gex_TM_t myteam;

int c_caffeinate(int argc, char *argv[])
{
  GASNET_SAFE(gex_Client_Init(&myclient, &myep, &myteam, "caffeine", &argc, &argv, 0));

  hello(argc, argv, myep, myteam);
}

int c_decaffeinate(int exit_code)
{
  gasnet_exit(exit_code);
}
