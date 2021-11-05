/*   $Source: bitbucket.org:berkeleylab/gasnet.git/tests/testhello.c $
 * Description: GASNet "Hello, World" test/example
 * Copyright 2010, The Regents of the University of California
 * Terms of use are as specified in license.txt
 */

#include <gasnetex.h>
#include <stdio.h>

#ifndef _GASNET_SAFE_
#include "gasnet_safe.h"
#endif

gex_Client_t myclient;
gex_EP_t myep;
gex_TM_t myteam;
gex_Rank_t rank, size;

void c_caffeinate(int argc, char *argv[])
{
  GASNET_SAFE(gex_Client_Init(&myclient, &myep, &myteam, "caffeine", &argc, &argv, 0));
  
  size_t segsz = GASNET_PAGESIZE;  
  
  int argi = 1;
  if (argi < argc) {
    if (!strcmp(argv[argi], "-m")) {
      segsz = gasnet_getMaxLocalSegmentSize();
    } else {
      size_t tmp = atol(argv[argi]);
      if (tmp) segsz = tmp;
    }
    ++argi;
  }
    
  gex_Segment_t mysegment;
  GASNET_SAFE(gex_Segment_Attach(&mysegment, myteam, segsz));
}

void c_decaffeinate(int exit_code)
{
  gasnet_exit(exit_code);
}

int c_this_image()
{
  return gex_TM_QueryRank(myteam) + 1;
}

int c_num_images()
{
  return gex_TM_QuerySize(myteam);
}

void c_sync_all()
{
  gasnet_barrier_notify(0,GASNET_BARRIERFLAG_ANONYMOUS);
  gasnet_barrier_wait(0,GASNET_BARRIERFLAG_ANONYMOUS);
}
