! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
submodule(rma_m) rma_s
  use caffeine_assert_m, only : assert

  implicit none

contains

  module procedure caf_put

    call assert(.not.(present(team) .and. present(team_number)), "caf_put: both team and team_number are present")

    ! assert that the size of coindices array needs to match the corank of the coarray

    ! assert that target and value need to be of the same type
    ! assert that target and value need to have the same shape (rank, size of each dimension)
    ! strides may be different for target and value

    ! values of the coindices must be within the bounds of the codimensions

    ! to do bounds checking, need to do translation between team and the team where the coarray was established
    ! need to set up coindices with respect to the provided team

    ! need to figure out which image we are dealing with
    ! caffeine should already have the coshape information
  end procedure

  module procedure caf_get

  end procedure

end submodule rma_s
