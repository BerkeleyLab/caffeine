! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
submodule(prif:prif_private_s) program_startup_s
  ! DO NOT ADD USE STATEMENTS HERE
  ! All use statements belong in prif_private_s.F90
  implicit none
contains

  module procedure prif_init
    use ieee_arithmetic, only: ieee_inexact, ieee_set_flag
    logical, save :: prif_init_called_previously = .false.

    if (prif_init_called_previously) then
       stat = PRIF_STAT_ALREADY_INIT
    else
       call caf_caffeinate( &
          total_heap_size, &
          initial_team%heap_mspace, &
          initial_team%heap_start, &
          initial_team%heap_size, &
          non_symmetric_heap_mspace, &
          initial_team%gex_team)
       call assert_init()
       current_team%info => initial_team
       initial_team%parent_team => initial_team
       initial_team%team_number = -1
       initial_team%this_image = caf_this_image(initial_team%gex_team)
       initial_team%num_images = caf_num_images(initial_team%gex_team)
       non_symmetric_heap_size = total_heap_size - initial_team%heap_size

       call sync_init()

       ! issue #259: Ensure we clear any IEEE FP exceptions potentially
       ! signalled from within the C-based initialization code above
       call ieee_set_flag(ieee_inexact, .false.)

       prif_init_called_previously = .true.
       stat = 0
    end if
  end procedure

#if ASSERT_PARALLEL_CALLBACKS
    subroutine assert_init()
      implicit none
      assert_this_image => assert_callback_this_image
      assert_error_stop => assert_callback_error_stop
    end subroutine
    pure function assert_callback_this_image() result(this_image_id)
      implicit none
      integer :: this_image_id
    
      this_image_id = initial_team%this_image
    end function
    
    pure subroutine assert_callback_error_stop(stop_code_char)
      implicit none
      character(len=*), intent(in) :: stop_code_char
      character(len=:), allocatable, target :: tmp
      tmp = stop_code_char
    
      call caf_fatal_error(tmp)
    end subroutine
#else 
    subroutine assert_init()  
    end subroutine     
#endif

end submodule program_startup_s
