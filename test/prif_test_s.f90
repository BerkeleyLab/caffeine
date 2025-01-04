! Copyright (c) 2020-2024, The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
submodule(prif_test_m) prif_test_s
  use iso_c_binding, only : c_funloc, c_bool
  use julienne_m, only : command_line_t
  use prif, only : prif_this_image_no_coarray, prif_co_broadcast, prif_co_reduce
  use prif_test_m, only : test_description_substring
  implicit none

contains

  impure elemental subroutine co_all(boolean)
    logical(c_bool), intent(inout) :: boolean
    call prif_co_reduce(boolean, c_funloc(both))
  contains
    pure function both(lhs,rhs) result(lhs_and_rhs)
      logical(c_bool), intent(in) :: lhs, rhs
      logical(c_bool) lhs_and_rhs
      lhs_and_rhs = lhs .and. rhs
    end function
  end subroutine

  module procedure report
    integer me

    call prif_this_image_no_coarray(this_image=me)

    if (me==1) then

      first_report: &
      if (.not. allocated(test_description_substring)) then
        block 
          type(command_line_t) command_line
          test_description_substring = command_line%flag_value("--contains")
        end block
        print *
        if (len(test_description_substring)==0) then
          print '(a)',"Running all tests."
          print '(a)',"(Add '-- --contains <string>' to run only tests with subjects or descriptions containing the specified string.)"
        else
          print '(*(a))',"Running only tests with subjects or descriptions containing '", test_description_substring,"'."
        end if
      end if first_report

      print '(*(a))', new_line(''), test%subject()

    end if

    call prif_co_broadcast(test_description_substring, source_image=1)
      
    associate(test_results => test%results())
      associate(num_tests => size(test_results))
        tests = tests + num_tests
        if (me==1) then
          block
            integer i
            do i=1,num_tests
              print '(3x,a)', test_results(i)%characterize()
            end do
          end block
        end if
        block 
          logical(c_bool), allocatable :: passing_tests(:)
          passing_tests = test_results%passed()
          call co_all(passing_tests)
          associate(num_passes => count(passing_tests))
            if (me==1) print '(a,2(i0,a))'," ",num_passes," of ", num_tests," tests pass."
            passes = passes + num_passes
          end associate
        end block
      end associate
    end associate

  end procedure

end submodule prif_test_s
