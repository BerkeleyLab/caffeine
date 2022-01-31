module caf_co_max_test
    use caffeine_m, only : caf_co_max, caf_num_images
    use vegetables, only: result_t, test_item_t, assert_equals, describe, it, assert_that, assert_equals
    use image_enumeration_m, only : caf_this_image, caf_num_images

    implicit none
    private
    public :: test_caf_co_max

contains
    function test_caf_co_max() result(tests)
        type(test_item_t) tests
    
        tests = describe( &
          "The caf_co_max subroutine computes the maximum", &
          [ it("default integer scalar with stat argument present", max_default_integer_scalars) &
           ,it("integer(c_int64_t) scalar with no optional arguments present", max_c_int64_scalars) &
           ,it("default integer 1D array elements with no optional arguments present", max_default_integer_1D_array) &
           ,it("default integer 7D array elements with stat argument present", max_default_integer_7D_array) &
           ,it("default real scalars with stat argument present", max_default_real_scalars) &
           ,it("double precision 2D array elements with no optional arguments present", max_double_precision_2D_array) &
           ,it("reverse-alphabetizes length-5 default character scalars with no optional arguments", &
               reverse_alphabetize_default_character_scalars) &
          !,it("character 2D array elements", max_double_precision_2D_array) &
        ])
    end function

    function max_default_integer_scalars() result(result_)
        type(result_t) result_
        integer i, status_
 
        status_ = -1
        i = -caf_this_image()
        call caf_co_max(i, stat=status_)
        result_ = assert_equals(-1, i) .and. assert_equals(0, status_)
    end function

    function max_c_int64_scalars() result(result_)
        use iso_c_binding, only : c_int64_t 
        type(result_t) result_
        integer(c_int64_t) i
 
        i = caf_this_image()
        call caf_co_max(i)
        result_ = assert_equals(caf_num_images(), int(i))
    end function

    function max_default_integer_1D_array() result(result_)
        type(result_t) result_
        integer i
        integer, allocatable :: array(:)
 
        associate(sequence_ => caf_this_image()*[(i, i=1, caf_num_images())])
          array = sequence_
          call caf_co_max(array)
          associate(max_sequence => caf_num_images()*[(i, i=1, caf_num_images())])
            result_ = assert_that(all(max_sequence == array))
          end associate
        end associate
    end function

    function max_default_integer_7D_array() result(result_)
        type(result_t) result_
        integer array(2,1,1, 1,1,1, 2), status_
 
        status_ = -1
        array = 3 + caf_this_image()
        call caf_co_max(array, stat=status_)
        result_ = assert_that(all(array == 3+caf_num_images())) .and. assert_equals(0, status_)
    end function

    function max_default_real_scalars() result(result_)
        type(result_t) result_
        real scalar
        real, parameter :: pi = 3.141592654
        integer status_

        status_ = -1
        scalar = -pi*caf_this_image()
        call caf_co_max(scalar, stat=status_)
        result_ = assert_equals(-dble(pi), dble(scalar) ) .and. assert_equals(0, status_)
    end function

    function max_double_precision_2D_array() result(result_)
        type(result_t) result_
        double precision, allocatable :: array(:,:)
        double precision, parameter :: tent(*,*) = dble(reshape(-[0,1,2,3,2,1], [3,2]))
 
        array = tent*dble(caf_this_image())
        call caf_co_max(array)
        result_ = assert_that(all(array==tent))
    end function

    function reverse_alphabetize_default_character_scalars() result(result_)
      type(result_t) result_
      character(len=*), parameter :: words(*) = [character(len=len("loddy")):: "loddy","doddy","we","like","to","party"]
      character(len=:), allocatable :: my_word

      associate(me => caf_this_image())
        associate(periodic_index => 1 + mod(me-1,size(words)))
          my_word = words(periodic_index)
          call caf_co_max(my_word)
        end associate
      end associate

      result_ = assert_equals(maxval(words), my_word)
    end function

end module caf_co_max_test
