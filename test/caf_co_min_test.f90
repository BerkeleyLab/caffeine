module caf_co_min_test
    use caffeine_m, only : caf_co_min, caf_num_images
    use vegetables, only: result_t, test_item_t, assert_equals, describe, it, assert_that, assert_equals
    use image_enumeration_m, only : caf_this_image, caf_num_images

    implicit none
    private
    public :: test_caf_co_min

contains
    function test_caf_co_min() result(tests)
        type(test_item_t) tests
    
        tests = describe( &
          "The caf_co_min subroutine computes the minimum", &
          [ it("default integer scalar with stat argument present", min_default_integer_scalars) &
           ,it("integer(c_int64_t) scalar with no optional arguments present", min_c_int64_scalars) &
           ,it("default integer 1D array elements with no optional arguments present", min_default_integer_1D_array) &
           ,it("default integer 7D array elements with stat argument present", min_default_integer_7D_array) &
           ,it("default real scalars with stat argument present", min_default_real_scalars) &
           ,it("double precision 2D array elements with no optional arguments present", min_double_precision_2D_array) &
          !,it("alphabetizes length-5 default character scalars with no optional arguments", &
          !    alphabetize_default_character_scalars) &
          !,it("character 2D array elements", min_double_precision_2D_array) &
        ])
    end function

    function min_default_integer_scalars() result(result_)
        type(result_t) result_
        integer i, status_
 
        status_ = -1
        i = -caf_this_image()
        call caf_co_min(i, stat=status_)
        result_ = assert_equals(-caf_num_images(), i) .and. assert_equals(0, status_)
    end function

    function min_c_int64_scalars() result(result_)
        use iso_c_binding, only : c_int64_t 
        type(result_t) result_
        integer(c_int64_t) i
 
        i = caf_this_image()
        call caf_co_min(i)
        result_ = assert_equals(1, int(i))
    end function

    function min_default_integer_1D_array() result(result_)
        type(result_t) result_
        integer i
        integer, allocatable :: array(:)
 
        associate(sequence_ => caf_this_image()*[(i, i=1, caf_num_images())])
          array = sequence_
          call caf_co_min(array)
          associate(min_sequence => [(i, i=1, caf_num_images())])
            result_ = assert_that(all(min_sequence == array))
          end associate
        end associate
    end function

    function min_default_integer_7D_array() result(result_)
        type(result_t) result_
        integer array(2,1,1, 1,1,1, 2), status_
 
        status_ = -1
        array = 3 - caf_this_image()
        call caf_co_min(array, stat=status_)
        result_ = assert_that(all(array == 3 - caf_num_images())) .and. assert_equals(0, status_)
    end function

    function min_default_real_scalars() result(result_)
        type(result_t) result_
        real scalar
        real, parameter :: pi = 3.141592654
        integer status_

        status_ = -1
        scalar = -pi*caf_this_image()
        call caf_co_min(scalar, stat=status_)
        result_ = assert_equals(-dble(pi*caf_num_images()), dble(scalar) ) .and. assert_equals(0, status_)
    end function

    function min_double_precision_2D_array() result(result_)
        type(result_t) result_
        double precision, allocatable :: array(:,:)
        double precision, parameter :: tent(*,*) = dble(reshape(-[0,1,2,3,2,1], [3,2]))
 
        array = tent*dble(caf_this_image())
        call caf_co_min(array)
        result_ = assert_that(all(array==tent))
    end function

    function alphabetize_default_character_scalars() result(result_)
      type(result_t) result_
      character(len=*), parameter :: words(*) = [character(len=len("loddy")):: "loddy","doddy","we","like","to","party"]
      character(len=:), allocatable :: my_word

      associate(me => caf_this_image())
        associate(periodic_index => 1 + mod(me-1,size(words)))
          my_word = words(periodic_index)
          call caf_co_min(my_word)
        end associate
      end associate

      associate(expected_word => minval(words(1:min(caf_num_images(), size(words)))))
        result_ = assert_equals(expected_word, my_word)
      end associate
    end function

end module caf_co_min_test
