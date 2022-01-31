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
          [ it("default integer scalar with no optional arguments present", min_default_integer_scalars) &
           ,it("integer(c_int64_t) scalar with stat argument present", min_c_int64_scalars) &
           ,it("default integer 1D array elements without no optional arguments present", min_default_integer_1D_array) &
           ,it("default integer 7D array elements with stat argument present", min_default_integer_7D_array) &
           ,it("default real scalars with no optional arguments present", min_default_real_scalars) &
           ,it("double precision 2D array elements with no optional arguments present)", min_double_precision_2D_array) &
           ,it("alphabetizes length-3 default character scalars with no optional arguments", alphabetize_default_character_scalars)&
          !,it("character 2D array elements (no result_image)", alphabetize_across_1D_arrays) &
        ])
    end function

    function min_default_integer_scalars() result(result_)
        type(result_t) result_
        integer i
 
        i = caf_this_image()
        call caf_co_min(i)
        result_ = assert_equals(1, i)
    end function

    function alphabetize_default_character_scalars() result(result_)
      type(result_t) result_
      character(len=*), parameter :: names(*) = [character(len=len("cat")):: "the","cat","in ","a","top","hat"]
      character(len=:), allocatable :: my_name

      associate(me => caf_this_image())
        associate(periodic_index => 1 + mod(me-1,size(names)))
          my_name = names(periodic_index)
          call caf_co_min(my_name)
        end associate
      end associate

      result_ = assert_equals(minval(names), my_name)
    end function

    function min_c_int64_scalars() result(result_)
        use iso_c_binding, only : c_int64_t 
        type(result_t) result_
        integer(c_int64_t) i
        integer status_
        status_ = -1
 
        i = -caf_this_image()
        call caf_co_min(i, stat=status_)
        result_ = assert_equals(-caf_num_images(), int(i)) .and. assert_equals(0, status_)
    end function

    function min_default_integer_1D_array() result(result_)
        type(result_t) result_
        integer i
        integer, allocatable :: array(:)
 
        associate(sequence_ => [(i*caf_this_image(), i=1, caf_num_images())])
          array = sequence_
          call caf_co_min(array)
          associate(min_sequence_ => [(i, i=1, caf_num_images())])
            result_ = assert_that(all(min_sequence_== array))
          end associate
        end associate
    end function

    function min_default_integer_7D_array() result(result_)
        type(result_t) result_
        integer array(2,1,1, 1,1,1, 2), status_

        status_ = -1
        array = 3 + caf_this_image()
        call caf_co_min(array, stat=status_)
        result_ = assert_that(all(array==4)) .and. assert_equals(0, status_)
    end function

    function min_default_real_scalars() result(result_)
        type(result_t) result_
        real scalar
        real, parameter :: pi = 3.141592654

        scalar = -pi*caf_this_image()
        call caf_co_min(scalar)
        result_ = assert_equals(-dble(pi*caf_num_images()), dble(scalar) )
    end function

    function min_double_precision_2D_array() result(result_)
        type(result_t) result_
        double precision, allocatable :: array(:,:)
        double precision, parameter :: input(*,*) = reshape(-[0,1,2,3,2,1], [3,2])
 
        array = input
        call caf_co_min(array)
        result_ = assert_that(all(input==array))
    end function

   ! function min_default_character_scalars() result(result_)
   !     type(result_t) result_
   !     real scalar
   !     character z
   !     character, parameter :: i=(0.,1)

   !     z = i
   !     call caf_co_min(z)
   !     result_ = assert_equals(dble(abs(i*caf_num_images())), dble(abs(z)) )
   ! end function
   !          
   ! function min_double_precision_character_3D_arrays() result(result_)
   !     type(result_t) result_
   !     integer, parameter :: dp = kind(1.D0)
   !     character(dp), allocatable :: array(:,:,:)
   !     character(dp), parameter :: &
   !       input(*,*,*) = reshape( [(-1.,0.) , (0.0,1.0), (1.0,0.0), (0.0,-1.0), (0.0,0.0), (0.0,1.0)], [3,1,2])
 
   !     array = input
   !     call caf_co_min(array)
   !     result_ = assert_equals(dble(product(abs(input))*caf_num_images()), dble(product(abs(array))))
   ! end function

end module caf_co_min_test
