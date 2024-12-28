module caf_co_min_test
    use prif, only : prif_co_min, prif_num_images, prif_this_image_no_coarray, prif_num_images
    use veggies, only: result_t, test_item_t, assert_equals, describe, it, assert_that, assert_equals, succeed

    implicit none
    private
    public :: test_prif_co_min

contains
    function test_prif_co_min() result(tests)
        type(test_item_t) tests

        tests = describe( &
          "The prif_co_min subroutine computes the minimum", &
          [ it("default integer scalar with stat argument present", min_default_integer_scalars) &
           ,it("integer(c_int64_t) scalar with no optional arguments present", min_c_int64_scalars) &
           ,it("default integer 1D array elements with no optional arguments present", min_default_integer_1D_array) &
           ,it("default integer 7D array elements with stat argument present", min_default_integer_7D_array) &
           ,it("default real scalars with stat argument present", min_default_real_scalars) &
           ,it("double precision 2D array elements with no optional arguments present", min_double_precision_2D_array) &
           ,it("length-5 string with no optional arguments", &
               alphabetically_1st_scalar_string) &
           ,it("elements across images with 2D arrays of strings", min_elements_in_2D_string_arrays) &
        ])
    end function

    function min_default_integer_scalars() result(result_)
        type(result_t) result_
        integer i, status_, me, num_imgs

        status_ = -1
        call prif_this_image_no_coarray(this_image=me)
        i = -me
        call prif_co_min(i, stat=status_)
        call prif_num_images(num_images=num_imgs)
        result_ = assert_equals(-num_imgs, i) .and. assert_equals(0, status_)
    end function

    function min_c_int64_scalars() result(result_)
        use iso_c_binding, only : c_int64_t
        type(result_t) result_
        integer(c_int64_t) i
        integer :: me

        call prif_this_image_no_coarray(this_image=me)
        i = me
        call prif_co_min(i)
        result_ = assert_equals(1, int(i))
    end function

    function min_default_integer_1D_array() result(result_)
        type(result_t) result_
        integer i, me, num_imgs
        integer, allocatable :: array(:)

        call prif_this_image_no_coarray(this_image=me)
        call prif_num_images(num_images=num_imgs)
        associate(sequence_ => me*[(i, i=1, num_imgs)])
          array = sequence_
          call prif_co_min(array)
          associate(min_sequence => [(i, i=1, num_imgs)])
            result_ = assert_that(all(min_sequence == array))
          end associate
        end associate
    end function

    function min_default_integer_7D_array() result(result_)
        type(result_t) result_
        integer array(2,1,1, 1,1,1, 2), status_, me, num_imgs

        status_ = -1
        call prif_this_image_no_coarray(this_image=me)
        array = 3 - me
        call prif_co_min(array, stat=status_)
        call prif_num_images(num_images=num_imgs)
        result_ = assert_that(all(array == 3 - num_imgs)) .and. assert_equals(0, status_)
    end function

    function min_default_real_scalars() result(result_)
        type(result_t) result_
        real scalar
        real, parameter :: pi = 3.141592654
        integer status_, me, num_imgs

        status_ = -1
        call prif_this_image_no_coarray(this_image=me)
        scalar = -pi*me
        call prif_co_min(scalar, stat=status_)
        call prif_num_images(num_images=num_imgs)
        result_ = assert_equals(-dble(pi*num_imgs), dble(scalar) ) .and. assert_equals(0, status_)
    end function

    function min_double_precision_2D_array() result(result_)
        type(result_t) result_
        double precision, allocatable :: array(:,:)
        double precision, parameter :: tent(*,*) = dble(reshape(-[0,1,2,3,2,1], [3,2]))
        integer :: me, num_imgs

        call prif_this_image_no_coarray(this_image=me)
        array = tent*dble(me)
        call prif_co_min(array)
        call prif_num_images(num_images=num_imgs)
        result_ = assert_that(all(array==tent*num_imgs))
    end function

    function min_elements_in_2D_string_arrays() result(result_)
      type(result_t) result_
      character(len=*), parameter :: script(*,*,*) = reshape( &
          [ "To be   ","or not  "   & ! images with odd image
          , "to      ","be.     "   & ! numbers get this slice
                                      ! ----------------------
          , "that    ","is      "   & ! images with even image
          , "the     ","question"], & ! numbers get this slice
          [2,2,2])
      character(len=len(script)), dimension(size(script,1),size(script,2)) :: slice
      integer me, ni

      call prif_this_image_no_coarray(this_image=me)
      call prif_num_images(ni)
      associate(slice_number => mod(me-1,size(script,3)) + 1)
        slice = script(:,:,slice_number)
      end associate
      call prif_co_min(slice)
      associate(expected => minval(script(:,:,1:min(ni,size(script,3))), dim=3))
        result_ = assert_that(all(expected == slice),"all(expected == scramlet)")
      end associate
    end function

    function alphabetically_1st_scalar_string() result(result_)
      type(result_t) result_
      integer, parameter :: length = len("to party!")
      character(len=length), parameter :: words(*) = [character(len=length):: "Loddy","doddy","we","like","to party!"]
      character(len=length) :: my_word, expected_word
      integer :: me, num_imgs

      call prif_this_image_no_coarray(this_image=me)
      associate(periodic_index => 1 + mod(me-1,size(words)))
        my_word = words(periodic_index)
        call prif_co_min(my_word)
      end associate

      call prif_num_images(num_images=num_imgs)
      expected_word = minval(words(1:min(num_imgs, size(words))), dim=1)
      result_ = assert_equals(expected_word, my_word)
    end function

end module caf_co_min_test
