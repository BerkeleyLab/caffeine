module caf_co_max_test
    use prif, only : prif_co_max, prif_this_image_no_coarray, prif_num_images
    use veggies, only: result_t, test_item_t, assert_equals, describe, it, assert_that, assert_equals

    implicit none
    private
    public :: test_prif_co_max

contains
    function test_prif_co_max() result(tests)
        type(test_item_t) tests

        tests = describe( &
          "The prif_co_max subroutine computes the maximum", &
          [ it("default integer scalar with stat argument present", max_default_integer_scalars) &
           ,it("integer(c_int64_t) scalar with no optional arguments present", max_c_int64_scalars) &
           ,it("default integer 1D array elements with no optional arguments present", max_default_integer_1D_array) &
           ,it("default integer 7D array elements with stat argument present", max_default_integer_7D_array) &
           ,it("default real scalars with stat argument present", max_default_real_scalars) &
           ,it("double precision 2D array elements with no optional arguments present", max_double_precision_2D_array) &
           ,it("reverse-alphabetizes length-5 default character scalars with no optional arguments", &
               reverse_alphabetize_default_character_scalars) &
           ,it("elements across images with 3D arrays of strings", max_elements_in_3D_string_arrays) &
        ])
    end function

    function max_default_integer_scalars() result(result_)
        type(result_t) result_
        integer i, status_, me

        status_ = -1
        call prif_this_image_no_coarray(this_image=me)
        i = -me
        call prif_co_max(i, stat=status_)
        result_ = assert_equals(-1, i) .and. assert_equals(0, status_)
    end function

    function max_c_int64_scalars() result(result_)
        use iso_c_binding, only : c_int64_t
        type(result_t) result_
        integer(c_int64_t) i
        integer :: me, num_imgs

        call prif_this_image_no_coarray(this_image=me)
        i = me
        call prif_co_max(i)
        call prif_num_images(num_images=num_imgs)
        result_ = assert_equals(num_imgs, int(i))
    end function

    function max_default_integer_1D_array() result(result_)
        type(result_t) result_
        integer i, me, num_imgs
        integer, allocatable :: array(:)

        call prif_this_image_no_coarray(this_image=me)
        call prif_num_images(num_images=num_imgs)
        associate(sequence_ => me*[(i, i=1, num_imgs)])
          array = sequence_
          call prif_co_max(array)
          associate(max_sequence => num_imgs*[(i, i=1, num_imgs)])
            result_ = assert_that(all(max_sequence == array))
          end associate
        end associate
    end function

    function max_default_integer_7D_array() result(result_)
        type(result_t) result_
        integer array(2,1,1, 1,1,1, 2), status_, me, num_imgs

        status_ = -1
        call prif_this_image_no_coarray(this_image=me)
        array = 3 + me
        call prif_co_max(array, stat=status_)
        call prif_num_images(num_images=num_imgs)
        result_ = assert_that(all(array == 3+num_imgs)) .and. assert_equals(0, status_)
    end function

    function max_default_real_scalars() result(result_)
        type(result_t) result_
        real scalar
        real, parameter :: pi = 3.141592654
        integer status_, me

        status_ = -1
        call prif_this_image_no_coarray(this_image=me)
        scalar = -pi*me
        call prif_co_max(scalar, stat=status_)
        result_ = assert_equals(-dble(pi), dble(scalar) ) .and. assert_equals(0, status_)
    end function

    function max_double_precision_2D_array() result(result_)
        type(result_t) result_
        double precision, allocatable :: array(:,:)
        double precision, parameter :: tent(*,*) = dble(reshape(-[0,1,2,3,2,1], [3,2]))
        integer :: me

        call prif_this_image_no_coarray(this_image=me)
        array = tent*dble(me)
        call prif_co_max(array)
        result_ = assert_that(all(array==tent))
    end function

    function max_elements_in_3D_string_arrays() result(result_)
      type(result_t) result_
      character(len=*), parameter :: script(*) = ["To be ","or not","to    ","be.   "]
      character(len=len(script)), dimension(2,1,2) :: scramlet, co_max_scramlet
      integer i, cyclic_permutation(size(script))

      associate(me => this_image())
        associate(cyclic_permutation => [(1 + mod(i-1,size(script)), i=me, me+size(script) )])
          scramlet = reshape(script(cyclic_permutation), shape(scramlet))
        end associate
      end associate

      co_max_scramlet = scramlet
      call prif_co_max(co_max_scramlet, result_image=1)

      block
        integer j, delta_j, num_imgs
        character(len=len(script)) expected_script(size(script)), expected_scramlet(size(scramlet,1),size(scramlet,2))

        call prif_num_images(num_images=num_imgs)
        do j=1, size(script)
          expected_script(j) = script(j)
          do delta_j = 1, max(num_imgs-1, size(script))
            associate(periodic_index => 1 + mod(j+delta_j-1, size(script)))
              expected_script(j) = max(expected_script(j), script(periodic_index))
            end associate
          end do
        end do
        expected_scramlet = reshape(expected_script, shape(expected_scramlet))

        result_ =  assert_that(all(scramlet == co_max_scramlet),"all(scramlet == co_max_scramlet)")
      end block

    end function

    function reverse_alphabetize_default_character_scalars() result(result_)
      type(result_t) result_
      character(len=*), parameter :: words(*) = [character(len=len("loddy")):: "loddy","doddy","we","like","to","party"]
      character(len=:), allocatable :: my_word
      integer :: me, num_imgs

      call prif_this_image_no_coarray(this_image=me)
      associate(periodic_index => 1 + mod(me-1,size(words)))
        my_word = words(periodic_index)
        call prif_co_max(my_word)
      end associate

      call prif_num_images(num_images=num_imgs)
      associate(expected_word => maxval(words(1:min(num_imgs, size(words)))))
        result_ = assert_equals(expected_word, my_word)
      end associate
    end function

end module caf_co_max_test
