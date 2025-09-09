program hello_world
  use iso_c_binding, only: c_bool
  use iso_fortran_env, only: output_unit,error_unit
  use prif, only : &
     prif_init &
    ,prif_this_image_no_coarray &
    ,prif_num_images &
    ,prif_stop &
    ,prif_error_stop &
    ,prif_sync_all &
    ,PRIF_STAT_ALREADY_INIT
  implicit none

  integer :: init_exit_code, me, num_imgs, exitcase = 1
  logical(kind=c_bool), parameter :: false = .false._c_bool, true = .true._c_bool
  character(len=256) :: arg_string

  call prif_init(init_exit_code)
  if (init_exit_code /= 0 .and. init_exit_code /= PRIF_STAT_ALREADY_INIT) then
    call prif_error_stop(quiet=false, stop_code_char="program startup failed")
  end if

  call prif_this_image_no_coarray(this_image=me)
  call prif_num_images(num_images=num_imgs)
  if (command_argument_count() > 0) then
    call get_command_argument(1, arg_string)
    read(arg_string, *) exitcase
  end if
  if (me == 1) write(output_unit,*) "testing exit case ", exitcase
  
  call prif_sync_all()

  write(output_unit,'(A,I1,A,I1)') "stdout from image ", me, " of ", num_imgs
  write(error_unit,'(A,I1,A,I1)')  "stderr from image ", me, " of ", num_imgs

  call prif_sync_all()

  select case (exitcase) 
  case (1)
    call prif_stop(quiet=true, stop_code_int=exitcase+100)
  case (2)
    call prif_stop(quiet=false, stop_code_int=exitcase+100)
  case (3)
    if (me == num_imgs) call prif_error_stop(quiet=true, stop_code_int=exitcase+100)
  case default
    if (me == num_imgs) call prif_error_stop(quiet=false, stop_code_int=exitcase+100)
  end select

  call prif_sync_all()

end program
