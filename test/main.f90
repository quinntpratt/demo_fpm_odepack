module test_demo
    ! we're not testing the main program here, just the my_odepack_demo local module.
    use my_odepack_demo, only : linspace, read_namelist, write_output
    use testdrive, only : error_type, unittest_type, new_unittest, check
    use iso_fortran_env, only: dp => real64, stderr => error_unit

    implicit none
    private

    public :: collect_demo

contains

    !> Collect all exported unit tests
    subroutine collect_demo(testsuite)
        !> Collection of tests
        type(unittest_type), allocatable, intent(out) :: testsuite(:)
        testsuite = [new_unittest("linspace", test_linspace)]
    end subroutine collect_demo

    !> Check linspace
    subroutine test_linspace(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        !> Vars for the test,
        real(dp) :: x(4)  

        !> Call the subroutine,  
        call linspace(0._dp, 1._dp, x)
        !> Summing the array [0, 0.25, 0.5, 1.0] = 2.0 
        call check(error, sum(x), 2.0_dp)
    end subroutine test_linspace

    !> Check read_namelist
    subroutine test_read_namelist(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
    end subroutine test_read_namelist

    !> Check write_output
    subroutine test_write_output(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
    end subroutine test_write_output

    subroutine test_main_program(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
    end subroutine test_main_program

end module test_demo

program tester
  use, intrinsic :: iso_fortran_env, only : error_unit
  use testdrive, only : run_testsuite
  use test_demo, only : collect_demo
  implicit none
  integer :: stat

  stat = 0
  call run_testsuite(collect_demo, error_unit, stat)

  if (stat > 0) then
    write(error_unit, '(i0, 1x, a)') stat, "test(s) failed!"
    error stop
  end if

end program tester