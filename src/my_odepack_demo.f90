module my_odepack_demo
  use iso_fortran_env, only: dp => real64, stderr => error_unit
  implicit none
  private

  public :: linspace, read_namelist, write_output
contains

  subroutine linspace(from, to, array)
    real(dp), intent(in) :: from, to
    real(dp), intent(out) :: array(:)
    real(dp) :: range
    integer :: n, i
    n = size(array)
    range = to - from

    if (n == 0) return

    if (n == 1) then
      array(1) = from
      return
    end if

    do i=1, n
      array(i) = from + range * (i - 1) / (n - 1)
    end do
  end subroutine

  subroutine read_namelist(file_path, NEQ, N, t0, tf, rtol, atol)
      ! reads a fortran .nml namelist from file.
      implicit none
      character(len=*), intent(in) :: file_path ! arb. length filename char.
      integer :: fid, rc ! integers for file ID and read-condition.
      ! variables from namelist,
      integer, intent(inout) :: NEQ, N
      real(dp), intent(inout) :: t0, tf, rtol, atol(1)
      logical :: here

      ! Namelist definition,
      namelist /PARAMS/ NEQ, N, t0, tf
      namelist /NUMERICAL/ rtol, atol

      ! Check whether file exists.
      inquire (file=file_path, exist=here) 
      if (.not.here) then
          write (stderr, '("ERROR: input file ", a, " does not exist")') file_path
          error stop
      end if

      ! Open and read Namelist file.
      open (action='read', file=file_path, iostat=rc, newunit=fid)
      read (nml=PARAMS, iostat=rc, unit=fid)
      ! iostat = -1 just means end of file.
      if (rc /= -1 .and. rc /= 0) write(*,*) "ERROR: I/O error during PARAMS namelist-read. iostat=", rc

      read (nml=NUMERICAL, iostat=rc, unit=fid)
      if (rc /= -1 .and. rc /= 0) write(*,*) "ERROR: I/O error during NUMERICAL namelist-read. iostat=", rc

      close (fid)
  end subroutine read_namelist

  subroutine write_output(file_path, output)
      implicit none
      character(len=*), intent(in) :: file_path ! arb. length filename char.
      real(dp), intent(in), allocatable :: output(:,:)
      integer :: io

      open(newunit=io,file=file_path,action="write", status="replace",form="unformatted")
      write(io) output
      close(unit=io)
        
  end subroutine write_output

end module my_odepack_demo
