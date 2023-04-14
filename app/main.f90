program main
  ! The "my_odepack_demo" module is in the ../src dir.
  !   use the my_odepack_demo for custom subroutines not part of the odepack_mod.
  !   e.g. we can include subroutines to read namelist files for inputs.
  use my_odepack_demo, only: linspace, read_namelist, write_output
  ! This "use" statement includes the ENTIRE odepack module from the (modern) odepack dependency.
  use odepack_mod
  use M_CLI2, only: set_args, get_args, sget, igets, set_mode
  implicit none
  ! Declarations
  character(len=:),allocatable  :: infile, outfile

  ! From the M_CLI2 module,
  ! WARNING: the CLI are parsed after "--"
  ! >> fpm run -- -t 3
  call set_args("--input:i 'input.nml' --output:o 'output.dat'")
  infile = sget('input')
  outfile = sget('output')
  print *, "input file: ", infile 
  print *, "output file: ", outfile

  ! Example call to the subroutine (below) which runs an ode-solve of the Lorentz ODE.
  call solve_ode(infile, outfile)

contains

  subroutine solve_ode(infile,outfile)
    ! lsoda_class is part of the ODEPACK module
    ! to understand these inputs see the DLSODA subroutine of the (legacy) odepack.f
    ! NEQ = number of equations
    ! N = total points in time.
    ! ITASK = 1 (normal calculation of y vs. t)
    ! ISTATE = 1 (flag modified during calculation)
    ! y(3) = 3-element vector for ICs.
    ! t = IC for independant variable.
    ! tout = "first time where output is desired"
    ! rtol = "relative tolerance parameter (scalar)"
    ! atol = "absolute tolerance parameter (scalar or array)"
    ! t_eval = time vector over which to evaluate the solution.
    ! ysol = array of shape: (NEQ, NTIME) to store the solution
    type(lsoda_class) :: ls
    integer :: i, NEQ, N, itask, istate
    real(dp) :: y(3), t, tf, tout, rtol, atol(1)
    real(dp), allocatable :: t_eval(:)
    real(dp), allocatable :: ysol(:,:)
    ! File IO params,
    character(len=:), allocatable, intent(in) :: infile
    character(len=:), allocatable, intent(in) :: outfile
    integer :: io, iostat
    ! Benchmarking params,
    real(dp) :: ctime(2)

    ! Read namelist
    call read_namelist(infile, NEQ, N, t, tf, rtol, atol)

    allocate(t_eval(N))
    allocate(ysol(NEQ,N))
    call ls%initialize(rhs_my_lorenz, NEQ, istate=istate)
    if (istate < 0) then
      print*,istate
      error stop
    endif

    ! populates the t_eval vector from t = 0 --> t = 100
    call linspace(t, tf, t_eval)
    ! numerical/solver settings,
    !rtol = 1.0e-8_dp
    !atol = 1.0e-8_dp
    itask = 1
    istate = 1
    ! populate ICs,
    y(:) = [1.0_dp,0.0_dp,0.0_dp]
    ! for each time; advance the solution from t --> tout through the t_eval vector.
    call cpu_time(ctime(1))
    do i = 1,size(t_eval)
      tout = t_eval(i)
      call ls%integrate(y, t, tout, rtol, atol, itask, istate)
      if (istate < 0) then
        print*,istate
        error stop
      endif
      ysol(:,i) = y(:)
    end do
    call cpu_time(ctime(2))
    print*,'LSODA time: ', 1000.0_dp*(ctime(2) - ctime(1)), " ms"

    ! Write the solution to a file,
    call write_output(outfile, ysol)

  end subroutine

  subroutine rhs_my_lorenz(self, neq, t, u, du, ierr)
    class(lsoda_class), intent(inout) :: self
    integer, intent(in) :: neq
    real(dp), intent(in) :: t
    real(dp), intent(in) :: u(neq)
    real(dp), intent(out) :: du(neq)
    integer, intent(out) :: ierr
    
    real(dp) :: x, y, z
    real(dp), parameter :: sigma = 10.0_dp, &
                           rho = 28.0_dp, &
                           beta = 8.0_dp/3.0_dp
    
    x = u(1)
    y = u(2)
    z = u(3)
    du(1) = sigma * (y - x)
    du(2) = x * (rho - z) - y
    du(3) = x * y - beta * z

    ierr = 0
  end subroutine

end program main
