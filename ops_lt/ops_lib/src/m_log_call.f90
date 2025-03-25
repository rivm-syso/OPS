module m_log_call

! Module m_log_call is used to create tests, and can be used to modify the reference output
! in existing tests.
!
! 1 Creating new tests.
! Test-routines are created following a set pattern.
! In the function to be tested, calls are added:
!   - the input is remembered;
!   - the output is remembered;
!   - based on the specification of input and output, the form of a function call is composed;
!   - During the execution of the function it is tracked whether a call was 'interesting':
!     are lines reached that are not yet in the coverage?
!   - Upon completion of an interesting call, a complete test is written away in Fortran syntax. 
!     This can be added (virtually) unchanged to the test bank.
!
! 2 Adjusting reference output
! Due to adjustments in the code, desired changes in the output of certain functions may occur;
! in that case, the reference output programmed in the test code must be adjusted. 
! In that case, (for example) the function print_array can be called, causing the new reference 
! output to appear on the screen, in Fortran syntax. This output can be included in the test program,
! and then the test is again ready for use in the new version of OPS-lib or OPS-lt. 
!
!-------------------------------------------------------------
! Example of use of log_call in subroutine ops_getlu
!-------------------------------------------------------------
! Add "use m_log_call" before line with IMPLICIT NONE.
! Add after declaration of all variables:
!    logical :: log_call = .false. ! if true -> call is 'interesting' -> genererate a test for this case
!    type(tcall_pattern) :: call_pattern
! ...
! IF (.NOT. iscell) THEN
!    ! Point outside land use grid -> take default grass (code 1):
!    landuse(1) = 1 ! dominant landuse class = grass
!    landuse(2) = 100 ! 100 % grass
!    landuse(3:NLU+1) = 0 ! 0% for the rest
! ELSE
!    ! Point inside land use grid -> create test Note: can be anywhere in the code where you want to 
!    log_call = .true.
! ENDIF
! if (log_call) then
!    ! Print test routine with current inputs and check outputs:
!    call_pattern = init_call(routinenaam)
!    call add_input( call_pattern, "xr", xr)
!    call add_input( call_pattern, "yr", yr)
!    call add_input( call_pattern, "in_trajectory", in_trajectory)
!    call add_input( call_pattern, "lugrid", lugrid)
!    call add_output( call_pattern, "error", error)
!    call add_output( call_pattern, "landuse", landuse)
!    call print_test(call_pattern)
!    stop 'A test was generated. Further processing is not needed.'
! endif

implicit none

   integer, save :: n_generated_files = 0
   character(len=400) :: generated_files(100)

   integer, save :: print_unit = 6

   integer, parameter :: cutoff_at = 100

   integer, parameter :: MAX_N_ARGS=100
   integer, parameter :: MAX_N_USES=20

   integer, parameter :: tp_REAL_1D=1, tp_INTEGER=2, tp_REAL=3, tp_INTEGER_1D=4, &
                         tp_STRING=5, tp_REAL_2D=6, tp_LOGICAL=7, tp_LOGICAL_1D=8, &
                         tp_INTEGER_2D=9, tp_SHORT_1D=10, tp_REAL_4D=11, tp_REAL_3D=12, &
                         tp_DERIVED=13, tp_STRING_1D=14, tp_DOUBLE=15, tp_REAL_5D=16

   integer, parameter :: TOO_MANY_ITEMS=1000
   integer, parameter :: TOO_MANY_STRINGS=100
   integer, parameter :: intent_IN=1, intent_OUT=2, intent_INOUT=3

   
   type text
       character(len=250), allocatable :: strings(:)
   end type text
   type int_array_2d
       integer, allocatable :: values(:,:)
   end type int_array_2d
   type int_array_1d
       integer, allocatable :: values(:)
   end type int_array_1d

   type logical_array_1d
       logical, allocatable :: values(:)
   end type logical_array_1d

   type real_array_3d
       real, allocatable :: values(:,:,:)
   end type real_array_3d
   type real_array_4d
       real, allocatable :: values(:,:,:,:)
   end type real_array_4d
   type real_array_5d
       real, allocatable :: values(:,:,:,:,:)
   end type real_array_5d
   type real_array_2d
       real, allocatable :: values(:,:)
   end type real_array_2d
   type real_array_1d
       real, allocatable :: values(:)
   end type real_array_1d

   type tderived
       character(len=100) :: type_name
       type(text) :: assert
       type(text) :: initialization
       type(text) :: uses
   end type tderived

   type tcall_pattern
       character(len=500) :: routinenaam

       integer :: n_uses = 0
       character(len=100)     :: uses(MAX_N_USES)

       integer :: nargs = 0
       character(len=500) :: argnames(MAX_N_ARGS)
       integer :: arg_types(MAX_N_ARGS)
       integer :: intents(MAX_N_ARGS)

       integer :: ints(MAX_N_ARGS)
       real    :: reals(MAX_N_ARGS)
       double precision    :: doubles(MAX_N_ARGS)
       logical :: logicals(MAX_N_ARGS)
       character(len=200)     :: strings(MAX_N_ARGS)
       type(real_array_5d)    :: r5d(MAX_N_ARGS)
       type(real_array_4d)    :: r4d(MAX_N_ARGS)
       type(real_array_3d)    :: r3d(MAX_N_ARGS)
       type(real_array_2d)    :: r2d(MAX_N_ARGS)
       type(real_array_1d)    :: r1d(MAX_N_ARGS)
       type(int_array_1d)     :: i1d(MAX_N_ARGS)
       type(int_array_2d)     :: i2d(MAX_N_ARGS)
       type(logical_array_1d) :: l1d(MAX_N_ARGS)
       
       type(text)             :: initialization(MAX_N_ARGS)
       type(text)             :: assert(MAX_N_ARGS)
   end type tcall_pattern

   interface print_value
      module procedure print_logical_value
      module procedure print_string_value
      module procedure print_double_value
      module procedure print_real_value
      module procedure print_error_value
      module procedure print_integer_value
   end interface print_value
   interface print_array
      module procedure print_string1d_array
      module procedure print_int2d_array
      module procedure print_int1d_array
      module procedure print_logical1d_array
      module procedure print_real1d_array
      module procedure print_real2d_array
      module procedure print_real3d_array
      module procedure print_real4d_array
      module procedure print_real5d_array
   end interface print_array
   interface print_declaration
      module procedure print_string_declaration
      module procedure print_string1d_declaration

      module procedure print_int_declaration
      module procedure print_int1d_declaration
      module procedure print_int2d_declaration

      module procedure print_logical_declaration
      module procedure print_logical1d_declaration

      module procedure print_double_declaration

      module procedure print_real_declaration
      module procedure print_real1d_declaration
      module procedure print_real2d_declaration
      module procedure print_real3d_declaration
      module procedure print_real4d_declaration
      module procedure print_real5d_declaration

      module procedure print_any_declaration
   end interface print_declaration
   interface add_arg
      module procedure add_derived_arg

      module procedure add_string_arg
      module procedure add_string1d_arg

      module procedure add_int_arg
      module procedure add_int1d_arg
      module procedure add_int2d_arg

      module procedure add_short1d_arg

      module procedure add_logical_arg
      module procedure add_logical1d_arg

      module procedure add_double_arg

      module procedure add_real_arg
      module procedure add_real1d_arg
      module procedure add_real2d_arg
      module procedure add_real3d_arg
      module procedure add_real4d_arg
      module procedure add_real5d_arg
   end interface add_arg
   interface add_input
      module procedure add_derived_input

      module procedure add_string_input
      module procedure add_string1d_input

      module procedure add_short1d_input

      module procedure add_logical_input
      module procedure add_logical1d_input

      module procedure add_int_input
      module procedure add_int1d_input
      module procedure add_int2d_input

      module procedure add_double_input

      module procedure add_real_input
      module procedure add_real1d_input
      module procedure add_real2d_input
      module procedure add_real3d_input
      module procedure add_real4d_input
      module procedure add_real5d_input
   end interface add_input
   interface add_output
      module procedure add_derived_output

      module procedure add_string_output
      module procedure add_string1d_output

      module procedure add_short1d_output

      module procedure add_logical_output
      module procedure add_logical1d_output

      module procedure add_int_output
      module procedure add_int1d_output
      module procedure add_int2d_output

      module procedure add_double_output

      module procedure add_real_output
      module procedure add_real1d_output
      module procedure add_real2d_output
      module procedure add_real3d_output
      module procedure add_real4d_output
      module procedure add_real5d_output
   end interface add_output
   interface add_inout
      module procedure add_derived_inout

      module procedure add_string_inout
      module procedure add_string1d_inout

      module procedure add_short1d_inout

      module procedure add_int_inout
      module procedure add_int1d_inout
      module procedure add_int2d_inout

      module procedure add_logical_inout
      module procedure add_logical1d_inout

      module procedure add_double_inout

      module procedure add_real_inout
      module procedure add_real1d_inout
      module procedure add_real2d_inout
      module procedure add_real3d_inout
      module procedure add_real4d_inout
      module procedure add_real5d_inout
   end interface add_inout

contains
    subroutine remove_generated_files()
       integer :: i, unit, stat
       do i = 1,n_generated_files
          open(newunit=unit, iostat=stat, file=trim(generated_files(i)), status='old')
          if (stat == 0) then
             print *,'Removing generated file "'//trim(generated_files(i))//'"'
             close(unit, status='delete')
          else
             print *,'Cannot remove generated file "'//trim(generated_files(i))//'"'
          end if
       end do
       n_generated_files = 0
    end subroutine remove_generated_files

    subroutine forget_generated_files()
       n_generated_files = 0
    end subroutine forget_generated_files

    subroutine remember_generated_file(fname)
       character(len=*), intent(in) :: fname
       n_generated_files = n_generated_files + 1
       generated_files(n_generated_files) = fname
    end subroutine remember_generated_file

    function open_generated_file(fname) result(unit)
    use no_pfunit, only: AssertEqual
       character(len=*), intent(in) :: fname
       integer :: unit, iostat
       open(newunit=unit, file = trim(fname), action = 'write', iostat=iostat)
       call AssertEqual(iostat,0,'Opening file "'//trim(fname)//'"', __LINE__, __FILE__)
       call remember_generated_file(fname)
    end function open_generated_file

    subroutine set_generated_code_file(fname)
    use no_pfunit, only: AssertEqual
       character(len=*), intent(in) :: fname
       integer :: iostat
       open(newunit=print_unit, file = trim(fname), action = 'write', iostat=iostat)
       call AssertEqual(iostat,0,'Opening file "'//trim(fname)//'"', __LINE__, __FILE__)
       call remember_generated_file(fname)
    end subroutine set_generated_code_file

    subroutine close_generated_code_file(fname)
    use no_pfunit, only: AssertEqual
       character(len=*), intent(in) :: fname
       integer :: stat
       close(print_unit, iostat=stat)
       call AssertEqual(stat,0,'Closing file "'//trim(fname)//'"', __LINE__, __FILE__)
       print_unit = 6
    end subroutine close_generated_code_file

    function init_call(routinenaam) result(call_pattern) 
       character(len=*), intent(in) :: routinenaam
       type(tcall_pattern) :: call_pattern
       call_pattern%routinenaam = routinenaam
       call_pattern%n_uses = 1
       call_pattern%uses(1) = 'no_pfunit'
    end function init_call

    subroutine add_derived_inout(call_pattern, name, value)
       type(tcall_pattern), intent(inout) :: call_pattern
       character(len=*),    intent(in)    :: name
       type(tderived),       intent(in)    :: value
       call add_arg(call_pattern, name, intent_INOUT, value)
    end subroutine add_derived_inout
    subroutine add_string1d_inout(call_pattern, name, value)
       type(tcall_pattern), intent(inout) :: call_pattern
       character(len=*),    intent(in)    :: name
       character(len=*),    intent(in)    :: value(:)
       call add_arg(call_pattern, name, intent_INOUT, value)
    end subroutine add_string1d_inout
    subroutine add_string_inout(call_pattern, name, value)
       type(tcall_pattern), intent(inout) :: call_pattern
       character(len=*),    intent(in)    :: name
       character(len=*),    intent(in)    :: value
       call add_arg(call_pattern, name, intent_INOUT, value)
    end subroutine add_string_inout
    subroutine add_double_inout(call_pattern, name, value)
       type(tcall_pattern), intent(inout) :: call_pattern
       character(len=*),    intent(in)    :: name
       double precision,    intent(in)    :: value
       call add_arg(call_pattern, name, intent_INOUT, value)
    end subroutine add_double_inout
    subroutine add_real_inout(call_pattern, name, value)
       type(tcall_pattern), intent(inout) :: call_pattern
       character(len=*),    intent(in)    :: name
       real,                intent(in)    :: value
       call add_arg(call_pattern, name, intent_INOUT, value)
    end subroutine add_real_inout
    subroutine add_logical_inout(call_pattern, name, value)
       type(tcall_pattern), intent(inout) :: call_pattern
       character(len=*),    intent(in)    :: name
       logical,             intent(in)    :: value
       call add_arg(call_pattern, name, intent_INOUT, value)
    end subroutine add_logical_inout
    subroutine add_logical1d_inout(call_pattern, name, value)
       type(tcall_pattern), intent(inout) :: call_pattern
       character(len=*),    intent(in)    :: name
       logical,             intent(in)    :: value(:)
       call add_arg(call_pattern, name, intent_INOUT, value)
    end subroutine add_logical1d_inout
    subroutine add_int_inout(call_pattern, name, value)
       type(tcall_pattern), intent(inout) :: call_pattern
       character(len=*),    intent(in)    :: name
       integer,             intent(in)    :: value
       call add_arg(call_pattern, name, intent_INOUT, value)
    end subroutine add_int_inout
    subroutine add_short1d_inout(call_pattern, name, value)
       type(tcall_pattern), intent(inout) :: call_pattern
       character(len=*),    intent(in)    :: name
       integer*2,           intent(in)    :: value(:)
       call add_arg(call_pattern, name, intent_INOUT, value)
    end subroutine add_short1d_inout
    subroutine add_int1d_inout(call_pattern, name, value)
       type(tcall_pattern), intent(inout) :: call_pattern
       character(len=*),    intent(in)    :: name
       integer,             intent(in)    :: value(:)
       call add_arg(call_pattern, name, intent_INOUT, value)
    end subroutine add_int1d_inout
    subroutine add_int2d_inout(call_pattern, name, value)
       type(tcall_pattern), intent(inout) :: call_pattern
       character(len=*),    intent(in)    :: name
       integer,             intent(in)    :: value(:,:)
       call add_arg(call_pattern, name, intent_INOUT, value)
    end subroutine add_int2d_inout
    subroutine add_real1d_inout(call_pattern, name, value)
       type(tcall_pattern), intent(inout) :: call_pattern
       character(len=*),    intent(in)    :: name
       real,                intent(in)    :: value(:)
       call add_arg(call_pattern, name, intent_INOUT, value)
    end subroutine add_real1d_inout
    subroutine add_real4d_inout(call_pattern, name, value)
       type(tcall_pattern), intent(inout) :: call_pattern
       character(len=*),    intent(in)    :: name
       real,                intent(in)    :: value(:,:,:,:)
       call add_arg(call_pattern, name, intent_INOUT, value)
    end subroutine add_real4d_inout
    subroutine add_real5d_inout(call_pattern, name, value)
       type(tcall_pattern), intent(inout) :: call_pattern
       character(len=*),    intent(in)    :: name
       real,                intent(in)    :: value(:,:,:,:,:)
       call add_arg(call_pattern, name, intent_INOUT, value)
    end subroutine add_real5d_inout
    subroutine add_real3d_inout(call_pattern, name, value)
       type(tcall_pattern), intent(inout) :: call_pattern
       character(len=*),    intent(in)    :: name
       real,                intent(in)    :: value(:,:,:)
       call add_arg(call_pattern, name, intent_INOUT, value)
    end subroutine add_real3d_inout
    subroutine add_real2d_inout(call_pattern, name, value)
       type(tcall_pattern), intent(inout) :: call_pattern
       character(len=*),    intent(in)    :: name
       real,                intent(in)    :: value(:,:)
       call add_arg(call_pattern, name, intent_INOUT, value)
    end subroutine add_real2d_inout

    subroutine add_derived_output(call_pattern, name, value)
       type(tcall_pattern), intent(inout) :: call_pattern
       character(len=*),    intent(in)    :: name
       type(tderived),   intent(in)    :: value
       call add_arg(call_pattern, name, intent_OUT, value)
    end subroutine add_derived_output
    subroutine add_string1d_output(call_pattern, name, value)
       type(tcall_pattern), intent(inout) :: call_pattern
       character(len=*),    intent(in)    :: name
       character(len=*),    intent(in)    :: value(:)
       call add_arg(call_pattern, name, intent_OUT, value)
    end subroutine add_string1d_output
    subroutine add_string_output(call_pattern, name, value)
       type(tcall_pattern), intent(inout) :: call_pattern
       character(len=*),    intent(in)    :: name
       character(len=*),    intent(in)    :: value
       call add_arg(call_pattern, name, intent_OUT, value)
    end subroutine add_string_output
    subroutine add_double_output(call_pattern, name, value)
       type(tcall_pattern), intent(inout) :: call_pattern
       character(len=*),    intent(in)    :: name
       double precision,    intent(in)    :: value
       call add_arg(call_pattern, name, intent_OUT, value)
    end subroutine add_double_output
    subroutine add_real_output(call_pattern, name, value)
       type(tcall_pattern), intent(inout) :: call_pattern
       character(len=*),    intent(in)    :: name
       real,                intent(in)    :: value
       call add_arg(call_pattern, name, intent_OUT, value)
    end subroutine add_real_output
    subroutine add_logical_output(call_pattern, name, value)
       type(tcall_pattern), intent(inout) :: call_pattern
       character(len=*),    intent(in)    :: name
       logical,             intent(in)    :: value
       call add_arg(call_pattern, name, intent_OUT, value)
    end subroutine add_logical_output
    subroutine add_logical1d_output(call_pattern, name, value)
      type(tcall_pattern), intent(inout) :: call_pattern
      character(len=*),    intent(in)    :: name
      logical,             intent(in)    :: value(:)
      call add_arg(call_pattern, name, intent_OUT, value)
   end subroutine add_logical1d_output
   subroutine add_int_output(call_pattern, name, value)
       type(tcall_pattern), intent(inout) :: call_pattern
       character(len=*),    intent(in)    :: name
       integer,             intent(in)    :: value
       call add_arg(call_pattern, name, intent_OUT, value)
    end subroutine add_int_output
    subroutine add_short1d_output(call_pattern, name, value)
       type(tcall_pattern), intent(inout) :: call_pattern
       character(len=*),    intent(in)    :: name
       integer*2,             intent(in)    :: value(:)
       call add_arg(call_pattern, name, intent_OUT, value)
    end subroutine add_short1d_output
    subroutine add_int1d_output(call_pattern, name, value)
       type(tcall_pattern), intent(inout) :: call_pattern
       character(len=*),    intent(in)    :: name
       integer,             intent(in)    :: value(:)
       call add_arg(call_pattern, name, intent_OUT, value)
    end subroutine add_int1d_output
    subroutine add_int2d_output(call_pattern, name, value)
       type(tcall_pattern), intent(inout) :: call_pattern
       character(len=*),    intent(in)    :: name
       integer,             intent(in)    :: value(:,:)
       call add_arg(call_pattern, name, intent_OUT, value)
    end subroutine add_int2d_output
    subroutine add_real1d_output(call_pattern, name, value)
       type(tcall_pattern), intent(inout) :: call_pattern
       character(len=*),    intent(in)    :: name
       real,                intent(in)    :: value(:)
       call add_arg(call_pattern, name, intent_OUT, value)
    end subroutine add_real1d_output
    subroutine add_real4d_output(call_pattern, name, value)
       type(tcall_pattern), intent(inout) :: call_pattern
       character(len=*),    intent(in)    :: name
       real,                intent(in)    :: value(:,:,:,:)
       call add_arg(call_pattern, name, intent_OUT, value)
    end subroutine add_real4d_output
    subroutine add_real5d_output(call_pattern, name, value)
       type(tcall_pattern), intent(inout) :: call_pattern
       character(len=*),    intent(in)    :: name
       real,                intent(in)    :: value(:,:,:,:,:)
       call add_arg(call_pattern, name, intent_OUT, value)
    end subroutine add_real5d_output
    subroutine add_real3d_output(call_pattern, name, value)
       type(tcall_pattern), intent(inout) :: call_pattern
       character(len=*),    intent(in)    :: name
       real,                intent(in)    :: value(:,:,:)
       call add_arg(call_pattern, name, intent_OUT, value)
    end subroutine add_real3d_output
    subroutine add_real2d_output(call_pattern, name, value)
       type(tcall_pattern), intent(inout) :: call_pattern
       character(len=*),    intent(in)    :: name
       real,                intent(in)    :: value(:,:)
       call add_arg(call_pattern, name, intent_OUT, value)
    end subroutine add_real2d_output

    subroutine add_derived_input(call_pattern, name, value)
       type(tcall_pattern), intent(inout) :: call_pattern
       character(len=*),    intent(in)    :: name
       type(tderived),       intent(in)    :: value
       call add_arg(call_pattern, name, intent_IN, value)
    end subroutine add_derived_input
    subroutine add_string1d_input(call_pattern, name, value)
       type(tcall_pattern), intent(inout) :: call_pattern
       character(len=*),    intent(in)    :: name
       character(len=*),    intent(in)    :: value(:)
       call add_arg(call_pattern, name, intent_IN, value)
    end subroutine add_string1d_input
    subroutine add_string_input(call_pattern, name, value)
       type(tcall_pattern), intent(inout) :: call_pattern
       character(len=*),    intent(in)    :: name
       character(len=*),    intent(in)    :: value
       call add_arg(call_pattern, name, intent_IN, value)
    end subroutine add_string_input
    subroutine add_double_input(call_pattern, name, value)
       type(tcall_pattern), intent(inout) :: call_pattern
       character(len=*),    intent(in)    :: name
       double precision,    intent(in)    :: value
       call add_arg(call_pattern, name, intent_IN, value)
    end subroutine add_double_input
    subroutine add_real_input(call_pattern, name, value)
       type(tcall_pattern), intent(inout) :: call_pattern
       character(len=*),    intent(in)    :: name
       real,             intent(in)    :: value
       call add_arg(call_pattern, name, intent_IN, value)
    end subroutine add_real_input
    subroutine add_int_input(call_pattern, name, value)
       type(tcall_pattern), intent(inout) :: call_pattern
       character(len=*),    intent(in)    :: name
       integer,             intent(in)    :: value
       call add_arg(call_pattern, name, intent_IN, value)
    end subroutine add_int_input
    subroutine add_logical_input(call_pattern, name, value)
       type(tcall_pattern), intent(inout) :: call_pattern
       character(len=*),    intent(in)    :: name
       logical,             intent(in)    :: value
       call add_arg(call_pattern, name, intent_IN, value)
    end subroutine add_logical_input
    subroutine add_logical1d_input(call_pattern, name, value)
      type(tcall_pattern), intent(inout) :: call_pattern
      character(len=*),    intent(in)    :: name
      logical,             intent(in)    :: value(:)
      call add_arg(call_pattern, name, intent_IN, value)
   end subroutine add_logical1d_input
    subroutine add_short1d_input(call_pattern, name, value)
       type(tcall_pattern), intent(inout) :: call_pattern
       character(len=*),    intent(in)    :: name
       integer*2,             intent(in)    :: value(:)
       call add_arg(call_pattern, name, intent_IN, value)
    end subroutine add_short1d_input
    subroutine add_int1d_input(call_pattern, name, value)
       type(tcall_pattern), intent(inout) :: call_pattern
       character(len=*),    intent(in)    :: name
       integer,             intent(in)    :: value(:)
       call add_arg(call_pattern, name, intent_IN, value)
    end subroutine add_int1d_input
    subroutine add_int2d_input(call_pattern, name, value)
       type(tcall_pattern), intent(inout) :: call_pattern
       character(len=*),    intent(in)    :: name
       integer,             intent(in)    :: value(:,:)
       call add_arg(call_pattern, name, intent_IN, value)
    end subroutine add_int2d_input
    subroutine add_real1d_input(call_pattern, name, value)
       type(tcall_pattern), intent(inout) :: call_pattern
       character(len=*),    intent(in)    :: name
       real,                intent(in)    :: value(:)
       call add_arg(call_pattern, name, intent_IN, value)
    end subroutine add_real1d_input
    subroutine add_real4d_input(call_pattern, name, value)
       type(tcall_pattern), intent(inout) :: call_pattern
       character(len=*),    intent(in)    :: name
       real,                intent(in)    :: value(:,:,:,:)
       call add_arg(call_pattern, name, intent_IN, value)
    end subroutine add_real4d_input
    subroutine add_real5d_input(call_pattern, name, value)
       type(tcall_pattern), intent(inout) :: call_pattern
       character(len=*),    intent(in)    :: name
       real,                intent(in)    :: value(:,:,:,:,:)
       call add_arg(call_pattern, name, intent_IN, value)
    end subroutine add_real5d_input
    subroutine add_real3d_input(call_pattern, name, value)
       type(tcall_pattern), intent(inout) :: call_pattern
       character(len=*),    intent(in)    :: name
       real,                intent(in)    :: value(:,:,:)
       call add_arg(call_pattern, name, intent_IN, value)
    end subroutine add_real3d_input
    subroutine add_real2d_input(call_pattern, name, value)
       type(tcall_pattern), intent(inout) :: call_pattern
       character(len=*),    intent(in)    :: name
       real,                intent(in)    :: value(:,:)
       call add_arg(call_pattern, name, intent_IN, value)
    end subroutine add_real2d_input

    subroutine copy_text(text_in, text_out)
    implicit none
       type(text), intent(in) :: text_in
       type(text), intent(out) :: text_out
       integer :: i,n
       n = size(text_in%strings,1)
       allocate(text_out%strings(n))
       do i = 1,n
          text_out%strings(i) = text_in%strings(i)
       end do
    end subroutine copy_text

    subroutine add_derived_arg(call_pattern, name, vintent, value)
       type(tcall_pattern), intent(inout) :: call_pattern
       character(len=*),    intent(in) :: name
       integer,             intent(in) :: vintent
       type(tderived),      intent(in)    :: value

       integer, parameter :: typ = tp_DERIVED
       integer :: i, j, n
       logical :: found
       call_pattern % nargs = call_pattern % nargs + 1
       call_pattern % strings (call_pattern % nargs)   = value%type_name
       call_pattern % intents (call_pattern % nargs)   = vintent
       call_pattern % argnames (call_pattern % nargs)  = name
       call_pattern % arg_types (call_pattern % nargs) = typ
       call copy_text(value%initialization,call_pattern%initialization(call_pattern % nargs))
       call copy_text(value%assert,call_pattern%assert(call_pattern % nargs))
       n = size(value%uses%strings)
       do i = 1,n
          found = .false.
          do j = 1,call_pattern%n_uses
              found =  call_pattern%uses(j) ==  value%uses%strings(i)
              if (found) exit
          end do
          if (.not. found) then
             call_pattern%n_uses = 1+call_pattern%n_uses
             if (call_pattern%n_uses > MAX_N_USES) then
                print *,'too many modules used'
                stop 1
             end if
             call_pattern%uses(call_pattern%n_uses) = value%uses%strings(i)
          end if
       end do
    end subroutine add_derived_arg
    subroutine add_double_arg(call_pattern, name, vintent, value)
       type(tcall_pattern), intent(inout) :: call_pattern
       character(len=*), intent(in) :: name
       integer, intent(in) :: vintent
       double precision, intent(in) :: value

       integer, parameter :: typ = tp_DOUBLE
       call_pattern % nargs = call_pattern % nargs + 1
       call_pattern % doubles (call_pattern % nargs)    = value
       call_pattern % intents (call_pattern % nargs) = vintent
       call_pattern % argnames (call_pattern % nargs) = name
       call_pattern % arg_types (call_pattern % nargs) = typ
    end subroutine add_double_arg
    subroutine add_real_arg(call_pattern, name, vintent, value)
       type(tcall_pattern), intent(inout) :: call_pattern
       character(len=*), intent(in) :: name
       integer, intent(in) :: vintent
       real, intent(in) :: value

       integer, parameter :: typ = tp_REAL
       call_pattern % nargs = call_pattern % nargs + 1
       call_pattern % reals (call_pattern % nargs)    = value
       call_pattern % intents (call_pattern % nargs) = vintent
       call_pattern % argnames (call_pattern % nargs) = name
       call_pattern % arg_types (call_pattern % nargs) = typ
    end subroutine add_real_arg

    subroutine add_string1d_arg(call_pattern, name, vintent, value)
#   ifndef __GFORTRAN__
       use ifport, only: getpid
#   endif
       type(tcall_pattern), intent(inout) :: call_pattern
       character(len=*), intent(in) :: name
       integer, intent(in) :: vintent
       character(len=*), intent(in) :: value(:)
       integer, parameter :: typ = tp_STRING_1D
       integer :: iarg, nvalue, i
       integer :: unit
       integer, save :: idx = 0
       character(len=300) :: fname

       nvalue = size(value,1)
       call_pattern % nargs = call_pattern % nargs + 1
       iarg = call_pattern % nargs 

       if (size(value) > TOO_MANY_STRINGS) then
          idx = idx + 1
          write(fname, '(a,i0,a,i0,a)') 'string1d_',getpid(),'_',idx,'.int'
          print '(10(a,i0))','saving (',size(value,1),') to "'//trim(fname)//'"'
          flush(6)
          unit = open_generated_file(fname)
          do i = 1,size(value,1)
             write(unit,'(a)') trim(value(i))
          end do
          close(unit)
          print '(10(a,i0))','saved (',size(value,1),') to "'//trim(fname)//'"'
          flush(6)
          call_pattern % strings(call_pattern % nargs) = trim(fname)
       else
          call_pattern % strings(call_pattern % nargs) = ' '
       end if
       print *,'allocating ',nvalue,' strings for initialization(',iarg,')'
       allocate(call_pattern % initialization(iarg) % strings(nvalue))
       do i = 1, nvalue
          call_pattern % initialization(iarg) % strings(i) = value(i)
       end do
       call_pattern % intents (iarg)   = vintent
       call_pattern % argnames (iarg)  = name
       call_pattern % arg_types (iarg) = typ
    end subroutine add_string1d_arg

    subroutine add_string_arg(call_pattern, name, vintent, value)
       type(tcall_pattern), intent(inout) :: call_pattern
       character(len=*), intent(in) :: name
       integer, intent(in) :: vintent
       character(len=*), intent(in) :: value
       integer, parameter :: typ = tp_STRING
       call_pattern % nargs = call_pattern % nargs + 1
       call_pattern % strings (call_pattern % nargs)   = trim(value)
       call_pattern % intents (call_pattern % nargs)   = vintent
       call_pattern % argnames (call_pattern % nargs)  = name
       call_pattern % arg_types (call_pattern % nargs) = typ
    end subroutine add_string_arg
    subroutine add_int_arg(call_pattern, name, vintent, value)
       type(tcall_pattern), intent(inout) :: call_pattern
       character(len=*), intent(in) :: name
       integer, intent(in) :: vintent
       integer, intent(in) :: value

       integer, parameter :: typ = tp_INTEGER
       call_pattern % nargs = call_pattern % nargs + 1
       call_pattern % ints (call_pattern % nargs)    = value
       call_pattern % intents (call_pattern % nargs) = vintent
       call_pattern % argnames (call_pattern % nargs) = name
       call_pattern % arg_types (call_pattern % nargs) = typ
       call_pattern % arg_types (call_pattern % nargs) = typ
    end subroutine add_int_arg
    subroutine add_logical_arg(call_pattern, name, vintent, value)
       type(tcall_pattern), intent(inout) :: call_pattern
       character(len=*), intent(in) :: name
       integer, intent(in) :: vintent
       logical, intent(in) :: value

       integer, parameter :: typ = tp_LOGICAL
       call_pattern % nargs = call_pattern % nargs + 1
       call_pattern % logicals (call_pattern % nargs)    = value
       call_pattern % intents (call_pattern % nargs) = vintent
       call_pattern % argnames (call_pattern % nargs) = name
       call_pattern % arg_types (call_pattern % nargs) = typ
       call_pattern % arg_types (call_pattern % nargs) = typ
    end subroutine add_logical_arg
    subroutine add_logical1d_arg(call_pattern, name, vintent, value)
      type(tcall_pattern), intent(inout) :: call_pattern
      character(len=*), intent(in) :: name
      integer, intent(in) :: vintent
      logical, intent(in) :: value(:)

      integer, parameter :: typ = tp_LOGICAL_1D
      call_pattern % nargs = call_pattern % nargs + 1
      allocate(call_pattern % l1d(call_pattern % nargs)%values(size(value,1)))
      call_pattern % l1d (call_pattern % nargs)%values    = value
      call_pattern % intents (call_pattern % nargs) = vintent
      call_pattern % argnames (call_pattern % nargs) = name
      call_pattern % arg_types (call_pattern % nargs) = typ
      call_pattern % arg_types (call_pattern % nargs) = typ
   end subroutine add_logical1d_arg

    subroutine add_short1d_arg(call_pattern, name, vintent, value)
       type(tcall_pattern), intent(inout) :: call_pattern
       character(len=*), intent(in) :: name
       integer, intent(in) :: vintent
       integer*2, intent(in) :: value(:)

       integer, parameter :: typ = tp_SHORT_1D
       call_pattern % nargs = call_pattern % nargs + 1
       allocate(call_pattern % i1d(call_pattern % nargs)%values(size(value,1)))
       call_pattern % i1d (call_pattern % nargs)%values    = value
       call_pattern % intents (call_pattern % nargs) = vintent
       call_pattern % argnames (call_pattern % nargs) = name
       call_pattern % arg_types (call_pattern % nargs) = typ
    end subroutine add_short1d_arg

    subroutine add_int1d_arg(call_pattern, name, vintent, value)
#   ifndef __GFORTRAN__
       use ifport, only: getpid
#   endif
       type(tcall_pattern), intent(inout) :: call_pattern
       character(len=*), intent(in) :: name
       integer, intent(in) :: vintent
       integer, intent(in) :: value(:)

       integer, parameter :: typ = tp_INTEGER_1D

       integer :: unit
       integer :: i
       integer, save :: idx = 0
       character(len=300) :: fname

       call_pattern % nargs = call_pattern % nargs + 1

       if (size(value) > TOO_MANY_ITEMS) then
          idx = idx + 1
          write(fname, '(a,i0,a,i0,a)') 'integer1d_',getpid(),'_',idx,'.int'
          print '(10(a,i0))',trim(name)//': saving (',size(value,1),') integers to "'//trim(fname)//'"'
          flush(6)
          unit = open_generated_file(fname)
          do i = 1,size(value,1)
             write(unit,'(i0)') value(i)
          end do
          close(unit)
          print '(10(a,i0))','saved (',size(value,1),') to "'//trim(fname)//'"'
          flush(6)
          call_pattern % strings(call_pattern % nargs) = trim(fname)
       else
          call_pattern % strings(call_pattern % nargs) = ' '
       end if

       allocate(call_pattern % i1d(call_pattern % nargs)%values(size(value,1)))
       call_pattern % i1d (call_pattern % nargs)%values    = value
       call_pattern % intents (call_pattern % nargs) = vintent
       call_pattern % argnames (call_pattern % nargs) = name
       call_pattern % arg_types (call_pattern % nargs) = typ
    end subroutine add_int1d_arg

    subroutine add_int2d_arg(call_pattern, name, vintent, value)
#   ifndef __GFORTRAN__
       use ifport, only: getpid
#   endif
       type(tcall_pattern), intent(inout) :: call_pattern
       character(len=*), intent(in) :: name
       integer, intent(in) :: vintent
       integer, intent(in) :: value(:,:)

       integer, parameter :: typ = tp_INTEGER_2D

       integer :: i, j
       integer :: unit
       integer, save :: idx = 0
       character(len=300) :: fname
       call_pattern % nargs = call_pattern % nargs + 1

       if (size(value) > TOO_MANY_ITEMS) then
          idx = idx + 1
          write(fname, '(a,i0,a,i0,a)') 'int2d_',getpid(),'_',idx,'.real'
          print '(10(a,i0))',trim(name)//': saving (',size(value,1),',',size(value,2),') integers to "'//trim(fname)//'"'
          flush(6)
          unit = open_generated_file(fname)
          do j = 1,size(value,2)
             do i = 1,size(value,1)
                write(unit,'(i0)') value(i,j)
             end do
          end do
          close(unit)
          print '(10(a,i0))','saved (',size(value,1),',',size(value,2),') to "'//trim(fname)//'"'
          flush(6)
          call_pattern % strings(call_pattern % nargs) = trim(fname)
       else
          call_pattern % strings(call_pattern % nargs) = ' '
       end if

       allocate(call_pattern % i2d(call_pattern % nargs)%values(size(value,1),size(value,2)))
       call_pattern % i2d (call_pattern % nargs)%values = value
       call_pattern % intents (call_pattern % nargs) = vintent
       call_pattern % argnames (call_pattern % nargs) = name
       call_pattern % arg_types (call_pattern % nargs) = typ
    end subroutine add_int2d_arg

    subroutine add_real5d_arg(call_pattern, name, vintent, value)
#   ifndef __GFORTRAN__
       use ifport, only: getpid
#   endif
       type(tcall_pattern), intent(inout) :: call_pattern
       character(len=*), intent(in) :: name
       integer, intent(in) :: vintent
       real, intent(in) :: value(:,:,:,:,:)

       integer, parameter :: typ = tp_REAL_5D
       integer :: i, j, k, l, m
       integer :: unit
       integer, save :: idx = 0
       character(len=300) :: fname

       call_pattern % nargs = call_pattern % nargs + 1

       if (size(value) > TOO_MANY_ITEMS) then
          idx = idx + 1
          write(fname, '(a,i0,a,i0,a)') 'real5d_',getpid(),'_',idx,'.real'
          print '(10(a,i0))',trim(name)//': saving (',size(value,1),',',size(value,2),',', &
              size(value,3),',',size(value,4),',',size(value,5),') reals to "'//trim(fname)//'"'
          flush(6)
          unit = open_generated_file(fname)
          do m = 1,size(value,5)
             do l = 1,size(value,4)
                do k = 1,size(value,3)
                   do j = 1,size(value,2)
                      do i = 1,size(value,1)
                         write(unit,'(e16.8)') value(i,j,k,l,m)
                      end do
                   end do
                end do
             end do
          end do
          close(unit)
          print '(10(a,i0))','saved (',size(value,1),',',size(value,2),',',size(value,3),',',size(value,4),',',size(value,5),') to "'//trim(fname)//'"'
          flush(6)
          call_pattern % strings(call_pattern % nargs) = trim(fname)
       else
          call_pattern % strings(call_pattern % nargs) = ' '
       end if

       allocate(call_pattern % r5d(call_pattern % nargs)%values( &
             size(value,1),size(value,2),size(value,3),size(value,4),size(value,5)))
       print *,'shape(r5d) = ',shape(call_pattern % r5d(call_pattern % nargs)%values)
       call_pattern % r5d (call_pattern % nargs)%values = value
       call_pattern % intents (call_pattern % nargs) = vintent
       call_pattern % argnames (call_pattern % nargs) = name
       call_pattern % arg_types (call_pattern % nargs) = typ
    end subroutine add_real5d_arg

    subroutine add_real4d_arg(call_pattern, name, vintent, value)
#   ifndef __GFORTRAN__
       use ifport, only: getpid
#   endif
       type(tcall_pattern), intent(inout) :: call_pattern
       character(len=*), intent(in) :: name
       integer, intent(in) :: vintent
       real, intent(in) :: value(:,:,:,:)

       integer, parameter :: typ = tp_REAL_4D
       integer :: i, j, k, l
       integer :: unit
       integer, save :: idx = 0
       character(len=300) :: fname
       call_pattern % nargs = call_pattern % nargs + 1

       if (size(value) > TOO_MANY_ITEMS) then
          idx = idx + 1
          write(fname, '(a,i0,a,i0,a)') 'real4d_',getpid(),'_',idx,'.real'
          print '(10(a,i0))',trim(name)//': saving (',size(value,1),',',size(value,2),',', &
                      size(value,3),',',size(value,4),') reals to "'//trim(fname)//'"'
          flush(6)
          unit = open_generated_file(fname)
          do l = 1,size(value,4)
             do k = 1,size(value,3)
                do j = 1,size(value,2)
                   do i = 1,size(value,1)
                      write(unit,'(e16.8)') value(i,j,k,l)
                   end do
                end do
             end do
          end do
          close(unit)
          print '(10(a,i0))','saved (',size(value,1),',',size(value,2),',',size(value,3),',',size(value,4),') to "'//trim(fname)//'"'
          flush(6)
          call_pattern % strings(call_pattern % nargs) = trim(fname)
       else
          call_pattern % strings(call_pattern % nargs) = ' '
       end if

       allocate(call_pattern % r4d(call_pattern % nargs)%values( &
             size(value,1),size(value,2),size(value,3),size(value,4)))
       call_pattern % r4d (call_pattern % nargs)%values = value
       call_pattern % intents (call_pattern % nargs) = vintent
       call_pattern % argnames (call_pattern % nargs) = name
       call_pattern % arg_types (call_pattern % nargs) = typ
    end subroutine add_real4d_arg

    subroutine add_real3d_arg(call_pattern, name, vintent, value)
#   ifndef __GFORTRAN__
       use ifport, only: getpid
#   endif
       type(tcall_pattern), intent(inout) :: call_pattern
       character(len=*), intent(in) :: name
       integer, intent(in) :: vintent
       real, intent(in) :: value(:,:,:)

       integer, parameter :: typ = tp_REAL_3D

       integer :: i, j, k
       integer :: unit
       integer, save :: idx = 0
       character(len=300) :: fname

       call_pattern % nargs = call_pattern % nargs + 1

       if (size(value) > TOO_MANY_ITEMS) then
          idx = idx + 1
          write(fname, '(a,i0,a,i0,a)') 'real3d_',getpid(),'_',idx,'.real'
          print '(10(a,i0))',trim(name)//': saving (',size(value,1),',',size(value,2),',', &
                     size(value,3),') reals to "'//trim(fname)//'"'
          flush(6)
          unit = open_generated_file(fname)
          do k = 1,size(value,3)
             do j = 1,size(value,2)
                do i = 1,size(value,1)
                   write(unit,'(e16.8)') value(i,j,k)
                end do
             end do
          end do
          close(unit)
          print '(10(a,i0))','saved (',size(value,1),',',size(value,2),',',size(value,3),') to "'//trim(fname)//'"'
          flush(6)
          call_pattern % strings(call_pattern % nargs) = trim(fname)
       else
          call_pattern % strings(call_pattern % nargs) = ' '
       end if

       allocate(call_pattern % r3d(call_pattern % nargs)%values( &
             size(value,1),size(value,2),size(value,3)))
       call_pattern % r3d (call_pattern % nargs)%values = value

       call_pattern % intents (call_pattern % nargs) = vintent
       call_pattern % argnames (call_pattern % nargs) = name
       call_pattern % arg_types (call_pattern % nargs) = typ
    end subroutine add_real3d_arg

    subroutine add_real2d_arg(call_pattern, name, vintent, value)
#   ifndef __GFORTRAN__
       use ifport, only: getpid
#   endif
       type(tcall_pattern), intent(inout) :: call_pattern
       character(len=*), intent(in) :: name
       integer, intent(in) :: vintent
       real, intent(in) :: value(:,:)

       integer, parameter :: typ = tp_REAL_2D
       integer :: i, j
       integer :: unit
       integer, save :: idx = 0
       character(len=300) :: fname

       call_pattern % nargs = call_pattern % nargs + 1

       if (size(value) > TOO_MANY_ITEMS) then
          idx = idx + 1
          write(fname, '(a,i0,a,i0,a)') 'real2d_',getpid(),'_',idx,'.real'
          print '(10(a,i0))',trim(name)//': saving (',size(value,1),',',size(value,2),') reals to "'//trim(fname)//'"'
          flush(6)
          unit = open_generated_file(fname)
          do j = 1,size(value,2)
             do i = 1,size(value,1)
                write(unit,'(e16.8)') value(i,j)
             end do
          end do
          close(unit)
          print '(10(a,i0))','saved (',size(value,1),',',size(value,2),') to "'//trim(fname)//'"'
          flush(6)
          call_pattern % strings(call_pattern % nargs) = trim(fname)
       else
          call_pattern % strings(call_pattern % nargs) = ' '
       end if

       allocate(call_pattern % r2d(call_pattern % nargs)%values(size(value,1),size(value,2)))
       call_pattern % r2d (call_pattern % nargs)%values = value
       call_pattern % intents (call_pattern % nargs) = vintent
       call_pattern % argnames (call_pattern % nargs) = name
       call_pattern % arg_types (call_pattern % nargs) = typ
    end subroutine add_real2d_arg

    subroutine add_real1d_arg(call_pattern, name, vintent, value)
#   ifndef __GFORTRAN__
       use ifport, only: getpid
#   endif
       type(tcall_pattern), intent(inout) :: call_pattern
       character(len=*), intent(in) :: name
       integer, intent(in) :: vintent
       real, intent(in) :: value(:)

       integer, parameter :: typ = tp_REAL_1D

       integer :: i
       integer :: unit
       integer, save :: idx = 0
       character(len=300) :: fname

       call_pattern % nargs = call_pattern % nargs + 1

       if (size(value) > TOO_MANY_ITEMS) then
          idx = idx + 1
          write(fname, '(a,i0,a,i0,a)') 'real1d_',getpid(),'_',idx,'.real'
          print '(10(a,i0))',trim(name)//': saving (',size(value,1),') reals to "'//trim(fname)//'"'
          flush(6)
          unit = open_generated_file(fname)
          do i = 1,size(value,1)
             write(unit,'(e16.8)') value(i)
          end do
          close(unit)
          print '(10(a,i0))','saved (',size(value,1),') to "'//trim(fname)//'"'
          flush(6)
          call_pattern % strings(call_pattern % nargs) = trim(fname)
       else
          call_pattern % strings(call_pattern % nargs) = ' '
       end if

       allocate(call_pattern % r1d(call_pattern % nargs)%values( &
             size(value,1)))
       call_pattern % r1d (call_pattern % nargs)%values = value

       call_pattern % intents (call_pattern % nargs) = vintent
       call_pattern % argnames (call_pattern % nargs) = name
       call_pattern % arg_types (call_pattern % nargs) = typ
    end subroutine add_real1d_arg

    subroutine print_array_declaration(name, typename, dims, vintent, fname)
    implicit none
       character(len=*), intent(in) :: name, typename
       integer,          intent(in) :: dims(:)
       integer,          intent(in) :: vintent
       character(len=*), intent(in) :: fname

       integer :: j
       if (fname==' ') then
          if (vintent == intent_OUT) then
             write(print_unit, '(10(a,i0))') '        '//typename//' :: '//name//'(',dims(1), &
                   (',',dims(j),j=2,size(dims)),')'
          else if (size(dims)==1) then
             write(print_unit, '(10(a,i0))') '        '//typename//' :: '//name//'(',dims(1), &
                   (',',dims(j),j=2,size(dims)),') = (/ &'
          else
             write(print_unit, '(10(a,i0))') '        '//typename//' :: '//name//'(',dims(1), &
                   (',',dims(j),j=2,size(dims)),') = reshape( (/ &'
          end if
       else
          write(print_unit, '(10(a))') '        '//typename//', allocatable :: '//name//'(:', &
                (',:',j=2,size(dims)),')'
       end if
    end subroutine print_array_declaration

    subroutine print_string1d_array(name, value, fname)
       character(len=*), intent(in) :: value(:)
       character(len=*), intent(in) :: name, fname

       integer :: nvals, i, strlen
       character(len=40) :: typnam
       character(len=500) :: line
       nvals = size(value,1) 
       strlen = 1
       do i = 1,nvals
          strlen = max(strlen, len_trim(value(i)))
       end do
       write(typnam,'(10(a,i0))') 'character(len=',strlen,')'
       call print_array_declaration(trim(name), trim(typnam), shape(value), intent_IN, fname)
       if (fname /= ' ') return
       do i = 1,nvals-1
          write(print_unit, '(10(a,i0))') '        "'//value(i)(1:strlen)//'", &'
       end do
       write(print_unit, '(10(a,i0))') '        "'//value(nvals)(1:strlen)//'" /)'
    end subroutine print_string1d_array

    subroutine print_int2d_array(name, value, fname)
       integer,          intent(in) :: value(:,:)
       character(len=*), intent(in) :: name, fname

       integer :: nvals, mvals, j, k
       character(len=500) :: line
       call print_array_declaration(trim(name), 'integer', shape(value), intent_IN, fname)
       if (fname /= ' ') return
       line = ' '
       nvals = size(value,1) 
       mvals = size(value,2)
       do k = 1,mvals
          do j = 1,nvals
             if (line == ' ') then
                 write(line,'(a,i0,a)') '           ',value(j,k)
             else
                 write(line,'(a,i0,a)') trim(line)//' ',value(j,k)
             end if
             if (k<mvals .or. j<nvals) line = trim(line) // ','
             if (len_trim(line) > cutoff_at .and. j<nvals) then
                write(print_unit, '(a)') trim(line)//" &"
                line = ''
             end if
          end do
       end do
       if (line == " ") then
          write(print_unit, '(10(a,i0))') '          /), (/',nvals,',',mvals,'/) )'
       else
          write(print_unit, '(a)') trim(line)//"/), &"
          write(print_unit, '(10(a,i0))') '          (/',nvals,',',mvals,'/) )'
       end if
    end subroutine print_int2d_array

    subroutine print_int1d_array(name, value, fname)
       integer,          intent(in) :: value(:)
       character(len=*), intent(in) :: name, fname

       integer :: nvals, j
       character(len=500) :: line
       nvals = size(value,1) 
       call print_array_declaration(trim(name), 'integer', shape(value), intent_IN, fname)
       if (fname /= ' ') return
       line = ' '
       do j = 1,nvals
          if (line == ' ') then
              write(line,'(a,i0,a)') '           ',value(j)
          else
              write(line,'(a,i0,a)') trim(line)//' ',value(j)
          end if
          if (j<nvals) line = trim(line) // ','
          if (len_trim(line) > cutoff_at .and. j<nvals) then
             write(print_unit, '(a)') trim(line)//" &"
             line = ''
          end if
       end do
       if (line == " ") then
          write(print_unit, '(a)') '          /)'
       else
          write(print_unit, '(a)') trim(line)//"/)"
       end if
    end subroutine print_int1d_array

    subroutine print_real5d_array(name, value, fname)
       real,             intent(in) :: value(:,:,:,:,:)
       character(len=*), intent(in) :: name
       character(len=*), intent(in) :: fname

       integer :: nvals, mvals, lvals, kvals, jvals, i, j, k, l, m
       character(len=500) :: line
       call print_array_declaration(trim(name), 'real', shape(value), intent_IN, fname)
       if (fname /= ' ') return
       line = ' '
       nvals = size(value,1) 
       mvals = size(value,2)
       lvals = size(value,3)
       kvals = size(value,4)
       jvals = size(value,5)
       do m = 1,jvals
          do l = 1,kvals
             do k = 1,lvals
                do j = 1,mvals
                   do i = 1,nvals
                      if (line == ' ') then
                          write(line,'(a,1p,e16.8,a)') '           ',value(i,j,k,l,m)
                      else
                          write(line,'(a,1p,e16.8,a)') trim(line)//' ',value(i,j,k,l,m)
                      end if
                      if (k<lvals .or. j<mvals .or. i<nvals .or. l<kvals .or. m<jvals) then
                         line = trim(line) // ','
                         if (len_trim(line) > cutoff_at) then
                            write(print_unit, '(a)') trim(line)//" &"
                            line = ''
                         end if
                      end if
                   end do
                end do
             end do
          end do
       end do
       if (line == " ") then
          write(print_unit, '(10(a,i0))') '          /), (/',nvals,',',mvals,',',lvals,',',kvals,',',mvals,'/) )'
       else
          write(print_unit, '(a)') trim(line)//"/), &"
          write(print_unit, '(10(a,i0))') '          (/',nvals,',',mvals,',',lvals,',',kvals,',',mvals,'/) )'
       end if
    end subroutine print_real5d_array

    subroutine print_real4d_array(name, value, fname)
       real,             intent(in) :: value(:,:,:,:)
       character(len=*), intent(in) :: name
       character(len=*), intent(in) :: fname

       integer :: nvals, mvals, lvals, kvals, i, j, k, l
       character(len=500) :: line
       call print_array_declaration(trim(name), 'real', shape(value), intent_IN, fname)
       if (fname /= ' ') return
       line = ' '
       nvals = size(value,1) 
       mvals = size(value,2)
       lvals = size(value,3)
       kvals = size(value,4)
       do l = 1,kvals
          do k = 1,lvals
             do j = 1,mvals
                do i = 1,nvals
                   if (line == ' ') then
                       write(line,'(a,1p,e16.8,a)') '           ',value(i,j,k,l)
                   else
                       write(line,'(a,1p,e16.8,a)') trim(line)//' ',value(i,j,k,l)
                   end if
                   if (k<lvals .or. j<mvals .or. i<nvals .or. l<kvals) then
                      line = trim(line) // ','
                      if (len_trim(line) > cutoff_at) then
                         write(print_unit, '(a)') trim(line)//" &"
                         line = ''
                      end if
                   end if
                end do
             end do
          end do
       end do
       if (line == " ") then
          write(print_unit, '(10(a,i0))') '          /), (/',nvals,',',mvals,',',lvals,',',kvals,'/) )'
       else
          write(print_unit, '(a)') trim(line)//"/), &"
          write(print_unit, '(10(a,i0))') '          (/',nvals,',',mvals,',',lvals,',',kvals,'/) )'
       end if
    end subroutine print_real4d_array

    subroutine print_real3d_array(name, value, fname)
       real,             intent(in) :: value(:,:,:)
       character(len=*), intent(in) :: name
       character(len=*), intent(in) :: fname

       integer :: nvals, mvals, lvals, i, j, k
       character(len=500) :: line
       call print_array_declaration(trim(name), 'real', shape(value), intent_IN, fname)
       if (fname /= ' ') return
       line = ' '
       nvals = size(value,1) 
       mvals = size(value,2)
       lvals = size(value,3)
       do k = 1,lvals
          do j = 1,mvals
             do i = 1,nvals
                if (line == ' ') then
                    write(line,'(a,1p,e16.8,a)') '           ',value(i,j,k)
                else
                    write(line,'(a,1p,e16.8,a)') trim(line)//' ',value(i,j,k)
                end if
                if (k<lvals .or. j<mvals .or. i<nvals) then
                   line = trim(line) // ','
                   if (len_trim(line) > cutoff_at) then
                      write(print_unit, '(a)') trim(line)//" &"
                      line = ''
                   end if
                end if
             end do
          end do
       end do
       if (line == " ") then
          write(print_unit, '(10(a,i0))') '          /), (/',nvals,',',mvals,',',lvals,'/) )'
       else
          write(print_unit, '(a)') trim(line)//"/), &"
          write(print_unit, '(10(a,i0))') '          (/',nvals,',',mvals,',',lvals,'/) )'
       end if
    end subroutine print_real3d_array

    subroutine print_real2d_array(name, value, fname)
       real,             intent(in) :: value(:,:)
       character(len=*), intent(in) :: name
       character(len=*), intent(in) :: fname

       integer :: nvals, mvals, lvals, i, j
       character(len=500) :: line
       call print_array_declaration(trim(name), 'real', shape(value), intent_IN, fname)
       if (fname /= ' ') return
       line = ' '
       nvals = size(value,1) 
       mvals = size(value,2)
       do j = 1,mvals
          do i = 1,nvals
             if (line == ' ') then
                 write(line,'(a,1p,e16.8,a)') '           ',value(i,j)
             else
                 write(line,'(a,1p,e16.8,a)') trim(line)//' ',value(i,j)
             end if
             if (j<mvals .or. i<nvals) then
                line = trim(line) // ','
                if (len_trim(line) > cutoff_at) then
                   write(print_unit, '(a)') trim(line)//" &"
                   line = ''
                end if
             end if
          end do
       end do
       if (line == " ") then
          write(print_unit, '(10(a,i0))') '          /), (/',nvals,',',mvals,'/) )'
       else
          write(print_unit, '(a)') trim(line)//"/), &"
          write(print_unit, '(10(a,i0))') '          (/',nvals,',',mvals,'/) )'
       end if
    end subroutine print_real2d_array

    subroutine print_real1d_array(name, value, fname)
       real,             intent(in) :: value(:)
       character(len=*), intent(in) :: name
       character(len=*), intent(in) :: fname

       integer :: nvals, i
       character(len=500) :: line
       print *,'declaring real array '//trim(name)//': ',shape(value)
       call print_array_declaration(trim(name), 'real', shape(value), intent_IN, fname)
       if (fname /= ' ') return
       nvals = size(value,1)
       line = ' '
       do i = 1,nvals
          if (line == ' ') then
              write(line,'(a,1p,e16.8,a)') '           ',value(i)
          else
              write(line,'(a,1p,e16.8,a)') trim(line)//' ',value(i)
          end if
          if (i<nvals) then
             line = trim(line) // ','
             if (len_trim(line) > cutoff_at) then
                write(print_unit, '(a)') trim(line)//" &"
                line = ''
             end if
          end if
       end do
       if (line == " ") then
          write(print_unit, '(a)') '          /)'
       else
          write(print_unit, '(a)') trim(line)//"/)"
       end if
    end subroutine print_real1d_array


    subroutine print_logical1d_array(name, value)
       logical,          intent(in) :: value(:)
       character(len=*), intent(in) :: name

       integer :: nvals, j
       character(len=1), parameter :: fname = ' '
       character(len=500) :: line
       call print_array_declaration(trim(name), 'real', shape(value), intent_IN, fname)
       if (fname /= ' ') return
       line = ' '
       nvals = size(value,1) 
       
       do j = 1,nvals
          if (line == ' ') then
              write(line,'(a,a,a)') '           ', logical_to_string(value(j))
          else
              write(line,'(a,a,a)') trim(line)//' ', logical_to_string(value(j))
          end if
          if (j<nvals) line = trim(line) // ','
          if (len_trim(line) > cutoff_at .and. j<nvals) then
             write(print_unit, '(a)') trim(line)//" &"
             line = ''
          end if
       end do
       if (line == " ") then
          write(print_unit, '(a)') '          /)'
       else
          write(print_unit, '(a)') trim(line)//"/)"
       end if
    end subroutine print_logical1d_array

    subroutine print_double_value(name, value)
       double precision,             intent(in) :: value
       character(len=*), intent(in) :: name
       write(print_unit, '(a,1p,e16.8)') '        double precision, parameter :: '//trim(name)//' = ',value
    end subroutine print_double_value
    subroutine print_real_value(name, value)
       real,             intent(in) :: value
       character(len=*), intent(in) :: name
       write(print_unit, '(a,1p,e16.8)') '        real, parameter :: '//trim(name)//' = ',value
    end subroutine print_real_value

    subroutine print_integer_value(name, value)
       integer,          intent(in) :: value
       character(len=*), intent(in) :: name
       write(print_unit, '(a,i0)') '        integer, parameter :: '//trim(name)//' = ',value
    end subroutine print_integer_value

    function replace_string(string_in, value_in, value_out) result(string_out)
    implicit none
       character(len=*), intent(in) :: string_in, value_in, value_out
       character(len=250) :: string_out, hlp
       integer :: istart, nvalue_in
       nvalue_in = len(value_in)
       string_out = trim(string_in)
       do 
          istart = index(string_out, value_in)
          if (istart==0) exit
          hlp = string_out
          string_out = hlp(:istart-1)//trim(value_out)//trim(hlp(istart+nvalue_in:))
       end do
    end function replace_string

    subroutine print_derived_text(name, type_name, text_in)
       character(len=*), intent(in) :: type_name
       character(len=*), intent(in) :: name
       type(text), intent(in) :: text_in
       integer :: i
       do i = 1,size(text_in%strings,1)
          write(print_unit, '(a)') '        '//trim(replace_string(text_in%strings(i),'${name}', name))
       end do
    end subroutine print_derived_text

    subroutine print_string_value(name, value)
       character(len=*), intent(in) :: value
       character(len=*), intent(in) :: name
       write(print_unit, '(a)') '        character(len=*), parameter :: '//trim(name)//' = "'//trim(value)//'"'
    end subroutine print_string_value

    subroutine print_error_value(name, haserror, message)
       logical,          intent(in) :: haserror
       character(len=*), intent(in) :: message
       character(len=*), intent(in) :: name
       call print_value(trim(name)//'_haserror', haserror)
       call print_value(trim(name)//'_message', message)
    end subroutine print_error_value

    subroutine print_logical_value(name, value)
       logical,          intent(in) :: value
       character(len=*), intent(in) :: name
       write(print_unit, '(a)') '        logical, parameter :: '//trim(name)//' = '//logical_to_string(value)
    end subroutine print_logical_value

    subroutine print_string1d_declaration(vintent, value, name, fname)
       integer,          intent(in) :: vintent
       character(len=*), intent(in) :: value(:)
       character(len=*), intent(in) :: name, fname

       integer :: nvals, i, strlen
       character(len=40) :: typnam
       nvals = size(value,1) 
       strlen = 1
       do i = 1,nvals
          strlen = max(strlen, len_trim(value(i)))
       end do
       if (vintent == intent_OUT) then
          write(typnam,'(10(a,i0))') 'character(len=',strlen,')'
          call print_array_declaration(trim(name), trim(typnam), shape(value), vintent, fname)
          call print_array('ref_'//trim(name), value, fname)
       else if (vintent == intent_INOUT) then
          call print_array('in_'//trim(name), value, fname)
       else
          call print_array(trim(name), value, fname)
       end if
    end subroutine print_string1d_declaration

    subroutine print_int2d_declaration(vintent, value, name, fname)
       integer,          intent(in) :: vintent
       integer,          intent(in) :: value(:,:)
       character(len=*), intent(in) :: name, fname

       integer :: nvals, mvals
       nvals = size(value,1) 
       mvals = size(value,2)
       if (vintent == intent_OUT) then
          call print_array_declaration(trim(name), 'integer', shape(value), vintent, fname)
          call print_array('ref_'//trim(name), value, fname)
       else if (vintent == intent_INOUT) then
          call print_array('in_'//trim(name), value, fname)
       else
          call print_array(trim(name), value, fname)
       end if
    end subroutine print_int2d_declaration

    subroutine print_real5d_declaration(vintent, value, name, fname)
       integer,          intent(in) :: vintent
       real,             intent(in) :: value(:,:,:,:,:)
       character(len=*), intent(in) :: name, fname

       integer :: nvals, mvals, kvals, lvals
       nvals = size(value,1) 
       mvals = size(value,2)
       lvals = size(value,3)
       kvals = size(value,4)
       mvals = size(value,5)
       if (vintent == intent_OUT) then
          call print_array_declaration(trim(name), 'real', shape(value), vintent, fname)
          call print_array('ref_'//trim(name), value, fname)
       else if (vintent == intent_INOUT) then
          call print_array('in_'//trim(name), value, fname)
       else
          call print_array(trim(name), value, fname)
       end if
    end subroutine print_real5d_declaration

    subroutine print_real4d_declaration(vintent, value, name, fname)
       integer,          intent(in) :: vintent
       real,             intent(in) :: value(:,:,:,:)
       character(len=*), intent(in) :: name, fname

       integer :: nvals, mvals, kvals, lvals
       nvals = size(value,1) 
       mvals = size(value,2)
       lvals = size(value,3)
       kvals = size(value,4)
       if (vintent == intent_OUT) then
          call print_array_declaration(trim(name), 'real', shape(value), vintent, fname)
          call print_array('ref_'//trim(name), value, fname)
       else if (vintent == intent_INOUT) then
          call print_array('in_'//trim(name), value, fname)
       else
          call print_array(trim(name), value, fname)
       end if
    end subroutine print_real4d_declaration

    subroutine print_real3d_declaration(vintent, value, name, fname)
       integer,          intent(in) :: vintent
       real,             intent(in) :: value(:,:,:)
       character(len=*), intent(in) :: name, fname

       integer :: nvals, mvals, lvals
       nvals = size(value,1) 
       mvals = size(value,2)
       lvals = size(value,3)
       if (vintent == intent_OUT) then
          call print_array_declaration(trim(name), 'real', shape(value), vintent, fname)
          call print_array('ref_'//trim(name), value, fname)
       else if (vintent == intent_INOUT) then
          call print_array('in_'//trim(name), value, fname)
       else
          call print_array(trim(name), value, fname)
       end if
    end subroutine print_real3d_declaration

    subroutine print_real2d_declaration(vintent, value, name, fname)
       integer,          intent(in) :: vintent
       real,             intent(in) :: value(:,:)
       character(len=*), intent(in) :: name, fname

       integer :: nvals, mvals
       nvals = size(value,1) 
       mvals = size(value,2)
       if (vintent == intent_OUT) then
          call print_array_declaration(trim(name), 'real', shape(value), vintent, fname)
          call print_array('ref_'//trim(name), value, fname)
       else if (vintent == intent_INOUT) then
          call print_array('in_'//trim(name), value, fname)
       else
          call print_array(trim(name), value, fname)
       end if
    end subroutine print_real2d_declaration

    subroutine print_real1d_declaration(vintent, value, name, fname)
       integer,          intent(in) :: vintent
       real,             intent(in) :: value(:)
       character(len=*), intent(in) :: name, fname

       integer :: nvals
       nvals = size(value,1) 
       if (vintent == intent_OUT) then
          call print_array_declaration(trim(name), 'real', shape(value), vintent, fname)
          call print_array('ref_'//trim(name), value, fname)
       else if (vintent == intent_INOUT) then
          call print_array('in_'//trim(name), value, fname)
       else
          call print_array(trim(name), value, fname)
       end if
    end subroutine print_real1d_declaration

    subroutine print_short1d_declaration(vintent, value, name)
       integer,          intent(in) :: vintent
       integer,          intent(in) :: value(:)
       character(len=*), intent(in) :: name

       character(len=1), parameter :: fname = ' '
       integer :: nvals
       nvals = size(value,1) 
       if (vintent == intent_OUT) then
          call print_array_declaration(trim(name), 'integer*2', shape(value), vintent, fname)
          call print_array('ref_'//trim(name), value, '')
       else if (vintent == intent_INOUT) then
          call print_array('in_'//trim(name), value, '')
       else
          call print_array(trim(name), value, '')
       end if
    end subroutine print_short1d_declaration

       

    subroutine print_int1d_declaration(vintent, value, name, fname)
       integer,          intent(in) :: vintent
       integer,          intent(in) :: value(:)
       character(len=*), intent(in) :: name, fname

       integer :: nvals
       nvals = size(value,1) 
       if (vintent == intent_OUT) then
          call print_array_declaration(trim(name), 'integer', shape(value), vintent, fname)
          call print_array('ref_'//trim(name), value, fname)
       else if (vintent == intent_INOUT) then
          call print_array('in_'//trim(name), value, fname)
       else
          call print_array(trim(name), value, fname)
       end if
    end subroutine print_int1d_declaration

    subroutine print_logical_declaration(vintent, value, name)
       integer,          intent(in) :: vintent
       logical,          intent(in) :: value
       character(len=*), intent(in) :: name
       if (vintent== intent_OUT) then
          write(print_unit, '(a)')    '        logical :: '//trim(name)
          call print_value('ref_'//trim(name), value)
       else if (vintent == intent_INOUT) then
          call print_value('in_'//trim(name), value)
       else 
          call print_value(trim(name), value)
       end if
    end subroutine print_logical_declaration

    subroutine print_logical1d_declaration(vintent, value, name)
      integer,          intent(in) :: vintent
      logical,          intent(in) :: value(:)
      character(len=*), intent(in) :: name

       character(len=1), parameter :: fname = ' '
      integer :: nvals
      nvals = size(value,1) 
      if (vintent == intent_OUT) then
         call print_array_declaration(trim(name), 'logical', shape(value), vintent, fname)
         call print_array('ref_'//trim(name), value)
      else if (vintent == intent_INOUT) then
         call print_array('in_'//trim(name), value)
      else
         call print_array(trim(name), value)
      end if
   end subroutine print_logical1d_declaration


    subroutine print_int_declaration(vintent, value, name)
       integer,          intent(in) :: vintent
       integer,          intent(in) :: value
       character(len=*), intent(in) :: name
       if (vintent== intent_OUT) then
          write(print_unit, '(a)')    '        integer :: '//trim(name)
          call print_value('ref_'//trim(name), value)
       else if (vintent == intent_INOUT) then
          call print_value('in_'//trim(name), value)
       else 
          call print_value(trim(name), value)
       end if
    end subroutine print_int_declaration

    subroutine print_double_declaration(vintent, value, name)
       integer,          intent(in) :: vintent
       double precision, intent(in) :: value
       character(len=*), intent(in) :: name
       if (vintent== intent_OUT) then
          write(print_unit, '(a)')    '        double precision :: '//trim(name)
          call print_value('ref_'//trim(name), value)
       else if (vintent == intent_INOUT) then
          call print_value('in_'//trim(name), value)
       else 
          call print_value(trim(name), value)
       end if
    end subroutine print_double_declaration
    subroutine print_real_declaration(vintent, value, name)
       integer,          intent(in) :: vintent
       real,             intent(in) :: value
       character(len=*), intent(in) :: name
       if (vintent== intent_OUT) then
          write(print_unit, '(a)')    '        real :: '//trim(name)
          call print_value('ref_'//trim(name), value)
       else if (vintent == intent_INOUT) then
          call print_value('in_'//trim(name), value)
       else 
          call print_value(trim(name), value)
       end if
    end subroutine print_real_declaration

    subroutine print_string_declaration(vintent, value, name)
       integer,          intent(in) :: vintent
       character(len=*), intent(in) :: value
       character(len=*), intent(in) :: name
       if (vintent== intent_OUT) then
          write(print_unit, '(a)')    '        character(len=200) :: '//trim(name)
          call print_value('ref_'//trim(name), value)
       else if (vintent == intent_INOUT) then
          call print_value('in_'//trim(name), value)
       else 
          call print_value(trim(name), value)
       end if
    end subroutine print_string_declaration

    subroutine print_derived_declaration(vintent, value, name)
       integer,          intent(in) :: vintent
       character(len=*), intent(in) :: value
       character(len=*), intent(in) :: name
       if (vintent== intent_OUT) then
          write(print_unit, '(a)')    '        type('//trim(value)//') :: '//trim(name)
          write(print_unit, '(a)')    '        type('//trim(value)//') :: ref_'//trim(name)
       else if (vintent == intent_INOUT) then
          continue
       else 
          write(print_unit, '(a)')    '        type('//trim(value)//') :: '//trim(name)
       end if
    end subroutine print_derived_declaration

    subroutine print_any_declaration(call_pattern, i)
       type(tcall_pattern), intent(in) :: call_pattern
       integer,             intent(in) :: i

       if (call_pattern % arg_types(i) == tp_DERIVED) then
          call print_derived_declaration(call_pattern % intents(i), call_pattern%strings(i), call_pattern%argnames(i))
       else if (call_pattern % arg_types(i) == tp_STRING) then
          call print_declaration(call_pattern % intents(i), call_pattern%strings(i), call_pattern%argnames(i))
       else if (call_pattern % arg_types(i) == tp_INTEGER) then
          call print_declaration(call_pattern % intents(i), call_pattern%ints(i), call_pattern%argnames(i))
       else if (call_pattern % arg_types(i) == tp_LOGICAL) then
          call print_declaration(call_pattern % intents(i), call_pattern%logicals(i), call_pattern%argnames(i))
       else if (call_pattern % arg_types(i) == tp_LOGICAL_1D) then
          call print_declaration(call_pattern % intents(i), call_pattern%l1d(i)%values, call_pattern%argnames(i))
       else if (call_pattern % arg_types(i) == tp_DOUBLE) then
          call print_declaration(call_pattern % intents(i), call_pattern%doubles(i), call_pattern%argnames(i))
       else if (call_pattern % arg_types(i) == tp_REAL) then
          call print_declaration(call_pattern % intents(i), call_pattern%reals(i), call_pattern%argnames(i))
       else if (call_pattern % arg_types(i) == tp_SHORT_1D) then
          call print_short1d_declaration(call_pattern % intents(i), call_pattern%i1d(i)%values, call_pattern%argnames(i))
       else if (call_pattern % arg_types(i) == tp_STRING_1D) then
          call print_declaration(call_pattern % intents(i), call_pattern%initialization(i)%strings, &
                                        call_pattern%argnames(i), call_pattern%strings(i))
       else if (call_pattern % arg_types(i) == tp_INTEGER_1D) then
          call print_declaration(call_pattern % intents(i), call_pattern%i1d(i)%values, &
                                        call_pattern%argnames(i), call_pattern%strings(i))
       else if (call_pattern % arg_types(i) == tp_INTEGER_2D) then
          call print_declaration(call_pattern % intents(i), call_pattern%i2d(i)%values, &
                                        call_pattern%argnames(i), call_pattern%strings(i))
       else if (call_pattern % arg_types(i) == tp_REAL_1D) then
          call print_declaration(call_pattern % intents(i), call_pattern%r1d(i)%values, &
                                        call_pattern%argnames(i), call_pattern%strings(i))
       else if (call_pattern % arg_types(i) == tp_REAL_2D) then
          call print_declaration(call_pattern % intents(i), call_pattern%r2d(i)%values, &
                                        call_pattern%argnames(i), call_pattern%strings(i))
       else if (call_pattern % arg_types(i) == tp_REAL_4D) then
          call print_declaration(call_pattern % intents(i), call_pattern%r4d(i)%values, &
                                        call_pattern%argnames(i), call_pattern%strings(i))
       else if (call_pattern % arg_types(i) == tp_REAL_5D) then
          call print_declaration(call_pattern % intents(i), call_pattern%r5d(i)%values, &
                                        call_pattern%argnames(i), call_pattern%strings(i))
       else if (call_pattern % arg_types(i) == tp_REAL_3D) then
          call print_declaration(call_pattern % intents(i), call_pattern%r3d(i)%values, &
                                        call_pattern%argnames(i), call_pattern%strings(i))
       else
          print '(a,i0)', '       illegal variable type ',call_pattern%arg_types(i)
       end if
    end subroutine print_any_declaration

    subroutine print_allocation(name, dims)
    implicit none
       character(len=*), intent(in) :: name
       integer :: dims(:)
       integer :: j
       write(print_unit, '(10(a,i0))') &
             '        allocate('//name// &
                      '(',dims(1),(',', dims(j), j=2,size(dims)),'), '// &
                      'stat=log_call_stat)' 
       write(print_unit, '(10(a,i0))') &
             '        call AssertEqual(log_call_stat,0,"allocation of '''// &
             name//'''", __LINE__, __FILE__)'
    end subroutine print_allocation

    subroutine print_read(name, dims, vintent, fname)
    implicit none
       character(len=*), intent(in) :: name
       integer,          intent(in) :: dims(:)
       integer,          intent(in) :: vintent
       character(len=*), intent(in) :: fname
       integer :: j
       character(len=*), parameter :: blanks = '                  '
       character(len=50) :: varnam

       if (vintent==intent_OUT) then
          varnam = 'ref_'//name
       else if (vintent==intent_INOUT) then
          varnam = 'in_'//name
       else
          varnam = name
       end if

       write(print_unit, '(a)') &
              '        open(newunit=fid, file="./level_1/resources/'//trim(fname)//'", status="old", action="read", iostat=log_call_stat)'
       write(print_unit, '(10(a,i0))') &
             '        call AssertEqual(log_call_stat,0,"opening '''// &
             trim(fname)//''' for reading variable '//name//'", __LINE__, __FILE__)'
  
       do j = size(dims),1,-1
          write(print_unit, '(10(a,i0))')'        '//blanks(1:3*(size(dims)-j))//'do i',j,' = 1,',dims(j)
       end do
       write(print_unit, '(a,10(a,i0))') '        '//blanks(1:3*size(dims))//'read(fid,*) '//trim(varnam)//'(i1',(', i',j,j=2,size(dims)),')'
       do j = size(dims),1,-1
          write(print_unit, '(10(a,i0))') '        '//blanks(1:3*(j-1))//'end do'
       end do
       write(print_unit, '(a)') &
              '        close(unit=fid, iostat=log_call_stat)'
       write(print_unit, '(10(a,i0))') &
             '        call AssertEqual(log_call_stat,0,"closing '''// &
             trim(fname)//''' for reading variable '//name//'", __LINE__, __FILE__)'
    end subroutine print_read
       


    subroutine print_initialization(call_pattern,i)
    implicit none
       type(tcall_pattern), intent(in) :: call_pattern
       integer, intent(in) :: i

       if (call_pattern%arg_types(i) == tp_DERIVED) then
          if (call_pattern%intents(i) == intent_OUT) then
              call print_derived_text('ref_'//call_pattern%argnames(i), call_pattern%strings(i), call_pattern%initialization(i))
          else
              call print_derived_text(call_pattern%argnames(i), call_pattern%strings(i), call_pattern%initialization(i))
          endif
       else if ( call_pattern%arg_types(i) == tp_INTEGER_1D .and.  &
                 call_pattern%strings(i)/= ' ') then 
          if (call_pattern%intents(i) == intent_OUT) then
              call print_allocation('ref_'//trim(call_pattern%argnames(i)),shape(call_pattern%i1d(i)%values))
          end if
          call print_allocation(trim(call_pattern%argnames(i)),shape(call_pattern%i1d(i)%values))
          call print_read(trim(call_pattern%argnames(i)),shape(call_pattern%i1d(i)%values), &
                          call_pattern%intents(i), call_pattern%strings(i))
       else if ( call_pattern%arg_types(i) == tp_INTEGER_2D .and.  &
                 call_pattern%strings(i)/= ' ') then 
          if (call_pattern%intents(i) == intent_OUT) then
              call print_allocation('ref_'//trim(call_pattern%argnames(i)),shape(call_pattern%i2d(i)%values))
          end if
          call print_allocation(trim(call_pattern%argnames(i)),shape(call_pattern%i2d(i)%values))
          call print_read(trim(call_pattern%argnames(i)),shape(call_pattern%i2d(i)%values), &
                          call_pattern%intents(i), call_pattern%strings(i))
       else if ( call_pattern%arg_types(i) == tp_REAL_1D .and.  &
                 call_pattern%strings(i)/= ' ') then 
          if (call_pattern%intents(i) == intent_OUT) then
              call print_allocation('ref_'//trim(call_pattern%argnames(i)),shape(call_pattern%r1d(i)%values))
          end if
          call print_allocation(trim(call_pattern%argnames(i)),shape(call_pattern%r1d(i)%values))
          call print_read(trim(call_pattern%argnames(i)),shape(call_pattern%r1d(i)%values), &
                          call_pattern%intents(i), call_pattern%strings(i))
       else if ( call_pattern%arg_types(i) == tp_REAL_2D .and.  &
                 call_pattern%strings(i)/= ' ') then 
          if (call_pattern%intents(i) == intent_OUT) then
              call print_allocation('ref_'//trim(call_pattern%argnames(i)),shape(call_pattern%r2d(i)%values))
          end if
          call print_allocation(trim(call_pattern%argnames(i)),shape(call_pattern%r2d(i)%values))
          call print_read(trim(call_pattern%argnames(i)),shape(call_pattern%r2d(i)%values), &
                          call_pattern%intents(i), call_pattern%strings(i))
       else if ( call_pattern%arg_types(i) == tp_REAL_3D .and.  &
                 call_pattern%strings(i)/= ' ') then 
          if (call_pattern%intents(i) == intent_OUT) then
              call print_allocation('ref_'//trim(call_pattern%argnames(i)),shape(call_pattern%r3d(i)%values))
          end if
          call print_allocation(trim(call_pattern%argnames(i)),shape(call_pattern%r3d(i)%values))
          call print_read(trim(call_pattern%argnames(i)),shape(call_pattern%r3d(i)%values), &
                          call_pattern%intents(i), call_pattern%strings(i))
       else if ( call_pattern%arg_types(i) == tp_REAL_4D .and.  &
                 call_pattern%strings(i)/= ' ') then 
          if (call_pattern%intents(i) == intent_OUT) then
              call print_allocation('ref_'//trim(call_pattern%argnames(i)),shape(call_pattern%r4d(i)%values))
          end if
          call print_allocation(trim(call_pattern%argnames(i)),shape(call_pattern%r4d(i)%values))
          call print_read(trim(call_pattern%argnames(i)),shape(call_pattern%r4d(i)%values), &
                          call_pattern%intents(i), call_pattern%strings(i))
       else if ( call_pattern%arg_types(i) == tp_REAL_5D .and.  &
                 call_pattern%strings(i)/= ' ') then 
          if (call_pattern%intents(i) == intent_OUT) then
              call print_allocation('ref_'//trim(call_pattern%argnames(i)),shape(call_pattern%r5d(i)%values))
          end if
          call print_allocation(trim(call_pattern%argnames(i)),shape(call_pattern%r5d(i)%values))
          call print_read(trim(call_pattern%argnames(i)),shape(call_pattern%r5d(i)%values), &
                          call_pattern%intents(i), call_pattern%strings(i))

       else if ( call_pattern%arg_types(i) == tp_STRING_1D .and.  &
                 call_pattern%strings(i)/= ' ') then 
          if (call_pattern%intents(i) == intent_OUT) then
              call print_allocation('ref_'//trim(call_pattern%argnames(i)),shape(call_pattern%initialization(i)%strings))
          end if
          call print_allocation(trim(call_pattern%argnames(i)),shape(call_pattern%initialization(i)%strings))
       elseif (call_pattern%intents(i) == intent_INOUT) then
          write(print_unit, '(a)') '        '//trim(call_pattern%argnames(i))//' = in_'//trim(call_pattern%argnames(i))
       end if

    end subroutine print_initialization

    subroutine print_test(call_pattern, fname)
       type(tcall_pattern),        intent(in) :: call_pattern
       character(len=*), optional, intent(in) :: fname
       character(len=500) :: line
       integer :: i
       if (present(fname)) call set_generated_code_file(fname)
       write(print_unit, *)
       write(print_unit, '(a)') 'module m_test_'//trim(call_pattern%routinenaam)
       write(print_unit, '(a)') 'implicit none'
       write(print_unit, '(a)') 'contains'
       write(print_unit, '(a)') '   subroutine test_'//trim(call_pattern%routinenaam)//'()'
       do i = 1,call_pattern%n_uses
           write(print_unit, '(a)') '   use '//trim(call_pattern%uses(i))
       end do
       write(print_unit, '(a)') '       integer         :: log_call_stat, i1, i2, i3, i4, i5, fid' 
       write(print_unit, '(a)') '       real, parameter :: tol = 1e-5'
       do i=1,call_pattern % nargs
          call print_declaration(call_pattern, i)
       end do

       do i=1,call_pattern % nargs
          call print_initialization(call_pattern, i)

       end do

       line = '        call '//trim(call_pattern%routinenaam)//'('
       do i=1,call_pattern % nargs
          if (call_pattern%intents(i) == intent_INOUT) cycle
          if (line == " ") then
              line = "            "//trim(call_pattern%argnames(i))
          else
              line = trim(line) // " "//trim(call_pattern%argnames(i))
          end if
          if (i<call_pattern % nargs) line = trim(line) // ","
          if (len_trim(line) > cutoff_at .and. i<call_pattern%nargs) then
             write(print_unit, '(a)') trim(line)//" &"
             line = ''
          end if
       end do
       if (line == " ") then
          write(print_unit, '(a)') '          )'
       else
          write(print_unit, '(a)') trim(line)//")"
       end if

       do i=1,call_pattern % nargs

          if (call_pattern % intents(i) == intent_IN) cycle
          if (call_pattern % intents(i) == intent_INOUT) cycle

          if (call_pattern % arg_types(i) == tp_DERIVED) then
             call print_derived_text(call_pattern%argnames(i), call_pattern%strings(i), call_pattern%assert(i))
          else if (call_pattern % arg_types(i) == tp_INTEGER    .or. &
              call_pattern % arg_types(i) == tp_INTEGER_1D .or. &
              call_pattern % arg_types(i) == tp_INTEGER_2D .or. &
              call_pattern % arg_types(i) == tp_LOGICAL    .or. &
              call_pattern % arg_types(i) == tp_STRING_1D  .or. &
              call_pattern % arg_types(i) == tp_LOGICAL_1D &
             ) then
             write(print_unit, '(a)') &
             '        call assertEqual( ref_'//trim(call_pattern%argnames(i))//', '// &
                                               trim(call_pattern%argnames(i))//', '// &
                                              '"'//trim(call_pattern%argnames(i))//'", '// &
                                              "__LINE__, __FILE__)"
          else
             write(print_unit, '(a)') &
             '        call assertEqual( ref_'//trim(call_pattern%argnames(i))//', '// &
                                               trim(call_pattern%argnames(i))//', tol, '// &
                                              '"'//trim(call_pattern%argnames(i))//'", '// &
                                              "__LINE__, __FILE__)"
          end if
       end do
       write(print_unit, '(a)') '   end subroutine test_'//trim(call_pattern%routinenaam)
       write(print_unit, '(a)') 'end module m_test_'//trim(call_pattern%routinenaam)
       write(print_unit, *)
       write(print_unit, '(a)') 'program p_test_'//trim(call_pattern%routinenaam)
       write(print_unit, '(a)') 'use m_test_'//trim(call_pattern%routinenaam)
       write(print_unit, '(a)') 'use no_pfunit'
       write(print_unit, '(a)') 'implicit none'
       write(print_unit, '(a)') '   call test_'//trim(call_pattern%routinenaam)//'()'
       write(print_unit, '(a)') '   call conclusion()'
       write(print_unit, '(a)') 'end program p_test_'//trim(call_pattern%routinenaam)
       if (present(fname)) call close_generated_code_file(fname)
    end subroutine print_test

    function logical_to_string(value)
      logical, intent(in) :: value
      character(:), allocatable :: logical_to_string
      if (value) then
         logical_to_string = '.true.'
      else
         logical_to_string = '.false.'
      end if
    end function
     
end module m_log_call
