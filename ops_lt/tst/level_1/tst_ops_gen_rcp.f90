module m_tst_ops_gen_rcp
implicit none
contains

   !-------------------------------------------------------------------
   subroutine tst_ops_gen_rcp012
   
   ! Test ops_gen_rcp for spgrid = 0,1,2
   ! See also tst_ops_gen_rcp2 and tst_ops_gen_rcp3
   
   use no_pfunit_ops_lt
   use m_fileutils, only: sysclose
   use m_aps, only: TApsGridReal
   use m_error, only: TError
   use m_ops_gen_rcp
   USE m_commonfile, only: fu_recep, namrecept
        
        
        integer, parameter :: ntest = 4     ! number of tests
        integer            :: itest         ! index of test
        
        ! Input ops_gen_rcp
        integer, parameter :: spgrid_(ntest) = (/ 0, 0, 1, 2 /)
        logical, parameter :: igrens_(ntest) = (/ .true., .false., .true., .true. /)
        integer            :: spgrid
        logical            :: igrens
        type(TApsGridReal) :: masker                  ! fraction of cell area inside NL
        real, parameter    :: grid =  1000.0          ! grid resolution [m]
        integer, parameter :: nrcol = 3
        integer, parameter :: nrrow = 4
        integer, parameter :: nrrcp = 12
        real, parameter    :: xul_cell_centre =    0.0 ! [m]
        real, parameter    :: yul_cell_centre = 3000.0 ! [m]
        logical, parameter :: varz = .true.
        logical, parameter :: perc = .true.

        ! Output ops_gen_rcp
        integer :: jump(nrrcp+1)
        integer :: ref_jump(nrrcp+1) 
        integer :: lu_rcp_dom_all(nrrcp)
        integer :: ref_lu_rcp_dom_all(nrrcp,ntest) 
        real :: xm(nrrcp)
        real :: ym(nrrcp)
        real :: zm(nrrcp)
        
        ! reference data for test 1,ntest:
        real :: ref_xm(nrrcp,ntest) = reshape( &
           (/      0.0,   1000.0,   2000.0,      0.0,   1000.0,   2000.0,      0.0,   1000.0,   2000.0,     0.0,    1000.0,   2000.0, &
                   0.0,   1000.0,   2000.0,      0.0,   1000.0,   2000.0,      0.0,   1000.0,   2000.0,     0.0,    1000.0,   2000.0, &
                   0.0,   1000.0,   2000.0,      0.0,   1000.0,   2000.0,      0.0,   1000.0,   2000.0,     0.0,    1000.0,   2000.0, &
              155050.0, 155060.0, 155070.0, 155080.0, 155090.0, 155100.0, 155150.0, 155200.0, 155300.0, 155400.0, 155500.0, 155600.0 /), &
           (/ nrrcp, ntest /))
                                                            
        real :: ref_ym(nrrcp,ntest) = reshape( &
           (/   3000.0,   3000.0,   3000.0,   2000.0,   2000.0,   2000.0,   1000.0,   1000.0,   1000.0,      0.0,      0.0,      0.0, &
                3000.0,   3000.0,   3000.0,   2000.0,   2000.0,   2000.0,   1000.0,   1000.0,   1000.0,      0.0,      0.0,      0.0, &
                3000.0,   3000.0,   3000.0,   2000.0,   2000.0,   2000.0,   1000.0,   1000.0,   1000.0,      0.0,      0.0,      0.0, &
              385000.0, 385000.0, 385000.0, 385000.0, 385000.0, 385000.0, 385000.0, 385000.0, 385000.0, 385000.0, 385000.0, 385000.0 /), &
           (/ nrrcp, ntest /))
        real :: ref_zm(nrrcp,ntest) 
        real :: frac(nrrcp)
        real :: ref_frac(nrrcp)
        real :: z0_rcp_all(nrrcp)
        real :: ref_z0_rcp_all(nrrcp)
        real, allocatable :: lu_rcp_per_user_all(:,:)
        real, allocatable :: ref_lu_rcp_per_user_all(:,:,:)
        character(len=8), allocatable :: namrcp(:)
        character(len=8), allocatable :: ref_namrcp(:)
        type(TError) :: error
        type(TError) :: ref_error

        ! Local:
        real, parameter    :: tol = 1e-5
        integer, parameter :: NLU = 9 ! number of land use classes
        integer            :: ircp    ! index receptor
        real               :: mask_val(nrrcp)   ! mask values

        ! Receptor file:
        namrecept = './level_1/resources/B16_spokes_Eindhoven_12.rcp'

        ! Reference values:
        ref_z0_rcp_all = 0.03
        ref_jump = 1
        ref_lu_rcp_dom_all(:,1) = -999; ref_lu_rcp_dom_all(:,2) = -999; ref_lu_rcp_dom_all(:,3) = -999; ref_lu_rcp_dom_all(:,4) = 0; 
        ref_zm(:,1) = 4.0; ref_zm(:,2) = 4.0; ref_zm(:,3) = 4.0; ref_zm(:,4) = 3.0; 

        ! Fill mask grid (in km), receptor oordinates are in m:
        masker%gridheader%xul_corner = 0.0
        masker%gridheader%yul_corner = 3.0 
        masker%gridheader%grixl = 1.0
        masker%gridheader%griyl = 1.0
        masker%gridheader%nrcol = nrcol
        masker%gridheader%nrrow = nrrow
        ALLOCATE(masker%value(nrcol,nrrow,1))
        ALLOCATE(masker%average(1))
        mask_val = (/ 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0, 1.1, 1.2 /)
        masker%value = reshape(mask_val,(/nrcol,nrrow,1/))
        masker%average(1) = 0.99 ! not used here

        ! Allocate and fill land use grid:
        ! Note; lu_rcp_per_user_all only defined for spgrid = 2, 3
        allocate(ref_lu_rcp_per_user_all(NLU,nrrcp,ntest))
        allocate(lu_rcp_per_user_all(NLU,nrrcp))
        do itest = 1,ntest 
           do ircp = 1,nrrcp
              if (spgrid_(itest) .eq. 2 .or. spgrid_(itest) .eq. 3) then
                 ref_lu_rcp_per_user_all(:, ircp, itest) = (/ 0.0, 10.0, 10.0, 0.0, 0.0, 10.0, 30.0, 20.0, 20.0 /) 
              else
                 ref_lu_rcp_per_user_all(:, ircp, itest) = (/ 0.0,  0.0,  0.0, 0.0, 0.0,  0.0,  0.0,  0.0,  0.0 /)
              endif
           enddo
        enddo

        ! Allocate receptor names and fill reference receptor names:
        allocate(ref_namrcp(nrrcp))
        allocate(namrcp(nrrcp))
        ref_error%haserror = .false.
        ref_error%message = ""
        do ircp = 1,nrrcp
           write(ref_namrcp(ircp),'(a,i0)') 'recep',ircp
        end do
 
        ! Loop over tests:
        do itest = 1,ntest
           spgrid = spgrid_(itest)
           igrens = igrens_(itest)
           write(*,'(a)') '---------------------------------------------------------------------------------------'
           write(*,*) 'Receptor grid type spgrid = ',spgrid, ', igrens = ',igrens

           ! Fill data with reference data
           lu_rcp_per_user_all = -999.0
           namrcp = ref_namrcp
           jump = ref_jump
           frac = -999.0
           z0_rcp_all = ref_z0_rcp_all
           lu_rcp_dom_all = -999
           zm = -999.0
           ref_error%haserror = .false.
           ref_error%message = ""
           if (igrens) then
              ref_frac = 1
           else
              ref_frac = mask_val
           endif

           ! Generate receptors:
           call ops_gen_rcp( spgrid, igrens, masker, grid, nrcol, nrrow, nrrcp, xul_cell_centre, yul_cell_centre, &
               jump, xm, ym, zm, frac, namrcp, lu_rcp_dom_all, z0_rcp_all, lu_rcp_per_user_all, varz, perc, error)
               write(*,'(a,12(1x,e12.5))') 'xm = ',xm
               write(*,'(a,12(1x,e12.5))') 'ym = ',ym
               write(*,'(a,12(1x,e12.5))') 'frac = ',frac
               !do ilu = 1, NLU
               !   write(*,'(a,12(1x,e12.5))') 'lu_rcp_per_user_all = ',lu_rcp_per_user_all(ilu,:)
               !enddo
           
           ! Assert outputs:
           call assertEqual( error, ref_error, "ops_gen_rcp", __LINE__, __FILE__)
           call assertEqual( ref_jump, jump, "jump", __LINE__, __FILE__)
           call assertEqual( ref_lu_rcp_dom_all(:,itest), lu_rcp_dom_all, "lu_rcp_dom_all", __LINE__, __FILE__)
           call assertEqual( ref_xm(:,itest), xm, tol, "xm", __LINE__, __FILE__)
           call assertEqual( ref_ym(:,itest), ym, tol, "ym", __LINE__, __FILE__)
           call assertEqual( ref_zm(:,itest), zm, tol, "zm", __LINE__, __FILE__)
           call assertEqual( ref_frac, frac, tol, "frac", __LINE__, __FILE__)
           call assertEqual( ref_z0_rcp_all, z0_rcp_all, tol, "z0_rcp_all", __LINE__, __FILE__)
           call assertEqual( ref_lu_rcp_per_user_all(:,:,itest), lu_rcp_per_user_all, tol, "lu_rcp_per_user_all", __LINE__, __FILE__)
           call assertEqual( ref_namrcp, namrcp, "namrcp", __LINE__, __FILE__)
           
           ! Close receptor file:
           CALL sysclose(fu_recep, namrecept, error)
       enddo
   end subroutine tst_ops_gen_rcp012

   !-------------------------------------------------------------------
   subroutine tst_ops_gen_rcp2
   
   ! tst ops_gen_rcp2 tests ops_gen_rcp for spgrid = 2 (receptors in user defined file).
   
   USE m_aps
   USE m_error
   USE m_fileutils
   USE m_commonfile, only: fu_err, fu_recep, namrecept, errnam
   Use iso_fortran_env, only: output_unit  ! standard output; input_unit and error_unit also available
   USE m_ops_get_dim
   USE m_ops_gen_rcp
   USE m_commonconst_lt, only: NLU
   use no_pfunit
   
   IMPLICIT NONE 
   
   LOGICAL, PARAMETER                               :: gen_ref_data = .false.  ! generate output file with reference data
                                                                               ! in test modus, this has to be false and data is tested against the reference data
   
   ! SUBROUTINE ARGUMENTS - INPUT
   INTEGER                                          :: spgrid                     ! type of receptor grid
   
   ! Following arguments are dummy in case spgrid = 2
   REAL                                             :: xc                          
   REAL                                             :: yc                          
   LOGICAL                                          :: igrens                      
   TYPE (TApsGridReal)                              :: masker
   REAL                                             :: grid                        
   INTEGER                                          :: nrcol                       
   INTEGER                                          :: nrrow                       
   REAL                                             :: xul_cell_centre            ! x-coordinate of centre of upper-left grid cell [m] 
   REAL                                             :: yul_cell_centre            ! y-coordinate of centre of upper-left grid cell [m]  
   
   ! These are used for spgrid = 2:
   INTEGER                                          :: nrrcp                      ! number of receptor points
   INTEGER                                          :: ref_nrrcp                  ! number of receptor points reference data
   LOGICAL                                          :: varz                       ! option for variable receptor height                  
   LOGICAL                                          :: perc                       ! option for land use percentages from receptor file
   
   ! SUBROUTINE ARGUMENTS - OUTPUT
   INTEGER  , ALLOCATABLE                           :: jump(:)                  ! number of grid points to jump to for next point in output            
   INTEGER  , ALLOCATABLE                           :: lu_rcp_dom_all(:)        ! dominant land use class for each receptor point
   REAL     , ALLOCATABLE                           :: xm(:)                    ! x-coordinates of receptors (m RDM)
   REAL     , ALLOCATABLE                           :: ym(:)                    ! y-coordinates of receptors (m RDM)
   REAL     , ALLOCATABLE                           :: zm(:)                    ! z-coordinates of receptor points (m)
   REAL     , ALLOCATABLE                           :: frac(:)                  ! fraction of output cell on land surface
   REAL     , ALLOCATABLE                           :: z0_rcp_all(:)            ! roughness lengths for all receptors; from z0-map or receptor file [m]
   REAL     , ALLOCATABLE                           :: lu_rcp_per_user_all(:,:) ! percentage of landuse for all receptors, user defined in receptor file
   CHARACTER*(12), ALLOCATABLE                      :: namrcp(:)                ! receptor names
   
   ! Reference data:
   INTEGER  , ALLOCATABLE                           :: ref_jump(:)                  ! number of grid points to jump to for next point in output            
   INTEGER  , ALLOCATABLE                           :: ref_lu_rcp_dom_all(:)        ! dominant land use class for each receptor point
   REAL     , ALLOCATABLE                           :: ref_xm(:)                    ! x-coordinates of receptors (m RDM)
   REAL     , ALLOCATABLE                           :: ref_ym(:)                    ! y-coordinates of receptors (m RDM)
   REAL     , ALLOCATABLE                           :: ref_zm(:)                    ! z-coordinates of receptor points (m)
   REAL     , ALLOCATABLE                           :: ref_frac(:)                  ! fraction of output cell on land surface
   REAL     , ALLOCATABLE                           :: ref_z0_rcp_all(:)            ! roughness lengths for all receptors; from z0-map or receptor file [m]
   REAL     , ALLOCATABLE                           :: ref_lu_rcp_per_user_all(:,:) ! percentage of landuse for all receptors, user defined in receptor file
   CHARACTER*(12), ALLOCATABLE                      :: ref_namrcp(:)                ! receptor names
   CHARACTER*512                                    :: ref_message                  ! string with error message
   
   TYPE (TError)                                    :: error                    ! error handling record
   INTEGER                                          :: ircp                     ! index of receptor
   INTEGER                                          :: ircp_dum                 ! index of receptor, dummy read from reference file
   CHARACTER*(512)                                  :: line                     ! line read from file
   INTEGER                                          :: nwords                   ! number of words in line
   LOGICAL                                          :: file_is_present          ! receptor file is present
   INTEGER                                          :: ifile                    ! file index
   INTEGER, PARAMETER                               :: fu_ref = 11              ! unit number output file
   INTEGER                                          :: ipos                     ! position of first digit in file number
   real, parameter                                  :: tol = 1e-5               ! tolerance for testing
   character(len=200), parameter                    :: fnam_ref = './level_1/resources/ref_ops_gen_rcp.out' ! name file with reference data
   
   write(*,'(/,72(''-''),/,a)')  ' tst_ops_gen_rcp2 performs multiple tests reading test files tst01.rcp, tst02.rcp, ... (spgid = 2)'
   write(*,'(3a)')    ' The file with reference data (',trim(fnam_ref),') contains output for each test:'
   write(*,'(a)')     ' first line: itest nrrcp'
   write(*,'(a)')     '   nrrcp > 0 -> no error'
   write(*,'(a)')     '   nrrcp = 0 -> error in ops_get_dim'
   write(*,'(a)')     '   nrrcp < 0 -> error in ops_gen_rcp'
   write(*,'(a,/,/)') ' next lines: receptor data for current test or error message'
   if (gen_ref_data) write(*,'(/,/,a,/,/)') ' !!!!!!!! NO TEST, GENERATE FILE WITH REFERENCE DATA !!!!!!!!!!!'

   ! Open output file with reference data:
   if (gen_ref_data) then
      open(fu_ref,FILE=fnam_ref, status = 'NEW')  ! you have to delete an old reference file first!
   else
      open(fu_ref,FILE=fnam_ref, status = 'OLD')
   endif
   
   ! File names (tst01.rcp. tst02.rcp, ...):
   errnam    = 'tst_ops_gen_rcp.err'
   namrecept = './level_1/resources/tst01.rcp'; ifile = 1; ipos = 24 ! position of first digit of file number
   inquire(FILE = namrecept, exist = file_is_present)
   ! write(*,*) file_is_present
   
   ! Test only spgrid = 2: 
   spgrid = 2
   
   ! Loop over receptor files:
   DO WHILE (file_is_present)
   
      !write(*,*) '----------------------------------------------------------------'
      !write(*,*) 'receptor file       = ',trim(namrecept)
      CALL sysclose(fu_recep, 'receptor file', error)
      call assertEqual( .false., error%haserror, "unexpected error closing receptor file", __LINE__, __FILE__)
   
      ! Determine number of receptorpoints:
      call ops_get_dim(spgrid, igrens, xc, yc, grid, nrcol, nrrow, nrrcp, xul_cell_centre, yul_cell_centre, masker, error)
      
      IF (error%haserror) THEN
          ! Errotr in ops_get_dim:
          !! Write error and continue with next file:
          ! CALL WriteError(output_unit, error)
          error%haserror = .false.
          if (gen_ref_data) then
             write(fu_ref,*) ifile,0
             write(fu_ref,'(a)') trim(error%message)
          else
              ! Read error message and check:
              read(fu_ref,*) ifile,ref_nrrcp
              read(fu_ref,'(a)') ref_message
              call assertequal(ref_nrrcp,0,"nrrcp in case of error in ops_get_dim",__LINE__,__FILE__)
              call assertequal(ref_message,error%message,"nrrcp",__LINE__,__FILE__)
          endif
      ELSE
   
         ! write(*,*) 'number of receptors = ',nrrcp
         
         ! Allocate arrays:
         allocate(jump(nrrcp+1))
         allocate(lu_rcp_dom_all(nrrcp))
         allocate(xm(nrrcp))            
         allocate(ym(nrrcp))            
         allocate(zm(nrrcp))            
         allocate(frac(nrrcp))          
         allocate(z0_rcp_all(nrrcp))    
         allocate(lu_rcp_per_user_all(NLU,nrrcp))
         allocate(namrcp(nrrcp))
         allocate(ref_jump(nrrcp+1))
         allocate(ref_lu_rcp_dom_all(nrrcp))
         allocate(ref_xm(nrrcp))            
         allocate(ref_ym(nrrcp))            
         allocate(ref_zm(nrrcp))            
         allocate(ref_frac(nrrcp))          
         allocate(ref_z0_rcp_all(nrrcp))    
         allocate(ref_lu_rcp_per_user_all(NLU,nrrcp))
         allocate(ref_namrcp(nrrcp))
         
         ! Determine values of varz and perc depending on number of columns in input file:
         open(fu_recep, file = namrecept)
         rewind(fu_recep)
         read(fu_recep,'(a)') line
         !write(*,'(a)') '==',trim(line)
         nwords = string_count_words(line)
         !write(*,*) 'number of words = ',nwords 
         CALL sysclose(fu_recep, 'receptor file', error)
         call assertEqual( .false., error%haserror, "unexpected error closing receptor file 2", __LINE__, __FILE__)
  
         ! determine varz and perc depending on number of words:
         if (nwords .eq. 4) then
            varz = .false.
            perc = .false.
         elseif (nwords .eq. 5) then
            varz = .true.
            perc = .false.
         elseif (nwords .eq. 15) then
            varz = .false.
            perc = .true.
         elseif (nwords .eq. 16) then
            varz = .true.
            perc = .true.
         else
            ! write(*,*) 'unknown number of words in header line = ',nwords
            ! write(*,*) 'continue for error message'
            varz = .false.
            perc = .false.
         endif
         
         ! Read receptor file:
         call ops_gen_rcp(spgrid, igrens, masker, grid, nrcol, nrrow, nrrcp, xul_cell_centre, yul_cell_centre, jump, xm, ym, zm, frac, namrcp, &
                          lu_rcp_dom_all, z0_rcp_all, lu_rcp_per_user_all, varz, perc, error)
         
         IF (error%haserror) THEN
            ! Write error and continue with next file:
            ! CALL WriteError(output_unit, error)
            if (gen_ref_data) then
               write(fu_ref,*) ifile,-nrrcp  ! minus sign indicates error
               write(fu_ref,'(a)') trim(error%message)
            else
               ! Read error message and check:
               read(fu_ref,*) ifile,ref_nrrcp
               read(fu_ref,'(a)') ref_message
               call assertequal(ref_nrrcp,-nrrcp,"nrrcp in case of error in ops_gen_rcp",__LINE__,__FILE__)
               call assertequal(ref_message,error%message,"nrrcp",__LINE__,__FILE__)
            endif
            error%haserror = .false.
         ELSE
            ! No error, write or read/test data:
            if (gen_ref_data) then
               write(fu_ref,*) ifile,nrrcp
               DO ircp = 1, nrrcp
                  write(fu_ref,'(i4,1x,a12,3(1x,f9.1),1x,f10.4,1x,i4,99(1x,f7.1))') ircp,namrcp(ircp),xm(ircp),ym(ircp),zm(ircp),z0_rcp_all(ircp),lu_rcp_dom_all(ircp),lu_rcp_per_user_all(:,ircp)
               ENDDO
            else
               read(fu_ref,*) ifile,ref_nrrcp
               call assertequal(ref_nrrcp,nrrcp,"nrrcp",__LINE__,__FILE__)
               DO ircp = 1, ref_nrrcp
                  read(fu_ref,'(i4,1x,a12,3(1x,f9.1),1x,f10.4,1x,i4,99(1x,f7.1))') ircp_dum,ref_namrcp(ircp),ref_xm(ircp),ref_ym(ircp),ref_zm(ircp),ref_z0_rcp_all(ircp),ref_lu_rcp_dom_all(ircp),ref_lu_rcp_per_user_all(:,ircp)
               ENDDO
               
               ! Assert outputs:
               call assertEqual( .false., error%haserror, "ops_gen_rcp", __LINE__, __FILE__)
               ! call assertEqual( ref_jump, jump, "jump", __LINE__, __FILE__) ! not for spgrid = 2
               call assertEqual( ref_lu_rcp_dom_all, lu_rcp_dom_all, "lu_rcp_dom_all", __LINE__, __FILE__)
               call assertEqual( ref_xm, xm, tol, "xm", __LINE__, __FILE__)
               call assertEqual( ref_ym, ym, tol, "ym", __LINE__, __FILE__)
               call assertEqual( ref_zm, zm, tol, "zm", __LINE__, __FILE__)
               ! call assertEqual( ref_frac, frac, tol, "frac", __LINE__, __FILE__) ! not tested for spgrid = 2
               call assertEqual( ref_z0_rcp_all, z0_rcp_all, tol, "z0_rcp_all", __LINE__, __FILE__)
               call assertEqual( ref_lu_rcp_per_user_all, lu_rcp_per_user_all, tol, "lu_rcp_per_user_all", __LINE__, __FILE__)
               call assertEqual( ref_namrcp, namrcp, "namrcp", __LINE__, __FILE__)

            endif
         ENDIF  
         
         ! Deallocate arrays:
         deallocate(jump)
         deallocate(lu_rcp_dom_all)
         deallocate(xm)            
         deallocate(ym)            
         deallocate(zm)            
         deallocate(frac)          
         deallocate(z0_rcp_all)    
         deallocate(lu_rcp_per_user_all)
         deallocate(namrcp)
         deallocate(ref_jump)
         deallocate(ref_lu_rcp_dom_all)
         deallocate(ref_xm)            
         deallocate(ref_ym)            
         deallocate(ref_zm)            
         deallocate(ref_frac)          
         deallocate(ref_z0_rcp_all)    
         deallocate(ref_lu_rcp_per_user_all)
         deallocate(ref_namrcp)
      ENDIF  ! Error in ops_get_dim
      
      ! Next file:
      ifile = ifile + 1
      write(namrecept(ipos:ipos+1),'(i2.2)') ifile  
      inquire(FILE = namrecept, exist = file_is_present)
      
   ENDDO ! end loop over receptor files                 


   ! Close files:
   if (gen_ref_data) close(fu_ref)
   close(fu_recep)

   return
  
   end subroutine tst_ops_gen_rcp2
   
   !-------------------------------------------------------------------
   subroutine tst_ops_gen_rcp3

   use no_pfunit
   use m_aps
   use m_ops_gen_rcp
   USE m_commonfile, only: fu_recep, namrecept
   Use iso_fortran_env, only: output_unit  ! standard output; input_unit and error_unit also available
   
   implicit none
   
   ! Test procedure for spgrid 3 (regular grid, not necessarily rectangular)
   
   integer, parameter :: NLU = 9 ! number of land use classes
   integer, parameter :: spgrid = 3
   logical, parameter :: igrens = .true.
   type(TApsGridReal) :: masker                  ! fraction of cell area inside NL
   integer, parameter :: nrcol = 12              ! number of columns in grid
   integer, parameter :: nrrow = 4               ! number of rows in grid
   integer, parameter :: nrrcp = 27              ! number of receptors in receptor file
   real               :: xul_cell_centre
   real               :: yul_cell_centre
   real               :: grid
   logical, parameter :: varz = .false.
   logical, parameter :: perc = .false.

   ! Output ops_gen_rcp
   integer :: jump(nrrcp+1) = 1  ! number of grid points to jump to for next point in output (default 1) 
   integer :: lu_rcp_dom_all(nrrcp)
   ! integer :: ref_lu_rcp_dom_all(nrrcp) 
   real :: xm(nrrcp)          ! x coordinates of receptors from receptor file
   real :: ym(nrrcp)          ! y coordinates of receptors from receptor file
   real :: zm(nrrcp)          ! y coordinates of receptors
   real :: frac(nrrcp)
   !real :: ref_frac(nrrcp)
   real :: z0_rcp_all(nrrcp)
   !real :: ref_z0_rcp_all(nrrcp)
   real :: lu_rcp_per_user_all(NLU,nrrcp)
   !real :: ref_lu_rcp_per_user_all(NLU,nrrcp)
   character(len=8) :: namrcp(nrrcp)
   !character(len=8) :: ref_namrcp(nrrcp)
   type(TError) :: error
   
   real, parameter                 :: tol = 1e-5               ! tolerance for testing

   ! Example for nrcol = 12, nrrow = 4
   ! x = receptor (must be present in receptor file - filled into (xm,ym))
   ! o = point to be skipped
   !
   !             m =     123456789012  
   ! --------------------------------
   ! y = 104 m | n = 1 | oooxxxxxxxxx  jump = 4111111111..
   ! y = 103 m | n = 2 | ooooxxxxxxoo         5111111..
   ! y = 102 m | n = 3 | ooxxxxxxoooo         5111111..
   ! y = 101 m | n = 4 | xxxxxxoooooo         51111117
   !                             
   ! x = m =             123456789012
   
   ! Reference data:
   real    :: ref_xm(nrrcp)
   real    :: ref_ym(nrrcp) 
   integer :: ref_jump(nrrcp+1) = (/ &
                   4, 1, 1, 1, 1, 1, 1, 1, 1, &
                   5, 1, 1, 1, 1, 1, & 
                   5, 1, 1, 1, 1, 1, & 
                   5, 1, 1, 1, 1, 1, 7 /)
   
   ! Receptor file:
   namrecept = './level_1/resources/tst_spgrid3.rcp'
   
   ! Define grid:
   xul_cell_centre = 1
   yul_cell_centre = 104
   grid = 1

   ! Fill reference data for (xm,ym); see example above:
   ref_ym( 1: 9) = 104; ref_xm( 1: 9) = (/ 4, 5, 6,  7,  8,  9, 10, 11, 12 /)
   ref_ym(10:15) = 103; ref_xm(10:15) = (/ 5, 6, 7,  8,  9, 10 /)
   ref_ym(16:21) = 102; ref_xm(16:21) = (/ 3, 4, 5,  6,  7,  8 /)
   ref_ym(22:27) = 101; ref_xm(22:27) = (/ 1, 2, 3,  4,  5,  6 /)

   ! Generate receptor grid for spgrid = 3:
   call ops_gen_rcp( spgrid, igrens, masker, grid, nrcol, nrrow, nrrcp, xul_cell_centre, yul_cell_centre, &
                     jump, xm, ym, zm, frac, namrcp, lu_rcp_dom_all, z0_rcp_all, lu_rcp_per_user_all, varz, perc, error)
   call assertEqual( .false., error%haserror, "unexpected error call ops_gen_rcp", __LINE__, __FILE__)
   if (error%haserror) CALL WriteError(output_unit, error)
   !write(*,'(a,27(1x,e12.5))') 'xm = ',xm
   !write(*,'(a,27(1x,e12.5))') 'ym = ',ym
   !write(*,'(a,28(1x,i6))') 'jump = ',jump
   
   ! Assert outputs:
   !call assertEqual( error, ref_error, "ops_gen_rcp", __LINE__, __FILE__)
   call assertEqual( ref_jump, jump, "jump", __LINE__, __FILE__)
   !call assertEqual( ref_lu_rcp_dom_all, lu_rcp_dom_all, "lu_rcp_dom_all", __LINE__, __FILE__)
   call assertEqual( ref_xm, xm, tol, "xm", __LINE__, __FILE__)
   call assertEqual( ref_ym, ym, tol, "ym", __LINE__, __FILE__)
   ! call assertEqual( ref_zm, zm, tol, "zm", __LINE__, __FILE__)
   !call assertEqual( ref_frac, frac, tol, "frac", __LINE__, __FILE__)
   !call assertEqual( ref_z0_rcp_all, z0_rcp_all, tol, "z0_rcp_all", __LINE__, __FILE__)
   !call assertEqual( ref_lu_rcp_per_user_all, lu_rcp_per_user_all, tol, "lu_rcp_per_user_all", __LINE__, __FILE__)
   !call assertEqual( ref_namrcp, namrcp, "namrcp", __LINE__, __FILE__)
   
   end subroutine tst_ops_gen_rcp3

end module m_tst_ops_gen_rcp

!--------------------------------------------------------------------
program p_tst_ops_gen_rcp
use m_tst_ops_gen_rcp
use no_pfunit
implicit none
   call tst_ops_gen_rcp012
   call tst_ops_gen_rcp2
   call tst_ops_gen_rcp3
   call conclusion()
end program p_tst_ops_gen_rcp

