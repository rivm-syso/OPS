module m_tst_ctrl
implicit none
contains

    subroutine test_ops_read_ctr(nlines, errmsg)
    use iso_c_binding, only: eoln => c_new_line
    use m_commonconst_lt,only: MISVALNUM
    use m_commonfile, only: fu_input, IOB_STDOUT, ctrnam
    use m_error, only: TError, write_error
    use m_fileutils, only: fix_slashes, sysopen
    use m_ops_tdo_proc, only: Tdo_proc
    use m_ops_read_ctr, only:  TCtrLayers, ops_read_ctr
    use no_pfunit
    implicit none
       integer,          intent(in) :: nlines
       character(len=*), intent(in) :: errmsg

       type(TCtrLayers) :: ctr
       type(TError) :: error
       integer :: iostat
       integer :: iline
       character(len=200) :: line
       logical :: open_ok

       integer,        parameter :: iarr(3) = (/ 0, 0, 0 /)
       character*(*),  parameter :: ref_project = 'A_test_src1_rcp1_year1\A1_SO2_point_hlow'
       character*(*),  parameter :: ref_runid   = ref_project
       integer,        parameter :: ref_year = 2005
       integer,        parameter :: ref_icm  =1
       character*(*),  parameter :: ref_namco = 'SO2 - gas.'
       real,           parameter :: ref_amol1 = 64.1
       logical,        parameter :: ref_gasv  = .true.            ! type of component (0: particle; 1: gas)
       logical,        parameter :: ref_do_proc = .true.          ! options to switch on/off specific processes
       logical,        parameter :: ref_idep = .true.
       integer,        parameter :: ref_kdeppar = MISVALNUM
       real,           parameter :: ref_ddeppar = real(MISVALNUM)
       integer,        parameter :: ref_knatdeppar = MISVALNUM
       real,           parameter :: ref_wdeppar  = real(MISVALNUM)
       real,           parameter :: ref_dg  = .136
       logical,        parameter :: ref_irrev  = 1
       real,           parameter :: ref_vchemc  = real(MISVALNUM)
       integer,        parameter :: ref_iopt_vchem = 0
       real,           parameter :: ref_vchemv = real(MISVALNUM)
       real,           parameter :: ref_emtrend = 1.0
       integer,        parameter :: ref_ncatsel = 1
       integer,        parameter :: ref_catsel(ref_ncatsel) = (/ 0 /)
       integer,        parameter :: ref_nemcat_road  = 0
       integer,        parameter :: ref_emcat_road(0) = iarr(1:0)
       integer,        parameter :: ref_nlandsel = 1
       integer,        parameter :: ref_landsel(ref_nlandsel ) = (/ 0 /)
       integer,        parameter :: ref_spgrid  = 2
       real,           parameter :: ref_xc = real(MISVALNUM)
       real,           parameter :: ref_yc = real(MISVALNUM)
       integer,        parameter :: ref_nrcol = MISVALNUM
       integer,        parameter :: ref_nrrow = MISVALNUM
       real,           parameter :: ref_grid = real(MISVALNUM)
       logical,        parameter :: ref_igrens = .false.
       real,           parameter :: ref_z0_user = 0.2
       integer,        parameter :: ref_intpol = 1
       integer,        parameter :: ref_ideh = 2
       logical,        parameter :: ref_igrid = 0
       logical,        parameter :: ref_checked = .true.
       logical,        parameter :: ref_f_z0user = .true.
       logical,        parameter :: ref_isec = .true.
       integer,        parameter :: ref_nsubsec = 0
       logical,        parameter :: ref_chem_meteo_prognosis = .false.

       integer         ::  unit

       ctrnam='./level_1/resources\tst_ctrl.in'
       call fix_slashes(ctrnam)
       open(fu_input, file=ctrnam, iostat=iostat)
       call assertEqual(iostat,0, 'opening file "'//trim(ctrnam)//'" for writing')
       write(fu_input,'(a,i0,a)') &
          '*-----------------------directory layer---------------------------------*' // eoln // &
          'DATADIR        ../data/GCN2022/Data/' // eoln // &
          '*-----------------------identification layer----------------------------*' // eoln // &
          'PROJECT        A_test_src1_rcp1_year1\A1_SO2_point_hlow' // eoln // &
          'RUNID          A_test_src1_rcp1_year1\A1_SO2_point_hlow' // eoln // &
          'YEAR           2005' // eoln // &
          '*-----------------------substance layer---------------------------------*' // eoln // &
          'COMPCODE       1' // eoln // &
          'COMPNAME       SO2 - gas.' // eoln // &
          'MOLWEIGHT      64.1' // eoln // &
          'PHASE          1' // eoln // &
          'LOSS           1' // eoln // &
          'DDSPECTYPE' // eoln // &
          'DDPARVALUE' // eoln // &
          'WDSPECTYPE' // eoln // &
          'WDPARVALUE' // eoln // &
          'DIFFCOEFF      .136' // eoln // &
          'WASHOUT        1' // eoln // &
          'CONVRATE' // eoln // &
          'LDCONVRATE' // eoln // &
          '*-----------------------emission layer----------------------------------*' // eoln // &
          'EMFILE         ./level_1/resources/Eindhoven_h5m.brn' // eoln // &
          'USDVEFILE' // eoln // &
          'USPSDFILE' // eoln // &
          'EMCORFAC       1.0' // eoln // &
          'TARGETGROUP    0' // eoln // &
          'COUNTRY        0' // eoln // &
          '*-----------------------receptor layer----------------------------------*' // eoln // &
          'RECEPTYPE      2' // eoln // &
          'XCENTER' // eoln // &
          'YCENTER' // eoln // &
          'NCOLS' // eoln // &
          'NROWS' // eoln // &
          'RESO' // eoln // &
          'OUTER' // eoln // &
          'RCPFILE        ./level_1/resources/rcp_Eindhoven.rcp' // eoln // &
          '*-----------------------meteo & surface char layer----------------------*' // eoln // &
          'ROUGHNESS      0.2' // eoln // &
          'Z0FILE' // eoln // &
          'LUFILE' // eoln // &
          'METEOTYPE      1' // eoln // &
          'MTFILE         ../data/GCN2022/Meteo/a005105c.005' // eoln // &
          '*-----------------------output layer------------------------------------*' // eoln // &
          'DEPUNIT        2' // eoln // &
          'PLTFILE        ./level_1/run001.plt' // eoln // &
          'PRNFILE        ./level_1/run001.lpt' // eoln // &
          'INCLUDE        0' // eoln // &
          'GUIMADE        1' // eoln
       close(fu_input)

       if (nlines < 1000) then

          open_ok = sysopen(fu_input, ctrnam, 'r', 'control file', error)
          if (error%haserror) call write_error(IOB_STDOUT, error)

          ctrnam='./level_1/resources\tst_ctrl2.in'
          call fix_slashes(ctrnam)
          open(newunit = unit, file=ctrnam, iostat=iostat)
          do iline = 1,nlines
             read(fu_input, '(a)') line
             write(unit, '(a)') trim(line)
          end do
          close(fu_input)
          close(unit)
       end if

       call assertEqual(iostat,0, 'opening file "'//trim(ctrnam)//'" for writing')
       call ops_read_ctr(ctr, error)
       close(fu_input)

       if (errmsg /= ' ') then
          call assertTrue(error%haserror, 'expected error from ops_read_ctr')
          !
          ! The error message cannot really be checked, because in the case where the file was already at an end,
          ! and you read some more, 
          ! ifort sets IOSTAT = IOSTAT_END, whereas gfortran sets IOSTAT=5001, with 
          ! IOMSG='Sequential READ or WRITE not allowed after EOF marker, possibly use REWIND or BACKSPACE'
          !
          if (.false.) call assertEqual(error%message, errmsg, 'expected error message from ops_read_ctr')
          return
       end if

       if (error%haserror) then
          call write_error(IOB_STDOUT, error)
          call assertFalse(error%haserror, 'error from ops_read_ctr')
       else
          call assertFalse(error%haserror, 'from ops_read_ctr')

          call assertEqual( ctr%identification%project, ref_project, "project")
          call assertEqual( ctr%identification%runid, ref_runid, "runid")
          call assertEqual( ctr%identification%year, ref_year, "year")

          call assertEqual( ctr%substance%icm, ref_icm, "icm")
          call assertEqual( ctr%substance%namco, ref_namco, "namco")
          call assertEqual( ctr%substance%amol1, ref_amol1, "amol1")
          call assertEqual( ctr%substance%gasv, ref_gasv, "gasv")
          call assertEqual( ctr%substance%do_proc%chem,        ref_do_proc, "control - file - loss : chem")
          call assertEqual( ctr%substance%do_proc%depl_drydep, ref_do_proc, "control - file - loss : depl_drydep")
          call assertEqual( ctr%substance%do_proc%depl_wetdep, ref_do_proc, "control - file - loss : depl_wetdep")
          call assertEqual( ctr%substance%do_proc%grad_drydep, ref_do_proc, "control - file - loss : grad_drydep")
          call assertEqual( ctr%substance%idep, ref_idep, "idep")
          call assertEqual( ctr%substance%kdeppar, ref_kdeppar, "kdeppar")
          call assertEqual( ctr%substance%ddeppar, ref_ddeppar, "ddeppar")
          call assertEqual( ctr%substance%knatdeppar, ref_knatdeppar, "knatdeppar")
          call assertEqual( ctr%substance%wdeppar, ref_wdeppar, "wdeppar")
          call assertEqual( ctr%substance%dg, ref_dg, "dg")
          call assertEqual( ctr%substance%irrev, ref_irrev, "irrev")
          call assertEqual( ctr%substance%vchemc, ref_vchemc, "vchemc")
          call assertEqual( ctr%substance%iopt_vchem, ref_iopt_vchem, "iopt_vchem")
          call assertEqual( ctr%substance%vchemv, ref_vchemv, "vchemv")

          call assertEqual( ctr%emission%emtrend, ref_emtrend, "emtrend")
          call assertEqual( ctr%emission%ncatsel, ref_ncatsel, "ncatsel")
          call assertEqual( ctr%emission%catsel(1:ctr%emission%ncatsel), ref_catsel, "catsel")
          call assertEqual( ctr%emission%nemcat_road, ref_nemcat_road, "nemcat_road")
          call assertEqual( ctr%emission%emcat_road(1:ctr%emission%nemcat_road) , ref_emcat_road, "emcat_road")
          call assertEqual( ctr%emission%nlandsel, ref_nlandsel, "nlandsel")
          call assertEqual( ctr%emission%landsel(1:ctr%emission%nlandsel), ref_landsel, "landsel")

          call assertEqual( ctr%receptor%spgrid, ref_spgrid, "spgrid")
          call assertEqual( ctr%receptor%xc, ref_xc, "xc")
          call assertEqual( ctr%receptor%yc, ref_yc, "yc")
          call assertEqual( ctr%receptor%nrcol, ref_nrcol, "nrcol")
          call assertEqual( ctr%receptor%nrrow, ref_nrrow, "nrrow")
          call assertEqual( ctr%receptor%grid, ref_grid, "grid")
          call assertEqual( ctr%receptor%igrens, ref_igrens, "igrens")

          call assertEqual( ctr%meteo_surface%z0_user, ref_z0_user, "z0_user")
          call assertEqual( ctr%meteo_surface%intpol, ref_intpol, "intpol")

          call assertEqual( ctr%output%ideh, ref_ideh, "ideh")
          call assertEqual( ctr%output%igrid, ref_igrid, "igrid")
          call assertEqual( ctr%output%checked, ref_checked, "checked")
          call assertEqual( ctr%output%f_z0user, ref_f_z0user, "f_z0user")
          call assertEqual( ctr%output%isec, ref_isec, "isec")
          call assertEqual( ctr%output%nsubsec, ref_nsubsec, "nsubsec")
          call assertEqual( ctr%output%chem_meteo_prognosis, ref_chem_meteo_prognosis, "chem_meteo_prognosis")
       endif
    end subroutine test_ops_read_ctr


    subroutine test_ops_read_ctr_year(ref_year, expect_prognisis, line_rest, errmsg)
    use iso_c_binding, only: eoln => c_new_line
    use m_commonfile, only: fu_input, IOB_STDOUT
    use m_fileutils, only: fix_slashes, sysopen
    use m_error, only: TError, write_error
    use m_ops_read_ctr, only:  ops_read_ctr_year
    use no_pfunit
    implicit none
       integer,          intent(in) :: ref_year
       logical,          intent(in) :: expect_prognisis
       character(len=*), intent(in) :: line_rest
       character(len=*), intent(in) :: errmsg

       integer  :: year        ! year for chemical maps
       logical  :: prognosis   ! use meteo prognosis in chemistry maps
       type(TError) :: error
       character(len=100) :: filename
       logical :: open_ok
       integer :: iostat

       filename='./level_1/resources\tst_ctrl.in'
       call fix_slashes(filename)
       open(fu_input, file=filename, iostat=iostat)
       call assertEqual(iostat,0, 'opening file "'//trim(filename)//'" for writing')
       write(fu_input,'(a,i0,a)') &
          '*-----------------------identification layer----------------------------*' // eoln // &
          'YEAR           ',ref_year, line_rest // eoln
       close(fu_input)

       open_ok = sysopen(fu_input, filename, 'r', 'control file', error)
       if (error%haserror) call write_error(IOB_STDOUT, error)
       call assertFalse(error%haserror, 'opening file "'//trim(filename)//'" for reading')
       call ops_read_ctr_year(year,prognosis,error)
       close(fu_input)

       if (errmsg /= ' ') then
          call assertTrue(error%haserror, 'expected error from  ops_read_ctr_year')
          call assertEqual(error%message, errmsg, 'expected error from  ops_read_ctr_year')
       else
          if (error%haserror) call write_error(IOB_STDOUT, error)
          call assertFalse(error%haserror, 'from  ops_read_ctr_year')
          call assertEqual(prognosis, expect_prognisis, "prognosis in control - file - year")
          call assertEqual(year, ref_year, "control - file - year")
       end if
    end subroutine test_ops_read_ctr_year



    subroutine test_ops_read_ctr_roads(ref_emcat_road, line_rest, errmsg)
    use iso_c_binding, only: eoln => c_new_line
    use m_commonfile, only: fu_input, IOB_STDOUT
    use m_fileutils, only: fix_slashes, sysopen
    use m_error, only: TError, write_error
    use m_ops_read_ctr, only:  ops_read_ctr_roads
    use no_pfunit
    implicit none
       integer,          intent(in) :: ref_emcat_road(:)              ! list of road emission categories (for vdHout NO2/NOx ratio)
       character(len=*), intent(in) :: line_rest
       character(len=*), intent(in) :: errmsg

       integer :: nemcat_road                ! number of road emission categories (for vdHout NO2/NOx ratio)
	   logical :: road_chem
	   logical :: road_disp
       integer :: emcat_road(3)              ! list of road emission categories (for vdHout NO2/NOx ratio)
       type(TError) :: error
       character(len=100) :: filename
       logical :: open_ok
       integer :: iostat, j

       filename='./level_1/resources\tst_ctrl.in'
       call fix_slashes(filename)
       open(fu_input, file=filename, iostat=iostat)
       call assertEqual(iostat,0, 'opening file "'//trim(filename)//'" for writing')
       if (size(ref_emcat_road,1) == 0) then
          write(fu_input,'(a)') &
             '*-----------------------identification layer----------------------------*' // eoln // &
             'YEAR           1956' // eoln
       else
          write(fu_input,'(10(a,i0))') &
             'ROADS           ',ref_emcat_road(1),(' ', ref_emcat_road(j), j=2,size(ref_emcat_road,1)), line_rest//eoln // &
			 'Roadsopt		  0 0' //eoln
       end if
       close(fu_input)

       open_ok = sysopen(fu_input, filename, 'r', 'control file', error)
       if (error%haserror) call write_error(IOB_STDOUT, error)
       call assertFalse(error%haserror, 'opening file "'//trim(filename)//'" for reading')
       call ops_read_ctr_roads(nemcat_road, road_chem, road_disp, emcat_road, error)
       close(fu_input)

       if (errmsg /= ' ') then
          call assertTrue(error%haserror, 'expected error from  ops_read_ctr_roads')
          call assertEqual(error%message, errmsg, 'expected error from  ops_read_ctr_roads')
       else
          if (error%haserror) call write_error(IOB_STDOUT, error)
          call assertFalse(error%haserror, 'from  ops_read_ctr_roads')
          call assertEqual(ref_emcat_road, emcat_road(1:nemcat_road), "control - file - roads")
       end if
    end subroutine test_ops_read_ctr_roads

    subroutine test_ops_read_ctr_loss(ref_ints, ref_idep, line_rest, errmsg)
    use m_ops_tdo_proc, only: Tdo_proc
    use iso_c_binding, only: eoln => c_new_line
    use m_commonfile, only: fu_input, IOB_STDOUT
    use m_fileutils, only: fix_slashes, sysopen
    use m_error, only: TError, write_error
    use m_ops_read_ctr, only:  ops_read_ctr_loss
    use no_pfunit
    implicit none
       integer,          intent(in) :: ref_ints(:)
       integer,          intent(in) :: ref_idep
       character(len=*), intent(in) :: line_rest
       character(len=*), intent(in) :: errmsg

       type(Tdo_proc) :: do_proc                    ! options to switch on/off specific processes
       logical        :: idep
       type(TError) :: error
       character(len=100) :: filename
       logical :: open_ok
       integer :: iostat, j

       filename='./level_1/resources\tst_ctrl.in'
       call fix_slashes(filename)
       open(fu_input, file=filename, iostat=iostat)
       call assertEqual(iostat,0, 'opening file "'//trim(filename)//'" for writing')
       if (ref_idep < 0) then
          write(fu_input,'(a)') &
             '*-----------------------identification layer----------------------------*' // eoln // &
             'YEAR           1956' // eoln
       else
          write(fu_input,'(10(a,i0))') &
             'LOSS           ',ref_idep,(' ', ref_ints(j), j=1,size(ref_ints,1)), line_rest//eoln
       end if
       close(fu_input)

       open_ok = sysopen(fu_input, filename, 'r', 'control file', error)
       if (error%haserror) call write_error(IOB_STDOUT, error)
       call assertFalse(error%haserror, 'opening file "'//trim(filename)//'" for reading')
       call ops_read_ctr_loss(do_proc, idep, error)
       close(fu_input)

       if (errmsg /= ' ') then
          call assertTrue(error%haserror, 'expected error from  ops_read_ctr_loss')
          call assertEqual(error%message, errmsg, 'expected error from  ops_read_ctr_loss')
       else
          if (error%haserror) call write_error(IOB_STDOUT, error)
          call assertFalse(error%haserror, 'from  ops_read_ctr_loss')
          call assertEqual(ref_idep==1, idep, "control - file - loss : idep")
          if (size(ref_ints)>=4) then
             call assertEqual( do_proc%chem,        ref_ints(1) == 1, "control - file - loss : chem")
             call assertEqual( do_proc%depl_drydep, ref_ints(2) == 1, "control - file - loss : depl_drydep")
             call assertEqual( do_proc%depl_wetdep, ref_ints(3) == 1, "control - file - loss : depl_wetdep")
             call assertEqual( do_proc%grad_drydep, ref_ints(4) == 1, "control - file - loss : grad_drydep")
          else
             call assertEqual( ref_idep==1,         idep, "control - file - loss : idep (2)")
             call assertEqual( do_proc%chem,        idep, "control - file - loss : chem")
             call assertEqual( do_proc%depl_drydep, idep, "control - file - loss : depl_drydep")
             call assertEqual( do_proc%depl_wetdep, idep, "control - file - loss : depl_wetdep")
             call assertEqual( do_proc%grad_drydep, idep, "control - file - loss : grad_drydep")
          end if
       end if
    end subroutine test_ops_read_ctr_loss

    subroutine test_ops_read_ctr_conv_rate( gasv, idep, isec, ref_vchemc, ref_iopt_vchem, errmsg)
    use m_ops_tdo_proc, only: Tdo_proc
    use iso_c_binding, only: eoln => c_new_line
    use m_commonfile, only: fu_input, IOB_STDOUT
    use m_fileutils, only: fix_slashes, sysopen
    use m_error, only: TError, write_error
    use m_ops_read_ctr, only:  ops_read_ctr_conv_rate
    use no_pfunit
    implicit none
       logical,          intent(in) :: gasv, idep, isec
       real,             intent(in) :: ref_vchemc
       integer,          intent(in) :: ref_iopt_vchem
       character(len=*), intent(in) :: errmsg

       real    :: vchemc
       integer :: iopt_vchem
       type(TError) :: error
       character(len=100) :: filename
       logical :: open_ok
       integer :: iostat
       real, parameter :: tol = 1e-5

       filename='./level_1/resources\tst_ctrl.in'
       call fix_slashes(filename)
       open(fu_input, file=filename, iostat=iostat)
       call assertEqual(iostat,0, 'opening file "'//trim(filename)//'" for writing')
       if (ref_iopt_vchem < 0) then
          write(fu_input,'(a)') &
             '*-----------------------identification layer----------------------------*' // eoln // &
             'YEAR           1956' // eoln
       elseif (ref_iopt_vchem==1) then
          write(fu_input,'(10(a,i0))') &
             'CONVRATE EMEP' // eoln
       elseif (ref_iopt_vchem==0) then
          write(fu_input,'(10(a,g0.10))') &
             'CONVRATE     ',ref_vchemc,eoln
       end if
       close(fu_input)

       open_ok = sysopen(fu_input, filename, 'r', 'control file', error)
       if (error%haserror) call write_error(IOB_STDOUT, error)
       call assertFalse(error%haserror, 'opening file "'//trim(filename)//'" for reading')
       call ops_read_ctr_conv_rate(gasv,idep,isec,vchemc,iopt_vchem,error)
       close(fu_input)

       if (errmsg /= ' ') then
          call assertTrue(error%haserror, 'expected error from  ops_read_ctr_conv_rate')
          call assertEqual(error%message, errmsg, 'expected error from  ops_read_ctr_conv_rate')
       else
          if (error%haserror) call write_error(IOB_STDOUT, error)
          call assertFalse(error%haserror, 'from  ops_read_ctr_conv_rate')
          call assertEqual(iopt_vchem, ref_iopt_vchem, "control - file - conv_rate: iopt_vchem")
          call assertEqual(vchemc, ref_vchemc, tol, "control - file - conv_rate: vchemc")
       end if
    end subroutine test_ops_read_ctr_conv_rate
end module m_tst_ctrl

program tst_ctrl
use m_commonconst_lt,only: MISVALNUM
use m_tst_ctrl
use no_pfunit, only: conclusion
implicit none
   integer :: iarr(3)
   integer :: nlines

   print *,'Testing ops_read_ctr_year'
   call test_ops_read_ctr_year(ref_year=2005, expect_prognisis=.false., line_rest=' ', errmsg=' ')
   call test_ops_read_ctr_year(ref_year=1975, expect_prognisis=.false., line_rest=' ', &
                      errmsg='invalid YEAR for chemical maps read from control file')
   call test_ops_read_ctr_year(ref_year=2005, expect_prognisis=.true., line_rest='    prognosis', errmsg=' ')
   call test_ops_read_ctr_year(ref_year=2005, expect_prognisis=.true., &
            line_rest='    additional useless information', errmsg='unexpected string beyond value for YEAR')

   print *,'Testing ops_read_ctr_roads'
   call test_ops_read_ctr_roads(ref_emcat_road = (/ 0 /), line_rest = ' ', errmsg = ' ')
   call test_ops_read_ctr_roads(ref_emcat_road = (/ 3100, 3200, 3300 /), line_rest = ' ', errmsg = ' ')
   call test_ops_read_ctr_roads(ref_emcat_road = (/ -10 /), line_rest = ' ', errmsg = 'Value read is below allowed lower limit')
   call test_ops_read_ctr_roads(ref_emcat_road = (/ 31000, 3200, 3300 /), line_rest = ' ', errmsg = 'Value read is above allowed upper limit')
   call test_ops_read_ctr_roads(ref_emcat_road = (/ 3100, 3200, 3300 /), line_rest = ' one two three', errmsg = 'Number is not an integer')

   print *,'Testing ops_read_ctr_loss'
   call test_ops_read_ctr_loss(ref_ints = iarr(1:0), ref_idep = 1, line_rest = ' ', errmsg = ' ')
   call test_ops_read_ctr_loss(ref_ints = iarr(1:0), ref_idep = 0, line_rest = ' spul ', errmsg = ' ')
   call test_ops_read_ctr_loss(ref_ints = iarr(1:0), ref_idep = 0, line_rest = ' ', errmsg = ' ')
   call test_ops_read_ctr_loss(ref_ints = iarr(1:0), ref_idep = -1, line_rest = ' ', errmsg = 'Undeclared parameter')
   call test_ops_read_ctr_loss(ref_ints = iarr(1:0), ref_idep = 2, line_rest = ' ', errmsg = 'Logical number should be 0 or 1')
   call test_ops_read_ctr_loss(ref_ints = (/1, 0, 0, 1, 1, 0, 1/), ref_idep = 1, line_rest = ' ', errmsg = ' ')
   call test_ops_read_ctr_loss(ref_ints = (/1, 0, 0, 1/), ref_idep = 1, line_rest = ' ', errmsg = ' ')
   call test_ops_read_ctr_loss(ref_ints = (/1, 0, 0, 1/), ref_idep = 0, line_rest = ' ', &
     errmsg = 'If all loss processes are switched off by idep, you cannot switch a specific process on; special OPS-users only')
   call test_ops_read_ctr_loss(ref_ints = (/1, 0/), ref_idep = 1, line_rest = ' ', errmsg = ' ')

   print *,'Testing ops_read_ctr_conv_rate'
   call test_ops_read_ctr_conv_rate( gasv=.true., idep=.true., isec=.true., ref_vchemc=0.0, ref_iopt_vchem=-1, errmsg = 'CONVRATE must have a value')
   call test_ops_read_ctr_conv_rate( gasv=.true., idep=.true., isec=.true., ref_vchemc=real(MISVALNUM), ref_iopt_vchem=1, errmsg = ' ')
   call test_ops_read_ctr_conv_rate( gasv=.true., idep=.true., isec=.false., ref_vchemc=real(MISVALNUM), ref_iopt_vchem=1, &
            errmsg = 'CONVRATE EMEP can only be used for acidifying components SO2, NOx, NH3')
   call test_ops_read_ctr_conv_rate( gasv=.true., idep=.false., isec=.true., ref_vchemc=real(MISVALNUM), ref_iopt_vchem=1, &
            errmsg = 'CONVRATE EMEP can only be used if deposition/chemical conversion switched on')
   call test_ops_read_ctr_conv_rate( gasv=.true., idep=.false., isec=.false., ref_vchemc=real(MISVALNUM), ref_iopt_vchem=1, &
            errmsg = 'CONVRATE EMEP can only be used for acidifying components SO2, NOx, NH3')
   call test_ops_read_ctr_conv_rate( gasv=.false., idep=.true., isec=.true., ref_vchemc=real(MISVALNUM), ref_iopt_vchem=1, &
            errmsg = 'CONVRATE EMEP can only be used for gasuous components')
   call test_ops_read_ctr_conv_rate( gasv=.false., idep=.true., isec=.false., ref_vchemc=real(MISVALNUM), ref_iopt_vchem=1, &
            errmsg = 'CONVRATE EMEP can only be used for acidifying components SO2, NOx, NH3')
   call test_ops_read_ctr_conv_rate( gasv=.false., idep=.false., isec=.true., ref_vchemc=real(MISVALNUM), ref_iopt_vchem=1, &
            errmsg = 'CONVRATE EMEP can only be used if deposition/chemical conversion switched on')
   call test_ops_read_ctr_conv_rate( gasv=.false., idep=.false., isec=.false., ref_vchemc=real(MISVALNUM), ref_iopt_vchem=1, &
            errmsg = 'CONVRATE EMEP can only be used for acidifying components SO2, NOx, NH3')
   call test_ops_read_ctr_conv_rate( gasv=.true., idep=.true., isec=.false., ref_vchemc=real(MISVALNUM), ref_iopt_vchem=0, &
            errmsg = 'CONVRATE must have a value')
   call test_ops_read_ctr_conv_rate( gasv=.true., idep=.false., isec=.true., ref_vchemc=real(MISVALNUM), ref_iopt_vchem=0, &
            errmsg = ' ')
   call test_ops_read_ctr_conv_rate( gasv=.true., idep=.false., isec=.false., ref_vchemc=real(MISVALNUM), ref_iopt_vchem=0, &
            errmsg = ' ')
   call test_ops_read_ctr_conv_rate( gasv=.false., idep=.true., isec=.true., ref_vchemc=real(MISVALNUM), ref_iopt_vchem=0, &
            errmsg = ' ')
   call test_ops_read_ctr_conv_rate( gasv=.false., idep=.true., isec=.false., ref_vchemc=real(MISVALNUM), ref_iopt_vchem=0, &
            errmsg = ' ')
   call test_ops_read_ctr_conv_rate( gasv=.false., idep=.false., isec=.true., ref_vchemc=real(MISVALNUM), ref_iopt_vchem=0, &
            errmsg = ' ')
   call test_ops_read_ctr_conv_rate( gasv=.false., idep=.false., isec=.false., ref_vchemc=real(MISVALNUM), ref_iopt_vchem=0, &
            errmsg = ' ')
   call test_ops_read_ctr_conv_rate( gasv=.true., idep=.true., isec=.false., ref_vchemc=real(17.3), ref_iopt_vchem=0, &
            errmsg = ' ')
   call test_ops_read_ctr_conv_rate( gasv=.true., idep=.false., isec=.true., ref_vchemc=real(17.3), ref_iopt_vchem=0, &
            errmsg = 'CONVRATE value cannot be specified for acidifying components SO2, NOx, NH3')
   call test_ops_read_ctr_conv_rate( gasv=.true., idep=.false., isec=.false., ref_vchemc=real(17.3), ref_iopt_vchem=0, &
            errmsg = 'CONVRATE value can only be specified if deposition/chemical conversion switched on')
   call test_ops_read_ctr_conv_rate( gasv=.false., idep=.true., isec=.true., ref_vchemc=real(17.3), ref_iopt_vchem=0, &
            errmsg = 'CONVRATE value cannot be specified for acidifying components SO2, NOx, NH3')
   call test_ops_read_ctr_conv_rate( gasv=.false., idep=.true., isec=.false., ref_vchemc=real(17.3), ref_iopt_vchem=0, &
            errmsg = 'CONVRATE value can only be used for gasuous components')
   call test_ops_read_ctr_conv_rate( gasv=.false., idep=.false., isec=.true., ref_vchemc=real(17.3), ref_iopt_vchem=0, &
            errmsg = 'CONVRATE value cannot be specified for acidifying components SO2, NOx, NH3')
   call test_ops_read_ctr_conv_rate( gasv=.false., idep=.false., isec=.false., ref_vchemc=real(17.3), ref_iopt_vchem=0, &
            errmsg = 'CONVRATE value can only be specified if deposition/chemical conversion switched on')

   print *,'testing ops_read_ctr'
   ! Test with all lines
   call test_ops_read_ctr(10000, ' ')

   ! Test for limited number of lines and test unexpected end of file:
   do nlines = 1,47
       call test_ops_read_ctr(nlines, 'Unexpected end-of-file')
   end do

   call conclusion()
end program tst_ctrl
