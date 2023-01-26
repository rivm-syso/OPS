!-------------------------------------------------------------------------------------------------------------------------------
!
! This program is free software: you can redistribute it and/or modify
! it under the terms of the GNU General Public License as published by
! the Free Software Foundation, either version 3 of the License, or
! (at your option) any later version.
!
! This program is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
! GNU General Public License for more details.
!
! You should have received a copy of the GNU General Public License
! along with this program.  If not, see <http://www.gnu.org/licenses/>.
!

!-------------------------------------------------------------------------------------------------------------------------------
! DESCRIPTION         : Print output for non-gridded receptors to print file (= PRNFILE in control file)
!-------------------------------------------------------------------------------------------------------------------------------
module m_ops_print_recep2
 
implicit none

contains

SUBROUTINE ops_print_recep2 (project, icm, gasv, idep, do_proc, isec, igrid, verb, namco, namsec, nam_pri_sec, coneh, depeh, conc_cf, amol21, &
        &  ugmoldep, nrrcp, nsubsec, namrcp, xm, ym, precip, cpri, csec, drydep, ddepri, wetdep, wdepri, cno2, cnox, lu_rcp_dom_all, z0_rcp_all, &
        &  gemcpri, gemcsec, ccr, gemddep, gemddpri, gemddsec, ddrpri, ddrsec, gemwdep, gemwdpri, gemwdsec, wdrpri, &
        &  wdrsec, gemprec, gemtdep, csubsec, gem_subsec, nam_subsec, totdep, scale_con, scale_sec, &
        &  scale_subsec, scale_dep, error)

use ops_print_table
use m_error
use m_utils
use m_commonconst_lt
use m_ops_print_kop
use m_ops_brondepl, only: Tdo_proc

IMPLICIT NONE

! CONSTANTS
CHARACTER*512                                    :: ROUTINENAAM                ! 
PARAMETER    (ROUTINENAAM = 'ops_print_recep2')

! SUBROUTINE ARGUMENTS - INPUT
CHARACTER*(*), INTENT(IN)                        :: project                    ! 
INTEGER*4, INTENT(IN)                            :: icm                        ! component number
LOGICAL,   INTENT(IN)                            :: gasv                       ! 
type(Tdo_proc),   INTENT(IN)                     :: do_proc                    ! options to switch on/off specific processes
LOGICAL,   INTENT(IN)                            :: isec                       ! 
LOGICAL,   INTENT(IN)                            :: verb                       ! 
CHARACTER*(*), INTENT(IN)                        :: namco                      ! 
CHARACTER*(*), INTENT(IN)                        :: namsec                     ! 
CHARACTER*(*), INTENT(IN)                        :: nam_pri_sec                ! name of primary + secondary component (SOx, NOy, NHy)
CHARACTER*(*), INTENT(IN)                        :: coneh                      ! 
CHARACTER*(*), INTENT(IN)                        :: depeh                      ! 
REAL*4,    INTENT(IN)                            :: conc_cf                    ! 
REAL*4,    INTENT(IN)                            :: amol21                     ! 
REAL*4,    INTENT(IN)                            :: ugmoldep                   ! 
INTEGER*4, INTENT(IN)                            :: nrrcp                      ! number of receptor points
INTEGER*4, INTENT(IN)                            :: nsubsec                    ! number of sub-secondary species
CHARACTER*(*), INTENT(IN)                        :: namrcp (nrrcp)             ! 
REAL*4,    INTENT(IN)                            :: xm(nrrcp)                  ! x-coordinates of receptors (m RDM)
REAL*4,    INTENT(IN)                            :: ym(nrrcp)                  ! y-coordinates of receptors (m RDM)
REAL*4,    INTENT(IN)                            :: precip(nrrcp)              ! total precipitation per year [mm/year]
REAL*4,    INTENT(IN)                            :: cpri(nrrcp)                ! primary concentration
REAL*4,    INTENT(IN)                            :: csec(nrrcp)                ! secondary concentration
REAL*4,    INTENT(IN)                            :: drydep(nrrcp)              ! dry deposition
REAL*4,    INTENT(IN)                            :: ddepri(nrrcp)              ! dry deposition of primary component
REAL*4,    INTENT(IN)                            :: wetdep(nrrcp)              ! wet deposition
REAL*4,    INTENT(IN)                            :: wdepri(nrrcp)              ! wet deposition of primary component
REAL*4,    INTENT(IN)                            :: cno2(nrrcp)                ! NO2 concentration (derived from NOx and parameterised ratio NO2/NOx)
REAL*4,    INTENT(IN)                            :: cnox(nrrcp)                ! NOx concentration, saved from previous iteration
INTEGER*4, INTENT(IN)                            :: lu_rcp_dom_all(nrrcp)      ! dominant land use class for each receptor point
REAL*4,    INTENT(IN)                            :: z0_rcp_all(nrrcp)          ! roughness lengths for all receptors; from z0-map or receptor file [m]
REAL*4,    INTENT(IN)                            :: gemcpri                    ! mean for prim. concentration
REAL*4,    INTENT(IN)                            :: gemcsec                    ! mean for sec. concentration
REAL*4,    INTENT(IN)                            :: ccr                        ! eff. chemical conversion rate
REAL*4,    INTENT(IN)                            :: gemddep                    ! mean for dry deposition
REAL*4,    INTENT(IN)                            :: gemddpri                   ! mean for dry deposition (pri)
REAL*4,    INTENT(IN)                            :: gemddsec                   ! mean for dry deposition (sec)
REAL*4,    INTENT(IN)                            :: ddrpri                     ! eff. dry deposition rate (prim)
REAL*4,    INTENT(IN)                            :: ddrsec                     ! eff. dry deposition rate (sec)
REAL*4,    INTENT(IN)                            :: gemwdep                    ! mean for wet deposition
REAL*4,    INTENT(IN)                            :: gemwdpri                   ! mean for wet deposition (pri)
REAL*4,    INTENT(IN)                            :: gemwdsec                   ! mean for wet deposition (sec)
REAL*4,    INTENT(IN)                            :: wdrpri                     ! eff. wet deposition rate (prim)
REAL*4,    INTENT(IN)                            :: wdrsec                     ! eff. wet deposition rate (sec)
REAL*4,    INTENT(IN)                            :: gemprec                    ! mean annual precpitation from meteo
REAL*4,    INTENT(IN)                            :: gemtdep                    ! mean for total deposition
REAL*4,    INTENT(IN)                            :: csubsec(nrrcp,nsubsec)     ! concentration of sub-secondary species [ug/m3]
REAL*4,    INTENT(IN)                            :: gem_subsec(nsubsec)        ! grid mean for concentration of sub-secondary species [ug/m3]
CHARACTER*(*), INTENT(IN)                        :: nam_subsec(nsubsec)        ! names of sub-secondary speciea
REAL*4,    INTENT(IN)                            :: totdep(nrrcp)              ! total deposition
REAL*4,    INTENT(IN)                            :: scale_con                  ! 
REAL*4,    INTENT(IN)                            :: scale_sec                  ! 
REAL*4,    INTENT(IN)                            :: scale_subsec(nsubsec)      ! scaling factor for sub-secondary species
REAL*4,    INTENT(IN)                            :: scale_dep                  ! 

! SUBROUTINE ARGUMENTS - I/O
LOGICAL,   INTENT(INOUT)                         :: idep                       ! 
LOGICAL,   INTENT(INOUT)                         :: igrid                      ! include receptor values in the Report output; is set with INCLUDE in ctr-file or -v option

! SUBROUTINE ARGUMENTS - OUTPUT
TYPE (Terror), INTENT(INOUT)                     :: error                      ! 

! LOCAL VARIABLES
INTEGER*4                                        :: i                          ! 
INTEGER*4                                        :: isubsec                    ! index of sub-secondary species
REAL*4                                           :: vdpri(nrrcp)               ! 
REAL*4                                           :: vdsec(nrrcp)               ! 
CHARACTER*4                                      :: vdeh                       ! 
CHARACTER*4                                      :: z0eh                       ! unit for z0 (eh << eenheid)
CHARACTER*4                                      :: lueh                       ! unit for land use (eh << eenheid)
CHARACTER*7                                      :: precip_eh                  ! unit for precipitation (eh << eenheid)
CHARACTER(LEN=40), ALLOCATABLE                   :: par_comp(:)                ! component names
CHARACTER(LEN=40), ALLOCATABLE                   :: par_nam(:)                 ! parameter names
REAL, ALLOCATABLE                                :: par_val(:,:)               ! parameter values
CHARACTER(LEN=40), ALLOCATABLE                   :: par_unit(:)                ! unit for parameter
REAL, ALLOCATABLE                                :: par_scale(:)               ! scale factors for parameter values (not used anymore, see print_values_par_val)
CHARACTER(LEN=40), ALLOCATABLE                   :: par_fmt(:)                 ! format for value (i = integer, e = E-format, f = F-format)
INTEGER                                          :: npar                       ! number of parameters = number of columns in table 
INTEGER                                          :: ipar                       ! index of parameter

!-------------------------------------------------------------------------------------------------------------------------------

! Definition of units for deposition velocity vd, roughness length z0, land use
vdeh = 'cm/s'
z0eh = 'm'
lueh = '-'
precip_eh = 'mm/year'

! To avoid unlogical combinations: 
IF (verb) igrid = .TRUE.  ! option -v -> igrid = true
IF (isec) idep  = .TRUE.  

! Print header:
CALL ops_print_kop (project,namco)

IF (igrid) THEN

   !---------------------------------------------------------------------------
   ! Print general information on components and count number of parameters
   !---------------------------------------------------------------------------
   ipar = 0
   IF (isec) THEN
      ! SO2, NOx, NH3:
      IF (icm .eq. 2) THEN      
         CALL print_conc_names(namco, namsec, nam_subsec, 'NO2', 'NOx') 
         ipar = ipar + 4 + nsubsec ! cpri, csec, csubsec, cno2, cnox
      ELSE
         CALL print_conc_names(namco, namsec, nam_subsec)
         ipar = ipar + 2 + nsubsec ! cpri, csec, csubsec
      ENDIF
      IF (idep) THEN
         CALL print_depo_names(namsec)
         ipar = ipar + 3 + 4   ! drydep, wetdep, totdep + dry / wet deposition primary / secondary species separately 
      ENDIF
   ELSE
      ! Other species
      CALL print_conc_names(namco)
      ipar = ipar + 1  ! cpri 
      IF (idep) THEN
         CALL print_depo_names()
         ipar = ipar + 3 ! drydep, wetdep, totdep
      ENDIF
   ENDIF
   write(fu_prt,'(a)') ' Calculated for specific locations'
   
   ! Verbose option:
   IF (verb) THEN
      IF (idep) THEN   
         ipar = ipar + 1; ! vdpri
         IF (isec) THEN
            ipar = ipar + 1; ! vdsec
         ENDIF
      ENDIF
      ipar = ipar + 3; ! z0, lu, precipitation
   ENDIF

   !--------------------------------------------------
   ! Allocate arrays for printing
   !--------------------------------------------------
   npar = ipar
   allocate(par_comp(npar))
   allocate(par_nam(npar))
   allocate(par_val(nrrcp,npar))
   allocate(par_unit(npar))
   allocate(par_scale(npar))
   allocate(par_fmt(npar))
   
   !--------------------------------------------------
   ! Fill arrays for printing
   !--------------------------------------------------
   ! Default E-format:
   par_fmt = 'e12.5'
   
   ! Always print primary concentration:
   ! Note: par_scale is not used anymore (as in old ops_print_recep), see print_values_par_val.
   ipar = 1; par_comp(ipar) = namco; par_nam = 'conc'; par_unit(ipar) = coneh; par_val(:,ipar) = cpri; par_scale(ipar) = scale_con;
   
   IF (idep) THEN
      ! Deposition primary + secondary (if  present) species:
      ipar = ipar + 1; par_comp(ipar) = nam_pri_sec; par_nam(ipar) = 'dry_dep'; par_unit(ipar) = depeh; par_val(:,ipar) = drydep; par_scale(ipar) = scale_dep;
      ipar = ipar + 1; par_comp(ipar) = nam_pri_sec; par_nam(ipar) = 'wet_dep'; par_unit(ipar) = depeh; par_val(:,ipar) = wetdep; par_scale(ipar) = scale_dep;
      ipar = ipar + 1; par_comp(ipar) = nam_pri_sec; par_nam(ipar) = 'tot_dep'; par_unit(ipar) = depeh; par_val(:,ipar) = totdep; par_scale(ipar) = scale_dep;
   ENDIF
 
   ! Concentration secondary and sub secondary species:
   IF (isec) THEN
      ipar = ipar + 1; par_comp(ipar) = namsec; par_nam(ipar) = 'conc'; par_unit(ipar) = coneh; par_val(:,ipar) = csec; par_scale(ipar) = scale_sec;
      DO isubsec = 1,nsubsec
         ipar = ipar + 1; par_comp(ipar) = nam_subsec(isubsec); par_nam(ipar) = 'conc'; par_unit(ipar) = 'ug/m3'; par_val(:,ipar) = csubsec(:,isubsec); par_scale(ipar) = scale_subsec(isubsec);      
      ENDDO
   ENDIF      
   
   ! Verbose option:
   IF (verb) THEN

      ! par_comp not used from now on:
      par_comp(ipar+1:npar) = '' 
      
      ! Add effective deposition velocity:  
      IF (idep) THEN   
         vdpri = ddepri/ugmoldep*1.0e2/(cpri/ conc_cf*3600.)/amol21
         ipar = ipar + 1; par_comp(ipar) = namco; par_nam(ipar) = 'vdpri';  par_unit(ipar) = vdeh; par_val(:,ipar) = vdpri; par_scale(ipar) = 1.0e3;
         IF (isec) THEN
            vdsec = (drydep - ddepri)/ugmoldep*1.0e2/ (csec*3600.)
            ipar = ipar + 1; par_comp(ipar) = namsec; par_nam(ipar) = 'vdsec';  par_unit(ipar) = vdeh; par_val(:,ipar) = vdsec; par_scale(ipar) = 1.0e3;
         ENDIF
      ENDIF

      ! Add z0, lu, precipitation:
      ipar = ipar + 1; par_nam(ipar) = 'z0';     par_unit(ipar) = z0eh;      par_val(:,ipar) = z0_rcp_all;           par_scale(ipar) = 1.0e3;
      ipar = ipar + 1; par_nam(ipar) = 'lu_dom'; par_unit(ipar) = lueh;      par_val(:,ipar) = REAL(lu_rcp_dom_all); par_scale(ipar) = 1.0;   par_fmt(ipar) = 'i12'
      ipar = ipar + 1; par_nam(ipar) = 'precip'; par_unit(ipar) = precip_eh; par_val(:,ipar) = precip;               par_scale(ipar) = 1.0;
   ENDIF

   ! Additional deposition values: 
   IF (isec .and. idep) THEN
       ! Dry / wet deposition primary / secondary species separately:
       ipar = ipar + 1; par_comp(ipar) = namco;  par_nam(ipar) = 'dry_dep'; par_unit(ipar) = depeh; par_val(:,ipar) = ddepri;        par_scale(ipar) = scale_dep;      
       ipar = ipar + 1; par_comp(ipar) = namsec; par_nam(ipar) = 'dry_dep'; par_unit(ipar) = depeh; par_val(:,ipar) = drydep-ddepri; par_scale(ipar) = scale_dep;      
       ipar = ipar + 1; par_comp(ipar) = namco;  par_nam(ipar) = 'wet_dep'; par_unit(ipar) = depeh; par_val(:,ipar) = wdepri;        par_scale(ipar) = scale_dep;      
       ipar = ipar + 1; par_comp(ipar) = namsec; par_nam(ipar) = 'wet_dep'; par_unit(ipar) = depeh; par_val(:,ipar) = wetdep-wdepri; par_scale(ipar) = scale_dep;      
       
       ! Deposition sub-secondary species:
    ENDIF

   ! NO2 and NOx concentrations:
   IF (icm .eq. 2) THEN
       ! NO2:
       ipar = ipar + 1; par_comp(ipar) = 'NO2'; par_nam = 'conc'; par_unit(ipar) = coneh; par_val(:,ipar) = cno2; par_scale(ipar) = scale_con;
       
       ! NOx:
       ipar = ipar + 1; par_comp(ipar) = 'NOx'; par_nam = 'conc'; par_unit(ipar) = coneh; par_val(:,ipar) = cnox; par_scale(ipar) = scale_con;
    ENDIF
   
   ! Check:
   IF (ipar .ne. npar) THEN
      write(*,*) 'internal programming error in ',trim(ROUTINENAAM)
      write(*,*) 'ipar = ',ipar,' npar = ',npar
      stop
   ENDIF
   
   !-----------------------------------------
   ! Print values to file (table)
   !-----------------------------------------
   call print_values_par_val(nrrcp,npar,namrcp,xm,ym,par_comp,par_nam,par_val,par_unit,par_scale,par_fmt)
   ! IF (error%haserror) GOTO 9000

   ! Deallocate arrays:
   deallocate(par_comp)
   deallocate(par_nam)
   deallocate(par_val)
   deallocate(par_unit)
   deallocate(par_scale)
   deallocate(par_fmt)

ENDIF ! IF (igrid)

!-----------------------------------------
! Output of summary statistics
!-----------------------------------------

! Header:
IF (igrid) THEN
  WRITE (fu_prt, '(a)') char(12)
  CALL ops_print_kop (project,namco)
ENDIF

! Info on component:
WRITE(fu_prt,'(/)')
WRITE(fu_prt,'('' Summary statistics for '',a)') namco(:LEN_TRIM(namco))
IF (.NOT.gasv) THEN
  WRITE (fu_prt, '(/,1x,a,'' considered as aerosol'')') namco(1:LEN_TRIM(namco))
ELSE
  WRITE (fu_prt, '(/,1x,a,'' considered as gaseous'')') namco(1:LEN_TRIM(namco))
  IF (.NOT.idep) THEN
    WRITE (fu_prt,'('' No conversion and deposition assumed'')')
  ELSE
    IF (.NOT.isec) THEN
      WRITE (fu_prt,'('' Dispersion and deposition of secundary'', '' component '', a, '' not included'')')                    &
          &  namsec(1:LEN_TRIM(namsec))
    ELSE
      WRITE (fu_prt,'('' Dispersion and deposition of secundary'', '' component '', a, '' included'')')                        &
          &  namsec(1:LEN_TRIM(namsec))
    ENDIF
    IF (do_proc%chem .or. do_proc%depl_drydep .or. do_proc%depl_wetdep .or. do_proc%grad_drydep) THEN
      write(fu_prt,'(a)') 'Settings for specific processes switched on (T) or off (F) (special OPS-users only)'
      write(fu_prt,'(l2,a)') do_proc%chem       , ' chemistry'
      write(fu_prt,'(l2,a)') do_proc%depl_drydep, ' depletion over trajectory due to dry deposition (deposition at receptor still possible)'
      write(fu_prt,'(l2,a)') do_proc%depl_wetdep, ' depletion over trajectory due to wet deposition (deposition at receptor still possible)'
      write(fu_prt,'(l2,a)') do_proc%grad_drydep, ' vertical gradient due to local deposition at receptor'
    ENDIF  
  ENDIF
ENDIF
WRITE(fu_prt,'(1x,80a)') ('-',i=1,79)

! Concentration of primary component always written to output
WRITE (fu_prt, '(/,'' average '',a,'' concentration'', T50, '': '', e9.3, a10)') namco(:LEN_TRIM(namco)), gemcpri, coneh

IF (idep)THEN
  IF (.NOT.isec) THEN
     ! Chemical conversion rate
    WRITE (fu_prt,'('' eff. chem. conv. rate'', T50, '': '', f9.3, '' %/h'')') ccr

    ! Average dry deposition, eff. dry deposition velocity:
    WRITE (fu_prt, '(/,'' average dry deposition'', T50, '': '', e9.3, a10)') gemddep, depeh
    WRITE (fu_prt, '('' effective dry deposition velocity'', T50, '': '', f9.3, '' cm/s'')') ddrpri

    ! Average wet deposition, eff. wet deposition rate, annual precipitation amount:
    WRITE (fu_prt, '(/,'' average wet deposition'', T50, '': '', e9.3, a10)') gemwdep, depeh
    WRITE (fu_prt, '('' effective wet deposition rate'', T50, '': '', f9.3, '' %/h'')') wdrpri
    WRITE (fu_prt, '('' annual precipitation amount'', T50, '': '', i9, '' mm'')') NINT(gemprec)

    ! Average deposition (dry+wet):
    WRITE (fu_prt, '(/,'' average deposition'', T50, '': '', e9.3, a10)') gemtdep, depeh

  ELSE

     ! Concentration of secondary component, chemical conversion rate:
    WRITE (fu_prt, '(/,'' average '',a,'' concentration'', T50, '': '', e9.3, a6)') namsec(:LEN_TRIM(namsec)), gemcsec,'ug/m3'
    WRITE (fu_prt,'('' eff. '',a,'' > '',a,'' chem. conv. rate'', T50, '': '', f9.3, '' %/h'')') namco(:LEN_TRIM(namco)), namsec(:LEN_TRIM(namsec)), ccr

    ! Concentration of sub-secondary species:
    do isubsec = 1,nsubsec
       WRITE (fu_prt, '(/,'' average '',a,'' concentration'', T50, '': '', e9.3, a6)') nam_subsec(isubsec)(:LEN_TRIM(nam_subsec(isubsec))), &
              gem_subsec(isubsec), 'ug/m3'
    enddo

    ! Mean dry deposition (total), mean dry deposition (primary), mean dry deposition (secondary):
    WRITE (fu_prt, '(/, '' average dry '',a,'' deposition'', '' (as '', a, '')'', T50, '': '', e9.3, a10)')                    &
        &  nam_pri_sec(:LEN_TRIM(nam_pri_sec)), namsec(:LEN_TRIM(namsec)), gemddep, depeh
    WRITE (fu_prt,'('' average dry '',a,'' deposition'', '' (as '', a, '')'', T50, '': '', e9.3, a10)')                        &
        &  namco(:LEN_TRIM(namco)), namsec(:LEN_TRIM(namsec)), gemddpri, depeh
    WRITE (fu_prt,'('' average dry '',a,'' deposition '', ''(as '', a, '')'', T50, '': '', e9.3, a10)')                        &
        &  namsec(:LEN_TRIM(namsec)), namsec(:LEN_TRIM(namsec)), gemddsec, depeh

    ! Effective dry deposition velocity (primary), effective dry deposition velocity (secondary):
    WRITE (fu_prt, '('' effective dry deposition velocity '',a, T50, '': '', f9.3, '' cm/s'')') namco(:LEN_TRIM(namco)),ddrpri
    WRITE (fu_prt, '('' effective dry deposition velocity '',a, T50, '': '', f9.3, '' cm/s'')') namsec(:LEN_TRIM(namsec)),ddrsec
    
    ! Mean wet deposition (total), mean wet deposition (primary),mean wet deposition (secondary):
    WRITE (fu_prt, '(/,'' average wet '',a,'' deposition'', '' (as '', a, '')'', T50, '': '', e9.3, a10)')                     &
        &  nam_pri_sec(:LEN_TRIM(nam_pri_sec)), namsec(:LEN_TRIM(namsec)), gemwdep, depeh
    WRITE (fu_prt, '('' average wet '',a,'' deposition'', '' (as '', a, '')'', T50, '': '', e9.3, a10)')                       &
        &  namco(:LEN_TRIM(namco)), namsec(:LEN_TRIM(namsec)), gemwdpri, depeh
    WRITE (fu_prt, '('' average wet '',a,'' deposition'', '' (as '', a, '')'', T50, '': '', e9.3, a10)')                       &
        &  namsec(:LEN_TRIM(namsec)), namsec(:LEN_TRIM(namsec)), gemwdsec, depeh

    ! Effective wet deposition velocity (primary), effective wet deposition velocity (secondary):
    WRITE (fu_prt,'('' effective wet deposition rate '',a, T50, '': '', f9.3, '' %/h'')') namco(:LEN_TRIM(namco)), wdrpri
    WRITE (fu_prt,'('' effective wet deposition rate '',a, T50, '': '', f9.3, '' %/h'')') namsec(:LEN_TRIM(namsec)), wdrsec

   ! Annual precipitation amount (region or interpolated):
    WRITE (fu_prt, '('' annual precipitation amount'', T50, '': '', i9, '' mm'')') NINT(gemprec)

   ! Average deposition (dry + wet):
    WRITE (fu_prt, '(/, '' average '',a,'' deposition'', '' (as '', a, '')'', T50, '': '', e9.3, a10)')                        &
        &  nam_pri_sec(:LEN_TRIM(nam_pri_sec)), namsec(:LEN_TRIM(namsec)), gemtdep, depeh
  ENDIF
ENDIF
WRITE(fu_prt,'(1x,80a)') ('-',i=1,79)

RETURN

9000 CALL ErrorCall(ROUTINENAAM, error)

END SUBROUTINE ops_print_recep2

end module m_ops_print_recep2
