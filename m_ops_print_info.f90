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
! DESCRIPTION           : Prints input parameters at the end of text output
!-------------------------------------------------------------------------------------------------------------------------------
module m_ops_print_info 

implicit none

contains

SUBROUTINE ops_print_info (project, gasv, isec, intpol, spgrid, z0_rcp, namco, nbron, bnr, bx, by, bsterkte, bqrv, bqtr,        &
        &  bwarmte, bhoogte, bdiam, bsigmaz, btgedr, bdegr, bcatnr, blandnr, emis, emtrend, jb, mb, idb, jt, mt, idt, iseiz,    &
        &  f_z0user, landmax, subbron, domlu, varz, perc, mindist, maxdist, error)

use m_commonfile
use m_commonconst_lt
use m_error
use m_utils
use m_ops_print_kop

IMPLICIT NONE

! CONSTANTS
CHARACTER*512                                    :: ROUTINENAAM                ! 
PARAMETER    (ROUTINENAAM = 'ops_print_info')

! SUBROUTINE ARGUMENTS - INPUT
CHARACTER*(*), INTENT(IN)                        :: project                    ! project name
LOGICAL,   INTENT(IN)                            :: gasv                       ! code for substance appearance (gas/particle)
LOGICAL,   INTENT(IN)                            :: isec                       ! true when comp=SO2,NOx,NH3
INTEGER*4, INTENT(IN)                            :: intpol                     ! 
INTEGER*4, INTENT(IN)                            :: spgrid                     ! code for type of receptor points
REAL*4,    INTENT(IN)                            :: z0_rcp                     ! roughness length at receptor; from z0-map [m]
CHARACTER*(*), INTENT(IN)                        :: namco                      ! substance name
INTEGER*4, INTENT(IN)                            :: nbron                      ! number of emission sources (after selection)
INTEGER*4, INTENT(IN)                            :: bnr(LSBUF)                 ! buffer with source numbers
INTEGER*4, INTENT(IN)                            :: bx(LSBUF)                  ! buffer with x-coordinates
INTEGER*4, INTENT(IN)                            :: by(LSBUF)                  ! buffer with y-coordinates
REAL*4,    INTENT(IN)                            :: bsterkte(LSBUF)            ! buffer with source strengths (industrial)
REAL*4,    INTENT(IN)                            :: bqrv(LSBUF)                ! buffer with source strengths (space heating)
REAL*4,    INTENT(IN)                            :: bqtr(LSBUF)                ! buffer with source strengths (traffic)
REAL*4,    INTENT(IN)                            :: bwarmte(LSBUF)             ! buffer with heat contents
REAL*4,    INTENT(IN)                            :: bhoogte(LSBUF)             ! buffer with source heights
REAL*4,    INTENT(IN)                            :: bdiam(LSBUF)               ! buffer with source diameters
REAL*4,    INTENT(IN)                            :: bsigmaz(LSBUF)             ! buffer with source heigth variances
INTEGER*4, INTENT(IN)                            :: btgedr(LSBUF)              ! buffer with diurnal variation codes
INTEGER*4, INTENT(IN)                            :: bdegr(LSBUF)               ! buffer with particle size distribution codes
INTEGER*4, INTENT(IN)                            :: bcatnr(LSBUF)              ! buffer with category codes
INTEGER*4, INTENT(IN)                            :: blandnr(LSBUF)             ! buffer with area codes
REAL*4,    INTENT(IN)                            :: emtrend                    ! emission correction factor
INTEGER*4, INTENT(IN)                            :: jb                         ! starting year of meteo
INTEGER*4, INTENT(IN)                            :: mb                         ! starting month of meteo
INTEGER*4, INTENT(IN)                            :: idb                        ! starting day of meteo
INTEGER*4, INTENT(IN)                            :: jt                         ! ending year of meteo
INTEGER*4, INTENT(IN)                            :: mt                         ! ending month of meteo
INTEGER*4, INTENT(IN)                            :: idt                        ! ending day of meteo
INTEGER*4, INTENT(IN)                            :: iseiz                      ! 
LOGICAL*4, INTENT(IN)                            :: f_z0user                   ! true if z0 is user specified
LOGICAL,   INTENT(IN)                            :: subbron                    
LOGICAL,   INTENT(IN)                            :: domlu                    
LOGICAL,   INTENT(IN)                            :: varz                    
LOGICAL,   INTENT(IN)                            :: perc                    
LOGICAL,   INTENT(IN)                            :: mindist                 
LOGICAL,   INTENT(IN)                            :: maxdist                    

! SUBROUTINE ARGUMENTS - I/O
REAL*4,    INTENT(INOUT)                         :: emis(6,NLANDMAX)
INTEGER*4, INTENT(INOUT)                         :: landmax                    ! number of countries in emission file

! SUBROUTINE ARGUMENTS - OUTPUT
TYPE (TError), INTENT(OUT)                       :: error                      ! error handling record

! LOCAL VARIABLES
INTEGER*4                                        :: istatgeb                   ! climatological area

! LOCAL VARIABLES
INTEGER*4                                        :: i                          ! 
INTEGER*4                                        :: ierr                       ! 
INTEGER*4                                        :: indx                       ! 
INTEGER*4                                        :: jndx                       ! 
INTEGER*4                                        :: statclass                  ! 
REAL*4                                           :: qb                         ! emission of individual source
CHARACTER*1                                      :: statcode                   ! 
CHARACTER*30                                     :: climper(0:6)               ! 


! DATA
DATA climper /'long term period', 'year period', 'winter period', 'summer period', 'winter month period',                      &
           &  'summer month period', 'unspecified'/
!-------------------------------------------------------------------------------------------------------------------------------
!
! Describe the meteo input data
!
WRITE (fu_prt, '(a)') char(12)
CALL ops_print_kop(project, namco)

IF (.NOT.subbron .or. domlu .or. varz .or. perc .or. mindist .or. maxdist) THEN 

  WRITE (fu_prt, '(/,'' Argument options used for this run:'')')
  WRITE (fu_prt, '(   '' -----------------------------------'')')

  IF (.NOT.subbron) WRITE (fu_prt, '(   '' nosub   (no sub-receptors are used) '')')
  IF (domlu)        WRITE (fu_prt, '(   '' domlu   (dominant landuse is used)'')')
  IF (varz)         WRITE (fu_prt, '(   '' varz    (receptor height is taken from receptorfile)'')')
  IF (perc)         WRITE (fu_prt, '(   '' perc    (landuse information is used from receptorfile)'')')
  IF (mindist)      WRITE (fu_prt, '(   '' mindist (minimal distance for calculation 5 km)'')')
  IF (maxdist)      WRITE (fu_prt, '(   '' maxdist (maxmimum distance for calculation 25 km)'')')

ELSE

  WRITE (fu_prt, '(/,'' No argument options used for this run.'')')
  WRITE (fu_prt, '(   '' --------------------------------------'')')

ENDIF

WRITE (fu_prt, '(//,'' Meteorological statistics used:'')')
WRITE (fu_prt, '(   '' -------------------------------'')')

777 FORMAT (' type of statistics   : ',1x,a)
778 FORMAT (' type of statistics   : ',1x,a,1x,i2)

indx = INDEX(kname,'.',.TRUE.)
jndx = 0
DO i=1,LEN_TRIM(kname)
  IF ((kname(i:i).EQ.'/').OR.(kname(i:i).EQ.'\')) jndx = i
ENDDO

ierr = 1
IF (indx.NE.0) READ (kname(jndx+1:jndx+1), '( A1 )', IOSTAT=ierr) statcode
IF (ierr.EQ.0) READ (kname(jndx+2:jndx+2), '( Z1 )', IOSTAT=ierr) statclass
IF (ierr.EQ.0) THEN
  IF (kname(indx+1:indx+1).EQ.'*') THEN
    istatgeb = 1
  ELSE
    READ (kname(indx+1:indx+3),'(I3.3)',IOSTAT=ierr) istatgeb
  ENDIF
ENDIF

IF (ierr.EQ.0) THEN
  IF (intpol.NE.0) THEN
    CALL print_region(KLIGEB(MIN(istatgeb+1,NKLIGEB)), istatgeb)
  ELSE
    WRITE (fu_prt, '(/,1x,''climatological area  : '',1x,a)') 'The Netherlands (interpolated meteo)'
  ENDIF
  IF (statcode(1:1).EQ.'a' .OR. statcode(1:1).EQ.'m' ) THEN
    WRITE (fu_prt,777) 'normal statistics'
  ELSEIF (statcode(1:1).EQ.'w') THEN
    WRITE (fu_prt,778) 'wind sector',statclass
  ELSEIF (statcode(1:1).EQ.'s') THEN
    WRITE (fu_prt,778) 'stability class',statclass
  ELSEIF (statcode(1:1).EQ.'t') THEN
    WRITE (fu_prt,778) '2-hour period number',statclass
  ELSEIF (statcode(1:1).EQ.'d') THEN
    WRITE (fu_prt,777) 'daytime statistics'
  ELSEIF (statcode(1:1).EQ.'n') THEN
    WRITE (fu_prt,777) 'night statistics'
  ELSE
    WRITE (fu_prt,778) 'unknown statistics for class',statclass
  ENDIF
  WRITE (fu_prt, '(1x  ,''climatological period: '', 1x, 3i2.2, '' - '', 3i2.2, 2x, a)') jb, mb, idb, jt, mt, idt,             &
   &  climper(iseiz)
ELSE
  CALL print_region(KLIGEB(NKLIGEB), istatgeb)
  WRITE (fu_prt,778) 'unknown statistics'
  WRITE (fu_prt, '(1x  ,''climatological period: '', 1x, 3i2.2, '' - '', 3i2.2, 2x, a)') jb, mb, idb, jt, mt, idt,             &
   &  climper(iseiz)
ENDIF
!
! Roughness length z0
!
WRITE (fu_prt, '(//,'' Surface roughness (z0) data used:'')')
WRITE (fu_prt, '(   '' ---------------------------------'')')

IF (f_z0user) THEN
  WRITE (fu_prt, '('' User supplied value for all receptor points: '', f4.2, '' m'')') z0_rcp
ELSE
  WRITE (fu_prt, '('' Regionally differentiated z0 values determined by OPS'')')
ENDIF
!
! Files used and generated
!
WRITE (fu_prt, '(//,'' Files used by OPS:'')')
WRITE (fu_prt, '(   '' ------------------'')')

WRITE (fu_prt, '('' Control parameter file'',T33,'': '', a)') ctrnam(:LEN_TRIM(ctrnam))

WRITE (fu_prt, '('' Emission data file'',T33,'': '',a)') brnam(:LEN_TRIM(brnam))

WRITE (fu_prt, '('' Diurnal variation file(s)'')')
WRITE (fu_prt, '(''   - pre-defined'',T33,'': '',a)') dvnam(:LEN_TRIM(dvnam))

IF (LEN_TRIM(usdvnam).NE.0) THEN
  WRITE (fu_prt, '(''   - user defined'',T33,'': '',a)') usdvnam(:LEN_TRIM(usdvnam))
ENDIF

IF (.NOT.gasv) THEN
  WRITE (fu_prt, '('' Part. size distr. file(s)'')')
  WRITE (fu_prt, '(''   - pre-defined'',T33,'': '',a)') psdnam(:LEN_TRIM(psdnam))
  IF (LEN_TRIM(uspsdnam).NE.0) THEN
    WRITE (fu_prt, '(''   - user defined'',T33,'': '',a)') uspsdnam(:LEN_TRIM(uspsdnam))
  ENDIF
ENDIF

IF (ANY(spgrid == (/2,3/))) THEN
  WRITE (fu_prt, '('' Receptor data file'',T33,'': '',a)') namrecept(:LEN_TRIM(namrecept))
ENDIF

IF (intpol.EQ.0) THEN
  WRITE (fu_prt, '('' Climatological data files'', T33, '': '', 2a, i3.3)') kname(:indx), '001...', NMETREG
ELSE
  WRITE (fu_prt, '('' Climatological data file'',T33,'': '',a)') kname
ENDIF

IF (.NOT. f_z0user) THEN
  WRITE (fu_prt, '('' Surface roughness file'',T33,'': '',a)') z0file(:LEN_TRIM(z0file))

ENDIF

IF (isec) THEN
  WRITE (fu_prt, '('' Landuse file'',T33,'': '',a)') lufile(:LEN_TRIM(lufile))
ENDIF

WRITE (fu_prt, '(//,'' Files produced by OPS:'')')
WRITE (fu_prt, '(   '' ----------------------'')')

WRITE (fu_prt, '('' Plotter output file'',T33,'': '',a)') pltnam(:LEN_TRIM(pltnam))

WRITE (fu_prt, '(" Printer output file (this file)",T33,": ",a)') prnnam(:LEN_TRIM(prnnam))
!
! Emission data
!
WRITE (fu_prt, '(a)') char(12)
CALL ops_print_kop(project, namco)

50 FORMAT (i4, 2i8, e10.3, f7.3, f6.1, f7.0, f6.1, 3i4, i5, 2x, a)

IF (nbron .LE. NBRMAX) THEN
   WRITE (fu_prt, '(//,'' Emission source data:'')')
   WRITE (fu_prt, '(   '' ---------------------'')')

   WRITE(fu_prt, '(/ ,'' Applied correction factor: '',F6.4,/)') emtrend

   WRITE (fu_prt, '(/,'' ssn    x(m)    y(m)   q (g/s) hc(MW)  h(m)   d(m)  s(m)  tb dgr cat area subst.'')')
   WRITE (fu_prt, '('' --- ------- ------- --------- ------ ----- ------ ----- --- --- --- ---- --------'')')

   DO i = 1, nbron
      qb=bsterkte(i)+bqrv(i)+bqtr(i)
      WRITE (fu_prt, 50, IOSTAT = ierr) bnr(i), bx(i), by(i), qb, bwarmte(i), bhoogte(i), bdiam(i), bsigmaz(i), btgedr(i),     &
      &  bdegr(i), bcatnr(i), blandnr(i), namco(1:LEN_TRIM(namco))
      IF (ierr .GT. 0) GOTO 9000
   ENDDO

ELSE
!
!  Write emission per category and per country/area
!  The data matrix is first sorted by land number (column 1)
!
   CALL SortMatrix (emis, landmax, 1)

   WRITE(fu_prt, '(//,'' Total emission (in tonnes/year) per country / area:'')')

   WRITE(fu_prt, '(   '' ---------------------------------------------------'')')

   WRITE(fu_prt, '(/ ,'' Applied correction factor: '',F6.4,/)') emtrend

   WRITE (fu_prt,*) '+---------------------------------------------------------------------+'
   WRITE (fu_prt,*) '| country |  total    | industry  | industry  | traffic   | space     |'
   WRITE (fu_prt,*) '| number  |           |  h > 35m  | h < 35m   |           | heating   |'
   WRITE (fu_prt,*) '|---------------------------------------------------------------------|'

   DO i = 1,landmax
     emis(2:,i)= emis(2:,i)*31.536
     WRITE (fu_prt, '(1x,a1,2x,i4,2x,5(1x,a1,1x,I9),1x,a1)') '|', NINT(emis(1, i)), '|', NINT(emis(6, i)), '|', NINT(emis(2,   &
        &  i)),'|',NINT(emis(3,i)),'|', NINT(emis(5, i)), '|', NINT(emis(4, i)), '|'
   ENDDO
   WRITE (fu_prt,*) '+---------------------------------------------------------------------+'

ENDIF

RETURN

9000 CALL SetError('Error writing source data', error)
CALL ErrorParam('error number', ierr, error)
CALL ErrorParam('source number', i, error)
CALL ErrorParam('number sources', nbron, error)
CALL ErrorParam('bnr', bnr(i), error)
CALL ErrorParam('bx', bx(i), error)
CALL ErrorParam('by', by(i), error)
CALL ErrorParam('bsterkte', bsterkte(i), error)
CALL ErrorParam('bwarmte', bwarmte(i), error)
CALL ErrorParam('bhoogte', bhoogte(i), error)
CALL ErrorParam('bdiam', bdiam(i), error)
CALL ErrorParam('bsigmaz', bsigmaz(i), error)
CALL ErrorParam('btgedr', btgedr(i), error)
CALL ErrorParam('bcatnr', bcatnr(i), error)
CALL ErrorParam('blandnr', blandnr(i), error)
CALL ErrorParam('bdegr', bdegr(i), error)
CALL ErrorParam('namco', namco(1:LEN_TRIM(namco)), error)

9100 CALL ErrorCall(ROUTINENAAM, error)

CONTAINS

!-------------------------------------------------------------------------------------------------------------------------------
! SUBROUTINE : print_region
! DESCRIPTION: Printing region name in a uniform manner (to have inferred (and therefore always long enough) length of region
!              name
!-------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE print_region (regionname, regionindex )

! CONSTANTS
CHARACTER*512                                    :: ROUTINENAAM                ! 
PARAMETER    (ROUTINENAAM = 'print_region')

! SUBROUTINE ARGUMENTS - INPUT
CHARACTER*(*), INTENT(IN)                        :: regionname                 ! 
INTEGER*4, INTENT(IN)                            :: regionindex                ! 

WRITE (fu_prt, '(/,1x,''climatological area  :  '', a, '' (region '', I1, '')'')') regionname(1:LEN_TRIM(regionname)),         &
                &  regionindex

END SUBROUTINE print_region
!-------------------------------------------------------------------------------------------------------------------------------

END SUBROUTINE ops_print_info

end module m_ops_print_info 
