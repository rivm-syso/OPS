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
! DESCRIPTION         : Subroutines supporting receptor point printing.
!-------------------------------------------------------------------------------------------------------------------------------
module ops_print_table

use m_error
use m_commonfile, only: fu_prt
use m_utils

IMPLICIT NONE

INTERFACE print_conc_names
   MODULE PROCEDURE print_conc_names
END INTERFACE

INTERFACE print_depo_names
   MODULE PROCEDURE print_depo_names
END INTERFACE

INTERFACE print_values
   MODULE PROCEDURE print_values
   MODULE PROCEDURE print_values_par_val
END INTERFACE

PRIVATE has_rcp_values
PRIVATE set_rcp_values

CONTAINS

!-------------------------------------------------------------------------------------------------------------------------------
! SUBROUTINE: print_conc_names
! PURPOSE:    prints names of concentration parameters
!-------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE  print_conc_names(namco, namsec, nam_subsec, nam_no2, nam_nox)

! CONSTANTS
CHARACTER*512                                    :: ROUTINENAAM                ! 
PARAMETER    (ROUTINENAAM = 'print_conc_names')

! SUBROUTINE ARGUMENTS - INPUT
CHARACTER*(*), INTENT(IN)                        :: namco                      ! 
CHARACTER*(*), INTENT(IN), OPTIONAL              :: namsec                     ! 
CHARACTER*(*), INTENT(IN), OPTIONAL              :: nam_subsec(:)              ! names of sub-secondary species
CHARACTER*(*), INTENT(IN), OPTIONAL              :: nam_no2                    ! name of NO2
CHARACTER*(*), INTENT(IN), OPTIONAL              :: nam_nox                    ! name of NOx

! Local variable
INTEGER                                          :: isubsec                    ! index of sub-secondary species
!-------------------------------------------------------------------------------------------------------------------------------
!
! FORMATS
!
700 FORMAT (/' Concentrations for ',15(1x,a))
701 FORMAT (/' Concentrations for ',a:,' and ',a:)
702 FORMAT (/' Concentrations for ',a:)

IF (PRESENT(nam_nox)) THEN
   WRITE(fu_prt, 700) namco(1:LEN_TRIM(namco)), namsec(:LEN_TRIM(namsec)), (nam_subsec(isubsec)(:LEN_TRIM(nam_subsec(isubsec))), isubsec = 1,size(nam_subsec)), &
                      nam_no2, nam_nox
ELSEIF (PRESENT(nam_no2)) THEN
   WRITE(fu_prt, 700) namco(1:LEN_TRIM(namco)), namsec(:LEN_TRIM(namsec)), (nam_subsec(isubsec)(:LEN_TRIM(nam_subsec(isubsec))), isubsec = 1,size(nam_subsec)), &
                      nam_no2
ELSEIF (PRESENT(nam_subsec)) THEN
   IF (size(nam_subsec) .gt. 0) THEN
      WRITE(fu_prt, 700) namco(1:LEN_TRIM(namco)), namsec(:LEN_TRIM(namsec)), (nam_subsec(isubsec)(:LEN_TRIM(nam_subsec(isubsec))), isubsec = 1,size(nam_subsec))
   ELSE
      WRITE(fu_prt, 701) namco(1:LEN_TRIM(namco)), namsec(:LEN_TRIM(namsec))
   ENDIF
ELSEIF (PRESENT(namsec)) THEN
   WRITE(fu_prt, 701) namco(1:LEN_TRIM(namco)), namsec(:LEN_TRIM(namsec))
ELSE
   WRITE(fu_prt, 702) namco(1:LEN_TRIM(namco))
ENDIF

RETURN
END SUBROUTINE print_conc_names

!-------------------------------------------------------------------------------------------------------------------------------
! SUBROUTINE: print_depo_names
! PURPOSE:    prints names of deposition parameters and of the extra parameters if -v option is invoked.
!-------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE  print_depo_names(namdep)

! CONSTANTS
CHARACTER*512                                    :: ROUTINENAAM                ! 
PARAMETER    (ROUTINENAAM = 'print_depo_names')

! SUBROUTINE ARGUMENTS - INPUT
CHARACTER*(*), INTENT(IN), OPTIONAL              :: namdep                     ! 

!-------------------------------------------------------------------------------------------------------------------------------
!
! FORMATS
!
701 FORMAT ( ' and depositions':,' as ',a)
!702 FORMAT ( ' Calculated for specific locations')

IF (PRESENT(namdep)) THEN
   WRITE(fu_prt, 701) namdep(1:LEN_TRIM(namdep))
ENDIF

!WRITE (fu_prt,702)

RETURN
END SUBROUTINE print_depo_names

!-------------------------------------------------------------------------------------------------------------------------------
! SUBROUTINE: print_values
! PURPOSE:    prints values at all the receptor points for certain parameters. The parameters and even their numbers are
!             variable (up to 14 currently, but this can easily be increased).
!-------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE  print_values    (nrrcp, namrcp, xm, ym, error, par1, spar1, par2, spar2, par3, spar3, par4, spar4, par5, spar5,    &
                             &  par6, spar6, par7, spar7, par8, spar8, par9, spar9, par10, spar10, par11, spar11, par12,       &
                             &  spar12, par13, spar13, par14, spar14, par15, spar15)

INTEGER                                          :: nrparam                    ! 
PARAMETER (nrparam = 14)

! CONSTANTS
CHARACTER*512                                    :: ROUTINENAAM                ! 
PARAMETER    (ROUTINENAAM = 'print_values')

! SUBROUTINE ARGUMENTS - INPUT
INTEGER*4, INTENT(IN)                            :: nrrcp                      ! 
CHARACTER*(*), INTENT(IN)                        :: namrcp(nrrcp)              ! receptor names
REAL*4,    INTENT(IN)                            :: xm(nrrcp)                  ! x-coordinates of receptors (m RDM)
REAL*4,    INTENT(IN)                            :: ym(nrrcp)                  ! y-coordinates of receptors (m RDM)
REAL*4,    INTENT(IN), OPTIONAL                  :: par1(nrrcp)                ! values of parameter
REAL*4,    INTENT(IN), OPTIONAL                  :: spar1                      ! factor in parameter
REAL*4,    INTENT(IN), OPTIONAL                  :: par2(nrrcp)                ! values of parameter
REAL*4,    INTENT(IN), OPTIONAL                  :: spar2                      ! factor in parameter
REAL*4,    INTENT(IN), OPTIONAL                  :: par3(nrrcp)                ! values of parameter
REAL*4,    INTENT(IN), OPTIONAL                  :: spar3                      ! factor in parameter
REAL*4,    INTENT(IN), OPTIONAL                  :: par4(nrrcp)                ! values of parameter
REAL*4,    INTENT(IN), OPTIONAL                  :: spar4                      ! factor in parameter
REAL*4,    INTENT(IN), OPTIONAL                  :: par5(nrrcp)                ! values of parameter
REAL*4,    INTENT(IN), OPTIONAL                  :: spar5                      ! factor in parameter
REAL*4,    INTENT(IN), OPTIONAL                  :: par6(nrrcp)                ! values of parameter
REAL*4,    INTENT(IN), OPTIONAL                  :: spar6                      ! factor in parameter
REAL*4,    INTENT(IN), OPTIONAL                  :: par7(nrrcp)                ! values of parameter
REAL*4,    INTENT(IN), OPTIONAL                  :: spar7                      ! factor in parameter
REAL*4,    INTENT(IN), OPTIONAL                  :: par8(nrrcp)                ! values of parameter
REAL*4,    INTENT(IN), OPTIONAL                  :: spar8                      ! factor in parameter
REAL*4,    INTENT(IN), OPTIONAL                  :: par9(nrrcp)                ! values of parameter
REAL*4,    INTENT(IN), OPTIONAL                  :: spar9                      ! factor in parameter
REAL*4,    INTENT(IN), OPTIONAL                  :: par10(nrrcp)               ! values of parameter
REAL*4,    INTENT(IN), OPTIONAL                  :: spar10                     ! factor in parameter
REAL*4,    INTENT(IN), OPTIONAL                  :: par11(nrrcp)               ! values of parameter
REAL*4,    INTENT(IN), OPTIONAL                  :: spar11                     ! factor in parameter
REAL*4,    INTENT(IN), OPTIONAL                  :: par12(nrrcp)               ! values of parameter
REAL*4,    INTENT(IN), OPTIONAL                  :: spar12                     ! factor in parameter
REAL*4,    INTENT(IN), OPTIONAL                  :: par13(nrrcp)               ! values of parameter
REAL*4,    INTENT(IN), OPTIONAL                  :: spar13                     ! factor in parameter
REAL*4,    INTENT(IN), OPTIONAL                  :: par14(nrrcp)               ! values of parameter
REAL*4,    INTENT(IN), OPTIONAL                  :: spar14                     ! factor in parameter
REAL*4,    INTENT(IN), OPTIONAL                  :: par15(nrrcp)               ! values of parameter
REAL*4,    INTENT(IN), OPTIONAL                  :: spar15                     ! factor in parameter

! SUBROUTINE ARGUMENTS - I/O
TYPE (TError), INTENT(INOUT)                     :: error                      ! should not happen as format string is long enough

! LOCAL VARIABLES
INTEGER*4                                        :: i                          ! 
INTEGER*4                                        :: j                          ! 
INTEGER*4                                        :: values(nrparam)            ! 
REAL*4                                           :: factors(nrparam)           ! 
REAL*4                                           :: factorscopy(nrparam)       ! 
INTEGER*4                                        :: nrpresent                  ! 
INTEGER*4                                        :: nrunit                     ! 
LOGICAL                                          :: dummybool                  ! 

CHARACTER*180                                    :: formatpar                  ! format in writing parameter names
CHARACTER*180                                    :: formatval                  ! format in writing parameter values
CHARACTER*180                                    :: formatunit                 ! format in writing parameter units
!
! Create factors array and determine nrpresent.
!
nrpresent=0
factors = 0.0

dummybool = has_rcp_values(spar1, nrpresent, factors) .AND. has_rcp_values(spar2, nrpresent, factors)                          &
   &  .AND. has_rcp_values(spar3, nrpresent, factors) .AND. has_rcp_values(spar4, nrpresent, factors)                          &
   &  .AND. has_rcp_values(spar5, nrpresent, factors) .AND. has_rcp_values(spar6, nrpresent, factors)                          &
   &  .AND. has_rcp_values(spar7, nrpresent, factors) .AND. has_rcp_values(spar8, nrpresent, factors)                          &
   &  .AND. has_rcp_values(spar9, nrpresent, factors) .AND. has_rcp_values(spar10, nrpresent, factors)                         &
   &  .AND. has_rcp_values(spar11, nrpresent, factors) .AND. has_rcp_values(spar12, nrpresent, factors)                        &
   &  .AND. has_rcp_values(spar13, nrpresent, factors) .AND. has_rcp_values(spar14, nrpresent, factors)                        &
   &  .AND. has_rcp_values(spar15, nrpresent, factors)
!
! Create the factorscopy, where only factors unequal to 1 are counted.
! Determine nrunit and create formatstrings that depends upon the factors.
!
CALL startformat(formatpar, error)
CALL startformat(formatunit, error)
CALL appendformat('33x,1p', formatpar, error)
CALL appendformat('33x', formatunit, error)

nrunit = 0
DO j = 1, nrpresent
  IF (factors(j) == 1.) THEN
    CALL appendformat('9X', formatunit, error)
    CALL appendformat('9X', formatpar, error)
  ELSE
    nrunit = nrunit + 1
    factorscopy(nrunit) = factors(j)
    CALL appendformat('8X,''x''', formatunit, error)
    CALL appendformat('2x,e7.0:', formatpar, error)
  ENDIF
ENDDO
IF (error%haserror) GOTO 9000
!
! Print format strings and write the factors.
!
IF (nrunit > 0) THEN
  WRITE (fu_prt, formatunit)
  WRITE (fu_prt, formatpar) (1./factorscopy(j), j=1,nrunit)
ENDIF
WRITE (fu_prt, '(/)')
!
! Create the values format string.
!
CALL startformat(formatval, error)
CALL appendformat('I4,1X,A12,2I8', formatval, error)
CALL appendformat(nrpresent,'I9', formatval, error)
IF (error%haserror) GOTO 9000
!
! Calculate and write the values.
!
DO i = 1, nrrcp
  j = 0
  IF (set_rcp_values(par1(i), factors, nrpresent, j, values)) THEN
    IF (set_rcp_values(par2(i), factors, nrpresent, j, values)) THEN
      IF (set_rcp_values(par3(i), factors, nrpresent, j, values)) THEN
        IF (set_rcp_values(par4(i), factors, nrpresent, j, values)) THEN
          IF (set_rcp_values(par5(i), factors, nrpresent, j, values)) THEN
            IF (set_rcp_values(par6(i), factors, nrpresent, j, values)) THEN
              IF (set_rcp_values(par7(i), factors, nrpresent, j, values)) THEN
                IF (set_rcp_values(par8(i), factors, nrpresent, j, values)) THEN
                  IF (set_rcp_values(par9(i), factors, nrpresent, j, values)) THEN
                    IF (set_rcp_values(par10(i), factors, nrpresent, j, values)) THEN
                      IF (set_rcp_values(par11(i), factors, nrpresent, j, values)) THEN
                        IF (set_rcp_values(par12(i), factors, nrpresent, j, values)) THEN
                           IF (set_rcp_values(par13(i), factors, nrpresent, j, values)) THEN
                              IF (set_rcp_values(par14(i), factors, nrpresent, j, values)) THEN
                                 IF (set_rcp_values(par15(i), factors, nrpresent, j, values)) THEN
                                 ENDIF
                              ENDIF
                           ENDIF
                        ENDIF
                      ENDIF
                    ENDIF
                  ENDIF
                ENDIF
              ENDIF
            ENDIF
          ENDIF
        ENDIF
      ENDIF
    ENDIF
  ENDIF

  WRITE(fu_prt, formatval) i, namrcp(i), NINT(xm(i)), NINT(ym(i)), (values(j), j=1, nrpresent)
ENDDO

RETURN

9000 CALL ErrorCall(ROUTINENAAM, error)

END SUBROUTINE print_values

!-------------------------------------------------------------------------------------------------------------------------------
! FUNCTION:   print_values_par_val
! PURPOSE:    print values stored in an array par_val(nrrcp,npar); note that all values are separated by one or more spaces 
!             allowing the use of free format read.
!-------------------------------------------------------------------------------------------------------------------------------
subroutine print_values_par_val(nrrcp,npar,namrcp,xm,ym,par_comp,par_nam,par_val,par_unit,par_scale,par_fmt)

! Input arguments
integer, intent(in)          :: nrrcp                      ! number of receptors = number of rows in table  
integer, intent(in)          :: npar                       ! number of parameters = number of columns in table 
character*(*), intent(in)    :: namrcp(nrrcp)              ! receptor names
real, intent(in)             :: xm(nrrcp)                  ! x-coordinates of receptors (m rdm)
real, intent(in)             :: ym(nrrcp)                  ! y-coordinates of receptors (m rdm)
character(len=*), intent(in) :: par_comp(npar)             ! component names
character(len=*)             :: par_nam(npar)              ! parameter names
real, intent(in)             :: par_val(nrrcp,npar)        ! parameter values
character(len=*), intent(in) :: par_unit(npar)             ! unit for parameter
real, intent(in)             :: par_scale(npar)            ! scale factors for parameter values  
character(len=*), intent(in) :: par_fmt(npar)              ! format for value (i = integer, e = E-format, f = F-format)

! Local variables
character (len = 30)         :: fmt_a                      ! A-format for 1 string 
character (len = 30)         :: fmt_i                      ! I-format for 1 integer 
character (len = 30)         :: fmt_f                      ! F-format for 1 real (used for coordinates) 
character (len = 500)        :: fmt1                       ! format for writing one row of output
integer                      :: ipar                       ! index of parameter
integer                      :: ircp                       ! index of receptor
character(len=30)            :: par_fmt_header(npar)       ! format for header of colums with parameters (A-format for string with the same width as the data)
integer                      :: i1                         ! index of possible .d in 'e12.5' or 'f12.1'

! Define formats (used for first four columns with nr, name, x, y):
fmt_i = '(1x,i12)'
fmt_a = '(1x,a12)'
fmt_f = '(1x,f12.1)'  ! format for coordinates

! Construct formats for header lines (A-format, same width as I, E or F-format);
! f.ex. 'e12.5' or 'i12' or 'f12.1' -> '(1x,a12)'
! we assume here that the first character is e, i, or f
DO ipar = 1,npar
	  ! Get index of possible .d in 'e12.5' or 'f12.1':
      i1 = index(par_fmt(ipar),'.')
	  
	  ! Construct format including space 1x (e.g. '(1x,a12)'):
	  IF (i1 .gt. 0) THEN
	     par_fmt_header(ipar) = '(1x,'// 'a' // par_fmt(ipar)(2:i1-1) // ')'
      ELSE
	     par_fmt_header(ipar) = '(1x,'// 'a' // par_fmt(ipar)(2:len_trim(par_fmt(ipar))) // ')'
	  ENDIF
ENDDO

! Write header; first header line with prameter names, second one with component names, third one with units:
call print_values_par_val_header1(npar,'nr','name','x-coord','y-coord',par_nam,par_fmt_header,fmt_a)
call print_values_par_val_header1(npar,'-','-','-','-',par_comp,par_fmt_header,fmt_a)
call print_values_par_val_header1(npar,'-','-','m','m',par_unit,par_fmt_header,fmt_a)

! Construct format for writing nr, name, x,y:
fmt1 = '(' // trim(fmt_i) // ',' // trim(fmt_a) // ',2' // trim(fmt_f) // ')'

! Write parameter values:
DO ircp = 1, nrrcp 
   
   write(fu_prt,fmt1, advance = 'no') ircp,namrcp(ircp),xm(ircp),ym(ircp)
   DO ipar = 1,npar
      ! If I-format (integer) -> round data before printing:
      IF (scan(par_fmt(ipar), 'Ii') .gt. 0) THEN
         write(fu_prt,'(1x,' // trim(par_fmt(ipar)) // ')' , advance = 'no') NINT(par_val(ircp,ipar)) 
      ELSE
         write(fu_prt,'(1x,' // trim(par_fmt(ipar)) // ')' , advance = 'no') par_val(ircp,ipar)
      ENDIF
   ENDDO
   write(fu_prt,'(a)') '' ! new line
ENDDO

! write(*,*) par_scale(1)  

end subroutine print_values_par_val

!-------------------------------------------------------------------------------------------------------------------------------
! FUNCTION:   print_values_par_val_header1
! PURPOSE:    print 1 header line of table; first 4 colums are for nr, name, x, y, then npar columns with parameter values.
!-------------------------------------------------------------------------------------------------------------------------------
subroutine print_values_par_val_header1(npar,str_col1,str_col2,str_col3,str_col4,str_col_next,par_fmt_header,fmt_a)

! SUBROUTINE ARGUMENTS INPUT 
INTEGER         , INTENT(IN) :: npar                 ! number of parame
CHARACTER(LEN=*), INTENT(IN) :: str_col1             ! string for column 1
CHARACTER(LEN=*), INTENT(IN) :: str_col2             ! string for column 2
CHARACTER(LEN=*), INTENT(IN) :: str_col3             ! string for column 3
CHARACTER(LEN=*), INTENT(IN) :: str_col4             ! string for column 4
CHARACTER(LEN=*), INTENT(IN) :: str_col_next(npar)   ! strings for columns 4+1, ..., 4+npar
CHARACTER(LEN=*), INTENT(IN) :: par_fmt_header(npar) ! format string for header of parameters
CHARACTER(LEN=*), INTENT(IN) :: fmt_a                ! format string for header string (column with name)

! LOCAL VARIABLES
INTEGER :: ipar ! index of parameter

! Write header line:
write(fu_prt, '(4' // trim(fmt_a) // ')', advance = 'no') str_col1,str_col2,str_col3,str_col4
DO ipar = 1,npar
   write(fu_prt,par_fmt_header(ipar), advance = 'no') trim(str_col_next(ipar))
ENDDO
write(fu_prt,'(a)') '' ! new line

end subroutine print_values_par_val_header1

!-------------------------------------------------------------------------------------------------------------------------------
! FUNCTION:   has_rcp_values
! PURPOSE:    Input is the next factor. If it is present than this factor is added to a factor array. A side-effect is that the
!             number of factors and thus of parameter arrays is established.
!-------------------------------------------------------------------------------------------------------------------------------
LOGICAL FUNCTION has_rcp_values(spar, nrpresent, factors)

! CONSTANTS
CHARACTER*512                                    :: ROUTINENAAM                ! 
PARAMETER    (ROUTINENAAM = 'has_rcp_values')

! SUBROUTINE ARGUMENTS - INPUT
REAL*4,    INTENT(IN), OPTIONAL                  :: spar                       ! factor in parameter

! SUBROUTINE ARGUMENTS - I/O
INTEGER*4, INTENT(INOUT)                         :: nrpresent                  ! 
REAL*4,    INTENT(INOUT)                         :: factors(:)                 ! 

has_rcp_values = PRESENT(spar)
IF (has_rcp_values) THEN
  nrpresent = nrpresent + 1
  factors(nrpresent) = spar
ENDIF

END FUNCTION has_rcp_values

!-------------------------------------------------------------------------------------------------------------------------------
! FUNCTION:   set_rcp_values
! PURPOSE:    For input recepor point the values of every parameter, multiplied by its factor, is put into a simple array that
!             is written out later. The parameter values are incorporated into the array one by one.
!-------------------------------------------------------------------------------------------------------------------------------
LOGICAL FUNCTION set_rcp_values(formatpar, factors, nrpresent, index, values)

! CONSTANTS
CHARACTER*512                                    :: ROUTINENAAM                ! 
PARAMETER    (ROUTINENAAM = 'set_rcp_values')

! SUBROUTINE ARGUMENTS - INPUT
REAL*4,    INTENT(IN)                            :: formatpar                  ! 
REAL*4,    INTENT(IN)                            :: factors(nrpresent)         ! 
INTEGER*4, INTENT(IN)                            :: nrpresent                  ! 

! SUBROUTINE ARGUMENTS - I/O
INTEGER*4, INTENT(INOUT)                         :: index                      ! 

! SUBROUTINE ARGUMENTS - OUTPUT
INTEGER*4, INTENT(OUT)                           :: values(nrpresent)          ! 

set_rcp_values = index /= nrpresent
IF (set_rcp_values) THEN
  index = index + 1
  values(index) = NINT(formatpar * factors(index))
ENDIF

END FUNCTION set_rcp_values

end module ops_print_table
