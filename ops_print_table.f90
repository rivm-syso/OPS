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
!                       Copyright by
!   National Institute of Public Health and Environment
!           Laboratory for Air Research (RIVM/LLO)
!                      The Netherlands
!   No part of this software may be used, copied or distributed without permission of RIVM/LLO (2002)
!
! SUBROUTINE
! NAME                : %M%
! SCCS (SOURCE)       : %P%
! RELEASE - LEVEL     : %R% - %L%
! BRANCH - SEQUENCE   : %B% - %S%
! DATE - TIME         : %E% - %U%
! WHAT                : %W%:%E%
! AUTHOR              : OPS-support
! FIRM/INSTITUTE      : RIVM/LLO
! LANGUAGE            : FORTRAN-77/90
! DESCRIPTION         : Subroutines supporting receptor point printing.
! EXIT CODES          :
! REFERENCE           :
! FILES I/O DEVICES   :
! SYSTEM DEPENDENCIES : HP-Fortran
! CALLED FUNCTIONS    :
! UPDATE HISTORY      :
!-------------------------------------------------------------------------------------------------------------------------------
MODULE ops_print_table

USE m_error
USE m_utils
USE m_commonfile

IMPLICIT NONE

INTERFACE print_conc_names
   MODULE PROCEDURE print_conc_names
END INTERFACE

INTERFACE print_depo_names
   MODULE PROCEDURE print_depo_names
END INTERFACE

INTERFACE print_values
   MODULE PROCEDURE print_values
END INTERFACE

PRIVATE has_rcp_values
PRIVATE set_rcp_values

CONTAINS

!-------------------------------------------------------------------------------------------------------------------------------
! SUBROUTINE: print_conc_names
! PURPOSE:    prints names of concentration parameters
!-------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE  print_conc_names(namco, namsec, nam_subsec)

! CONSTANTS
CHARACTER*512                                    :: ROUTINENAAM
PARAMETER    (ROUTINENAAM = 'print_conc_names')

! SUBROUTINE ARGUMENTS - INPUT
CHARACTER*(*), INTENT(IN)                        :: namco
CHARACTER*(*), INTENT(IN), OPTIONAL              :: namsec
CHARACTER*(*), INTENT(IN), OPTIONAL              :: nam_subsec(:)              ! names of sub-secondary species

! Local variable
INTEGER                                          :: isubsec                    ! index of sub-secondary species
!-------------------------------------------------------------------------------------------------------------------------------
!
! FORMATS
!
700 FORMAT (/' Concentrations for ',9(1x,a))
701 FORMAT (/' Concentrations for ',a:,' and ',a:)
702 FORMAT (/' Concentrations for ',a:)

IF (PRESENT(nam_subsec)) THEN
   WRITE(fu_prt, 700) namco(1:LEN_TRIM(namco)), namsec(:LEN_TRIM(namsec)), (nam_subsec(isubsec)(:LEN_TRIM(nam_subsec(isubsec))), isubsec = 1,size(nam_subsec))
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
CHARACTER*512                                    :: ROUTINENAAM
PARAMETER    (ROUTINENAAM = 'print_depo_names')

! SUBROUTINE ARGUMENTS - INPUT
CHARACTER*(*), INTENT(IN), OPTIONAL              :: namdep

!-------------------------------------------------------------------------------------------------------------------------------
!
! FORMATS
!
701 FORMAT ( ' and depositions':,' as ',a)
702 FORMAT ( ' Calculated for specific locations')

IF (PRESENT(namdep)) THEN
   WRITE(fu_prt, 701) namdep(1:LEN_TRIM(namdep))
ENDIF

WRITE (fu_prt,702)

RETURN
END SUBROUTINE print_depo_names

!-------------------------------------------------------------------------------------------------------------------------------
! SUBROUTINE: print_values
! PURPOSE:    prints values at all the receptor points for certain parameters. The parameters and even their numbers are
!             variable (up to 14 currently, but this can easily be increased).
!-------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE  print_values    (nrrcp, namrcp, xm, ym, error, par1, spar1, par2, spar2, par3, spar3, par4, spar4, par5, spar5,    &
                             &  par6, spar6, par7, spar7, par8, spar8, par9, spar9, par10, spar10, par11, spar11, par12,       &
                             &  spar12, par13, spar13, par14, spar14)

INTEGER                                          :: nrparam
PARAMETER (nrparam = 14)

! CONSTANTS
CHARACTER*512                                    :: ROUTINENAAM
PARAMETER    (ROUTINENAAM = 'print_values')

! SUBROUTINE ARGUMENTS - INPUT
INTEGER*4, INTENT(IN)                            :: nrrcp
CHARACTER*(*), INTENT(IN)                        :: namrcp(nrrcp)
REAL*4,    INTENT(IN)                            :: xm(nrrcp)
REAL*4,    INTENT(IN)                            :: ym(nrrcp)
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

! SUBROUTINE ARGUMENTS - I/O
TYPE (TError), INTENT(INOUT)                     :: error                      ! should not happen as format string is long enough

! LOCAL VARIABLES
INTEGER*4                                        :: i
INTEGER*4                                        :: j
INTEGER*4                                        :: values(nrparam)
REAL*4                                           :: factors(nrparam)
REAL*4                                           :: factorscopy(nrparam)
INTEGER*4                                        :: nrpresent
INTEGER*4                                        :: nrunit
LOGICAL                                          :: dummybool

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
   &  .AND. has_rcp_values(spar13, nrpresent, factors) .AND. has_rcp_values(spar14, nrpresent, factors)
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
! FUNCTION:   has_rcp_values
! PURPOSE:    Input is the next factor. If it is present than this factor is added to a factor array. A side-effect is that the
!             number of factors and thus of parameter arrays is established.
!-------------------------------------------------------------------------------------------------------------------------------
LOGICAL FUNCTION has_rcp_values(spar, nrpresent, factors)

! CONSTANTS
CHARACTER*512                                    :: ROUTINENAAM
PARAMETER    (ROUTINENAAM = 'has_rcp_values')

! SUBROUTINE ARGUMENTS - INPUT
REAL*4,    INTENT(IN), OPTIONAL                  :: spar                       ! factor in parameter

! SUBROUTINE ARGUMENTS - I/O
INTEGER*4, INTENT(INOUT)                         :: nrpresent
REAL*4,    INTENT(INOUT)                         :: factors(:)

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
CHARACTER*512                                    :: ROUTINENAAM
PARAMETER    (ROUTINENAAM = 'set_rcp_values')

! SUBROUTINE ARGUMENTS - INPUT
REAL*4,    INTENT(IN)                            :: formatpar
REAL*4,    INTENT(IN)                            :: factors(nrpresent)
INTEGER*4, INTENT(IN)                            :: nrpresent

! SUBROUTINE ARGUMENTS - I/O
INTEGER*4, INTENT(INOUT)                         :: index

! SUBROUTINE ARGUMENTS - OUTPUT
INTEGER*4, INTENT(OUT)                           :: values(nrpresent)

set_rcp_values = index /= nrpresent
IF (set_rcp_values) THEN
  index = index + 1
  values(index) = NINT(formatpar * factors(index))
ENDIF

END FUNCTION set_rcp_values

END MODULE ops_print_table
