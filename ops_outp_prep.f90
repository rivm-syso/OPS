!-------------------------------------------------------------------------------------------------------------------------------
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
!                       Copyright (C) 2002 by
!   National Institute of Public Health and Environment
!           Laboratory for Air Research (RIVM/LLO)
!                      The Netherlands
!
! SUBROUTINE
!
! NAME               : %M%
! SCCS(SOURCE)       : %P%
! RELEASE - LEVEL    : %R% - %L%
! BRANCH -SEQUENCE   : %B% - %S%
! DATE - TIME        : %E% - %U%
! WHAT               : %W%:%E%
! AUTHOR             : Wilco de Vries
! FIRM/INSTITUTE     : RIVM LLO
! LANGUAGE           : FORTRAN-77/90
! DESCRIPTION        : Prepare output process (print/plot)
! EXIT CODES         :
! FILES AND OTHER    :
!    I/O DEVICES
! SYSTEM DEPENDENCIES: HP-Fortran
! CALLED FUNCTIONS   :
! UPDATE HISTORY :
!-------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE ops_outp_prep(nrrcp, icm, conc_cf, rhno3_rcp, csec, drydep, wetdep, cpri, totdep, cseccor, scale_con, scale_sec,     &
                      &  scale_sec_cor, scale_dep)

IMPLICIT NONE

! SUBROUTINE ARGUMENTS - INPUT
INTEGER*4, INTENT(IN)                            :: nrrcp                      ! 
INTEGER*4, INTENT(IN)                            :: icm                        ! 
REAL*4,    INTENT(IN)                            :: conc_cf                    ! 
REAL*4,    INTENT(IN)                            :: rhno3_rcp(nrrcp)           ! 
REAL*4,    INTENT(IN)                            :: csec(nrrcp)                ! 
REAL*4,    INTENT(IN)                            :: drydep(nrrcp)              ! 
REAL*4,    INTENT(IN)                            :: wetdep(nrrcp)              ! 

! SUBROUTINE ARGUMENTS - I/O
REAL*4,    INTENT(INOUT)                         :: cpri(nrrcp)                ! 

! SUBROUTINE ARGUMENTS - OUTPUT
REAL*4,    INTENT(OUT)                           :: totdep(nrrcp)              ! 
REAL*4,    INTENT(OUT)                           :: cseccor(nrrcp)             ! 
REAL*4,    INTENT(OUT)                           :: scale_con                  ! 
REAL*4,    INTENT(OUT)                           :: scale_sec                  ! 
REAL*4,    INTENT(OUT)                           :: scale_sec_cor              ! 
REAL*4,    INTENT(OUT)                           :: scale_dep                  ! 

! LOCAL VARIABLES
INTEGER*4                                        :: j                          ! 

! CONSTANTS
CHARACTER*512                                    :: ROUTINENAAM                ! 
PARAMETER    (ROUTINENAAM = 'ops_outp_prep')

! SCCS-ID VARIABLES
CHARACTER*81                                     :: sccsida                    ! 
sccsida = '%W%:%E%'//char(0)
!-------------------------------------------------------------------------------------------------------------------------------
!
! Calculate 
! 1. totdep = total deposition = dry deposition + wet deposition
! 2. corrected NOx concentration (to account for HNO2 and PAN contributions to NO2)
! 3. concentration of second secondary substance; 
!
! rhno3 = ratio [HNO3]/[NO3]_total (NO3_total = HNO3+NO3_aerosol)
!
!                                            [HNO3]            [NO3_aerosol]
! cseccor = csec (1 - rhno3) = csec (1 - ------------ ) = csec ------------- = [NO3_aerosol]
!                                         [NO3]_total           [NO3]_total
!
DO j = 1, nrrcp
  totdep(j) = drydep(j) + wetdep(j)
  IF (icm == 2) THEN
    cpri(j)    = cpri(j) * conc_cf 
    cseccor(j) = csec(j) - csec(j) * rhno3_rcp(j) 
  ENDIF
ENDDO
!
! Scaling factors for concentration and deposition fields
!
CALL ops_scalefac(nrrcp, cpri, csec, drydep, wetdep, scale_con, scale_sec, scale_dep, cseccor, scale_sec_cor)

RETURN

END SUBROUTINE ops_outp_prep
