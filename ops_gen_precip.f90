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
!
! NAME                  : %M%
! SCCS (SOURCE)         : %P%
! RELEASE - LEVEL       : %R% - %L%
! BRANCH -SEQUENCE      : %B% - %S%
! DATE - TIME           : %E% - %U%
! WHAT                  : %W%:%E%
! AUTHOR                : OPS-support 
! FIRM/INSTITUTE        : RIVM/LLO
! LANGUAGE              : FORTRAN-77/90
! DESCRIPTION           : Generate precipitation for receptors (sum of precipitation over
!                         all wind direction sectors and stability classes).
! EXIT CODES            :
! FILES AND I/O DEVICES :
! SYSTEM DEPENDENCIES   : HP-Fortran
! CALLED FUNCTIONS      :
! UPDATE HISTORY        :
!-------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE ops_gen_precip(uurtot, astat, trafst, precip, error)

!DEC$ ATTRIBUTES DLLIMPORT:: ops_statparexp

USE m_error
USE m_commonconst

IMPLICIT NONE

! CONSTANTS
CHARACTER*512                                    :: ROUTINENAAM                ! 
PARAMETER    (ROUTINENAAM = 'ops_gen_precip')

! SUBROUTINE ARGUMENTS - INPUT
REAL*4,    INTENT(IN)                            :: uurtot                    
REAL*4,    INTENT(IN)                            :: astat(NTRAJ, NCOMP, NSTAB, NSEK)
REAL*4,    INTENT(IN)                            :: trafst(NTRAJ)             

! SUBROUTINE ARGUMENTS - OUTPUT
REAL*4,    INTENT(OUT)                           :: precip                     ! array with precipitation per receptorpoint
TYPE (TError), INTENT(OUT)                       :: error                      ! error handling record

! LOCAL VARIABLES
INTEGER*4                                        :: i                          ! index of receptor
INTEGER*4                                        :: isek                       ! index of wind sector 
INTEGER*4                                        :: isekt                      ! dummy output of ops_statparexp
INTEGER*4                                        :: istab                      ! index of stability class
INTEGER*4                                        :: iwd                        ! wind direction [degrees]
INTEGER*4                                        :: itra                       ! dummy output of ops_statparexp
REAL*4                                           :: hbron                      ! source height, dummy input for ops_statparexp
REAL*4                                           :: disx                       ! distance source receptor, dummy input for ops_statparexp
REAL*4                                           :: disxx                      ! dummy output of ops_statparexp
REAL*4                                           :: radius                     ! source diameter, dummy input for ops_statparexp
REAL*4                                           :: qww                        ! heat content of source, dummy input for ops_statparexp; 
                                                                               ! setting it to 0 prevents unnecessary computation of plume rise
                                                                               ! in ops_statparexp 
REAL*4                                           :: V_stack                    ! here a dummy
REAL*4                                           :: Ts_stack                   ! here a dummy       
LOGICAL                                          :: emis_horizontal            ! here a dummy
REAL*4                                           :: D_stack                    ! here a dummy
REAL*4                                           :: vw10                       ! here a dummy
REAL*4                                           :: aksek(12)                  ! here a dummy
REAL*4                                           :: h0                         ! here a dummy
REAL*4                                           :: hum                        ! here a dummy
REAL*4                                           :: ol                         ! here a dummy
REAL*4                                           :: shear                      ! here a dummy
REAL*4                                           :: rcaerd                     ! here a dummy
REAL*4                                           :: rcnh3d                     ! here a dummy
REAL*4                                           :: rcno2d                     ! here a dummy
REAL*4                                           :: temp_C                     ! here a dummy
REAL*4                                           :: uster                      ! here a dummy
REAL*4                                           :: pcoef                      ! here a dummy
REAL*4                                           :: htot                       ! here a dummy
REAL*4                                           :: htt                        ! here a dummy
REAL*4                                           :: aant                       ! here a dummy
REAL*4                                           :: xl                         ! here a dummy
REAL*4                                           :: rb                         ! here a dummy
REAL*4                                           :: ra4                        ! here a dummy
REAL*4                                           :: ra50                       ! here a dummy
REAL*4                                           :: xvglbr                     ! here a dummy
REAL*4                                           :: xvghbr                     ! here a dummy
REAL*4                                           :: xloc                       ! here a dummy
REAL*4                                           :: xl100                      ! here a dummy
REAL*4                                           :: rad                        ! here a dummy
REAL*4                                           :: rcso2                      ! here a dummy
REAL*4                                           :: coef_space_heating         ! here a dummy
REAL*4                                           :: buil                       ! here a dummy
REAL*4                                           :: regenk                   
REAL*4                                           :: rint                     
REAL*4                                           :: percvk                  

! SCCS-ID VARIABLES
CHARACTER*81                                     :: sccsida                    ! 
sccsida = '%W%:%E%'//char(0)
!-------------------------------------------------------------------------------------------------------------------------------
!
! Initialise dummy source; this is needed because ops_statparexp needs source information
! in order to retrieve the correct meteo data from meteo statistics; 
! the source influences the distance class, and wind shear (via plume rise).
disx   = 100 ! (first distance class, i.e. local meteo) 
hbron  = 10
radius = 0
qww    = 0
D_stack = -999.
V_stack = -999.
Ts_stack = -999.
emis_horizontal = .FALSE.

! Initialise summed precipitation for this receptorpoint:
precip=0

! Loop over wind sectors and stability classes:
DO isek = 1, NSEK
  iwd=(isek-1)*360/NSEK ! wind direction [degrees]
  DO istab = 1, NSTAB
!
!   Compute relevant parameters regenk (rain probability), rint (rain intensity) and 
!   percvk (fraction of occurrence of meteo class) for this wind direction sector and stability class
!

    CALL ops_statparexp(istab, hbron, qww, D_stack, V_stack, Ts_stack, emis_horizontal, iwd, radius, uurtot, astat, trafst, disx, isek, disxx, isekt, vw10, aksek, h0,  &
                     &  hum, ol, shear, rcaerd, rcnh3d, rcno2d, temp_C, uster, pcoef, htot, htt, itra, aant, xl, rb, ra4,                              &
                     &  ra50, xvglbr, xvghbr, xloc, xl100, rad, rcso2, coef_space_heating, regenk, buil, rint, percvk, error)
    IF (error%haserror) GOTO 9999
!
!   Add contribution to precipitation amount (8760 = number of hours in a year)
!
    precip=precip+regenk*rint*percvk*8760.
  ENDDO
ENDDO
RETURN

9999 CALL ErrorCall(ROUTINENAAM, error)
RETURN

END SUBROUTINE ops_gen_precip
