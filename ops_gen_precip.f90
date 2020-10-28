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
real,      INTENT(IN)                            :: uurtot
real,      INTENT(IN)                            :: astat(NTRAJ, NCOMP, NSTAB, NSEK)
real,      INTENT(IN)                            :: trafst(NTRAJ)

! SUBROUTINE ARGUMENTS - OUTPUT
real,      INTENT(OUT)                           :: precip                     ! array with precipitation per receptorpoint
TYPE (TError), INTENT(OUT)                       :: error                      ! error handling record

! LOCAL VARIABLES
INTEGER*4                                        :: i                          ! index of receptor
INTEGER*4                                        :: isek                       ! index of wind sector 
INTEGER*4                                        :: isekt                      ! dummy output of ops_statparexp
INTEGER*4                                        :: istab                      ! index of stability class
INTEGER*4                                        :: iwd                        ! wind direction [degrees]
INTEGER*4                                        :: itra                       ! dummy output of ops_statparexp
real                                             :: hbron                      ! source height, dummy input for ops_statparexp
real                                             :: disx                       ! distance source receptor, dummy input for ops_statparexp
real                                             :: disxx                      ! dummy output of ops_statparexp
real                                             :: radius                     ! source diameter, dummy input for ops_statparexp
real                                             :: qww                        ! heat content of source, dummy input for ops_statparexp;
                                                                               ! setting it to 0 prevents unnecessary computation of plume rise
                                                                               ! in ops_statparexp 
real                                             :: V_stack                    ! here a dummy
real                                             :: Ts_stack                   ! here a dummy
LOGICAL                                          :: emis_horizontal            ! here a dummy
real                                             :: D_stack                    ! here a dummy
real                                             :: vw10                       ! here a dummy
real                                             :: aksek(12)                  ! here a dummy
real                                             :: h0                         ! here a dummy
real                                             :: hum                        ! here a dummy
real                                             :: ol                         ! here a dummy
real                                             :: shear                      ! here a dummy
real                                             :: rcaerd                     ! here a dummy
real                                             :: rcnh3d                     ! here a dummy
real                                             :: rcno2d                     ! here a dummy
real                                             :: temp_C                     ! here a dummy
real                                             :: uster                      ! here a dummy
real                                             :: pcoef                      ! here a dummy
real                                             :: htot                       ! here a dummy
real                                             :: htt                        ! here a dummy
real                                             :: aant                       ! here a dummy
real                                             :: xl                         ! here a dummy
real                                             :: rb                         ! here a dummy
real                                             :: ra4                        ! here a dummy
real                                             :: ra50                       ! here a dummy
real                                             :: xvglbr                     ! here a dummy
real                                             :: xvghbr                     ! here a dummy
real                                             :: xloc                       ! here a dummy
real                                             :: xl100                      ! here a dummy
real                                             :: rad                        ! here a dummy
real                                             :: rcso2                      ! here a dummy
real                                             :: coef_space_heating         ! here a dummy
real                                             :: buil                       ! here a dummy
real                                             :: regenk
real                                             :: rint
real                                             :: percvk

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
