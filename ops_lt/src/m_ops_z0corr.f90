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
! DESCRIPTION        : Correct friction velocity (uster) and Monin-Obukhov length (ol) at a standard roughness length for a 
!                      situation with another roughness length. The main assumption here is that the wind speed at 50 m height
!                      is not influenced by the roughness of the surface. Temperature effects are not taken into account.
!                      An iterative procedure is used: starting with uster1 compute a new uster2 and ol2 and continue the iteration,
!                      until the difference between the old and new uster is less than 1.5%.
!-------------------------------------------------------------------------------------------------------------------------------
MODULE m_ops_z0corr

implicit none

contains

SUBROUTINE ops_z0corr(varin_meteo, z01, uster1, ol1, z02, uster2, ol2, error)

use m_ops_varin
use m_ops_meteo, only : ops_meteo_cutoff_obukhov, ops_meteo_cutoff_uster
use m_commonconst_lt
USE m_error
USE m_ops_logfile
USE m_commonfile, only: fu_log

IMPLICIT NONE

! CONSTANTS
CHARACTER*512                                    :: ROUTINENAAM                ! 
PARAMETER      (ROUTINENAAM = 'ops_z0corr')

! CONSTANTS
REAL                                             :: C1                         ! 
REAL                                             :: Z                          ! 
PARAMETER   (C1  = 93500.) 
PARAMETER   (Z   = 50.)

! SUBROUTINE ARGUMENTS - INPUT
TYPE(Tvarin_meteo), INTENT(IN)                   :: varin_meteo                ! input variables for meteo
REAL,      INTENT(IN)                            :: z01                        ! standard roughness length [m]
REAL,      INTENT(IN)                            :: uster1                     ! friction velocity at standard roughness length 
REAL,      INTENT(IN)                            :: ol1                        ! Monin-Obukhov length at standard roughness length [m]
REAL,      INTENT(IN)                            :: z02                        ! new roughness length [m]

! SUBROUTINE ARGUMENTS - OUTPUT
REAL,      INTENT(OUT)                           :: uster2                     ! friction velocity at new roughness length 
REAL,      INTENT(OUT)                           :: ol2                        ! Monin-Obukhov length at standard roughness length [m]

! SUBROUTINE ARGUMENTS - INPUT/OUTPUT
TYPE(Terror),INTENT(INOUT)                       :: error                      ! error handling structure

! LOCAL VARIABLES
INTEGER                                          :: n                          ! iteration index
REAL                                             :: h0                         ! 
REAL                                             :: delta                      ! difference between old and new iterand for uster2
REAL                                             :: phim                       ! 
REAL                                             :: u50                        ! wind speed at 50 m height
REAL                                             :: uold                       ! uster at previous iteration
REAL                                             :: delta_old                  ! old difference between old and new iterand for uster2
REAL                                             :: ur                         ! ratio uster/uold
REAL                                             :: ol_save                    ! Obukhov length before cut-off

!-------------------------------------------------------------------------------------------------------------------------------
! 
!                        T rho_a cp (u*)^3
! (2.1) OPS report: L = -------------------
!                         g H0 kappa
!
!  rho_a : air density  = 1.292 kg/m3 (0 C), 1.247 kg/m3 (20 C), 1.204 kg/m3 (20 C), pressure = 1 atm
!  cp    : specific heat capacity = 1003.5 J/(kg K), sea level, dry, T=0 C; 1012 J/(kg/K), typical room conditions (T = 23 C)
!  kappa : von Karman constant = 0.4 [-]
!  g     : accelaration of gravity = 9.81 m/s2 
!  T     : absolute temperature [K]
!  H0    : surface heat flux [W/m2]
!
!                          T rho_a cp (u*)^3    T rho_a cp  (u*)^3       (u*)^3                               
! From this follows: H0  = ----------------- = ------------ ------ =  C1 ------
!                           g L kappa            g kappa      L            L                                  
!
!           T rho_a cp       K kg J s2       kg m2 s2     kg
! [C1] = [ ------------ ] = ------------- = --------- = ------     (J = kg m2/s2) 
!            g kappa         m3 kg K m       s2 m4        m2
!
! actual values in code: rho = 1.29 kg/m3, cp = 1005 J/(kg K), kappa=0.4, g=9.81 m/s2, T=283 K; c1=rho*cp*T/(kappa*g) = 93467 kg/m2.
!
! Initialise iteration
! write(*,*) 'ops_zocorr:',z01, uster1, ol1, z02
ol2       = ol1
uster2    = uster1
delta     = 0
delta_old = 0

! Correction only if roughness lengths differ more than 10%:
IF (ABS(z01 - z02) .GE. (0.1*z01 - EPS_DELTA)) THEN

   ! Compute wind speed at height Z (= 50 m):
   CALL stabcm(Z, ol1, phim)
   u50 = uster1/0.4*(ALOG(Z/z01) - phim) ! 2.3 OPS report without last term (can be neglected)
   h0  = -uster1**3*c1/ol1               ! 2.1 OPS report

   n   = 0
   !-------------------
   ! START ITERATION
   !-------------------
50 CONTINUE
      n      = n + 1
      uold   = uster2
   
      ! If delta is growing -> no convergence
      IF (delta_old .LT. delta .and. delta_old .NE. 0 .and. ol1 .LT. 0) THEN
         IF (.NOT. ops_openlog(error)) GOTO 9999
         WRITE(fu_log,'('' no convergence in subr. z0corr'')')
         WRITE(fu_log,'(6f8.2)') z01,uster1,ol1,z02,uster2,ol2
         RETURN
      ENDIF
      delta_old=delta
      CALL stabcm(Z, ol2, phim)
      uster2 = 0.4*u50/(ALOG(Z/z02) - phim)
      ol2    = -uster2**3*c1/h0 ! 2.1 OPS report
      delta  = ABS(uold - uster2)
      ur     = uster2/uold ! ratio
      IF (ur .LE. 0) THEN
         IF (.NOT. ops_openlog(error)) GOTO 9999
         WRITE(fu_log,'('' negative u* in subr. z0corr'')')
         RETURN
      ENDIF
   
      ! Define new iterand for H0 (0.8 and 0.1 are relaxation factors):
      IF (h0 .LT. 0) THEN
         h0 = h0*ur**0.8
      ELSE
         h0 = h0*ur**0.1
      ENDIF
   
      ! If percentual difference of iterands > 1.5% AND number of iterations < 40 -> continue iteration 
      IF ((delta .GT. (0.015*uster2 + EPS_DELTA)) .AND. (n .LT. 40)) THEN
   GOTO 50
      ENDIF
   ! END ITERATION
   
   ! Converged OR number of iterations >= 40;
   
   ! limit L such that very small values do not occurr:
   ol_save = ol2
   !call ops_meteo_cutoff_obust(varin_meteo,z02,ol2,uster2) 
   call ops_meteo_cutoff_obukhov(varin_meteo%ol_cutoff_iopt1, varin_meteo%ol_add_stable0, &
        varin_meteo%ol_max_unstable1, varin_meteo%ol_min_stable1, varin_meteo%ol_z0_ratio_cutoff1, &
        varin_meteo%ol_z0_ratio_cutoff2, z02, ol2)
   call ops_meteo_cutoff_uster(varin_meteo%uster_cutoff_iopt1, varin_meteo%uster_min2, ol_save, ol2, uster2)

ENDIF

RETURN
9999 CALL ErrorCall(ROUTINENAAM, error)

CONTAINS

!-------------------------------------------------------------------------------------------------------------------------------
! SUBROUTINE         : stabcm
! DESCRIPTION        : Stability correction for momentum
!-------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE stabcm(h, ol, phim)

! CONSTANTS
CHARACTER*512                                    :: ROUTINENAAM                ! 
PARAMETER      (ROUTINENAAM = 'stabcm')

! SUBROUTINE ARGUMENTS - INPUT
REAL,      INTENT(IN)                            :: h                          ! hoogte
REAL,      INTENT(IN)                            :: ol                         ! Monin Obukhovlengte

! SUBROUTINE ARGUMENTS - OUTPUT
REAL,      INTENT(OUT)                           :: phim                       ! correctiefactor

! LOCAL VARIABLES
REAL                                             :: y                          ! hulpvariabele voor berekening

!-------------------------------------------------------------------------------------------------------------------------------
IF (ol .GT. (0. + EPS_DELTA)) THEN
  phim =  - 17.*(1. - exp( - 0.29*h/ol)) ! 2.8 OPS report
!
! v Ulden and Holtslag
!
ELSE
  y    =(1.-15*h/ol)**0.25
  phim = 2.*LOG((1. + y)/2.) + LOG((1. + y*y)/2.) - (2.*ATAN(y)) + (PI/2.)  ! 2.4 OPS report
ENDIF

RETURN
END SUBROUTINE stabcm

END SUBROUTINE ops_z0corr

END MODULE m_ops_z0corr
