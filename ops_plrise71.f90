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
! NAME               : %M%
! SCCS(SOURCE)       : %P%
! RELEASE - LEVEL    : %R% - %L%
! BRANCH -SEQUENCE   : %B% - %S%
! DATE - TIME        : %E% - %U%
! WHAT               : %W%:%E%
! AUTHOR             : OPS-support
! FIRM/INSTITUTE     : RIVM/LLO
! LANGUAGE           : FORTRAN-77/90
! DESCRIPTION        : Deze routine berekent de pluimhoogte.
!    This routine includes plume rise formulations given by Briggs(1969) and Briggs(1971)
!    This method is equal to the method used in the (old) Dutch National Model (TNO, 1976).
!    The formulas used in 'STACKS' are the same, except for convective cases (Erbrink, 1995)
!                                                  960121
!    Extra iteration, because wind speed depends on plume height and vice versa
!
! EXIT CODES         :
! REFERENCE          :
! FILES AND OTHER    :
!    I/O DEVICES
! SYSTEM DEPENDENCIES: HP-Fortran
! CALLED FUNCTIONS   :
! UPDATE HISTORY     :
!-------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE ops_plrise71(z0, xl, ol, uster, hbron, qw, xloc, htt, onder)

USE m_commonconst                                                               ! EPS_DELTA only 

IMPLICIT NONE

! CONSTANTS
CHARACTER*512                                    :: ROUTINENAAM                ! 
PARAMETER    (ROUTINENAAM = 'ops_plrise71')

! SUBROUTINE ARGUMENTS - INPUT
REAL*4,    INTENT(IN)                            :: z0                         ! 
REAL*4,    INTENT(IN)                            :: xl                         ! 
REAL*4,    INTENT(IN)                            :: ol                         ! Monin-Obukhovlengte
REAL*4,    INTENT(IN)                            :: uster                      ! frictiesnelheid
REAL*4,    INTENT(IN)                            :: hbron                      ! 
REAL*4,    INTENT(IN)                            :: qw                         ! warmte inhoud van het rookgas (MW)
REAL*4,    INTENT(IN)                            :: xloc                       ! 

! SUBROUTINE ARGUMENTS - OUTPUT
REAL*4,    INTENT(OUT)                           :: htt                        ! 
REAL*4,    INTENT(OUT)                           :: onder                      ! 

! LOCAL VARIABLES
REAL*4                                           :: delh                       ! 
REAL*4                                           :: f                          ! 
REAL*4                                           :: us                        ! wind speed at effective plume height
                                                                               ! representative for the whole plume rise length
REAL*4                                           :: dtdz                       ! 
REAL*4                                           :: hs                          ! 
REAL*4                                           :: s                          ! 

! Iteration variables
! iteration converges if |delh - delh_prev| < epsa + epsr*delh
integer                                          :: it                         ! iteration index
logical                                          :: converged                  ! iteration has converged
real                                             :: delh_prev                  ! plume rise of previous iteration
integer, parameter                               :: maxit = 10                 ! maximal number of iterations 
real, parameter                                  :: epsa = 0.1                 ! absolute error tolerance (m)
real, parameter                                  :: epsr = 0.05                ! relative error tolerance 


! SCCS-ID VARIABLES
CHARACTER*81                                     :: sccsida                    ! 
sccsida = '%W%:%E%'//char(0)
!-------------------------------------------------------------------------------------------------------------------------------
!
! Briggs is developed for large stacks (energy production,..);
! should not be used for low emissions, e.g. emissions from animal housing.
!

! Default no plume rise yet
htt     = hbron
hs      = hbron
delh    = 0

IF ( qw .GT. (0. + EPS_DELTA)) THEN

   ! Compute wind speed from logarithmic wind profile at height h_stack: 
   CALL ops_wvprofile(z0,hs,uster,ol, us)


   ! f = stack buoyancy flux (3.27 OPS report)
   ! f = g/(pi*0.0013*T) = 9.81/(3.14*0.0013*273)*qw    
   f = 8.8*qw                                           ! 960107 see briggs (1975)

   ! We want to use a wind speed that is representative for the whole plume rise length, 
   ! but because we don't know the plume rise yet, we need an iteration.
   ! Initialisation for iteration:
   converged = .false.
   it        = 1
   delh_prev = -999.

   ! Do iteration:
   do while (.not. converged .and. it .le. maxit) 
   
      ! plume rise for unstable or neutral conditions, L < 0 or |L| > 50 (3.25 - 3.28 OPS report):
      ! original value plrise_nonstab_Fbsplit = 55
      IF ( ol .LT. (0. - EPS_DELTA) .OR. ABS(ol) .GT. 50 ) THEN
         IF ( f .GE. 55 ) THEN
            delh = 38.8*f**0.6/us                                                 ! Briggs 1971/Seinfeld 1986 (as in the Dutch Nat. Mod.)
         ELSE
            delh = 21.3*f**0.75/us
         ENDIF
      ELSE
        ! Stable conditions, 0 < L < 50 (3.28 OPS report)
        ! use fixed potential temperature gradient dtheta/dz = 0.006 (K/m); is valid for conditions above mixing layer.
        ! For low emissions and stable atmospheric conditions, dtheta/dz = 0.2 K/m
        ! original value: plrise_stab_dtheta_dz = 0.006
         dtdz = 0.006
         s    = 9.81/283*dtdz
         delh = 2.6*(f/(s*us))**0.333                                             ! Briggs (1975)
      ENDIF

      ! Check for convergence:
      converged = (abs(delh - delh_prev) .lt. epsa + epsr*delh )
      
      ! Update for next iteration:
      if (.not. converged .and. it .lt. maxit) then
        ! Compute wind speed at z = h_stack + 1/2 plume_rise:
         CALL ops_wvprofile(z0,hs+delh/2,uster,ol, us)
         delh_prev = delh
      endif
      it = it + 1
   enddo

   ! Check for convergence (considered as a non-fatal error):
!   if (.not. converged) then
!      write(fu_err,*) ' -------------------------------------------------------'
!      write(fu_err,*) ' WARNING, iteration in ops_plrise71_it has not converged'
!      write(fu_err,*) ' plume rise                   : ', delh
!      write(fu_err,*) ' plume rise previous iteration: ', delh_prev
!      write(fu_err,*) ' max. number of iterations    : ', maxit
!      write(fu_err,*) ' heat content (MW)            : ', qw
!      write(fu_err,*) ' stack height                 : ', hs
!   endif

   ! Compute total source height = stack_height + plume rise:
   htt = hbron + delh

ENDIF
!
! Is there plume penetration?
!
! The emission distribution of an area source has a sigma equal to the height of the source hbron.
! If hbron is close to the inversion height, the emission must be distributed over mixing layer and reservoir layer.
! last change: 21 Oct 2002
! Based on Kincaid data
!
! htt < hbron not yet possible (only plume rise is computed here)
! htt = hbron is possible
! onder is fraction (0-1) of plume in mixing layer ("onder"= below)
! onder = 1 -> plume completely below mixing height
! onder = 0 -> plume completely above mixing height
IF( (hbron .GT. xl + EPS_DELTA) .OR. (htt .LE. hbron + EPS_DELTA) ) THEN
   onder = (xl - htt)/xl + 0.5   ! OPS
ELSE
   onder = (xl - htt)/delh + 0.5  ! Briggs (1975) and NNM
ENDIF
!
! Temperature inversion effects in stable and unstable situations.
! In principle only applicable in situations close to the source, not if mixing height has increased much compared to the local
! mixing height.
! Laatst gewijzigd: 21 Oct 2002
!
! Stable conditions and stack < mixing height -> add extra amount plrise_ci_add_stab_unstab to onder;
IF ( hbron .LT. xloc .AND. ABS(ol) .LT. 100 ) THEN
   onder = onder + 0.35
ENDIF

! Limit onder, such that 0 <= onder <= 1
IF (onder .GT. (1. + EPS_DELTA)) THEN
   onder = 1.
ELSE IF (onder .LT. (0. - EPS_DELTA)) THEN
   onder = 0.
ELSE
   CONTINUE
ENDIF

! Plume centre is maximal equal to mixing haight:
IF ((htt .GT. (xl + EPS_DELTA)) .AND. (onder .GT. (0. + EPS_DELTA))) THEN
   htt = xl
ENDIF

RETURN
END SUBROUTINE ops_plrise71
