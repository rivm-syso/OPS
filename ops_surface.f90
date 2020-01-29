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
! NAME               : %M%
! SCCS (SOURCE)      : %P%
! RELEASE - LEVEL    : %R% - %L%
! BRANCH - SEQUENCE  : %B% - %S%
! DATE - TIME        : %E% - %U%
! WHAT               : %W%:%E%
! AUTHOR             : HvJ/Franka Loeve (Cap Volmac)
! FIRM/INSTITUTE     : RIVM LLO
! LANGUAGE           : FORTRAN-77/90
! DESCRIPTION        : This routine calculates sigmaz in the surface layer (all stabilities) on the basis of Monin Obukhov
!                      similarity theory and a Kz profile given by Businger(1973)
! EXIT CODES         :
! FILES AND OTHER    :
!    I/O DEVICES
! SYSTEM DEPENDENCIES: HP Fortran
! CALLED FUNCTIONS   :
! UPDATE HISTORY     :
!-------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE ops_surface(z0, zi, ol, uster, h, x, uh, zu, szs)

USE m_commonconst                                                              ! EPS_DELTA only

IMPLICIT NONE

! CONSTANTS
CHARACTER*512                                    :: ROUTINENAAM                ! 
PARAMETER      (ROUTINENAAM = 'ops_surface')

! CONSTANTS
REAL*4                                           :: K                          ! von Karman constant
PARAMETER   (K = 0.35)

! SUBROUTINE ARGUMENTS - INPUT
REAL*4,    INTENT(IN)                            :: z0                         ! roughness length (m)
REAL*4,    INTENT(IN)                            :: zi                         ! mixing height (m)
REAL*4,    INTENT(IN)                            :: ol                         ! Monin-Obukhov length  (m)
REAL*4,    INTENT(IN)                            :: uster                      ! friction velocity (m)
REAL*4,    INTENT(IN)                            :: h                          ! source heigth, including plume rise (m)
REAL*4,    INTENT(IN)                            :: x                          ! downwind distance  (m)

! SUBROUTINE ARGUMENTS - OUTPUT
REAL*4,    INTENT(OUT)                           :: uh                         ! wind speed at downwind distance x and height zu [m/s]
REAL*4,    INTENT(OUT)                           :: zu                         ! representative plume height, taking into account reflection 
                                                                               ! at the top of the mixing layer and at the ground surface [m]
REAL*4,    INTENT(OUT)                           :: szs                        ! vertical dispersion coefficient for surface layer [m]

! LOCAL VARIABLES
INTEGER*4                                        :: iter                       ! 
INTEGER*4                                        :: last                       ! 
REAL*4                                           :: a                          ! 
REAL*4                                           :: kz                         ! 
REAL*4                                           :: phih                       ! 
REAL*4                                           :: s                          ! 
REAL*4                                           :: zw                         ! 
REAL*4                                           :: zwold                      ! 

! SCCS-ID VARIABLES
CHARACTER*81                                     :: sccsida                    ! 
sccsida = '%W%:%E%'//char(0)
!-------------------------------------------------------------------------------------------------------------------------------
!
! An iterative procedure is needed, since sigma_z (verical spread of the plume)
! depends on Kz and u (wind speed) and these two depend on the effective plume 
! heights zw (for computation of Kz) and zu (for computation of u), which depend on sigma_z.
!

!
iter = 0
IF (ol .LT. (0. - EPS_DELTA)) THEN
   zw = .07*x**.8
ELSE IF (ol .GT. (0. + EPS_DELTA)) THEN
   zw = .04*x**.7
ELSE
   zw = h
ENDIF

! Limit zw such that zw >= stack_height, but in any case zw <= mixing_height/2:
IF (h .GT. (zw + EPS_DELTA)) THEN
   zw = h
ENDIF
IF (zw .GT. (zi/2. + EPS_DELTA)) THEN
   zw = zi/2.
ENDIF                

! Initially zu = zw.
zu    = zw
last  = 0
zwold = zw

!----------------------
! Start iteration
!----------------------
50 CONTINUE

   !
   ! Psi_h = non-dimensional temperature gradient (Businger, 1973, below 3.17 OPS report)
   !
   IF (ol .GT. (0. + EPS_DELTA)) THEN
      phih = 0.74 + 4.7*zw/ol 
   ELSE
      phih = 0.74*(1.-9*zw/ol)**(-0.5)
   ENDIF
   !
   ! compute wind speed at height zu from log-profile
   !
   CALL ops_wvprofile(z0, zu, uster, ol, uh)
   !
   ! Compute Kz at effective plume height zw;
   ! for L  > 0, according to Businger (1973); 3.17 OPS report 
   ! for L <= 0, according to Brost and Wyngaard (1978)
   ! The Businger formula includes an extra calibration factor a, derived from prairie grass data.
   !
   IF (ol .GT. (0. + EPS_DELTA)) THEN
      a = 1.2
      IF (ol .LT. (30. - EPS_DELTA)) THEN
         a = a*2.
      ENDIF
      kz = K*uster*zw/phih*a
   ELSE
      kz = K*uster*zw/phih*(1. - zw/zi)**1.5
   ENDIF
   
   ! sigma_z as function of Kz (3.18 OPS report)
   szs = SQRT(2.*kz*x/uh)
   
   ! Compute new values of zw and zu, depending on value of sigma_z
   IF (last .NE. 1 .AND. iter .LE. 12 ) THEN  
   ! IF (iter .LE. 12 ) THEN  
      last = 0
      iter = iter + 1

      ! s = effective plume width
      s = szs*.69 ! OPS report s = 0.67*szs (see text below 3.18)  
      ! s = szs*.69 + h/3   

      !--------------------------------------------------------------------------------------------------
      ! 1. Plume well mixed (s > zi/2)
      !    -> iteration finished, zw unchanged; zu = 1/2 mixing height
      !--------------------------------------------------------------------------------------------------
      IF (s .GE. (zi/2. - EPS_DELTA)) THEN
         zu = zi/2.

         ! set new value for zw ref. Sterk, 14-10-2015 
         IF (ol .LT. (0. - EPS_DELTA)) THEN
            zw = zu*0.75
         ELSE
            zw = zu
         ENDIF
         last = 1
         !write(*,'(3a,3(1x,i6),99(1x,e12.5))') trim(ROUTINENAAM),',A1,', &
         !    ' ircp,istab,iter,zw,zu,uh,szs,s:', &
         !      -999,-999,iter,zw,zu,uh,szs,s

         GOTO 50

      !--------------------------------------------------------------------------------------------------
      ! 2. Plume not well mixed AND plume does not touch the ground (s < h)
      !--------------------------------------------------------------------------------------------------
      ELSE IF (h .GE. (s - EPS_DELTA)) THEN
        
         ! zw = h - sigma_z
         zw = h - szs    
         ! zw = h - 0.1*s  
         
         ! zw < h/2  -> zw = h/2, zu = stack_height; iteration finished
         IF (zw .LT. (h/2. - EPS_DELTA)) THEN  
            zw = h/2.                          
         !IF (zw .LT. (h - EPS_DELTA)) THEN      
         !   zw = h                              
            zu = h
            last = 1

         ! zw > h/2 AND relative difference between zw and zwold > 10% ->
         ! -> subtract 0.6*(difference between iterands) to get new zw value (0.6 is relaxation factor); set zu = stack_height
         ELSE IF ((ABS((zw - zwold)/zw)) .GT. (0.1 + EPS_DELTA)) THEN 
         !ELSE IF ((ABS((zw - zwold)/zw)) .GT. (0.01 + EPS_DELTA)) THEN 
            zw = zw - (zw - zwold)*0.6                                            ! 960202
            zu = h
            zwold = zw

            ! zw > mixing_height/2 OR zw > h + s -> iteration finished
            IF ((zw .GT. (zi/2. + EPS_DELTA)) .OR. (zw .GT. (h + s + EPS_DELTA))) THEN
               last = 1
            ENDIF                                                                 ! 940904

         ! zw > h/2 AND relative difference between zw and zwold <= 10% ->
         ! -> iteration has converged; zu = h, zw unchanged
         ELSE
            last = 1
            zu   = h
         ENDIF
         !write(*,'(3a,3(1x,i6),99(1x,e12.5))') trim(ROUTINENAAM),',A2,', &
         !    ' ircp,istab,iter,zw,zu,uh,szs,s:', &
         !      -999,-999,iter,zw,zu,uh,szs,s

         GOTO 50

      !--------------------------------------------------------------------------------------------------
      ! 3. Plume not well mixed AND plume touches the ground (s > h)
      !--------------------------------------------------------------------------------------------------

      !  relative difference between zu and s > 10% ->
      ! -> substract 0.6*(zu-s) from zu to get new value of zu (0.6 is relaxation factor);
      ! limit zu such that zu >= stack_height and in any case zu <= mixing_height/2
      ELSE IF (ABS((zu - s)/zu) .GT. (0.1 + EPS_DELTA)) THEN
         zu = zu - (zu - s)*0.6                                                   ! 960202
         IF (zu .LT. (h - EPS_DELTA)) THEN
            zu = h
         ENDIF
         IF (zu .GT. (0.5*zi + EPS_DELTA)) THEN
            last = 1                                                              ! 940904
            zu   = zi/2.
         ENDIF

         ! set new value for zw
         IF (ol .LT. (0. - EPS_DELTA)) THEN
            zw = zu*0.75
         ELSE
            zw = zu
         ENDIF
         !write(*,'(3a,2(1x,i6),99(1x,e12.5))') trim(ROUTINENAAM),',A3,', &
         !    ' ircp,istab,iter,zw,zu,uh,szs,s:', &
         !      -999,-999,iter,zw,zu,uh,szs,s
         GOTO 50
      ELSE
         ! relative difference between zu and s <= 10% -> finish iteration
         CONTINUE
      ENDIF
   ENDIF

! end of iteration {last = 1 OR iter > 12}

RETURN
END SUBROUTINE ops_surface
