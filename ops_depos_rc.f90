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
! SCCS(SOURCE)       : %P%
! RELEASE - LEVEL    : %R% - %L%
! BRANCH -SEQUENCE   : %B% - %S%
! DATE - TIME        : %E% - %U%
! WHAT               : %W%:%E%
! AUTHOR             :
! FIRM/INSTITUTE     : RIVM/LLO
! LANGUAGE           : FORTRAN-F77/90
! USAGE              : %M%
! DESCRIPTION        : Compute resistances for dry deposition.
! EXIT CODES         :
! FILES AND OTHER    :
!    I/O DEVICES
! SYSTEM DEPENDENCIES: HP Fortran
! CALLED FUNCTIONS   : depac
! UPDATE HISTORY :
!-------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE ops_depos_rc(icm, iseiz, mb, gym ,temp_C, uster, glrad, hum, nwet, ratns, catm, c_ave_prev, lu_per, ra, rb, rc_eff_pos, rc_eff)

USE m_commonconst
USE m_depac318

IMPLICIT NONE

! CONSTANTS
CHARACTER*512                                    :: ROUTINENAAM                ! 
PARAMETER      (ROUTINENAAM = 'ops_depos_rc')

! SUBROUTINE ARGUMENTS - INPUT
INTEGER*4, INTENT(IN)                            :: icm                        ! 
INTEGER*4, INTENT(IN)                            :: iseiz                      ! 
INTEGER*4, INTENT(IN)                            :: mb                         ! 
INTEGER*4, INTENT(IN)                            :: nwet                       ! 
REAL*4,    INTENT(IN)                            :: hum                        ! 
REAL*4,    INTENT(IN)                            :: uster                      ! friction velocity [m/s]
REAL*4,    INTENT(IN)                            :: temp_C                     ! temperature at height zmet_T [C]
REAL*4,    INTENT(IN)                            :: gym                        !
REAL*4,    INTENT(IN)                            :: glrad                      ! 
REAL*4,    INTENT(IN)                            :: ratns                      ! 
REAL*4,    INTENT(IN)                            :: catm 
REAL*4,    INTENT(IN)                            :: c_ave_prev
REAL*4,    INTENT(IN)                            :: ra
REAL*4,    INTENT(IN)                            :: rb
REAL*4,    INTENT(IN)                            :: lu_per(NLU)                ! land use percentages for all land use classes

! SUBROUTINE ARGUMENTS - OUTPUT
REAL*4,    INTENT(OUT)                           :: rc_eff_pos                 ! canopy resistance, no re-emission [s/m]  
REAL*4,    INTENT(OUT)                           :: rc_eff                     ! canopy resistance, re-emission allowed [s/m];  

! LOCAL VARIABLES
INTEGER*4                                        :: day_of_year                ! 
INTEGER*4                                        :: mnt                        ! 
INTEGER*4, DIMENSION(2)                          :: mnt_select                 ! 
INTEGER*4                                        :: luclass                    ! 
REAL*4                                           :: som_vd_month               ! summed vd over representative months
REAL*4                                           :: som_vd_eff_ave             ! summed vd over land use classes (vd = 1/Ra + Rb + Rc_eff)
REAL*4                                           :: som_vd_eff_ave_pos         ! summed vd over land use classes (vd = 1/Ra + Rb + Rc_eff_pos)
REAL*4                                           :: telmaand 
REAL*4                                           :: rc_eff_ave                 ! canopy resistance, re-emission allowed, averaged over representative months
REAL*4                                           :: rc_eff_ave_pos             ! canopy resistance, no re-emission, averaged over representative months
REAL*4                                           :: rc_tot
REAL*4                                           :: sinphi
REAL*4                                           :: ccomp_tot
REAL*4, PARAMETER                                :: catm_min = 0.1E-05
REAL*4                                           :: rc_eff_depac               ! canopy resistance from depac, re-emission allowed [s/m];  

! SCCS-ID VARIABLES
CHARACTER*81                                     :: sccsida                    ! 
sccsida = '%W%:%E%'//char(0)
!-------------------------------------------------------------------------------------------------------------------------------


! Initialise sums:
som_vd_month       = 0.0
som_vd_eff_ave     = 0.0
som_vd_eff_ave_pos = 0.0
   
! loop over land use classes:
DO luclass = 1,NLU
  IF (lu_per(luclass) /= 0.0) THEN

    telmaand = 0.0
    som_vd_month = 0.0
!
!  Select representative month(s)
!
! iseiz: 0 = long term, 1 = year, 2 = winter, 3 = summer, 4 = month in winter, 5 = month in summer
!
! Arable land and long term or year run; average over April and July;
! April and July are representative months in order to be able to compute deposition over a whole year;
! this has been tested against results for 12 separate months
!
! No arable land; choose one representative month;
! long term , year run -> May
! winter               -> November
! summer               -> June
! 1 month              -> use mb, which has been read in ops_read_meteo
!
    SELECT CASE(iseiz)
    CASE(0, 1)
      IF (luclass == 2) THEN
        mnt_select(1)=4
        mnt_select(2)=7
      ELSE
        mnt_select=5
      ENDIF
    CASE(2)
       mnt_select=11
    CASE(3)
      mnt_select=6
    CASE DEFAULT
      mnt_select=mb
    END SELECT
!      Compute Rc only for mnt_select(1) and if necessary mnt_select(2) 
!
    DO mnt=1,12
      IF (mnt .EQ. mnt_select(1) .OR. mnt .EQ. mnt_select(2) ) THEN
!
!          Set approximate day of year:
!
        day_of_year = mnt*30-15   
!
!          Set sin of solar elevation angle; 
!          fit of sinphi is based on hourly data of global radiation (cloudy hours are filtered out)
!
        sinphi = 0.00237*glrad-.00000186*glrad*glrad  
!
!          Update month counter:
! 
       telmaand = telmaand+1
!
!           DEPAC has 3 outputs:
!           rc_tot      : total canopy resistance Rc (is not used here)
!           ccomp_tot   : total compensation point (is not used here)
!           rc_eff_depac: effective Rc (includes effect of compensation point); rc_eff_depac depends on the value of Ra and Rb.
!          
        CALL depac318(CNAME(icm,5), day_of_year, gym ,temp_C, uster, glrad, sinphi, hum, nwet, luclass, nint(ratns),   & 
                    & rc_tot, c_ave_prev, max(catm,catm_min), ccomp_tot, ra, rb, rc_eff_depac)
!
!          Detect missing values and set default values
!
        IF (rc_eff_depac  .EQ. -9999) rc_eff_depac = 10000
      
        som_vd_month = som_vd_month + 1/(rc_eff_depac + ra + rb)
      ENDIF
    ENDDO ! loop over representative months
!
!   Compute average over selected months:
!
    rc_eff_ave  = telmaand / som_vd_month  - (ra + rb)
!
!   Negative values for effective Rc (re-emission) is not allowed in _pos variables; reset Rc = 1000 
! 
    IF (rc_eff_ave .GT. 0 ) THEN
      rc_eff_ave_pos = rc_eff_ave
    ELSE
      rc_eff_ave_pos = 1000  
    ENDIF
!
!      Compute average weighted conductance over the landuse types
!
    som_vd_eff_ave_pos = som_vd_eff_ave_pos + lu_per(luclass)/sum(lu_per(1:NLU)) * 1/(rc_eff_ave_pos + (ra + rb))
    som_vd_eff_ave     = som_vd_eff_ave     + lu_per(luclass)/sum(lu_per(1:NLU)) * 1/(rc_eff_ave     + (ra + rb))
  ENDIF
ENDDO  ! loop over land use classes
!
!  Compute rc with and without (_pos) re-emission: 

rc_eff_pos   = 1/som_vd_eff_ave_pos - (ra + rb)
rc_eff = 1/som_vd_eff_ave - (ra + rb)

END SUBROUTINE ops_depos_rc
