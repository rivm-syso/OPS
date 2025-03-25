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
! DESCRIPTION        : Compute long term concentration for a given source and source-receptor distance;
!                      special version including correction for heavy plumes. 
!                      Concentration due to transport and dispersion only; no removal processes yet.
!                      Here, the concentration is computed at z = 0 m height. May be a problem very near a source,
!                      where there is a strong concentration profile. Later on, we apply a concentration profile, due to deposition,
!                      and there we calculate concentrations at measuring height.
!-------------------------------------------------------------------------------------------------------------------------------
module m_ops_conltexp










 
implicit none

contains

SUBROUTINE ops_conltexp(varin_meteo, varin_unc, zrcp, rond, ol, qbron, sigz0, road_disp, lroad_corr, uster, z0, htt, onder, vw10, pcoef, ircp, istab, disx, disxx, grof, iwd, qww, hbron,        &
                     &  dispg, radius, htot, c0_undepl_total, c_zrcp_undepl_total, sigz, ueff, xl, virty, error)

use m_ops_varin
use m_error
use m_ops_virtdist
use m_commonconst_lt, only: NSTAB

IMPLICIT NONE

! CONSTANTS
CHARACTER*512                                    :: ROUTINENAAM                ! 
PARAMETER    (ROUTINENAAM = 'ops_conltexp')

! SUBROUTINE ARGUMENTS - INPUT
TYPE(Tvarin_meteo), INTENT(IN)                   :: varin_meteo                ! input variables for meteo
type(Tvarin_unc), intent(in)                     :: varin_unc
REAL,      INTENT(IN)                            :: zrcp                       ! receptor height
INTEGER,   INTENT(IN)                            :: rond                       ! = 1 -> circular source
REAL,      INTENT(IN)                            :: ol                         ! Monin-Obukhov lengte
REAL,      INTENT(IN)                            :: qbron                      ! source strength [g/s]
REAL,      INTENT(IN)                            :: sigz0                      ! initial vertical dispersion length [m]
LOGICAL,   INTENT(IN)                            :: road_disp                  ! TRUE if user wants OPS to interpret sigz0 as SRM2 initial spread
LOGICAL,   INTENT(IN)                            :: lroad_corr                 ! TRUE if current emission category is a road and linear distance is within dist_road_corr
REAL,      INTENT(IN)                            :: uster                      ! frictiesnelheid
REAL,      INTENT(IN)                            :: z0                         ! ruwheidslengte (m)
REAL,      INTENT(IN)                            :: htt                        ! plume height at source, including plume rise [m]
REAL,      INTENT(IN)                            :: onder                      ! fraction of emission below mixing height [-]
REAL,      INTENT(IN)                            :: vw10                       ! wind speed at 10 m height [m/s]
REAL,      INTENT(IN)                            :: pcoef                      ! coefficient in wind speed power law
INTEGER,   INTENT(IN)                            :: ircp                       ! index of receptorpoint (here only used for debug write statement)
INTEGER,   INTENT(IN)                            :: istab                      ! index of stability class
REAL,      INTENT(IN)                            :: disx                       ! linear distance between source and receptor [m] (here only used for debug write statement)
REAL,      INTENT(IN)                            :: disxx                      ! effective travel distance between source and receptor [m] 
REAL,      INTENT(IN)                            :: grof                       ! = 1 -> coarse particles
INTEGER,   INTENT(IN)                            :: iwd                        ! wind direction if wind is from source to receptor (degrees)
REAL,      INTENT(IN)                            :: qww                        ! heat content
REAL,      INTENT(IN)                            :: hbron                      ! emission height at source (stack height), without plume rise [m]
REAL,      INTENT(IN)                            :: dispg                      ! coefficient for vertical dispersion coefficient sigma_z; sigma_z = dispg*x^disp [-]

! SUBROUTINE ARGUMENTS - I/O
REAL,      INTENT(INOUT)                         :: radius                     ! radius of area source [m]
REAL,      INTENT(INOUT)                         :: htot                       ! plume height at receptor, including plume descent due to heavy particles [m]
                                                                               ! note that htot may be changed in ops_conlt_par_oppbr
REAL,      INTENT(INOUT)                         :: xl                         ! maximal mixing height over transport distance [m] (extrapolated when x > 1000km, largest distance category in meteo statistics; xl = 2.*htt when xl < htt)
TYPE (TError), INTENT(INOUT)                     :: error                      ! error handling record 


! SUBROUTINE ARGUMENTS - OUTPUT
REAL,      INTENT(OUT)                           :: c0_undepl_total            ! undepleted concentration at z = 0 m  (including part of plume above mixing layer); is needed for secondary species
REAL,      INTENT(OUT)                           :: c_zrcp_undepl_total        ! undepleted concentration at z = zrcp (including part of plume above mixing layer)
REAL,      INTENT(OUT)                           :: sigz                       ! vertical dispersion length [m]
REAL,      INTENT(OUT)                           :: ueff                       ! wind speed at effective transport height heff; 
                                                                               ! for short distances heff = plume height;
                                                                               ! for large distances heff = 1/2 mixing height;
                                                                               ! heff is interpolated for intermediate distances.
REAL,      INTENT(OUT)                           :: virty                      ! distance virtual point source - centre area source [m]

! LOCAL VARIABLES
REAL                                             :: pld                        ! plume descent [m]
INTEGER                                          :: itransp_dist               ! index into different transport distance regimes

!-------------------------------------------------------------------------------------------------------------------------------

! Check for receptor above mixing layer:
if (zrcp .gt. xl) then
   call SetError('cannot compute concentrations above mixing layer yet',error)
   call ErrorParam('mixing layer height [m]',xl,error)
   call ErrorParam('receptor height',zrcp,error)
   goto 9999
endif

!--------------------------------------------------------------------
! Compute transport and dispersion parameters
! pld (plume descent), htot (plume height at receptor incl. plume descent (htot = htt - pldaling)), ueff, sigz, virty, 
! radius (correction for square source), itransp_dist (different transport distance regimes)
! Note: xl may be adjusted in case onder = 0. 
!--------------------------------------------------------------------
call ops_conlt_trans_disp(varin_meteo, varin_unc, onder, disx, disxx, htt, vw10, pcoef, ircp, istab, dispg, rond, grof, iwd, & 
   qww, hbron, uster, z0, ol, sigz0, road_disp, lroad_corr, &
   pld, htot, ueff, sigz, virty, xl, radius, itransp_dist, error)

! Compute concentration at height zrcp, using a Gaussian plume model:
call ops_conlt_gauss_zrcp(zrcp, onder, qbron, disxx, htt, & 
              grof, pld, htot, ueff, sigz, virty, xl, radius, itransp_dist, ircp, istab, disx, c0_undepl_total, c_zrcp_undepl_total, error)

return

9999 CALL ErrorCall(ROUTINENAAM, error)
   
end subroutine ops_conltexp




subroutine ops_sigz0_srm2(sigz0,z0,sz0fit)


!--------------------------------------------------------------------
!  Change initial vertical dispersion, to match OPS result with SRM2. Only used for ROADS sectors, and if roads_disp == TRUE
!--------------------------------------------------------------------



! Input variables:
REAL,      INTENT(IN)                            :: sigz0                      ! initial vertical dispersion length [m]
REAL,      INTENT(IN)                            :: z0                         ! roughness length [m]

! Output variables
REAL,      INTENT(OUT)                           :: sz0fit                     ! initial vertical dispersion length [m]

! Local variables
REAL, DIMENSION(3)                               :: z0bins = (/0.055, 0.17, 0.55/)         ! boundaries of z0 bins to which the ROADS function is applied
REAL, DIMENSION(4)                               :: coefA = (/1.05, 1.15, 1.33, 1.62/)         ! intercepts
REAL, DIMENSION(4)                               :: coefB = (/1.53, 2.31, 2.25, 6.6/)         ! slopes


IF (z0 .LT. z0bins(1)) THEN
  sz0fit = coefA(1)+coefB(1)*sigz0
ELSEIF (z0 .LT. z0bins(2)) THEN
  sz0fit = coefA(2)+coefB(2)*sigz0
ELSEIF (z0 .LT. z0bins(3)) THEN
  sz0fit = coefA(3)+coefB(3)*sigz0
ELSE
  sz0fit = coefA(4)+coefB(4)*sigz0
ENDIF


end subroutine ops_sigz0_SRM2


!-------------------------------------------------------------------------------------------------------------------------------
subroutine ops_conlt_trans_disp(varin_meteo, varin_unc, onder, disx, disxx, htt, vw10, pcoef, ircp, istab, dispg, rond, grof, iwd, & 
   qww, hbron, uster, z0, ol, sigz0, road_disp, lroad_corr, &
   pld, htot, ueff, sigz, virty, xl, radius, itransp_dist, error)

!-----------------------------------------------------------------------------------
! Compute transport and dispersion parameters needed for long-term concentration
!-----------------------------------------------------------------------------------

use m_ops_meteo
use m_commonconst_lt, only: EPS_DELTA, HUMAX, DISPH, NSTAB
use m_ops_virtdist
use m_ops_varin, only: Tvarin_meteo, Tvarin_unc
use m_error

! Input variables:
TYPE(Tvarin_meteo), INTENT(IN)                   :: varin_meteo                ! input variables for meteo
type(Tvarin_unc), intent(in)                     :: varin_unc
REAL,      INTENT(IN)                            :: onder                      ! fraction of emission below mixing height [-]
REAL,      INTENT(IN)                            :: disx                       ! linear distance between source and receptor [m] (here only used for debug write statement)
REAL,      INTENT(IN)                            :: disxx                      ! effective travel distance between source and receptor [m] 
REAL,      INTENT(IN)                            :: htt                        ! plume height at source, including plume rise [m]
REAL,      INTENT(IN)                            :: vw10                       ! wind speed at 10 m height [m/s]
REAL,      INTENT(IN)                            :: pcoef                      ! coefficient in powerlaw for wind speed
INTEGER,   INTENT(IN)                            :: ircp                       ! index of receptorpoint (here only used for debug write statement)
INTEGER,   INTENT(IN)                            :: istab                      ! index of stability class
REAL,      INTENT(IN)                            :: dispg                      ! coefficient for vertical dispersion coefficient sigma_z; sigma_z = dispg*x^disp [-]
INTEGER,   INTENT(IN)                            :: rond                       ! = 1 -> circular source
REAL,      INTENT(IN)                            :: grof                       ! = 1 -> coarse particles
INTEGER,   INTENT(IN)                            :: iwd                        ! wind direction if wind is from source to receptor (degrees)
REAL,      INTENT(IN)                            :: qww                        ! heat content
REAL,      INTENT(IN)                            :: hbron                      ! emission height at source (stack height), without plume rise [m]
REAL,      INTENT(IN)                            :: uster                      ! frictiesnelheid
REAL,      INTENT(IN)                            :: z0                         ! ruwheidslengte (m)
REAL,      INTENT(IN)                            :: ol                         ! Monin-Obukhov lengte
REAL,      INTENT(IN)                            :: sigz0                      ! initial vertical dispersion length [m]
LOGICAL,   INTENT(IN)                            :: road_disp                  ! TRUE if user wants OPS to interpret sigz0 as SRM2 initial spread
LOGICAL,   INTENT(IN)                            :: lroad_corr                 ! TRUE if current emission category is a road and linear distance is within dist_road_corr

! Input/output variables:
REAL,      INTENT(INOUT)                         :: htot                       ! plume height at receptor, including plume descent due to heavy particles [m]
REAL,      INTENT(INOUT)                         :: xl                         ! mixing height
REAL,      INTENT(INOUT)                         :: radius                     ! radius of area source [m]
TYPE (TError), INTENT(INOUT)                     :: error                      ! error handling record 

! Output variables:
REAL,      INTENT(OUT)                           :: pld                        ! plume descent [m]
REAL,      INTENT(OUT)                           :: ueff                       ! wind speed at effective transport height 
REAL,      INTENT(OUT)                           :: sigz                       ! vertical dispersion length [m]
REAL,      INTENT(OUT)                           :: virty                      ! distance virtual point source - centre area source [m]
INTEGER,   INTENT(OUT)                           :: itransp_dist               ! index into different transport distance regimes

! Local variables:
REAL                                             :: disp                       ! exponent in sigma_z = dispg*x**disp
REAL                                             :: hf                         ! effective transport height [m]
REAL                                             :: h                          ! plume height, including plume descent and limit to HUMAX 
REAL                                             :: humax_scaled               ! HUMAX with xl_fact scaling applied.
REAL                                             :: tl                         ! 
REAL                                             :: u1                         ! 
REAL                                             :: utl                        ! 
REAL                                             :: pp                         ! help variable sigma_z/zi
REAL                                             :: qq                         ! help variable 0.6*sqrt(1 - htot/xl))
REAL                                             :: sz0fit                     ! sigmaz0 




CHARACTER*512                                    :: ROUTINENAAM                ! 
PARAMETER    (ROUTINENAAM = 'ops_conlt_trans_disp')

! pld = plume descent
pld  = htt - htot
humax_scaled = varin_unc%unc_meteo%xl_fact * HUMAX

IF (ABS(onder) .LE. EPS_DELTA) THEN
   !--------------------------------------------------------------------
   ! Plume completely above mixing layer
   !--------------------------------------------------------------------

   ! Set h = plume height, including plume descent due to heavy particles and limit to HUMAX 
   IF (htot .GT. (humax_scaled + EPS_DELTA)) THEN
      h = humax_scaled
   ELSE
      h = htot
   ENDIF

   ! Compute wind speed at plume height using power law (2.9 OPS report)
   ueff = vw10*((h/10.)**pcoef)
   
   ! Compute sigma_z (representative over the trajectory) as function of the source receptor distance:
   ! sigma_z = dispg*(disxx**disph) (3.16 new OPS report); section 3.2.1. OPS report
   sigz = dispg*(disxx**DISPH(istab))
   IF (sigz .LT. (1. - EPS_DELTA)) THEN
      sigz = 1
   ENDIF

   ! If plume above mixing layer -> concentration at ground level = 0, but concentration still needed for e.g. wet deposition
   ! pluimdikte boven menglaag 2*sigz
   IF (sigz .LT. (40. - EPS_DELTA)) THEN
      xl = 80.
   ELSE
      xl = 2.*sigz
   ENDIF

   ! Compute distance between (virtual point source) and (centre of area source);
   ! for a point source, virty = 0.
   virty = ops_virtdist(radius, rond)

ELSE
   !--------------------------------------------------------------------
   !  Plume (partly) inside mixing layer;
   !--------------------------------------------------------------------

   ! Compute disp, i.e. exponent in the formula sigma_z = dispg*x**disp:
   disp = DISPH(istab)
   IF (ABS(grof - 1.) .LE. EPS_DELTA) THEN

      ! grof = 1 ("grof" = coarse): heavy particles
      ! Note that u1, tl and utl are local to this code block, only disp is used outside this block.
      
      ! Compute u1 = wind speed at htt/2 
      ! htt = plume height, excluding plume descent due to heavy particles [m]
      u1 = vw10*((htt/20.)**pcoef)

      ! tl = characteristic travel time (s): 
      IF (htt .GT. (50. + EPS_DELTA)) THEN
         tl = 1000.
      ELSE
         tl = 500.
      ENDIF
      
      ! utl = u1 * tl + radius = characteristic travel distance [m]: 
      utl = (u1*tl) + radius

      ! Correction for disp, only for larger distance (disxx > characteristic travel distance):
      IF (utl .LT. (disxx - EPS_DELTA)) THEN
         disp=ALOG(utl**(DISPH(istab)-.5)*disxx**.5)/ALOG(disxx)
      ENDIF
   ENDIF
   
   
   ! If current emission catagory is in emcat, the distance is within dist_road_corr (lroad_corr == TRUE) and if road_disp is TRUE, then interpret sigz0 as SRM2 initial dispersion and apply fit functions
   IF (road_disp .AND. lroad_corr) THEN
		CALL ops_sigz0_srm2(sigz0,z0,sz0fit)
   ELSE
        sz0fit = sigz0
   ENDIF
   
   !--------------------------------------------------------------------
   ! Plume (partly) inside mixing layer
   !--------------------------------------------------------------------

   IF (radius .GT. (0. + EPS_DELTA)) THEN
      ! Compute dispersion parameters for an area source:
      CALL ops_conlt_par_oppbr(rond, iwd, disxx, istab, disp, htt, grof, dispg, sz0fit, radius, virty, sigz, pld, htot)
   ELSE
      ! Compute dispersion parameters for a point source:
      CALL ops_conlt_par_puntbr(qww, disxx, disp, htot, hbron, dispg, sz0fit, sigz, virty)
   ENDIF
   if (error%debug) write(*,'(3a,2(1x,i6,";"),99(1x,e12.5,";"))') trim(ROUTINENAAM),',A; ','ircp; istab; disx; disxx; qww; disp; htt; htot; hbron; onder; pld; dispg; sigz; virty; ', &
                                                                                            ircp, istab, disx, disxx, qww, disp, htt, htot, hbron, onder, pld, dispg, sigz, virty

   ! Compute help variable pp = sigma_z/mixing_height = sigma_z/zi
   pp = sigz/xl
   
   !--------------------------------------------------------------------------------------
   ! Compute transport and dispersion parammeters for different transport distances
   !--------------------------------------------------------------------------------------
   !
   !  Distinguish three cases (zi = xl = mixing height); see manual "lange-termijnmodel", April 1976, section 10.2.8)
   !
   !  (2*zi - htot)**2                           (2*zi - htot)**2                                                  sigma_z          1          htot
   !  ----------------- = x <=> (2*sigma_z**2) = ------------------ <=> sigma_z = sqrt[1/(2x)] * (2*zi - htot) <=> -------- = sqrt(---) * (2 - ----)
   !  (2*sigma_z**2)                                     x                                                            zi           2x           zi
   !
   
   !  itransp_dist = 1: short distance,                                           0 < sigma_z/zi <= 0.6*sqrt(1-htot/zi)
   !  itransp_dist = 2: intermediate distance,                  0.6*sqrt(1-htot/zi) < sigma_z/zi <= 0.9
   !  itransp_dist = 3: large distance and/or well mixed plume,                 0.9 < sigma_z/zi
   !
   !  itransp_dist = 3 (large distance > 100 km and/or well mixed plume (sigma_z > 1.6*mixing_height)  AND grof < 0.2
   !
   IF ((pp .GT. (1.6 + EPS_DELTA) .OR. disxx .GT. (100000. + EPS_DELTA)) .AND. grof .LT. (.2 - EPS_DELTA)) THEN

      !------------------------------------------------------------------------
      ! Plume (partly) inside mixing layer; large distances (itransp_dist = 3)
      !------------------------------------------------------------------------
      itransp_dist = 3
      
      ! At large distances, the plume is well mixed over the mixing layer;
      ! in this case, the effective transport height = hf = mixing_height/2 (limited by HUMAX)
      IF (xl/2. .GT. (humax_scaled + EPS_DELTA)) THEN
         hf = humax_scaled
      ELSE
         hf = xl/2.
      ENDIF

     ! Compute effective wind velocity at height hf (power law)
      ueff = vw10*((hf/10.)**pcoef)                                            ! 920906
      
   ELSE

      !------------------------------------------------------------------------------------------
      ! Plume (partly) inside mixing layer; short or intermediate distances (itransp_dist = 1,2)
      !------------------------------------------------------------------------------------------

      qq = .6*sqrt(1. - htot/xl)

      ! Compute hf = effective transport height over trajectory, by interpolating effective plume height at short distance and 
      ! (mixing heigth)/2 at large distance (where plume is assumed to be well mixed over whole mixing layer).
      ! For heavy particles (grof > 0.2) hf = plume_height/2
      ! hf = (1. - pp/1.6)*htt + (pp/1.6)*xl/2. ?
      ! pp = sigma_z/mixing_height
      
      
      IF (grof .GT. (.2 + EPS_DELTA)) THEN
         hf = htt/2.
      ELSE
         hf = (1. - pp/1.6)*htt + (pp/1.6)*xl/2.
      ENDIF
      IF (hf .GT. (humax_scaled + EPS_DELTA)) THEN
         hf = humax_scaled
      ENDIF

      ! Compute wind speed at effective transport height by either a power law (hf > 50 m)
      ! or by a logarithmic wind profile (hf <= 50 m). 
      IF ( hf .GT. 50 ) THEN
         ueff = vw10*(hf/10)**pcoef
      ELSE
         CALL ops_wv_log_profile(z0, hf, uster, ol, varin_meteo, ueff)
      ENDIF

      ! Check for intermediate distance pp > qq <=> sigma_z/xl > .6*sqrt(1. - htot/xl)
      ! For coarse particles (grof = 1) switch to condition pp > qq/3 instead of normal pp > qq;
      ! idea is that for coarse particles we have a descending plume and the reflection
      ! at the earth surface takes place earlier 
      ! Note: qq = .6*sqrt(1. - htot/xl), pp = sigma_z/mixing_height
      IF (pp .GT. (qq/(grof*2. + 1.) + EPS_DELTA)) THEN 

         !-----------------------------------------------------------------------------------
         ! Plume (partly) inside mixing layer; intermediate distances (itransp_dist = 2)
         !-----------------------------------------------------------------------------------
         itransp_dist = 2
         
      ELSE
         !-----------------------------------------------------------------------------------
         ! Plume (partly) inside mixing layer; short distances (itransp_dist = 1)
         !-----------------------------------------------------------------------------------
         itransp_dist = 1

      ENDIF
   ENDIF
ENDIF

RETURN

end subroutine ops_conlt_trans_disp

!-------------------------------------------------------------------------------------------------------------------------------
! SUBROUTINE         : ops_conlt_par_oppbr
! DESCRIPTION        : Compute different parameters for an area source.
!-------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE ops_conlt_par_oppbr(rond, iwd, disxx, istab, disp, htt, grof, dispg, sigz0, radius, virty, sigz, pld, htot)

use Binas, only: deg2rad
use m_ops_virtdist
use m_commonconst_lt, only: EPS_DELTA, NSTAB

! CONSTANTS
CHARACTER*512                                    :: ROUTINENAAM                ! 
PARAMETER    (ROUTINENAAM = 'ops_conlt_par_oppbr')

! SUBROUTINE ARGUMENTS - INPUT
INTEGER,   INTENT(IN)                            :: rond                       ! 
INTEGER,   INTENT(IN)                            :: iwd                        ! wind direction if wind is from source to receptor (degrees)
REAL,      INTENT(IN)                            :: disxx                      ! effective travel distance between source and receptor [m] 
INTEGER,   INTENT(IN)                            :: istab                      ! 
REAL,      INTENT(IN)                            :: disp                       ! 
REAL,      INTENT(IN)                            :: htt                        ! plume height at source, including plume rise [m]
REAL,      INTENT(IN)                            :: grof                       ! 
REAL,      INTENT(IN)                            :: dispg                      ! coefficient for vertical dispersion coefficient sigma_z; sigma_z = dispg*x^disp [-]
REAL,      INTENT(IN)                            :: sigz0                      ! initial vertical dispersion length [m]

! SUBROUTINE ARGUMENTS - I/O
REAL,      INTENT(INOUT)                         :: radius                     ! 
REAL,      INTENT(INOUT)                         :: pld                        ! plume descent [m]
REAL,      INTENT(INOUT)                         :: htot                       ! plume height at receptor, including plume descent due to heavy particles [m]

! SUBROUTINE ARGUMENTS - OUTPUT
REAL,      INTENT(OUT)                           :: virty                      ! distance virtual point source - centre area source [m]
REAL,      INTENT(OUT)                           :: sigz                       ! vertical dispersion length [m]

! LOCAL VARIABLES
REAL                                             :: sz                         ! 
REAL                                             :: cr                         ! 
REAL                                             :: radr                       ! 
REAL                                             :: dx                         ! 
REAL                                             :: dy                         ! 
REAL                                             :: s1                         ! 
REAL                                             :: s2                         ! 
REAL                                             :: dsx                        ! 
REAL                                             :: radius2                    ! 
REAL,   PARAMETER                                :: ZWCOR(NSTAB) = (/ 1.2, 1.1, 0.8, 0.6, 0.75, 0.6 /) ! correction for heavy plumes

!-------------------------------------------------------------------------------------------------------------------------------
!
! Compute distance between (virtual point source) and (centre of area source);
! note that for a point source, virty = 0.
virty = ops_virtdist(radius, rond)

IF (rond .EQ. 1) THEN
   ! Circular area source:
   cr = 1.
ELSE

   !   Square area source;
   !   Compute correction factor cr for corrected source radius r' = r*cr, such that r' represents a square area source 
   !
   !   |
   ! dy-....... 
   !   |     /.
   !   |    / .
   !   |   /  .
   !   |  /   .
   !   | /    .
   !   |------|----
   !          dx
   
   dx      = ABS(radius*SIN(FLOAT(iwd)*deg2rad))
   dy      = ABS(radius*COS(FLOAT(iwd)*deg2rad))
   radius2 = AMAX1(dx, dy)
   cr      = radius/radius2
ENDIF
!
! radr = corrected radius; includes correction factor 1.006 because wind direction iwd is integer
!
radr = radius*cr/1.006
!
! Compute integrated sigma_z over area source (in the form sigma_z = dispg*x**disph).
! The approximation of this integral is exact for DISPH(istab) = 1.0.
! See figure 3.6 OPS report.
! First compute s1 = sigma_z at area source's edge near receptor (disxx-radr) (s1 = 0 inside area source)
!           and s1 = sigma_z at area source's edge far from receptor (disxx+radius)


IF (disxx .LT. (radr - EPS_DELTA)) THEN
   s1  = 0.
   dsx = radius
ELSE
   s1  = dispg*((disxx - radr)**disp)
   dsx = disxx
ENDIF
s2 = 0.92*dispg*((dsx + radius)**disp) 

! sz OPS report: represents the distribution of source heights within the area source
sz = 0.1      

IF (abs(s2-s1) .LE. 1.E-04) s2 = s1*1.001

! Compute sigma_z (3.39 OPS report); 
! sigz0: initial vertical dispersion length (due to turbulence at source and/or different emission heights in area source [m]
sigz   = (s2-s1)/alog((sz+s2)/(sz+s1))
sigz   = sqrt(sigz*sigz+sigz0*sigz0)
radius = radr
!
! Correction plume descent of heavy plumes inside area source; empirical relation that is adequate
! correctie pluimdaling van zware pluimen binnen opp bron dit is een empirische relatie die redelijk voldoet
!
IF ((disxx .LE. (radius + EPS_DELTA)) .AND. (ABS(grof - 1.) .LE. EPS_DELTA)) THEN
   pld  = (sigz*zwcor(istab)) + (pld/15.)
   htot = htt - pld
ENDIF

RETURN
END SUBROUTINE ops_conlt_par_oppbr

!-------------------------------------------------------------------------------------------------------------------------------
! SUBROUTINE         : ops_conlt_par_puntbr
! DESCRIPTION        : Compute parameters for a point (=punt) source (br << bron)
!-------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE ops_conlt_par_puntbr(qww, disxx, disp, htot, hbron, dispg, sigz0, sigz, virty)

use m_commonconst_lt, only: EPS_DELTA


! CONSTANTS
CHARACTER*512                                    :: ROUTINENAAM                ! 
PARAMETER    (ROUTINENAAM = 'ops_conlt_par_puntbr')

! SUBROUTINE ARGUMENTS - INPUT
REAL,      INTENT(IN)                            :: qww                        ! heat content
REAL,      INTENT(IN)                            :: disxx                      ! effective travel distance between source and receptor [m] 
REAL,      INTENT(IN)                            :: disp                       ! 
REAL,      INTENT(IN)                            :: htot                       ! plume height at receptor, including plume descent due to heavy particles [m]
REAL,      INTENT(IN)                            :: hbron                      ! emission height at source (stack height), without plume rise [m]
REAL,      INTENT(IN)                            :: dispg                      ! coefficient for vertical dispersion coefficient sigma_z; sigma_z = dispg*x^disp [-]
REAL,      INTENT(IN)                            :: sigz0                      ! initial vertical dispersion length [m]

! SUBROUTINE ARGUMENTS - OUTPUT
REAL,      INTENT(OUT)                           :: sigz                       ! vertical dispersion length [m] 
REAL,      INTENT(OUT)                           :: virty                      ! distance virtual point source - centre area source [m]. Zero for a point source.

! LOCAL VARIABLES
REAL                                             :: a                          ! 

!-------------------------------------------------------------------------------------------------------------------------------
!
! Pasquill dispersion (point sources)
! sigz0: initial vertical dispersion length (due to turbulence at source and/or different emission heights in area source) [m]
IF (ABS(qww) .LE. EPS_DELTA) THEN
   sigz = dispg*(disxx**disp)
   sigz = SQRT((sigz*sigz) + (sigz0*sigz0))
ELSE
!
!  Buoyant plumes: sigma_z taken from OML model (Berkowicz and Olesen, 1986)
!  sigma_z in the form dispg*x**disp
!
   sigz = dispg*(disxx**disp)

   ! Add extra vertical dispersion due to buoyant plumes; 2.5066 = sqrt(2*pi);
   ! taken from OML model (Berkowicz and Olesen, 1986)
   a = (htot - hbron)/2.5066 
   a = AMIN1(sigz, a)/1.5                                                      ! 960115
   sigz = SQRT((sigz*sigz) + (a*a) + (sigz0*sigz0))
ENDIF

! Point source, so extra virtual distance for area source = 0
virty = 0.
   
RETURN
END SUBROUTINE ops_conlt_par_puntbr

!--------------------------------------------------------------------
subroutine ops_conlt_gauss_zrcp(zrcp, onder, qbron, disxx, htt, & 
              grof, pld, htot, ueff, sigz, virty, xl, radius, itransp_dist, ircp, istab, disx, c0_undepl_total, c_zrcp_undepl_total,error)

use m_commonconst_lt, only: EPS_DELTA, NSEK
use binas, only: twopi
use m_gauss_cwi
use m_error

!--------------------------------------------------------------------
! Compute concentration at height zrcp, using a Gaussian plume model
!--------------------------------------------------------------------

! Input variables:
REAL,      INTENT(IN)                            :: zrcp                       ! receptor height
REAL,      INTENT(IN)                            :: onder                      ! fraction of emission below mixing height [-]
REAL,      INTENT(IN)                            :: qbron                      ! source strength [g/s]
REAL,      INTENT(IN)                            :: disxx                      ! effective travel distance between source and receptor [m] 
REAL,      INTENT(IN)                            :: htt                        ! plume height at source, including plume rise [m]
REAL,      INTENT(IN)                            :: grof                       ! = 1 -> coarse particles
REAL,      INTENT(IN)                            :: pld                        ! plume descent [m]
REAL,      INTENT(IN)                            :: htot                       ! plume height at receptor, including plume descent due to heavy particles [m]
REAL,      INTENT(IN)                            :: ueff                       ! wind speed at effective transport height 
REAL,      INTENT(IN)                            :: sigz                       ! vertical dispersion length [m]
REAL,      INTENT(IN)                            :: virty                      ! distance virtual point source - centre area source [m]
REAL,      INTENT(IN)                            :: xl                         ! mixing height
REAL,      INTENT(IN)                            :: radius                     ! radius of area source [m]
INTEGER,   INTENT(IN)                            :: itransp_dist               ! index into different transport distance regimes
INTEGER,   INTENT(IN)                            :: ircp                       ! index of receptorpoint (here only used for debug write statement) (here only used for debug write statement)
INTEGER,   INTENT(IN)                            :: istab                      ! index of stability class (here only used for debug write statement)
REAL,      INTENT(IN)                            :: disx                       ! linear distance between source and receptor [m] (here only used for debug write statement)


! Input/output variables:
TYPE (TError), INTENT(INOUT)                     :: error                      ! error handling record 

! Output variables:
REAL,      INTENT(OUT)                           :: c0_undepl_total            ! undepleted concentration at z = 0 m (including part of plume above mixing layer); is needed for secondary species
REAL,      INTENT(OUT)                           :: c_zrcp_undepl_total        ! undepleted concentration at z = zrcp (including part of plume above mixing layer)

! Local variables:
REAL,   PARAMETER                                :: fac = 1.0e6                ! conversion factor g -> ug; fac = 1e6
REAL,   PARAMETER                                :: PICON = 126987.            ! = fac/[pi * sqrt(2*pi)], fac = conversion factor g -> ug; fac = 1e6
REAL,   PARAMETER                                :: PS = 159155.               ! = fac/(2*pi), fac = conversion factor g -> ug; fac = 1e6
REAL                                             :: rr                         ! help variable 2*sigz*sigz 
REAL                                             :: a                          ! reflection term source-surface-mixing height-surface
REAL                                             :: b                          ! reflection term source-mixing height-surface
REAL                                             :: cls                        ! (1 + a + b)
REAL                                             :: f                          ! 
REAL                                             :: f1                         ! 
REAL                                             :: f2                         ! 
REAL                                             :: vs                         ! settling velocity [m/s]
REAL                                             :: sedfac                     ! sedimentation factor to suppress reflection term in case of heavy plumes
REAL                                             :: diffy                      ! diffusion factor in y-direction [1/m]
REAL                                             :: cwi0_undepl_total          ! undepleted cross wind integrated concentration at z = 0 m  (including part of plume above mixing layer)
REAL                                             :: cwi_zrcp_undepl_total      ! undepleted cross wind integrated concentration at z = zrcp (including part of plume above mixing layer)
INTEGER,   PARAMETER                             :: iopt_gauss_zrcp = 1        ! option for receptor height in Gauss function
                                                                               ! iopt_gauss_zrcp = 1 -> old subroutine ops_conltexp;                      c_zrcp_total = c(z=0)
                                                                               ! iopt_gauss_zrcp = 2 -> new subroutine gauss_cwi, but evaluated at z = 0; c_zrcp_total = c(z=0)
                                                                               ! iopt_gauss_zrcp = 3 -> new subroutine gauss_cwi evaluated at zrcp,       c_zrcp_total = c(z=zrcp)


CHARACTER*512                                    :: ROUTINENAAM                ! 
PARAMETER    (ROUTINENAAM = 'ops_conlt_gauss_zrcp')

if ((iopt_gauss_zrcp .eq. 1) .or. (grof .gt. 0.0)) then

   !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   ! Compute concentration according to old method, at height z = 0 m
   
   
   
   !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

   IF (ABS(onder) .LE. EPS_DELTA) THEN
      !--------------------------------------------------------------------
      ! Plume completely above mixing layer
      !--------------------------------------------------------------------
   
      ! Plume above mixing layer and receptor inside mixing layer -> concentration = 0
      c0_undepl_total     = 0.0
      c_zrcp_undepl_total = 0.0
   
   ELSE
      !--------------------------------------------------------------------
      !  Plume (partly) inside mixing layer;
      !--------------------------------------------------------------------
   
      !  itransp_dist = 1: short distance
      !  itransp_dist = 2: intermediate distance
      !  itransp_dist = 3: large distance and/or well mixed plume
      !  See par_transp_disp for definitions.
      
      IF (itransp_dist .eq. 3) then
         
         !--------------------------------------------------------------------
         ! Plume (partly) inside mixing layer; large distances (itransp_dist = 3)
         ! Plume well mixed, Dz = 1/zi
         !--------------------------------------------------------------------
         ! 
         !           Q(x)                Q(x)   NSEK     1
         ! C(x,0) = ----- D (x) D (x) = ----- --------  ---- ,  3.7, 3.8, 3.9 OPS report
         !            u    y     z        u    2 pi x    zi
         ! 
         ! qbron in g/s, u in m/s, x and zi in m; c0_undepl_total in ug/m3; PS = fac/(2*pi), fac = conversion factor g -> ug; fac = 1e6
         c0_undepl_total     = (qbron*NSEK*PS)/(ueff*(disxx + virty)*xl)
         c_zrcp_undepl_total = c0_undepl_total
         
      ELSE
   
         !-----------------------------------------------------------------------------------
         ! Plume (partly) inside mixing layer; short or intermediate distances (itransp_dist = 1,2)
         !-----------------------------------------------------------------------------------
         rr = sigz*sigz*2.
   
         IF (itransp_dist .eq. 2) then
            !-----------------------------------------------------------------------------------
            ! Plume (partly) inside mixing layer; intermediate distances (itransp_dist = 2)
            !-----------------------------------------------------------------------------------
   
            ! a = factor in reflection term from source-surface-mixing_layer-surface (descends with ascending emission height)
            ! b = factor in reflection term from source-mixing_layer-surface (ascends with ascending emission height)
            ! 3.15 in OPS report is rewritten as follows (note: zi = xl = mixing height)
            !                                       
            ! In the following h = htot = htt - pld 
            ! Note that for z = 0, two terms can be taken together:
            !          -h**2 +        -(-h)**2 = -2 h**2. 
            ! -(2 zi + h)**2 + -(-2 zi - h)**2 = -2 (2 zi + h)**2,
            ! -(2 zi - h)**2 + -(-2 zi + h)**2 = -2 (2 zi - h)**2.
            ! 
            !                   2                   -h**2                 -(2 zi + h)**2           -(2 zi - h)**2
            ! D (x,0) = ------------------- { exp[--------------] + exp [----------------] + exp [---------------] } = 
            !  z        sqrt(2 pi) sigma_z         2 sigma_z**2            2 sigma_z**2             2 sigma_z**2
            !
            !            2                  -h**2                    -[(2 zi + h)**2 - h**2]          -[(2 zi - h)**2 - h**2]
            !  = ------------------- exp[--------------] * {1 + exp [------------------------] + exp [----------------------] }
            !    sqrt(2 pi) sigma_z       2 sigma_z**2                    2 sigma_z**2                      2 sigma_z**2
            !
            !            2                   -h**2                    
            !  = ------------------- exp[--------------] * {1 + a + b} = 
            !    sqrt(2 pi) sigma_z       2 sigma_z**2                
            !
            !
            !            2                   -h**2                    
            !  = ------------------- exp[--------------] * cls,
            !    sqrt(2 pi) sigma_z       2 sigma_z**2                
            !
            !                 -[(2 zi + h)**2 - h**2]                -[(2 zi - h)**2 - h**2]
            !  with a =  exp [------------------------] and b = exp [----------------------].
            !                      2 sigma_z**2                         2 sigma_z**2
            !
            ! Note: rr = sigz*sigz*2, h = htot = htt - pld
            !
            ! Factor EXP(-4*   xl   *pld/rr) -> 0 if distance surface - mixing layer is large and/or pld/rr is large; in this case, the reflection surface-mixing layer does not occur
            ! Factor EXP(-4*(xl-htt)*pld/rr) -> 0 if distance  source - mixing layer is large and/or pld/rr is large; in this case, the reflection  source-mixing layer does not occur
            ! Note: there is no sedimentation factor for the reflection term source - surface (part of term 1 in cls) in case of sedimentation.
            a   = EXP( - 4.*xl     *pld/rr)* EXP(-((2.*xl  + htot)**2 - htot*htot)/rr) 
            b   = EXP( - 4*(xl-htt)*pld/rr)* EXP(-((2.*xl-htt-pld)**2 - htot*htot)/rr) 
            cls = 1. + a + b
         ELSE
            !-----------------------------------------------------------------------------------
            ! Plume (partly) inside mixing layer; short distances (itransp_dist = 1)
            !-----------------------------------------------------------------------------------
   
            ! Short distance -> no reflection at mixing layer:
            cls = 1.
            
         ENDIF
   
         !------------------------------------------------------------------------------------------
         ! Plume (partly) inside mixing layer; short or intermediate distances (itransp_dist = 1,2)
         !------------------------------------------------------------------------------------------
   
         ! Compute concentration at receptor for cases 1 and 2
         ! 
         !           Q(x)                Q(x)   NSEK        2                      -h**2      
         ! C(x,0) = ----- D (x) D (x) = ----- --------  ------------------- exp[--------------] * cls,  3.7, 3.8, 3.15 OPS report
         !            u    y     z        u    2 pi x   sqrt(2 pi) sigma_z       2 sigma_z**2
         ! 
         ! rr = sigz*sigz*2.
         ! qbron in g/s, u in m/s, x and zi in m; c0_undepl_total in ug/m3; PICON = fac/[pi * sqrt(2*pi)], fac = conversion factor g -> ug = 1e6
         c0_undepl_total     = qbron*EXP( - (htot*htot/rr))*NSEK*PICON* cls/(ueff*(disxx + virty)*sigz)
         c_zrcp_undepl_total = c0_undepl_total
      ENDIF
   ENDIF ! (onder = 0)
   
else 
   
   ! iopt_gauss_zrcp = 2,3
   !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   ! Use subroutine gauss_cwi to compute undepleted concentration
   ! subroutine gauss_cwi(q,hemis2,x,z,u,sigz,zi,vs,sedfac,c)  
   ! Input: 
   !   q      = qbron
   !   hemis2 = htot = emission height with plume rise, with plume descent [m]
   !   x      = disxx + virty
   !   z      = either 0 or zrcp
   !   u      = ueff
   !   sigz   = sigz
   !   zi     = xl
   !   vs     = settling velocity - not yet implemented
   !   sedfac = sedimentation factor - not yet implemented
   !
   ! Output: 
   !   c      : cross wind integrated concentration at height z [g/m2]
   !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

   IF (grof .gt. 0.0) THEN
      write(*,*) 'This version is not yet suited for coarse particles'
      write(*,*) 'The effect of the sedimentation factor on the reflection term in gauss_cwi has to be established'
      write(*,*) 'Fatal error' 
      stop
   ENDIF
   vs = 0.0; sedfac = 0.0; 

   if (iopt_gauss_zrcp .eq. 2) then
      ! Compute crosswind integrated concentration at z = 0:
      call gauss_cwi(qbron,htot,disxx+virty,0.0 ,ueff,sigz,xl,vs,sedfac,cwi0_undepl_total)
      cwi_zrcp_undepl_total = cwi0_undepl_total
   elseif (iopt_gauss_zrcp .eq. 3) then
      ! Compute crosswind integrated concentration at z = 0 and z = zrcp:
      call gauss_cwi(qbron,htot,disxx+virty,0.0 ,ueff,sigz,xl,vs,sedfac,cwi0_undepl_total)
      call gauss_cwi(qbron,htot,disxx+virty,zrcp,ueff,sigz,xl,vs,sedfac,cwi_zrcp_undepl_total)
   else
      write(*,*) 'Unexpected value for iopt_gauss_zrcp: ',iopt_gauss_zrcp
      stop
   endif

   !                                                          NSEK    
   ! Multiply with diffusion factor in y-direction: D (x) = -------- and convert g/m3 -> ug/m3 (fac):
   !                                                 y       2 pi x    
   diffy               = NSEK/(twopi*(disxx + virty))
   c0_undepl_total     = fac*diffy*cwi0_undepl_total
   c_zrcp_undepl_total = fac*diffy*cwi_zrcp_undepl_total
endif
   
! Debug output:
if (error%debug) write(*,'(3a,2(1x,i6,";"),99(1x,e12.5,";"))') & 
   trim(ROUTINENAAM),',A; ','ircp; istab; disx; disxx; zrcp; c0_undepl_total; c_zrcp_undepl_total; ', &
                             ircp, istab, disx, disxx, zrcp, c0_undepl_total, c_zrcp_undepl_total

IF (onder > 0) THEN   
   ! Correction for concentration within area source.
   ! Because this model assumes horizontal dispersion in a sector of 30 deg., consequently the emission
   ! is spread out over a segment instead of a circle. This gives an underestimation at the outer parts
   ! of the area source when the emission height is very low.
   IF (disxx .LT. (radius - EPS_DELTA)) THEN
      f   = (disxx + virty)/virty
      f1  = (radius - disxx)/radius
      f2  = EXP( - (htt*htt)/(sigz*sigz))
      c0_undepl_total     = c0_undepl_total    *((f - 1)*(f1**.4)*f2 + 1.) 
      c_zrcp_undepl_total = c_zrcp_undepl_total*((f - 1)*(f1**.4)*f2 + 1.) 
   ENDIF
ENDIF

RETURN

end subroutine ops_conlt_gauss_zrcp

end module m_ops_conltexp
