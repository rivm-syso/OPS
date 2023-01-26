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
! DESCRIPTION        : Get parameters as windspeed, mixing height, frequency etc. from the meteo statistics as a function of
!                      transport distance, wind direction and stability class. Parameters are interpolated between values of the
!                      different distance classes and wind sectors. A preliminary plume rise is also calculated here, in order to 
!                      be able to determine the wind sector (wind shear depends on emission height).
!
! The following steps are taken:
! ops_plumerise_prelim: compute preliminary plume rise
! ops_wv_powerlaw_metstat     : get values of vw10 and pcoef 
! windsek             : compute, given a source-receptor direction, isec_in = the wind sector where this direction lies in, 
!                       (isec1,isec2) = the wind sectors between which to interpolate and s_wind = interpolation factor, 
!                       taking into account plume rise and wind shear.
! interp_ctr          : compute itra = distance class with travel distance ~nearest to the current source-receptor distance;
!                       ids = distance class where the current source-receptor distance lies in and interpolation factors (s_dist).
! windcorr            : set interpolation factor for area sources if wind sector does not occur.
!
! Compute for the current stability class and for all distance classes 
! tal = number of hours of wind blowing from source to receptor (interpolated between sector isec1 and isec2). 
!
! interp_tra           : interpolate meteo parameters over distance classes (ids-1, ids)
! interp_sek           : interpolate meteo parameters over wind sectors (isec1,isec2)
! bepafst              : compute the effective travel distance between source and receptor
! 
! IF (Area source and not a special case)
! |  ronafhpar          : average meteo parameters contributions of an area source from multiple contributing sectors to a receptor
! ----
!
! Compute percvk = fraction of occurrence of {distance/stability/wind-direction} class
!
!-------------------------------------------------------------------------------------------------------------------------------
module m_ops_statparexp

implicit none

contains

SUBROUTINE ops_statparexp(istab, hbron, qww, D_stack, V_stack, Ts_stack, emis_horizontal, iwd, radius, uurtot, astat, trafst, disx, isec_prelim, disxx, isec1, vw10, h0,  &
                       &  hum, ol_metreg_rcp, shear, rc_aer_ms, rc_nh3_ms, rc_no2_ms, temp_C, uster_metreg_rcp, pcoef, htot, htt, itra, aant,                        &
                       &   xl, rb_ms, ra_ms_4, ra_ms_zra, xvglbr, xvghbr, xloc,xl100, rad, rc_so2_ms, coef_space_heating, regenk, buil, rint, percvk, error)

use m_error
USE m_commonconst_lt
USE m_commonfile
USE m_ops_plumerise
USE m_ops_meteo

IMPLICIT NONE

! CONSTANTS
CHARACTER*512                                    :: ROUTINENAAM                ! 
PARAMETER      (ROUTINENAAM = 'ops_statparexp')

! SUBROUTINE ARGUMENTS - INPUT
INTEGER*4, INTENT(IN)                            :: istab                      ! index of stsbility class
REAL*4,    INTENT(IN)                            :: hbron                      ! emission height at source (stack height), without plume rise [m]
REAL*4,    INTENT(IN)                            :: qww                        ! heat contyent of source [MW]
REAL*4,    INTENT(IN)                            :: D_stack                    ! diameter of the stack [m]
REAL*4,    INTENT(IN)                            :: V_stack                    ! exit velocity of plume at stack tip [m/s]
REAL*4,    INTENT(IN)                            :: Ts_stack                   ! temperature of effluent from stack [K]                     
LOGICAL,   INTENT(IN)                            :: emis_horizontal            ! horizontal outflow of emission
INTEGER*4, INTENT(IN)                            :: iwd                        ! wind direction if wind is from source to receptor (degrees)
REAL*4,    INTENT(IN)                            :: radius                     ! radius of area source [m]
REAL*4,    INTENT(IN)                            :: uurtot                     ! total number of hours from meteo statistics
REAL*4,    INTENT(IN)                            :: astat(NTRAJ, NCOMP, NSTAB, NSEK) ! meteo parameters
REAL*4,    INTENT(IN)                            :: trafst(NTRAJ)              ! travel distances for each distance class [m]  
REAL*4,    INTENT(IN)                            :: disx                       ! linear distance between source and receptor [m]                    
INTEGER*4, INTENT(IN)                            :: isec_prelim                ! index of preliminary source-receptor wind sector (wind shear not yet taken into account) 

! SUBROUTINE ARGUMENTS - I/O
TYPE (TError), INTENT(INOUT)                     :: error                      ! error handling record

! SUBROUTINE ARGUMENTS - OUTPUT
REAL*4,    INTENT(OUT)                           :: disxx                      ! effective travel distance between source and receptor [m]
INTEGER*4, INTENT(OUT)                           :: isec1                      ! first of two interpolating wind sectors
REAL*4,    INTENT(OUT)                           :: vw10                       ! 
REAL*4,    INTENT(OUT)                           :: h0                         ! 
REAL*4,    INTENT(OUT)                           :: hum                        ! relative humidity [%]
REAL*4,    INTENT(OUT)                           :: ol_metreg_rcp              ! 
REAL*4,    INTENT(OUT)                           :: shear                      ! 
REAL*4,    INTENT(OUT)                           :: rc_aer_ms                  ! Rc(SO4-aerosol) from meteo statistics [s/m]
REAL*4,    INTENT(OUT)                           :: rc_nh3_ms                  ! Rc(NH3) from meteo statistics (NOT USED)
REAL*4,    INTENT(OUT)                           :: rc_no2_ms                  ! Rc(NO2) from meteo statistics [s/m] (NOT USED) 
REAL*4,    INTENT(OUT)                           :: temp_C                     ! temperature at height zmet_T [C]
REAL*4,    INTENT(OUT)                           :: uster_metreg_rcp           ! 
REAL*4,    INTENT(OUT)                           :: pcoef                      ! 
REAL*4,    INTENT(OUT)                           :: htot                       ! plume height at receptor, including plume descent due to heavy particles [m]
REAL*4,    INTENT(OUT)                           :: htt                        ! plume height at source, including plume rise [m]
INTEGER*4, INTENT(OUT)                           :: itra                       ! distance class with travel distance ~nearest to the current source-receptor distance
REAL*4,    INTENT(OUT)                           :: aant                       ! number of hours of occurrence of interpolation of distance classes/wind sectors for the current stability class
REAL*4,    INTENT(OUT)                           :: xl                         ! 
REAL*4,    INTENT(OUT)                           :: rb_ms                      ! boundary layer resistance SO2 from meteo statistics [s/m]
REAL*4,    INTENT(OUT)                           :: ra_ms_4                    ! aerodynamic resistance at 4 m from meteo statistics [s/m] 
REAL*4,    INTENT(OUT)                           :: ra_ms_zra                  ! aerodynamic resistance at height zra from meteo statistics [s/m]  
REAL*4,    INTENT(OUT)                           :: xvglbr                     ! 
REAL*4,    INTENT(OUT)                           :: xvghbr                     ! 
REAL*4,    INTENT(OUT)                           :: xloc                       ! 
REAL*4,    INTENT(OUT)                           :: xl100                      ! 
REAL*4,    INTENT(OUT)                           :: rad                        ! 
REAL*4,    INTENT(OUT)                           :: rc_so2_ms                   ! Rc(SO2) from meteo statistics [s/m] 
REAL*4,    INTENT(OUT)                           :: coef_space_heating         ! space heating coefficient (degree-day values in combination with a wind speed correction) [C m^1/2 / s^1/2] 
REAL*4,    INTENT(OUT)                           :: regenk                     ! rain probability [-]
REAL*4,    INTENT(OUT)                           :: buil                       ! 
REAL*4,    INTENT(OUT)                           :: rint                       ! rain intensity [mm/h] 
REAL*4,    INTENT(OUT)                           :: percvk                     ! fraction of occurrence of {distance/stability/wind-direction} class

! LOCAL VARIABLES
INTEGER*4                                        :: isec2                      ! second of two interpolating wind sectors 
INTEGER*4                                        :: ids                        ! 
INTEGER*4                                        :: ispecial                   ! 
INTEGER*4                                        :: isec_in                    ! source-receptor wind sector, taking into account wind shear 
INTEGER*4                                        :: itrx                       ! = itra, but if one of interpolating distance classes is missing, itrx is the non-missing class
INTEGER*4                                        :: iwdd                       ! wind direction if wind is from source to receptor (degrees), including turning angle correction
INTEGER*4                                        :: itraj                      ! index of distance class
REAL*4                                           :: ccor                       ! concentration correction factor for area sources
REAL*4                                           :: stt(NCOMP)                 ! meteo parameters, interpolated between distance classes (ICOMP = 2-8, 19-NCOMP)
REAL*4                                           :: tal(NTRAJ)                 ! number of hours of wind blowing from source to receptor (interpolated between sector isec1 and isec2)
REAL*4                                           :: dscor(NTRAJ)               ! 
REAL*4                                           :: phi                        ! 
REAL*4                                           :: r                          ! 
REAL*4                                           :: r4                         ! 
REAL*4                                           :: r50                        ! 
REAL*4                                           :: s_wind                     ! interpolation factor (0-1) for the contribution of wind sector isec2
                                                                               ! (i.e. the second interpolation sector), to the wind direction 
                                                                               ! from source to receptor  
REAL*4                                           :: s_dist(NTRAJ)              ! interpolation factor for each distance class (interpolates data between lower and upper class boundary). 
                                                                               ! Note that if ids is the class index where the source-receptor distance lies in, then 
                                                                               ! 0 <= s_dist(ids) <= 1 and s_dist(i) = 0 for i /= ids
REAL*4                                           :: stta(NCOMP)                ! 
REAL*4                                           :: sttr(NCOMP)                ! 
REAL*4                                           :: sa                         ! 
REAL*4                                           :: so                         ! 
REAL*4                                           :: sp                         ! interpolation factor for wind sectors; sp = s_wind, sp = 0 or 1 if one of the sectors is missing
real                                             :: dum                        ! dummy output variable

!-------------------------------------------------------------------------------------------------------------------------------
!
! Compute preliminary plume rise; preliminary in the sense that later on (ops_conc_ini)
! stability is defined in terms of L and U*, instead of stability class (as in ops_plumerise_prelim) 
! and that corrections may be applied (e.g. for heavy particles); furthermore isec_prelim is also preliminary.
! Also get values of vw10 and pcoef 
!
! write(*,'(a,4(1x,e12.5),2(1x,i6))') 'before call ops_plumerise_prelim: ',hbron,htt,htt-hbron,-999.0,istab,isec_prelim
! write(*,'(a,4(1x,e12.5))') 'before call ops_plumerise_prelim: ',hbron,htt,htt-hbron,-999.0
call ops_plumerise_prelim(istab,isec_prelim,astat,hbron,qww,D_stack,V_stack,Ts_stack,emis_horizontal,htt,error) 
if (error%haserror) goto 9999
call ops_wv_powerlaw_metstat(istab,isec_prelim,astat,hbron,dum,vw10,pcoef)
!write(*,'(a,4(1x,e12.5))') 'after call ops_plumerise_prelim: ',hbron,htt,htt-hbron,-999.0

! Compute, given a source-receptor direction, isec_in = the wind sector where this direction lies in, 
! (isec1,isec2) = the wind sectors between which to interpolate and the s_wind = interpolation factor, taking into account plume rise and wind shear.
CALL windsek(istab, htt, disx, iwd, astat, isec_prelim, isec1, shear, htot, iwdd, isec_in, isec2, s_wind)

! Compute itra = distance class with travel distance ~nearest to the current source-receptor distance;
! ids = distance class where the current source-receptor distance lies in
! and interpolation factors (s_dist) for each trajectory distance class and current source-receptor distance.
CALL interp_ctr(disx, trafst, itra, s_dist, ids)

! Set interpolation factor for wind sector (s_wind) in the case that a wind sector that does not occur.
! This may happen, especially when we have meteo statistics for shorter periods (e.g. month).
! For area sources, surrounding wind sectors have to be taken into account. 
! For point sources, there is no contribution if a wind sector does not occur.
CALL windcorr(itra, istab, radius, disx, isec1, iwdd, isec2, astat, isec_in, ispecial, phi, s_wind)

! Compute for the current stability class and for all distance classes 
! tal = number of hours of wind blowing from source to receptor (interpolated between sector isec1 and isec2). 

!

!  s_wind: interpolation factor (0-1) for the contribution of sector isec2 to the direction iwdd; 
!          first half of sector:                  s_wind = 0.5 - 1; 
!          second half of sector:                 s_wind = 0 - 0.5. 
!          For a parameter p, interpolation is given by  p(iwdd) = s_wind*p(isec2) + (1-s_wind)*p(isec1)
!
DO itraj = 1, NTRAJ

   !  If one of the interpolating sectors does not occur, set interpolation factor to 0 or 1 (sp = NINT(s_wind))
   !  first half of sector:                  s_wind = 1; p(iwdd) = p(isec2)
   !  second half of sector:                 s_wind = 0; p(iwdd) = p(isec1)
   ! Note: astat(:,1,:,:) = number of hours for distance/stability/wind direction class
   IF ((ABS(astat(itraj, 1, istab, isec1)) .LE. EPS_DELTA) .OR. (ABS(astat(itraj, 1, istab, isec2)) .LE. EPS_DELTA)) THEN
      sp = NINT(s_wind)

      ! If the current distance class equals the class where the receptor lies in -> 
      ! special case, reset s_wind to sp, which is 0 or 1
      IF (itra .EQ. itraj) THEN
         s_wind   = sp
         ispecial = 1
      ENDIF
   ELSE

      ! Both interpolating sectors occur or do not occur. In the first case, set interpolation factor sp = s_wind.
      ! In the latter case, aant will be zero and there is no contribution from this source (value of sp is irrelevant).
      sp = s_wind
   ENDIF

   ! Interpolate (between wind sectors isec1 and isec2) number of hours that this meteo class occurs:
   tal(itraj) = (1. - sp)*astat(itraj, 1, istab, isec1) + (sp*astat(itraj, 1, istab, isec2))
ENDDO

! Interpolate over distance classes; 
! note that aant is interpolated over both distance classes (ids-1/ids) and wind sectors (isec1/isec2).
CALL interp_tra(itra, s_dist, ids, istab, isec_in, tal, astat, itrx, aant, stt)

! Continue if interpolated number of hours of the {{ids-1/ids),istab,(isec1/isec2)} combination is larger than 0
IF (aant > EPS_DELTA) THEN
!
! Interpolate meteo parameters over wind sectors 
! Note that for distance class itra, s_wind = 0 or 1 if one of the sectors does not occur
!
  CALL interp_sek(istab, isec_in, itrx, isec2, s_wind, isec1, stt, astat, xl, vw10, rb_ms, ra_ms_4, ra_ms_zra, xvglbr, xvghbr, uster_metreg_rcp,  &
               &  temp_C, ol_metreg_rcp, h0, xloc, xl100, rad, rc_so2_ms, hum, pcoef, rc_nh3_ms, rc_no2_ms, rc_aer_ms, &
               &  buil, rint, shear, dscor, coef_space_heating, regenk)
!
! Compute the effective travel distance between source and receptor
!
  CALL bepafst(itra, s_dist, trafst, disx, dscor, xl, disxx)
!
! Check for area source and special case
!
  IF ((radius .GT. EPS_DELTA) .AND. (ispecial .NE. 1)) THEN
!
!    Area source and not a special case;
!    average meteo parameters contributions of an area source from multiple contributing sectors to a receptor.
!    Two types of average are computed: sa,stta = near-source average, so,sstr = near-receptor average,
!    where sa, so = average number of hours of contributing meteo classes, 
!       stta,sttr = average meteo parameter of contributing meteo classes.
!
     CALL ronafhpar(radius, disxx, istab, s_wind, isec1, astat, s_dist,ids, sa, phi, so, stta, sttr)
     IF (so .GT. (0. + EPS_DELTA)) sttr = sttr/so 
!
!    Compute r , a measure for the distance between the receptor and the edge of the area source
!    disxx << radius : r -> 1 (near source) 
!    disxx >= radius : r = 0
!
     IF (radius .GT. (disxx + EPS_DELTA)) THEN
        r = (radius - disxx)**.33/(radius**.33)
     ELSE
        r = 0
     ENDIF
!
!    Area source: interpolate parameters between near-source values sa (number of hours),stta (other meteo parameters)
!    and near-receptor values so (number of hours),sttr (other meteo parameters). s_wind, so, stta and sttr are all averaged
!    over all sectors that contribute from area source to the receptor.
!    Note that the interpolated number of hours is stored in variable so.

!
!    Note: rc_so2_ms is not used for SO2; instead OPS uses DEPAC RC-values. It is only used in the statement Rc(HNO2) = Rc(SO2)
!
     so               = (r*sa)       + (1. - r)*so
     xl               = (r*stta(2))  + (1. - r)*sttr(2)
     rb_ms            = (r*stta(4))  + (1. - r)*sttr(4)
     r4               = (r*stta(5))  + (1. - r)*sttr(5)
     r50              = (r*stta(6))  + (1. - r)*sttr(6)
     regenk           = (r*stta(11)) + (1. - r)*sttr(11)
     rad              = (r*stta(14)) + (1. - r)*sttr(14)
     rc_so2_ms        = (r*stta(16)) + (1. - r)*sttr(16)
     uster_metreg_rcp = (r*stta(19)) + (1. - r)*sttr(19)
     temp_C           = (r*stta(20)) + (1. - r)*sttr(20)
     ol_metreg_rcp    = (r*stta(22)) + (1. - r)*sttr(22)
     h0               = (r*stta(23)) + (1. - r)*sttr(23)
     rc_no2_ms        = (r*stta(25)) + (1. - r)*sttr(25)
     rc_nh3_ms        = (r*stta(26)) + (1. - r)*sttr(26)
     rc_aer_ms        = (r*stta(27)) + (1. - r)*sttr(27)
     ra_ms_4          = r4  - rb_ms
     ra_ms_zra        = r50 - rb_ms
     
     
  ELSE
     ! Not an area source or a special case; 
     ! so = aant = interpolated number of hours of the {{ids-1/ids),istab,(isec1/isec2)} combination 
     so = aant
  ENDIF
!
! ccor = correction factor number of hours for an area sources;
! (ccor = 1 if not an area source or a special case)
!
  ccor = so/aant
!
! aant = number of hours of occurrence of interpolation of distance classes/wind sectors for the current stability class
! percvk = fraction of occurrence of {distance/stability/wind-direction} class
!          i.e. (number of hours that a {distance/stability/wind-direction} class occurs) / (total number of hours)
!
  percvk = (aant*ccor)/uurtot
  !WRITE(*,'(3a,4(1x,i6),99(1x,e12.5))') & 
  !    trim(ROUTINENAAM),',A,',' itrx,istab,isec_in,isec_prelim,aant,ccor,uurtot,percvk: ', &
  !                              itrx,istab,isec_in,isec_prelim,aant,ccor,uurtot,percvk

ELSE

!
! aant <= 0 -> set aant and percvk to 0
!
  aant   = 0
  percvk = 0
ENDIF

! Error when percvk < 0
IF (percvk < - EPS_DELTA) GOTO 1000

RETURN

1000 CALL SetError('Negative value for percvk encountered', error)
CALL ErrorParam('percvk', percvk, error)
CALL ErrorParam('aant', aant, error)
CALL ErrorParam('ccor', ccor, error)
CALL ErrorParam('uurtot', uurtot, error)
CALL ErrorParam('so', so, error)

9999 CALL ErrorCall(ROUTINENAAM, error)
RETURN

!-------------------------------------------------------------------------------------------------------------------------------

CONTAINS

!-------------------------------------------------------------------------------------------------------------------------------
! SUBROUTINE         : bepafst
! DESCRIPTION        : Compute the effective travel distance between source and receptor. This is done by 
!                      interpolating the ratio (effective travel distance)/(linear distance) for the distance 
!                      class where the linear source-receptor distance lies in.
!-------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE bepafst(itra, s_dist, trafst, disx, dscor, xl, disxx)

! CONSTANTS
CHARACTER*512                                    :: ROUTINENAAM                ! 
PARAMETER      (ROUTINENAAM = 'bepafst')

! SUBROUTINE ARGUMENTS - INPUT
INTEGER*4, INTENT(IN)                            :: itra                       ! distance class with travel distance ~nearest to the current source-receptor distance
REAL*4,    INTENT(IN)                            :: s_dist(NTRAJ)              ! interpolation factor for each distance class (interpolates data between lower and upper class boundary).
REAL*4,    INTENT(IN)                            :: trafst(NTRAJ)              ! travel distances for each distance class [m]
REAL*4,    INTENT(IN)                            :: disx                       ! linear distance between source and receptor ('as the crow flies') [m]

! SUBROUTINE ARGUMENTS - I/O
REAL*4,    INTENT(INOUT)                         :: dscor(NTRAJ)               ! Note: dscor is not used anymore after this routine
REAL*4,    INTENT(INOUT)                         :: xl                         ! 

! SUBROUTINE ARGUMENTS - OUTPUT
REAL*4,    INTENT(OUT)                           :: disxx                      ! effective travel distance between source and receptor [m]

! LOCAL VARIABLES
INTEGER*4                                        :: ids_loc                    ! local value of ids, such that trafst(ids_loc) < disx <= trafst(ids_loc+1)
INTEGER*4                                        :: itraj                      ! index of distance class

!-------------------------------------------------------------------------------------------------------------------------------
!
! Default value for zero dscor values, indicating a non-existent distance/stability/wind-sector class.
! dscor is in km, trafst is in m       
! see METPRO -> trafst = 0, 100, 300, 1000 km
!
DO itraj = 2, NTRAJ
  IF (ABS(dscor(itraj) - 0.) .LE. EPS_DELTA) THEN
     dscor(itraj) = trafst(itraj)/1000.
  ENDIF
ENDDO

! No correction for itra = 1
IF (itra .EQ. 1) THEN
   disxx = disx
ELSE
  
  !-------------------------
  ! itra > 1
  !-------------------------
  ! Determine ids_loc = index at which trafst(ids_loc) < disx <= trafst(ids_loc+1); ids_loc is minimal 2.
  ! Note: ids_loc is local variable here
  ! Note: in interp_ctr, ids is determined such that trafst(ids-1) < disx <= trafst(ids);
  !       this means that ids_loc = ids-1 (and extra demand that ids_loc >= 2, because for itra = 1 
  !       (near source) ids_loc is not needed; there we have disxx = disx)
  
  !
  !                                                         0 < disx <= trafst(2) -> ids = 2
  !         0 < disx <= trafst(3) -> ids_loc = 2    trafst(2) < disx <= trafst(3) -> ids = 3
  ! trafst(3) < disx <= trafst(4) -> ids_loc = 3    trafst(3) < disx <= trafst(4) -> ids = 4
  ! trafst(4) < disx              -> ids_loc = 4     
  
  ids_loc = NTRAJ
  DO WHILE (ids_loc.GT.2 .AND. disx .LE. (trafst(ids_loc) + EPS_DELTA))
    ids_loc = ids_loc - 1
  ENDDO ! {ids_loc <= 2 OR disx > trafst(ids_loc)}
!
! Check whether ids_loc = NTRAJ (or equivalently disx > trafst(NTRAJ)) 
!
  IF (ids_loc.EQ.NTRAJ) THEN
!
!   Outside largest distance (1000 km); compute xl and disxx

!   xl    : maximum mixing height [m]
!   disxx : effective travel distance between source and receptor [m]
!   disx  : linear distance between source and receptor ('as the crow flies') [m]
!   dscor : effective travel distance for each distance class for current stability class and current wind direction 
!          (dscor has been interpolated between wind sectors) [km]
!   trafst: travel distance for each distance class [m]
! 
!   The maximum mixing height increases with travelled distance. If travelled distance is larger,
!   the chance of meeting a higher mixing height is larger, the mixing volume is larger; compression
!   does not take place. 
!   Mixing height growth factor 0.3 from analysis of mixing height computation as function of travelled distance. 
!   
!   Extrapolation after trafst(NTRAJ) is more than dscor(NTRAJ) "more meanders in travel path the further you go"
!
!                           disx               
!   xl = xl * [1 + 0.3*(------------- - 1)] ,linear growth in mixing height, each 1000 km a growth of 
!                       trafst(NTRAJ)        0.3 times the original mixing height (assuming trafst(NTRAJ) = 1000 km).    
!
!    disxx            dscor(NTRAJ)           disx
!   -------  = 1 + [ -------------- - 1] * -------------
!    disx            trafst(NTRAJ)         trafst(NTRAJ)
! 
    xl    = xl*(1. + (disx/trafst(NTRAJ) - 1.)*.3)
    disxx = disx*(1. + (dscor(NTRAJ)*1000./trafst(NTRAJ) - 1.)* disx/trafst(NTRAJ))
  ELSE
!
!   Not outside largest distance; interpolation
!
!   If we leave out the conversion km -> m
!
!   disxx      dscor(ids_loc)                         dscor(ids_loc+1)
!   ------ = {---------------*[1-s_dist(ids_loc+1)] + ----------------*s_dist(ids_loc+1) }; 
!   disx      trafst(ids_loc)                         trafst(ids_loc+1)
!
!   s_dist is interpolation factor for distance class.
!   Compare with aant = (1. - s_dist(ids))*tal(ids-1) + s_dist(ids)*tal(ids)
!
    disxx = disx*(dscor(ids_loc)*1000./trafst(ids_loc)*(1. - s_dist(ids_loc+1)) + dscor(ids_loc+1)*1000./trafst(ids_loc+1)*s_dist(ids_loc+1))
  ENDIF
ENDIF

RETURN
END SUBROUTINE bepafst

!-------------------------------------------------------------------------------------------------------------------------------
! SUBROUTINE         : ronafhpar
! DESCRIPTION        : Average meteo parameters contributions of an area source from multiple contributing sectors to a receptor.
!                      Two types of average are computed: sa,stta = near-source average, so,sstr = near-receptor average,
!                      where sa, so = average number of hours of contributing meteo classes, 
!                         stta,sttr = average meteo parameter of contributing meteo classes.
!                      (ronafhpar: r << "richting" = direction, onafh << "onafhankelijk" = independent)
!-------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE ronafhpar(radius, disxx, istab, s_wind, isec1, astat, s_dist, ids, sa, phi, so, stta, sttr)

! CONSTANTS
CHARACTER*512                                    :: ROUTINENAAM                ! 
PARAMETER      (ROUTINENAAM = 'ronafhpar')

! SUBROUTINE ARGUMENTS - INPUT
REAL*4,    INTENT(IN)                            :: radius                     ! 
REAL*4,    INTENT(IN)                            :: disxx                      ! 
INTEGER*4, INTENT(IN)                            :: istab                      ! 
REAL*4,    INTENT(IN)                            :: s_wind                     ! interpolation factor (0-1) for wind sectors
INTEGER*4, INTENT(IN)                            :: isec1                      ! first of two interpolating wind sectors 
REAL*4,    INTENT(IN)                            :: astat(NTRAJ, NCOMP, NSTAB, NSEK) ! 
REAL*4,    INTENT(IN)                            :: s_dist(NTRAJ)              ! interpolation factor for each distance class (interpolates data between lower and upper class boundary)
INTEGER*4, INTENT(IN)                            :: ids                        ! 

! SUBROUTINE ARGUMENTS - OUTPUT
REAL*4,    INTENT(OUT)                           :: sa                         ! 
REAL*4,    INTENT(OUT)                           :: phi                        ! 
REAL*4,    INTENT(OUT)                           :: so                         ! 
REAL*4,    INTENT(OUT)                           :: stta(NCOMP)                ! 
REAL*4,    INTENT(OUT)                           :: sttr(NCOMP)                ! 

! LOCAL VARIABLES
INTEGER*4                                        :: i                          ! 
INTEGER*4                                        :: icomp                      ! 
INTEGER*4                                        :: lpsek                      ! 
INTEGER*4                                        :: jsek                       ! 
INTEGER*4                                        :: scomp(14)                  ! 
REAL*4                                           :: a                          ! 
REAL*4                                           :: asek                       ! 
REAL*4                                           :: statfactor                 ! 
REAL*4                                           :: zz                         ! 
REAL*4                                           :: p1                         ! 
REAL*4                                           :: p2                         ! 
REAL*4                                           :: pa                         ! 

! DATA
!     De arrayelementen uit de meteostatistiek die hier gebruikt worden.
DATA scomp / 2, 4, 5, 6, 11, 14, 16, 19, 20, 22, 23, 25, 26, 27 /

!-------------------------------------------------------------------------------------------------------------------------------
!
!  The following meteo parameters are averaged:
!
!         2. maximal mixing height over transport distance [m]
!         4. boundary layer resistance Rb for SO2 [s/m]
!         5. aerodynamic resistance 4 m + boundary layer resistance [s/m]
!         6. aerodynamic resistance 50 m + boundary layer resistance [s/m]
!         11. precipitation probability []
!         14. global radiation [W/m2]
!         16. surface resistance Rc for SO2 [s/m] 
!         19. friction velocity u* [m/s]
!         20. temperature T [degree C]
!         22. Monin-Obukhov length L [m]
!         23. sensible heat flux H0 [W/m2]
!         25. surface resistance Rc of NO2 [s/m]
!         26. surface resistance Rc of NH3 [s/m]
!         27. surface resistance Rc of aerosol [s/m] <\astat>
!
! Currently not averaged:
! wish for
! future version
! (HvJ)
! no        1. number of hours for which a certain combination of classes has occurred [-]
! yes       3. wind speed (at 10 m height) [m/s]
! no        7. ratio effective dry deposition velocity over transport distance and average dry deposition velocity over transport distance for low sources [-]
! no        8. ratio effective dry deposition velocity over transport distance and average dry deposition velocity over transport distance for high sources [-]
! no        9. effective travel distance []
! yes      10. degree day or domestic heating coefficient (= 19-T for T < 12 degree C) [degree C] 
! no       12. length of rainfall period []
! no       13. rain intensity []
! yes      15. wind speed power law coefficient [-]
! no       17. percentage of total hours that certain stability/mixing height class occurs per 2 hour block, source oriented [%]
! no       18. percentage of total hours that certain stability/mixing height class occurs per 2 hour block, receptor oriented [%]
! no       21. turning angle for wind shear (at reference height) []
! yes      24. relative humidity [%]

!
! Initialise summed parameters:
sa   = 0.
so   = 0.
stta = 0.
sttr = 0.

IF (radius .GE. (disxx - EPS_DELTA)) THEN
!
!  Receptor inside area source; compute direction independent averages for several (meteo) parameters.
!  Compute weighed average stta for distance class 1 (vicinity of source) with weighing factor = 
!  = statfactor = number of hours for combination {distance class=1,stability class, wind sector}.
!  sa = number of hours of {distance class=1,stability class, wind sector}, summed over all wind sectors 
!  Note: astat(:,1,:,:) = number of hours for distance/stability/wind direction class

!
!  asek = number of contributing sectors
!
   asek = 6.
   DO lpsek = 1, NSEK
      statfactor = astat(1, 1, istab, lpsek)
      sa = sa + astat(1,  1, istab, lpsek)
      DO icomp = 1, 14
        stta(scomp(icomp)) = stta(scomp(icomp)) + astat(1, scomp(icomp), istab, lpsek) * statfactor
      ENDDO
   ENDDO
   DO icomp = 1, 14
      stta(scomp(icomp)) = stta(scomp(icomp))/sa
   ENDDO

   ! Later on we count only contributions from 6 wind sectors, to keep consistency here we only use half of the hours;
   ! note that the receptor is inside the area source, so strictly speaking we have contributions from all sectors. 
   sa = sa/2
   
ELSE
!
!  Receptor outside source: compute number of contributing sectors (3.34, 3.36 new OPS report).
!  In this case, averaged meteo parameter in vicinity of source = stta = 0 
!
   zz   = SQRT(disxx*disxx - radius*radius)
   phi  = ATAN2(zz, radius)
   phi  = (2.*PI) - 2.*(phi + 5.*PI/12.) ! = 2*pi - 2*[atan(sqrt(x^2-R^2)/R) + (5/12)*pi] 
   asek = (phi*6.)/PI
   IF (asek .GT. (6. + EPS_DELTA)) THEN
      asek = 6.  
   ENDIF
ENDIF

! Extra distance correction
a = radius*1.7 
IF (disxx .LE. (radius - EPS_DELTA)) THEN
   ! inside area source
   asek = 1 + (asek - 1)*((a - disxx)/a)**0.33 
ELSE
   r    = (radius/disxx)**3
   asek = 1 + (asek - 1)*(r*((a - radius)/a)**0.33+(1 - r))
ENDIF
!
! Loop over neighbouring sectors (3 sectors before isec1) to (3 sectors after isec1)
! and average meteo parameters for current source-receptor distance (interpolation
! between distance classes ids-1 and ids). Note that isec1 is the first of the two interpolating wind sectors.
!
DO i = -3, 3
   
   ! pa is correction factor to correct for the deviation of the neighbouring sector
   ! w.r.t to the source-receptor direction;
   ! 0 <= pa <= 1 and for i = 0 (central sector): pa = asek/2 - s_wind + 1/2
   ! 
   ! isec_in: wind sector in which the source-receptor direction lies, taking into account wind shear.
   ! s_wind between 0 and 0.5 -> source-receptor direction lies in isec1, so isec1 is the middle of the contributing sectors
   ! s_wind between 0.5 and 1 -> source-receptor direction lies in isec1+1, so isec1+1 is the middle of the contributing sectors
   !
   pa = (asek/2.) - ABS(FLOAT(i) - s_wind) + .5
   
   IF (pa .GT. (0. + EPS_DELTA)) THEN
      IF (pa .GT. (1. + EPS_DELTA)) THEN
         pa = 1.
      ENDIF

      ! Set jsek = index of neighbouring sector:
      jsek = isec1 + i
      IF (jsek .LT. 1) THEN
         jsek = jsek + NSEK
      ELSE IF (jsek .GT. NSEK) THEN
         jsek = jsek - NSEK
      ELSE
         CONTINUE
      ENDIF
!
!     Compute weighing factors p1 and p2 for interpolation between distance classes ids-1 and ids resp.
!     s_dist(ids): interpolation factor for distance class ids
!     Note: astat(:,1,:,:) = number of hours for distance/stability/wind direction class
!
      p1  = pa*(1. - s_dist(ids))*astat(ids-1, 1, istab, jsek) 
      p2  = pa*s_dist(ids)*astat(ids, 1, istab, jsek)
      DO icomp = 1, 14
         sttr(scomp(icomp)) = sttr(scomp(icomp)) + p1*astat(ids-1, scomp(icomp), istab, jsek)+ p2*astat(ids, scomp(icomp),     &
                    &  istab, jsek)
      ENDDO
!
!     so = pa * number of hours in stability class istab, interpolated between distance classes ids and ids-1, 
!          accumulated over windsectors isec1-3,isec1-2, ... jsek
!
      so = so + p1 + p2
   ENDIF
   IF (pa .LT. (0. - EPS_DELTA)) THEN
      pa = 0.
   ENDIF
   
ENDDO
!
! End of loop 
! so = pa * number of hours in stability class istab, interpolated between distance classes ids and ids-1, 
!      accumulated over al contributing windsectors isec1-3,isec1-2, ... , isec1+3.

RETURN
END SUBROUTINE ronafhpar

!-------------------------------------------------------------------------------------------------------------------------------
! SUBROUTINE         : windsek
! DESCRIPTION        : Compute, given a source - receptor direction, the wind sector where this direction lies in,
!                      the wind sectors between which to interpolate and the interpolation factor,
!                      taking into account plume rise and wind shear
!-------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE windsek(istab, htt, disx, iwd, astat, isec_prelim, isec1, shear, htot, iwdd, isec_in, isec2, s_wind)

! CONSTANTS
CHARACTER*512                                    :: ROUTINENAAM                ! 
PARAMETER      (ROUTINENAAM = 'windsek')

! SUBROUTINE ARGUMENTS - INPUT
INTEGER*4, INTENT(IN)                            :: istab                      ! index of stability class
REAL*4,    INTENT(IN)                            :: htt                        ! plume height at source, including plume rise [m]
REAL*4,    INTENT(IN)                            :: disx                       ! source receptor distance
INTEGER*4, INTENT(IN)                            :: iwd                        ! wind direction if wind is from source to receptor (degrees)
REAL*4,    INTENT(IN)                            :: astat(NTRAJ, NCOMP, NSTAB, NSEK) ! meteo statistics
INTEGER*4, INTENT(IN)                            :: isec_prelim                ! index of preliminary source-receptor wind sector (wind shear not yet taken into account)

! SUBROUTINE ARGUMENTS - OUTPUT
INTEGER*4, INTENT(OUT)                           :: isec1                      ! first of two interpolating wind sectors
REAL*4,    INTENT(OUT)                           :: shear                      ! 
REAL*4,    INTENT(OUT)                           :: htot                       ! plume height at receptor, including plume descent due to heavy particles [m]
INTEGER*4, INTENT(OUT)                           :: iwdd                       ! wind direction if wind is from source to receptor (degrees), including turning angle correction
INTEGER*4, INTENT(OUT)                           :: isec_in                    ! source-receptor wind sector, taking into account wind shear  
INTEGER*4, INTENT(OUT)                           :: isec2                      ! second of two interpolating wind sectors  
REAL*4,    INTENT(OUT)                           :: s_wind                     ! interpolation factor (0-1) for the contribution of wind sector 

! LOCAL VARIABLES
REAL*4                                           :: alpha                      ! 
REAL*4                                           :: sek                        ! 

!-------------------------------------------------------------------------------------------------------------------------------
!
! Compute wind shear for higher altitudes.
! Use measured wind directions (20-200 m) at Cabauw. The empirical relation of Van Ulden & Holtslag is used for the
! whole mixing layer, also above 200 m. 
!

! Get turning angle for wind shear at reference height
shear = astat(1, 21, istab, isec_prelim)

! Get plume height (incl. plume rise); minimal 10 m
IF (htt .LT. (10. - EPS_DELTA)) THEN
   htot = 10.
ELSE
   htot = htt
ENDIF
!
! Compute turning angle at plume height (Van Ulden & Holtslag (1985))
! (2.10 OPS report); here zref = 180 m = height of Cabauw tower
!
alpha = 1.58*shear*(1.-EXP(-1.0*htot/180))

!
! Compute wind direction iwdd between source and receptor, including turning angle correction.
! For a large source-receptor distance, the turning angle correction goes to zero.
! disx >= 100000: iwd = iwdd
! disx -> 100000: (disx/100000) -> 1, iwdd -> iwd
! disx -> 0     : (disx/100000) -> 0, iwdd -> iwd - alpha
!
IF (disx .LT. (100000. - EPS_DELTA)) THEN
   iwdd = iwd - NINT(alpha*(1. - disx/100000.))
ELSE
   iwdd = iwd
ENDIF
IF (iwdd .GE. 360) THEN
   iwdd = iwdd - 360
ELSE IF (iwdd .LT. 0) THEN
   iwdd = iwdd + 360
ELSE
   CONTINUE
ENDIF
!
! Compute indices of wind sector for corrected wind direction iwdd;
! sek    : wind direction index (float), 1.0 <= sek <= 13.0
! isec_in: index of wind direction sector where iwdd lies in; isec_in = NINT((sek)
! isec1  : index of first sector for interpolation (first in clockwise direction);
!          if iwdd is in the first half of the sector, this is the sector before the sector where iwdd lies in;
!          if iwdd is in the second half of the sector, this is the sector where iwdd lies in.
!          isec1 = INT(sek)
! isec2  : index of second sector for interpolation (second in clockwise direction); isec2 = isec1 + 1
! s_wind : interpolation factor (0-1) for the contribution of sector isec2 to the direction iwdd; 
!          first half of sector:                  s_wind = [0.5, 1]
!          halfway sector, coming from 0 degrees: s_wind = 1
!          halfway sector, going to 360 degrees:  s_wind = 0 
!          second half of sector:                 s_wind = [0, 0.5]. 
!          For a parameter p, interpolation is given by p(iwdd) = s_wind*p(isec2) + (1-s_wind)*p(isec1)
!
! iwdd  sek  isec_in  isec1 isec2  s_wind
! 000   1.00    1       1     2   0.00
! 005   1.17    1       1     2   0.17   sector 1: 345 - 15
! 010   1.33    1       1     2   0.33
!                                 
! 015   1.50    2       1     2   0.50
! 020   1.67    2       1     2   0.67
! 025   1.83    2       1     2   0.83   sector 2: 15 - 45
! 030   2.00    2       2     3   0.00
! 035   2.17    2       2     3   0.17
! 040   2.33    2       2     3   0.33
!                                 
! 045   2.50    3       2     3   0.50
! 050   2.67    3       2     3   0.67   sector 3: 45 - 75
! 055   2.83    3       2     3   0.83
!                                 
! ......................................................
!                                 
! 325  11.83   12      11    12   0.83
! 330  12.00   12      12     1   0.00   sector 12: 315 - 345
! 335  12.17   12      12     1   0.17
! 340  12.33   12      12     1   0.33
!                                 
! 345  12.50    1      12     1   0.50
! 350  12.67    1      12     1   0.67   sector 1: 345 - 15
! 355  12.83    1      12     1   0.83
! 360  13.00    1       1     2   0.00
!
sek     = FLOAT(iwdd)*NSEK/360. + 1.
isec_in = NINT(sek)
isec1   = INT(sek)
isec2   = isec1 + 1
s_wind  = sek - FLOAT(isec1)

! Correction for sector index > NSEK
IF (isec2 .GT. NSEK) THEN
   isec2 = isec2 - NSEK
ENDIF
IF (isec1 .GT. NSEK) THEN
   isec1 = isec1 - NSEK
ENDIF
IF (isec_in .GT. NSEK) THEN
   isec_in = isec_in - NSEK
ENDIF

RETURN
END SUBROUTINE windsek

!-------------------------------------------------------------------------------------------------------------------------------
! SUBROUTINE         : windcorr
! DESCRIPTION        : Correct wind direction sector for area sources in the case that a wind sector that does not occur.
!                      This may happen, especially when we have meteo statistics for shorter periods (e.g. month).
!                      For area sources, surrounding wind sectors have to be taken into account.
!                      If for a point source, the wind sector does not occur, there is no contribution from this wind sector.
!-------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE windcorr(itra, istab, radius, disx, isec1, iwdd, isec2, astat, isec_in, ispecial, phi, s_wind)

USE Binas, only: rad2deg

! CONSTANTS
CHARACTER*512                                    :: ROUTINENAAM                ! 
PARAMETER      (ROUTINENAAM = 'windcorr')

! SUBROUTINE ARGUMENTS - INPUT
INTEGER*4, INTENT(IN)                            :: itra                       ! distance class with travel distance ~nearest to the current source-receptor distance
INTEGER*4, INTENT(IN)                            :: istab                      ! 
REAL*4,    INTENT(IN)                            :: radius                     ! 
REAL*4,    INTENT(IN)                            :: disx                       ! 
INTEGER*4, INTENT(IN)                            :: isec1                      ! first of two interpolating wind sectors 
INTEGER*4, INTENT(IN)                            :: iwdd                       ! wind direction if wind is from source to receptor (degrees), including turning angle correction
INTEGER*4, INTENT(IN)                            :: isec2                      ! second of two interpolating wind sectors 
REAL*4,    INTENT(IN)                            :: astat(NTRAJ, NCOMP, NSTAB, NSEK) ! 

! SUBROUTINE ARGUMENTS - I/O
INTEGER*4, INTENT(INOUT)                         :: isec_in                    ! source-receptor wind sector, taking into account wind shear  

! SUBROUTINE ARGUMENTS - OUTPUT
INTEGER*4, INTENT(OUT)                           :: ispecial                   ! 
REAL*4,    INTENT(OUT)                           :: phi                        ! is not used as output
REAL*4,    INTENT(OUT)                           :: s_wind                     ! interpolation factor (0-1) for the contribution of wind sector 

! LOCAL VARIABLES
INTEGER*4                                        :: iwr                        ! 

!-------------------------------------------------------------------------------------------------------------------------------
!
! Sector/wind direction correction for area sources, in case a wind sector does not occur and the neighbouring sector(s) do(es).
! Area sources can have more than one contributing sectors. 


! Default nothing special
ispecial = 0

! If meteo class, where source-receptor direction lies in (isec_in), does not occur AND area source
! Note: astat(:,1,:,:) = number of hours for distance/stability/wind direction class
IF ((ABS(astat(itra, 1, istab, isec_in)) .LE. EPS_DELTA) .AND. (radius .GT. (0. + EPS_DELTA))) THEN

   ! radius < disx  (receptor outside area source) ->
   ! radius >= disx (receptor inside area source)  -> phi = 60 degrees
   ! disx >> radius, then radius/disx -> 0, asin(radius/disx) -> 0, phi -> pi/NSEK = 15 degrees (NSEK = 12) 
   ! disx = radius,  then radius/disx -> 1, asin(radius/disx) -> pi/2, phi -> 105 degrees
   ! disx < radius,  then phi = 60 degrees
   ! 
   IF (radius .LT. (disx - EPS_DELTA)) THEN
      phi = (ASIN(radius/disx) + PI/NSEK)*rad2deg
   ELSE
      phi = 60. 
   ENDIF

   ! iwr = wind direction halfway sector isec1   
   iwr = (isec1 - 1)*360/NSEK
!   
!  Criteria for special case:
!  1. iwr < iwdd < iwr + phi (source-receptor direction lies in second half of isec1
!  2. meteo class isec1, which is used as first sector for interpolation, does not occur (astat(itra, 1, istab, isec1) = 0)
!  Note that 1. is split into iwdd > iwr AND phi > iwdd - iwr
!  In this case, set isec_in (wind sector that iwdd is in) = isec1 and interpolation factor s_wind = 0
! 

! BUG has only effect for shorter periods; for year runs a non-occurring class does not occur ...
! Note that if ispecial = 1, the contribution of neighbouring sectors is not taken into account (see ronafhpar). 

!
   IF ((iwdd .GT. iwr) .AND. (ABS(astat(itra, 1, istab, isec1)) .LE. EPS_DELTA) .AND.                                           &
     &  phi .GT. (FLOAT(iwdd - iwr) + EPS_DELTA)) THEN
      isec_in  = isec1
      s_wind   = 0.
      ispecial = 1
!
!  Criteria for special case:
!  1. iwr - phi < iwdd < iwr (source-receptor direction lies in first half of isec1
!  2. meteo class isec2, which is used as second sector for interpolation, does not occur (astat(itra, 1, istab, isec2) = 0)
!  Note that 1 is split into iwr > iwdd AND phi > iwr - iwdd
!  In this case, set isec_in (wind sector that iwdd is in) = isec2 and interpolation factor s_wind = 1 
!

! BUG has only effect for shorter periods; for year runs a non-occurring class does not occur ...
!
   ELSE IF ((iwr .GT. iwdd) .AND. (ABS(astat(itra, 1, istab, isec2)) .LE. EPS_DELTA) .AND.                                        &
          &  phi .GT. (FLOAT(iwr - iwdd) + EPS_DELTA)) THEN
      isec_in  = isec2
      s_wind   = 1.
      ispecial = 1
   ELSE
      CONTINUE
   ENDIF
ENDIF

RETURN
END SUBROUTINE windcorr

!-------------------------------------------------------------------------------------------------------------------------------
! SUBROUTINE         : interp_ctr
! DESCRIPTION        : compute interpolation coefficients for different trajectory distance classes
!                      and current source-receptor distance.
!-------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE interp_ctr(disx, trafst, itra, s_dist, ids)

! CONSTANTS
CHARACTER*512                                    :: ROUTINENAAM                ! 
PARAMETER      (ROUTINENAAM = 'interp_ctr')

! SUBROUTINE ARGUMENTS - INPUT
REAL*4,    INTENT(IN)                            :: disx                       ! source recptor distance [m]m 
REAL*4,    INTENT(IN)                            :: trafst(NTRAJ)              ! travel distances for each distance class [m]

! SUBROUTINE ARGUMENTS - OUTPUT
INTEGER*4, INTENT(OUT)                           :: itra                       ! distance class with travel distance ~nearest to the current source-receptor distance
REAL*4,    INTENT(OUT)                           :: s_dist(NTRAJ)              ! interpolation factor for each distance class (interpolates data between lower and upper class boundary) 
INTEGER*4, INTENT(OUT)                           :: ids                        ! class index, such that trafst(ids-1) < disx <= trafst(ids)

! LOCAL VARIABLES
INTEGER*4                                        :: i                          ! 

!-------------------------------------------------------------------------------------------------------------------------------
!
! First determine itra = distance class boundary ~ nearest to the current source-receptor distance;
! set D(i) = trafst(i) = distance class boundary, then itra switches at
!
!          D(i) - 2 D(i-1)    D(i)
! D(i-1) + --------------- = ------
!               2              2
!
!



!
! name current source-receptor distance d ( = disx), then 
!      0 < d <= D(2)/2 -> itra = 1
! D(2)/2 < d <= D(3)/2 -> itra = 2
! D(3)/2 < d <= D(4)/2 -> itra = 3
! D(4)/2 < d           -> itra = 4
 
itra = NTRAJ
DO i = NTRAJ,2,-1
  IF (disx .LE. (trafst(itra-1) + (trafst(itra)- 2.*trafst(itra-1))/2.+ EPS_DELTA)) itra = i - 1
ENDDO
!
! Compute sector index ids, such that trafst(ids-1) < disx <= trafst(ids)
! Name current source-receptor distance d = disx, then
!    0 < d <= D(2) -> ids = 2
! D(2) < d <= D(3) -> ids = 3
! D(3) < d <= D(4) -> ids = 4
!
! and interpolation factor s_dist(ids)
!
!                     D(ids) - d                d - D(ids-1)
! s_dist(ids)  = 1 - ------------------ = -------------------
!                     D(ids) - D(ids-1)    D(ids) - D(ids-1)
!
! d = D(ids-1) -> s_dist(ids) = 0
! d = D(ids)   -> s_dist(ids) = 1
!
! s_dist(i) = 0, for i /= ids 
!
IF (disx .LE. (trafst(2) + EPS_DELTA)) THEN
   ids = 2
    ! here D(1) is replaced by -1 m) == trafst(2)+1 = 100001 m does not make much difference
   s_dist(2) = 1. - (trafst(2) - disx)/(trafst(2) + 1.)
ELSE
   s_dist(2) = 0.
   ids = 3
   DO WHILE (ids .LT. NTRAJ .AND. disx .GE. (trafst(ids)-EPS_DELTA))
      s_dist(ids) = 0.
      ids = ids + 1
   ENDDO
   s_dist(ids) = 1. - (trafst(ids)-disx)/(trafst(ids)-trafst(ids-1))
ENDIF

! Interpolation factors for distance classes further away are 0
s_dist(ids+1:NTRAJ) = 0.

! Limit interpolation factor to 1 (for distances larger than trafst(NTRAJ))
IF (s_dist(NTRAJ) .GT. (1. + EPS_DELTA)) THEN
   s_dist(NTRAJ) = 1.
ENDIF

RETURN
END SUBROUTINE interp_ctr

!-------------------------------------------------------------------------------------------------------------------------------
! SUBROUTINE         : interp_tra
! DESCRIPTION        : Interpolate between distance classes for specific meteo parameters (2 <= ICOMP <= 8 OR ICOMP >= 19) 
!                      and store interpolated meteo parameters into stt (same index numbering as astat)
!-------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE interp_tra(itra, s_dist, ids, istab, isec_in, tal, astat, itrx, aant, stt)

! CONSTANTS
CHARACTER*512                                    :: ROUTINENAAM                ! 
PARAMETER      (ROUTINENAAM = 'interp_tra')

! SUBROUTINE ARGUMENTS - INPUT
INTEGER*4, INTENT(IN)                            :: itra                       ! distance class with travel distance ~nearest to the current source-receptor distance
REAL*4,    INTENT(IN)                            :: s_dist(NTRAJ)              ! interpolation factor for each distance class (interpolates data between lower and upper class boundary) 
INTEGER*4, INTENT(IN)                            :: ids                        ! distance class where source-rceptor distance is in (s(ids /= 0)
INTEGER*4, INTENT(IN)                            :: istab                      ! index of stability class
INTEGER*4, INTENT(IN)                            :: isec_in                    ! source-receptor wind sector, taking into account wind shear  
REAL*4,    INTENT(IN)                            :: tal(NTRAJ)                 ! number of hours of wind blowing from source to receptor (interpolated between sector isec1 and isec2)
REAL*4,    INTENT(IN)                            :: astat(NTRAJ, NCOMP, NSTAB, NSEK) ! 

! SUBROUTINE ARGUMENTS - OUTPUT
INTEGER*4, INTENT(OUT)                           :: itrx                       ! = itra, but if one of interpolating distance classes is missing, itrx is the non-missing class
REAL*4,    INTENT(OUT)                           :: aant                       ! number of hours of occurrence of interpolation of distance classes/wind sectors for the current stability class
REAL*4,    INTENT(OUT)                           :: stt(NCOMP)                 ! meteo parameters, interpolated between distance classes (ICOMP = 2-8, 19-NCOMP) 

! LOCAL VARIABLES
INTEGER*4                                        :: icomp                      ! 

!-------------------------------------------------------------------------------------------------------------------------------
!
! First initialise itrx to itra = index of distance class boundary ~nearest to the current source-receptor distance;

itrx = itra
!
! Interpolate between distance classes ids-1 and ids to get 
! aant = number of hours of occurrence of interpolation of distance classes/wind sectors for the current stability class
!
aant = (1. - s_dist(ids))*tal(ids-1) + s_dist(ids)*tal(ids)

! Define itrx = itra, but if itra is missing, itrx = the neighbour which exists
! if itra is missing:
!    itra = ids-1 -> itrx = ids   = itra+1
!    itra = ids   -> itrx = ids-1 = itra-1

! Check whether both distance classes (ids-1,ids) occur (note: astat(:,1,:,:) = number of hours for distance/stability/wind direction class):
IF ((ABS(astat(ids-1, 1, istab, isec_in)) .GT. EPS_DELTA) .AND. (ABS(astat(ids, 1, istab, isec_in)) .GT. EPS_DELTA)) THEN

   ! Both distance classes (ids-1, ids) occur -> interpolate meteo parameters 2-8 and 19-NCOMP between these two distance classes
   DO icomp = 2, NCOMP
      IF (.NOT. ((icomp .GT. 8) .AND. (icomp .LT. 19))) THEN
         stt(icomp) = (1. -  s_dist(ids)) * astat(ids-1, icomp, istab, isec_in) + s_dist(ids)*astat(ids, icomp, istab, isec_in)
      ENDIF
   ENDDO
   ! itrx  = itra (default value)

ELSE
   ! One of (ids-1,ids) does not occur -> no interpolation, but get value of neighbouring distance class that does exist (= itrx)
   ! If both distance classes (ids-1,ids) do not occur -> itrx = itra; in this case, stt will not be used because aant and percvk 
   !                                                      will be 0, meaning there is no contribution from this source.
   IF ((itra .EQ. ids-1) .AND. (ABS(astat(ids-1, 1, istab, isec_in)) .LE. EPS_DELTA)) THEN
      itrx = ids   ! itrx = itra+1
   ELSE IF ((itra .EQ. ids) .AND. (ABS(astat(ids, 1, istab, isec_in)) .LE. EPS_DELTA)) THEN
      itrx = ids-1 ! itrx = itra-1
   ELSE
      itrx = itra
   ENDIF

   ! No interpolation, use parameters 2-8, 19-NCOMP from distance class itrx:
   DO icomp = 2, NCOMP
      IF (.NOT. ((icomp .GT. 8) .AND. (icomp .LT. 19))) THEN
         stt(icomp) = astat(itrx, icomp, istab, isec_in)
      ENDIF
   ENDDO
ENDIF

RETURN
END SUBROUTINE interp_tra

!-------------------------------------------------------------------------------------------------------------------------------
! SUBROUTINE         : interp_sek
! DESCRIPTION        : In deze routine worden de meteostatistiek geinterpoleerd over de windsektoren.
!-------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE interp_sek(istab, isec_in, itrx, isec2, s_wind, isec1, stt, astat, xl, vw10, rb_ms, ra_ms_4, ra_ms_zra, xvglbr, xvghbr, uster_metreg_rcp,  &
                   &  temp_C, ol_metreg_rcp, h0, xloc, xl100, rad, rc_so2_ms, hum, pcoef, rc_nh3_ms, rc_no2_ms, rc_aer_ms, buil, rint, shear, &
                   &  dscor, coef_space_heating, regenk)    

! CONSTANTS
CHARACTER*512                                    :: ROUTINENAAM                ! 
PARAMETER      (ROUTINENAAM = 'interp_sek')

! CONSTANTS
INTEGER*4                                        :: MENGH(NSTAB)               ! menghoogte

! SUBROUTINE ARGUMENTS - INPUT
INTEGER*4, INTENT(IN)                            :: istab                      ! 
INTEGER*4, INTENT(IN)                            :: isec_in                    ! source-receptor wind sector, taking into account wind shear  
INTEGER*4, INTENT(IN)                            :: itrx                       ! 
INTEGER*4, INTENT(IN)                            :: isec2                      ! second of two interpolating wind sectors 
REAL*4,    INTENT(IN)                            :: s_wind                     ! interpolation factor (0-1) for the contribution of wind sector 
INTEGER*4, INTENT(IN)                            :: isec1                      ! first of two interpolating wind sectors 
REAL*4,    INTENT(IN)                            :: stt(NCOMP)                 ! meteo parameters, interpolated between distance classes (ICOMP = 2-8, 19-NCOMP)
REAL*4,    INTENT(IN)                            :: astat(NTRAJ, NCOMP, NSTAB, NSEK) ! 

! SUBROUTINE ARGUMENTS - OUTPUT
REAL*4,    INTENT(OUT)                           :: xl                         ! 
REAL*4,    INTENT(OUT)                           :: vw10                       ! 
REAL*4,    INTENT(OUT)                           :: rb_ms                      ! boundary layer resistance SO2 from meteo statistics [s/m] 
REAL*4,    INTENT(OUT)                           :: ra_ms_4                    ! aerodynamic resistance at 4 m from meteo statistics [s/m]
REAL*4,    INTENT(OUT)                           :: ra_ms_zra                  ! aerodynamic resistance at height zra from meteo statistics [s/m]   
REAL*4,    INTENT(OUT)                           :: xvglbr                     ! 
REAL*4,    INTENT(OUT)                           :: xvghbr                     ! 
REAL*4,    INTENT(OUT)                           :: uster_metreg_rcp           ! 
REAL*4,    INTENT(OUT)                           :: temp_C                     ! temperature at height zmet_T [C]
REAL*4,    INTENT(OUT)                           :: ol_metreg_rcp              ! 
REAL*4,    INTENT(OUT)                           :: h0                         ! 
REAL*4,    INTENT(OUT)                           :: xloc                       ! 
REAL*4,    INTENT(OUT)                           :: xl100                      ! 
REAL*4,    INTENT(OUT)                           :: rad                        ! 
REAL*4,    INTENT(OUT)                           :: rc_so2_ms                  ! Rc(SO2) from meteo statistics [s/m] 
REAL*4,    INTENT(OUT)                           :: hum                        ! relative humidity [%]
REAL*4,    INTENT(OUT)                           :: pcoef                      ! 
REAL*4,    INTENT(OUT)                           :: rc_nh3_ms                  ! Rc(NH3) from meteo statistics [s/m] (NOT USED)
REAL*4,    INTENT(OUT)                           :: rc_no2_ms                  ! Rc(NO2) from meteo statistics [s/m] (NOT USED)
REAL*4,    INTENT(OUT)                           :: rc_aer_ms                  ! Rc(SO4-aerosol) from meteo statistics [s/m] 
REAL*4,    INTENT(OUT)                           :: buil                       ! 
REAL*4,    INTENT(OUT)                           :: rint                       ! 
REAL*4,    INTENT(OUT)                           :: shear                      ! 
REAL*4,    INTENT(OUT)                           :: dscor(NTRAJ)               ! 
REAL*4,    INTENT(OUT)                           :: coef_space_heating         ! space heating coefficient (degree-day values in combination with a wind speed correction) [C m^1/2 / s^1/2] 
REAL*4,    INTENT(OUT)                           :: regenk                     ! 

! LOCAL VARIABLES
REAL*4                                           :: sp                         ! interpolation factor between wind sectors, taking into account missing sectors

! DATA
! MENGH is default value for mixing height for 6 stability classes
DATA MENGH /300, 985, 302, 537, 50, 153/

!-------------------------------------------------------------------------------------------------------------------------------
!
! In stt, meteo parameters are stored that have been interpolated for the distance already (ICOMP = 2-8, 19-NCOMP)
! For these parameters, no interpolation between wind sectors is needed.
! The same numbering is used as for astat.
!        2. maximal mixing height over transport distance [m]
!        3. wind speed (at 10 m height) [m/s]
!        4. boundary layer resistance Rb for SO2 [s/m]
!        5. aerodynamic resistance 4 m + boundary layer resistance [s/m]
!        6. aerodynamic resistance 50 m + boundary layer resistance [s/m]
!        7. ratio effective dry deposition velocity over transport distance and average dry deposition velocity over transport distance for low sources [-]
!        8. ratio effective dry deposition velocity over transport distance and average dry deposition velocity over transport distance for high sources [-]
!           See OPS-doc/meteo, bookmark correction_factor_deposition_velocity
!        19. friction velocity u* [m/s]
!        20. temperature T [degree C]
!        21. turning angle for wind shear (at reference height) []
!        22. Monin-Obukhov length L [m]
!        23. sensible heat flux H0 [W/m2]
!
xl               = stt(2)
vw10             = stt(3)
rb_ms            = stt(4)
ra_ms_4          = stt(5) - rb_ms
ra_ms_zra        = stt(6) - rb_ms
xvglbr           = stt(7)
xvghbr           = stt(8)
uster_metreg_rcp = stt(19)
temp_C           = stt(20)
shear            = stt(21)
ol_metreg_rcp    = stt(22)
h0               = stt(23)
!
! Special cases for mixing height
! xl100: mixing height at 100 km 
IF (astat(2, 2, istab, isec_in) .LE. (0. + EPS_DELTA)) THEN
   xl100 = xl ! default value at receptor
ELSE
   xl100 = astat(2, 2, istab, isec_in)
ENDIF

! xloc local mixing height (near source)
IF (ABS(astat(1, 2, istab, isec_in)) .LE. EPS_DELTA) THEN
   xloc = FLOAT(MENGH(istab))
ELSE
   xloc = astat(1, 2, istab, isec_in)
ENDIF

! We use here maximum mixing height over trajectory; so the following should hold:
! xloc < xl and xloc < xl100
IF (xloc .GT. (xl + EPS_DELTA)) THEN
   xloc = xl
ENDIF
IF (xloc .GT. (xl100 + EPS_DELTA)) THEN
   xloc = xl100
ENDIF
!
! itrx is equal to itra = index of distance class boundary ~nearest to the current source-receptor distance
! or (if meteo class itra does not exist) itra+1 or itra-1.



!
! Get interpolation factor sp for wind sector isec2 (second interpolation sector);
! if sector isec2 does not occur -> sp = 0; otherwise sp = s_wind

IF (ABS(astat(itrx, 1, istab, isec2)) .LE. EPS_DELTA) THEN
   sp = 0.
ELSE
   sp = s_wind
ENDIF
!
! Interpolate meteo parameters 14-16 and 24-27 for current stability class and distance class itrx, 
! between wind direction sector isec1 and sector isec2
! 
rad       = (1. - sp)*astat(itrx, 14, istab, isec1) + sp*astat(itrx, 14, istab, isec2)
hum       = (1. - sp)*astat(itrx, 24, istab, isec1) + sp*astat(itrx, 24, istab, isec2)
pcoef     = (1. - sp)*astat(itrx, 15, istab, isec1) + sp*astat(itrx, 15, istab, isec2)
rc_so2_ms = (1. - sp)*astat(itrx, 16, istab, isec1) + sp*astat(itrx, 16, istab, isec2)
rc_no2_ms = (1. - sp)*astat(itrx, 25, istab, isec1) + sp*astat(itrx, 25, istab, isec2)
rc_nh3_ms = (1. - sp)*astat(itrx, 26, istab, isec1) + sp*astat(itrx, 26, istab, isec2)
rc_aer_ms = (1. - sp)*astat(itrx, 27, istab, isec1) + sp*astat(itrx, 27, istab, isec2)
!
! Interpolate effective travel distances (astat(9)) for each distance class,
! between wind sectors isec1 and isec2;
! no correction for effective travel distance of distance class 1 (vicinity of source),
! but dscor(1) will not be used in interp_sek.
!
dscor(2:NTRAJ)=(1. - sp)*astat(2:NTRAJ, 9, istab, isec1) + sp*astat(2:NTRAJ, 9, istab, isec2)
!
! Interpolate meteo parameters 10-11 for current stability class and distance class,
! between wind direction sectors isec1 and isec2
!
coef_space_heating = (1. - sp)*astat(itrx, 10, istab, isec1) + sp*astat(itrx, 10, istab, isec2)
regenk             = (1. - sp)*astat(itrx, 11, istab, isec1) + sp*astat(itrx, 11, istab, isec2)
!
! No interpolation for buil (length of rainfall period) and rint (rain intensity);
! they are not interpolated, because there may be many zeros (-> does not occur) in one of the two interpolating sectors
! and in that case interpolation is not well defined.
!
buil   = astat(itrx, 12, istab, isec_in)
rint   = astat(itrx, 13, istab, isec_in)

RETURN
END SUBROUTINE interp_sek

!-------------------------------------------------------------------------------------------------------------------------------

END SUBROUTINE ops_statparexp

end module m_ops_statparexp
