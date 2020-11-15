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
! SCCS (SOURCE)      : %P%
! RELEASE - LEVEL    : %R% - %L%
! BRANCH - SEQUENCE  : %B% - %S%
! DATE - TIME        : %E% - %U%
! WHAT               : %W%:%E%
! AUTHOR             : OPS-support   
! FIRM/INSTITUTE     : RIVM/LLO
! LANGUAGE           : FORTRAN-77/90
! DESCRIPTION        : Get parameters as windspeed, mixing height, frequency etc. from the meteo statistics as a function of
!                      transport distance, wind direction and stability class. Parameters are interpolated between values of the
!                      different distance classes and wind sectors. Plume rise is also calculated here, because of its 
!                      dependency on wind speed.
! EXIT CODES         :
! FILES AND OTHER    :
!    I/O DEVICES
! SYSTEM DEPENDENCIES: HP Fortran
! CONTAINS PROCEDURES: bepafst, voorlpl, ronafhpar, windsek, windcorr, interp_ctr, interp_tra, interp_sek
! UPDATE HISTORY :
!-------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE ops_statparexp(istab, hbron, qww, D_stack, V_stack, Ts_stack, emis_horizontal, iwd, radius, uurtot, astat, trafst, disx, isek, disxx, isekt, vw10, aksek, h0,  &
                       &  hum, ol_metreg_rcp, shear, rcaer, rcnh3, rcno2, temp_C, uster_metreg_rcp, pcoef, htot, htt, itra, aant,                        &
                       &   xl, rb, ra4, ra50, xvglbr, xvghbr, xloc,xl100, rad, rcso2, coef_space_heating, regenk, buil, rint, percvk, error)

!DEC$ ATTRIBUTES DLLEXPORT:: ops_statparexp

USE m_error
USE m_commonconst
USE m_commonfile
USE m_ops_plumerise

IMPLICIT NONE

! CONSTANTS
CHARACTER*512                                    :: ROUTINENAAM                ! 
PARAMETER      (ROUTINENAAM = 'ops_statparexp')

! SUBROUTINE ARGUMENTS - INPUT
INTEGER*4, INTENT(IN)                            :: istab                       
real,      INTENT(IN)                            :: hbron
real,      INTENT(IN)                            :: qww
real,      INTENT(IN)                            :: D_stack                    ! diameter of the stack [m]
real,      INTENT(IN)                            :: V_stack                    ! exit velocity of plume at stack tip [m/s]
real,      INTENT(IN)                            :: Ts_stack                   ! temperature of effluent from stack [K]
LOGICAL,   INTENT(IN)                            :: emis_horizontal            ! horizontal outflow of emission
INTEGER*4, INTENT(IN)                            :: iwd                         
real,      INTENT(IN)                            :: radius
real,      INTENT(IN)                            :: uurtot
real,      INTENT(IN)                            :: astat(NTRAJ, NCOMP, NSTAB, NSEK)
real,      INTENT(IN)                            :: trafst(NTRAJ)
real,      INTENT(IN)                            :: disx                       ! linear distance between source and receptor [m]
INTEGER*4, INTENT(IN)                            :: isek                       ! 

! SUBROUTINE ARGUMENTS - I/O
TYPE (TError), INTENT(INOUT)                     :: error                      ! error handling record

! SUBROUTINE ARGUMENTS - OUTPUT
real,      INTENT(OUT)                           :: disxx                      ! effective travel distance between source and receptor [m]
INTEGER*4, INTENT(OUT)                           :: isekt                      ! 
real,      INTENT(OUT)                           :: vw10
real,      INTENT(OUT)                           :: aksek(12)
real,      INTENT(OUT)                           :: h0
real,      INTENT(OUT)                           :: hum
real,      INTENT(OUT)                           :: ol_metreg_rcp
real,      INTENT(OUT)                           :: shear
real,      INTENT(OUT)                           :: rcaer
real,      INTENT(OUT)                           :: rcnh3
real,      INTENT(OUT)                           :: rcno2
real,      INTENT(OUT)                           :: temp_C                     ! temperature at height zmet_T [C]
real,      INTENT(OUT)                           :: uster_metreg_rcp
real,      INTENT(OUT)                           :: pcoef
real,      INTENT(OUT)                           :: htot
real,      INTENT(OUT)                           :: htt
INTEGER*4, INTENT(OUT)                           :: itra                       ! 
real,      INTENT(OUT)                           :: aant
real,      INTENT(OUT)                           :: xl
real,      INTENT(OUT)                           :: rb
real,      INTENT(OUT)                           :: ra4
real,      INTENT(OUT)                           :: ra50
real,      INTENT(OUT)                           :: xvglbr
real,      INTENT(OUT)                           :: xvghbr
real,      INTENT(OUT)                           :: xloc
real,      INTENT(OUT)                           :: xl100
real,      INTENT(OUT)                           :: rad
real,      INTENT(OUT)                           :: rcso2
real,      INTENT(OUT)                           :: coef_space_heating         ! space heating coefficient (degree-day values in combination with a wind speed correction) [C m^1/2 / s^1/2]
real,      INTENT(OUT)                           :: regenk
real,      INTENT(OUT)                           :: buil
real,      INTENT(OUT)                           :: rint
real,      INTENT(OUT)                           :: percvk

! LOCAL VARIABLES
INTEGER*4                                        :: is                         ! 
INTEGER*4                                        :: ids                        ! 
INTEGER*4                                        :: ispecial                   ! 
INTEGER*4                                        :: iss                        ! 
INTEGER*4                                        :: itrx                       ! 
INTEGER*4                                        :: iwdd                       ! 
INTEGER*4                                        :: itraj                      ! index of distance class
real                                             :: ccor                       ! concentration correction factor for area sources
real                                             :: stt(NCOMP)
real                                             :: tal(NTRAJ)
real                                             :: dscor(NTRAJ)
real                                             :: phi
real                                             :: r
real                                             :: r4
real                                             :: r50
real                                             :: s                          ! interpolation factor (0-1) for the contribution of wind sector is
                                                                               ! (i.e. the second interpolation sector), to the wind direction
                                                                               ! from source to receptor

real                                             :: s1(NTRAJ)                  ! interpolation factor for distance class (interpolates data between
                                                                               ! lower and upper class boundary). Note that if ids is the class index
                                                                               ! where the source-receptor distance lies in, then 0 <= s1(ids) <= 1 and
                                                                               ! s1(i) = 0 for i /= ids

real                                             :: stta(NCOMP)
real                                             :: sttr(NCOMP)
real                                             :: sa
real                                             :: so
real                                             :: sp
real                                             :: dum                        ! dummy output variable

! SCCS-ID VARIABLES
CHARACTER*81                                     :: sccsida                    ! 
sccsida = '%W%:%E%'//char(0)
!-------------------------------------------------------------------------------------------------------------------------------
!
! Compute preliminary plume rise; preliminary in the sense that later on (ops_conc_ini)
! stability is defined in terms of L and U*, instead of stability class (as in ops_plumerise_prelim) 
! and that corrections may be applied (e.g. for heavy particles).
! Also get values of vw10 and pcoef 
!
! write(*,'(a,4(1x,e12.5),2(1x,i6))') 'before call ops_plumerise_prelim: ',hbron,htt,htt-hbron,-999.0,istab,isek
! write(*,'(a,4(1x,e12.5))') 'before call ops_plumerise_prelim: ',hbron,htt,htt-hbron,-999.0
call ops_plumerise_prelim(istab,isek,astat,hbron,qww,D_stack,V_stack,Ts_stack,emis_horizontal,htt,error) 
if (error%haserror) goto 9999
call ops_wv_powerlaw(istab,isek,astat,hbron,dum,vw10,pcoef)
!write(*,'(a,4(1x,e12.5))') 'after call ops_plumerise_prelim: ',hbron,htt,htt-hbron,-999.0

! Compute, given a source - receptor direction (taking into account plume rise and wind shear),
! the wind sector where this direction lies in (iss), the wind sectors between which to 
! interpolate (isekt,is) and the interpolation factor (s).
!
CALL windsek(istab, htt, disx, iwd, astat, isek, isekt, shear, htot, iwdd, iss, is, s)
!
! Compute itra = distance class boundary ~nearest to the current source-receptor distance;
! ids = distance class where the current source-receptor distance lies in
! and interpolation factors (s1) for different trajectory distance classes and current source-receptor distance.
!
CALL interp_ctr(disx, trafst, itra, s1, ids)
!
! Set interpolation factor for wind sector (s) in the case that a wind sector that does not occur.
! This may happen, especially when we have meteo statistics for shorter periods (e.g. month).
! For area sources, surrounding wind sectors have to be taken into account.
!
CALL windcorr(itra, istab, radius, disx, isekt, iwdd, is, astat, iss, ispecial, phi, s)
!
! Compute for the current stability class and for all distance classes 
! tal = number of hours of wind blowing from source to receptor (interpolated between sector isekt and is). 

!

!  s    : interpolation factor (0-1) for the contribution of sector is to the direction iwdd; 
!         first half of sector:                  s = 0.5 - 1; 
!         second half of sector:                 s = 0 - 0.5. 
!         For a parameter p, interpolation is given by  p(iwdd) = s*p(is) + (1-s)*p(isekt)
!  If one of the interpolating sectors does not occur, set interpolation factor to 0 or 1 (NINT(s))
!         first half of sector:                  s = 1; p(iwdd) = p(is)
!         second half of sector:                 s = 0; p(iwdd) = p(isekt)
! 
!  If the current distance class equals the class where the receptor lies in, reset s to sp
!
DO itraj = 1, NTRAJ
   IF ((ABS(astat(itraj, 1, istab, isekt)) .LE. EPS_DELTA) .OR. (ABS(astat(itraj, 1, istab, is)) .LE. EPS_DELTA)) THEN
      sp = NINT(s)
      IF (itra .EQ. itraj) THEN
         s = sp
         ispecial = 1
      ENDIF
   ELSE
!
!    Both interpolating sectors occur
!
      sp = s
   ENDIF
!
!  Interpolate (between wind sectors isekt and is) number of hours that this meteo class occurs
!
   tal(itraj) = (1. - sp)*astat(itraj, 1, istab, isekt) + (sp*astat(itraj, 1, istab, is))
ENDDO
!
! Interpolate over distance classes; 
! note that aant is interpolated over both distance classes (ids-1/ids) and wind sectors (isekt/is).
!
CALL interp_tra(itra, s1, ids, istab, iss, tal, astat, itrx, aant, stt)
!
! Continue if interpolated number of hours of the {{ids-1/ids),istab,(isekt/is)} combination is larger than 0
!
IF (aant > EPS_DELTA) THEN
!
! Interpolate meteo parameters over wind sectors 
!
  CALL interp_sek(istab, iss, itrx, is, s, isekt, stt, astat, xl, vw10, rb, ra4, ra50, xvglbr, xvghbr, uster_metreg_rcp,        &
               &  temp_C, ol_metreg_rcp, h0, xloc, xl100, sp, rad, rcso2, hum, pcoef, rcnh3, rcno2, rcaer,                      &
               &  buil, rint, shear, dscor, coef_space_heating, regenk)
!
! Compute the effective travel distance between source and receptor
!
  CALL bepafst(itra, s1, trafst, disx, dscor, xl, disxx)
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
     CALL ronafhpar(radius, disxx, istab, s, isekt, astat, s1,ids, aksek, sa, phi, so, stta, sttr)
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
!    and near-receptor values so (number of hours),sttr (other meteo parameters). s, so, stta and sttr are all averaged
!    over all sectors that contribute from area source to the receptor.
!    Note that the interpolated number of hours is stored in variable so.

!
!    Note: rcso2 is no longer used; instead OPS uses DEPAC RC-values 
!
     so               = (r*sa)       + (1. - r)*so
     xl               = (r*stta(2))  + (1. - r)*sttr(2)
     rb               = (r*stta(4))  + (1. - r)*sttr(4)
     r4               = (r*stta(5))  + (1. - r)*sttr(5)
     r50              = (r*stta(6))  + (1. - r)*sttr(6)
     regenk           = (r*stta(11)) + (1. - r)*sttr(11)
     rad              = (r*stta(14)) + (1. - r)*sttr(14)
     rcso2            = (r*stta(16)) + (1. - r)*sttr(16)
     uster_metreg_rcp = (r*stta(19)) + (1. - r)*sttr(19)
     temp_C           = (r*stta(20)) + (1. - r)*sttr(20)
     ol_metreg_rcp    = (r*stta(22)) + (1. - r)*sttr(22)
     h0               = (r*stta(23)) + (1. - r)*sttr(23)
     rcno2            = (r*stta(25)) + (1. - r)*sttr(25)
     rcnh3            = (r*stta(26)) + (1. - r)*sttr(26)
     rcaer            = (r*stta(27)) + (1. - r)*sttr(27)
     ra4              = r4 - rb
     ra50             = r50 - rb
     
     
  ELSE
     ! Not an area source or a special case; 
     ! so = aant = interpolated number of hours of the {{ids-1/ids),istab,(isekt/is)} combination 
     so = aant
  ENDIF
!
! ccor = correction factor number of hours for an area sources;
! (ccor = 1 if not an area source or a special case)
!
  ccor = so/aant
!
! aant = number of hours of occurrence of combination of distance class and wind direction sector 
!        for the current stability class
! percvk = fraction of occurrence of {distance/stability/wind-direction} class
!          i.e. (number of hours that a {distance/stability/wind-direction} class occurs) / (total number of hours)
!
  percvk = (aant*ccor)/uurtot
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
SUBROUTINE bepafst(itra, s, trafst, disx, dscor, xl, disxx)

! CONSTANTS
CHARACTER*512                                    :: ROUTINENAAM                ! 
PARAMETER      (ROUTINENAAM = 'bepafst')

! SUBROUTINE ARGUMENTS - INPUT
INTEGER*4, INTENT(IN)                            :: itra                       ! 
real,      INTENT(IN)                            :: s(NTRAJ)
real,      INTENT(IN)                            :: trafst(NTRAJ)
real,      INTENT(IN)                            :: disx                       ! linear distance between source and receptor ('as the crow flies') [m]

! SUBROUTINE ARGUMENTS - I/O
real,      INTENT(INOUT)                         :: dscor(NTRAJ)               ! Note: dscor is not used anymore after this routine
real,      INTENT(INOUT)                         :: xl

! SUBROUTINE ARGUMENTS - OUTPUT
real,      INTENT(OUT)                           :: disxx                      ! effective travel distance between source and receptor [m]

! LOCAL VARIABLES
INTEGER*4                                        :: ids                        ! 

! SCCS-ID VARIABLES
CHARACTER*81                                     :: sccsida                    ! 
sccsida = '%W%:%E%'//char(0)
!-------------------------------------------------------------------------------------------------------------------------------
!
! Default value for zero dscor values, indicating a non-existent distance/stability/wind-sector class.
! Note: ids is a local variable here
! dscor is in km, trafst is in m       
!
DO ids = 2, NTRAJ
  IF (ABS(dscor(ids) - 0.) .LE. EPS_DELTA) THEN
     dscor(ids) = trafst(ids)/1000.
  ENDIF
ENDDO

! No correction for itra = 1
IF (itra .EQ. 1) THEN
   disxx = disx
ELSE
!
! Determine ids index at which trafst(ids) < disx <= trafst(ids+1); ids is minimal 2.
! Note: ids is local variable here
! Note: in interp_ctr, ids is determined such that trafst(ids-1) < disx <= trafst(ids)

!
!         0 < disx <= trafst(3) -> ids = 2
! trafst(3) < disx <= trafst(4) -> ids = 3
! trafst(4) < disx              -> ids = 4
!
  ids = NTRAJ
  DO WHILE (ids.GT.2 .AND. disx .LE. (trafst(ids) + EPS_DELTA))
    ids = ids - 1
  ENDDO ! {ids <= 2 OR disx > trafst(ids)}
!
! Check whether ids = NTRAJ (or equivalently disx > trafst(NTRAJ)) 
!
  IF (ids.EQ.NTRAJ) THEN
!
!   Outside largest distance (1000 km); compute xl and disxx

!   xl    : maximum mixing height [m]
!   disxx : effective travel distance between source and receptor [m]
!   disx  : linear distance between source and receptor ('as the crow flies') [m]
!   dscor : effective travel distance for each distance class for current stability class and current wind direction 
!          (dscor has been interpolated between wind sectors) [km]
!   trafst: distance for each distance class [m]
! 
!   The maximum mixing height increases with travelled distance. If travelled distance is larger,
!   the chance of meeting a higher mixing height is larger, the mixing volume is larger; compression
!   does not take place. 


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
!   disxx      dscor(ids)                 dscor(ids+1)
!   ------ = {------------*[1-s(ids+1)] + -------------*s(ids+1) }; 
!   disx      trafst(ids)                 trafst(ids+1)
!
!   s is interpolation factor for distance class.

!
    disxx = disx*(dscor(ids)*1000./trafst(ids)*(1. - s(ids+1)) + dscor(ids+1)*1000./trafst(ids+1)*s(ids+1))
  ENDIF
ENDIF

RETURN
END SUBROUTINE bepafst

!-------------------------------------------------------------------------------------------------------------------------------
! SUBROUTINE         : voorlpl
! DESCRIPTION        : Bepaling van de voorlopige pluimstijging.
!-------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE voorlpl(istab, isek, hbron, qww, astat, vw10, pcoef, htt)

! CONSTANTS
CHARACTER*512                                    :: ROUTINENAAM
PARAMETER      (ROUTINENAAM = 'voorlpl')

! CONSTANTS
real                                             :: VWREP(NSTAB)

! SUBROUTINE ARGUMENTS - INPUT
INTEGER*4, INTENT(IN)                            :: istab                      ! 
INTEGER*4, INTENT(IN)                            :: isek                       ! 
real,      INTENT(IN)                            :: hbron
real,      INTENT(IN)                            :: qww
real,      INTENT(IN)                            :: astat(NTRAJ, NCOMP, NSTAB, NSEK)

! SUBROUTINE ARGUMENTS - OUTPUT
real,      INTENT(OUT)                           :: vw10
real,      INTENT(OUT)                           :: pcoef
real,      INTENT(OUT)                           :: htt

! LOCAL VARIABLES
real                                             :: delh
real                                             :: utop

! DATA
DATA VWREP /2.6, 3.8, 4.0, 6.9, 1.4, 2.5/

! SCCS-ID VARIABLES
CHARACTER*81                                     :: sccsida                    ! 
sccsida = '%W%:%E%'//char(0)
!-------------------------------------------------------------------------------------------------------------------------------
!
! Get wind speed [m/s] at 10 m height.
! Use VWREP if we have a zero wind speed from meteo statistics.
! VWREP: representative (long term average) wind speed per stability class
!
IF (ABS(astat(1, 3, istab, isek)) .LE. EPS_DELTA) THEN
   vw10 = VWREP(istab)
ELSE
   vw10 = astat(1, 3, istab, isek)
ENDIF
!
! Compute preliminary plume rise
!

! Get coefficient in wind speed power law
pcoef = astat(1, 15, istab, isek)

delh  = 0.
htt   = hbron
IF (ABS(qww) .GT. EPS_DELTA) THEN

! use power law to determine Utop (wind velocity at top of stack) 
   IF (hbron .GT. (10. + EPS_DELTA)) THEN
      utop = vw10*(hbron/10.)**pcoef
   ELSE
      utop = vw10
   ENDIF
   write(*,'(a,2(1x,e12.5))') 'voorlpl a',hbron,utop
   
   IF (istab .GE. 5) THEN
!
!     plume rise for stable conditions
!     (3.28 OPS report)
!     Note: temperature gradient of 0.006 K/m not suited for lower sources;
!     more suited for industrial sources
!
      delh = 65.*(qww/utop)**.333
      write(*,'(a,2(1x,e12.5))') 'voorlpl b',hbron,delh

!
!  plume rise for non-stable conditions; split into Qww < 6 and Qww > 6
!  (3.25, 3.26 OPS report)
!
   ELSE IF (qww .LT. (6. - EPS_DELTA)) THEN
      delh = 109.*(qww**.75)/utop
      write(*,'(a,2(1x,e12.5))') 'voorlpl c',hbron,delh
   ELSE
      delh = 143.*(qww**.6)/utop
      write(*,'(a,2(1x,e12.5))') 'voorlpl d',hbron,delh
   ENDIF
!
!  Compute preliminary plume height
!
   htt = hbron + delh
ENDIF

RETURN
END SUBROUTINE voorlpl

!-------------------------------------------------------------------------------------------------------------------------------
! SUBROUTINE         : ronafhpar
! DESCRIPTION        : Average meteo parameters contributions of an area source from multiple contributing sectors to a receptor.
!                      Two types of average are computed: sa,stta = near-source average, so,sstr = near-receptor average,
!                      where sa, so = average number of hours of contributing meteo classes, 
!                         stta,sttr = average meteo parameter of contributing meteo classes.
!                      (ronafhpar: r << "richting" = direction, onafh << "onafhankelijk" = independent)
!                      Note: in subroutine call, ronafhpar is called with isek = isekt.
!-------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE ronafhpar(radius, disxx, istab, s, isek, astat, s1,ids, aksek, sa, phi, so, stta, sttr)

! CONSTANTS
CHARACTER*512                                    :: ROUTINENAAM                ! 
PARAMETER      (ROUTINENAAM = 'ronafhpar')

! SUBROUTINE ARGUMENTS - INPUT
real,      INTENT(IN)                            :: radius
real,      INTENT(IN)                            :: disxx
INTEGER*4, INTENT(IN)                            :: istab                      ! 
real,      INTENT(IN)                            :: s
INTEGER*4, INTENT(IN)                            :: isek                       ! middle of contributing wind sectors; note that ronafhpar
                                                                               ! is called with isek = isekt, i.e. the first of the two 
                                                                               ! interpolating wind sectors
real,      INTENT(IN)                            :: astat(NTRAJ, NCOMP, NSTAB, NSEK)
real,      INTENT(IN)                            :: s1(NTRAJ)
INTEGER*4, INTENT(IN)                            :: ids                        ! 

! SUBROUTINE ARGUMENTS - OUTPUT
real,      INTENT(OUT)                           :: aksek(12)
real,      INTENT(OUT)                           :: sa
real,      INTENT(OUT)                           :: phi
real,      INTENT(OUT)                           :: so
real,      INTENT(OUT)                           :: stta(NCOMP)
real,      INTENT(OUT)                           :: sttr(NCOMP)

! LOCAL VARIABLES
INTEGER*4                                        :: i                          ! 
INTEGER*4                                        :: icomp                      ! 
INTEGER*4                                        :: lpsek                      ! 
INTEGER*4                                        :: jsek                       ! 
INTEGER*4                                        :: scomp(14)                  ! 
real                                             :: a
real                                             :: asek
real                                             :: statfactor
real                                             :: zz
real                                             :: p1
real                                             :: p2
real                                             :: pa

! DATA
!     De arrayelementen uit de meteostatistiek die hier gebruikt worden.
DATA scomp / 2, 4, 5, 6, 11, 14, 16, 19, 20, 22, 23, 25, 26, 27 /

! SCCS-ID VARIABLES
CHARACTER*81                                     :: sccsida                    ! 
sccsida = '%W%:%E%'//char(0)
!-------------------------------------------------------------------------------------------------------------------------------
!
!  The following meteo parameters are averaged:
!
!         2. maximal mixing height over transport distance [m]
!         4. boundary layer resistance Rb for SO2 [s/m]
!         5. aërodynamic resistance 4 m + boundary layer resistance [s/m]
!         6. aërodynamic resistance 50 m + boundary layer resistance [s/m]
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
! ( )
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
! Loop over neighbouring sectors (3 sectors before isek) to (3 sectors after isek)
! and average meteo parameters for current source-receptor distance (interpolation
! between distance classes ids-1 and ids). Note that ronafhpar is called with 
! isek = isekt, i.e. the first of the two interpolating wind sectors.
!
DO i = -3, 3
   
   ! pa is correction factor to correct for the deviation of the neighbouring sector
   ! w.r.t to the source-receptor direction;
   ! 0 <= pa <= 1 and for i = 0 (central sector): pa = asek/2 - s + 1/2
   ! 
   ! iss: wind sector in which the source-receptor direction lies.
   ! s between 0 and 0.5 -> source-receptor direction lies in isekt, so isekt is the middle of the contributing sectors
   ! s between 0.5 and 1 -> source-receptor direction lies in isekt+1, so isekt+1 is the middle of the contributing sectors
   !
   pa = (asek/2.) - ABS(FLOAT(i) - s) + .5
   
   IF (pa .GT. (0. + EPS_DELTA)) THEN
      IF (pa .GT. (1. + EPS_DELTA)) THEN
         pa = 1.
      ENDIF

      ! Set jsek = index of neighbouring sector:
      jsek = isek + i
      IF (jsek .LT. 1) THEN
         jsek = jsek + NSEK
      ELSE IF (jsek .GT. NSEK) THEN
         jsek = jsek - NSEK
      ELSE
         CONTINUE
      ENDIF
!
!     Compute weighing factors p1 and p2 for interpolation between distance classes ids-1 and ids resp.
!     s1: interpolation factor for distance class ids
!
      p1  = pa*(1. - s1(ids))*astat(ids-1, 1, istab, jsek) 
      p2  = pa*s1(ids)*astat(ids, 1, istab, jsek)
      DO icomp = 1, 14
         sttr(scomp(icomp)) = sttr(scomp(icomp)) + p1*astat(ids-1, scomp(icomp), istab, jsek)+ p2*astat(ids, scomp(icomp),     &
                    &  istab, jsek)
      ENDDO
!
!     so = pa * number of hours in stability class istab, interpolated between distance classes ids and ids-1, 
!          accumulated over windsectors isek-3,isek-2, ... jsek
!
      so = so + p1 + p2
   ENDIF
   IF (pa .LT. (0. - EPS_DELTA)) THEN
      pa = 0.
   ENDIF
   
   aksek(i + 4) = pa 
ENDDO
!
! End of loop 
! so = pa * number of hours in stability class istab, interpolated between distance classes ids and ids-1, 
!      accumulated over al contributing windsectors isek-3,isek-2, ... , isek+3.

RETURN
END SUBROUTINE ronafhpar

!-------------------------------------------------------------------------------------------------------------------------------
! SUBROUTINE         : windsek
! DESCRIPTION        : Compute, given a source - receptor direction, the wind sector where this direction lies in,
!                      the wind sectors between which to interpolate and the interpolation factor,
!                      taking into account plume rise and wind shear
!-------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE windsek(istab, htt, disx, iwd, astat, isek, isekt, shear, htot, iwdd, iss, is, s)

! CONSTANTS
CHARACTER*512                                    :: ROUTINENAAM                ! 
PARAMETER      (ROUTINENAAM = 'windsek')

! SUBROUTINE ARGUMENTS - INPUT
INTEGER*4, INTENT(IN)                            :: istab                      ! 
real,      INTENT(IN)                            :: htt
real,      INTENT(IN)                            :: disx
INTEGER*4, INTENT(IN)                            :: iwd                        ! 
real,      INTENT(IN)                            :: astat(NTRAJ, NCOMP, NSTAB, NSEK)
INTEGER*4, INTENT(IN)                            :: isek                       ! 

! SUBROUTINE ARGUMENTS - OUTPUT
INTEGER*4, INTENT(OUT)                           :: isekt                      ! 
real,      INTENT(OUT)                           :: shear
real,      INTENT(OUT)                           :: htot
INTEGER*4, INTENT(OUT)                           :: iwdd                       ! 
INTEGER*4, INTENT(OUT)                           :: iss                        ! 
INTEGER*4, INTENT(OUT)                           :: is                         ! 
real,      INTENT(OUT)                           :: s

! LOCAL VARIABLES
real                                             :: alpha
real                                             :: sek

! SCCS-ID VARIABLES
CHARACTER*81                                     :: sccsida                    ! 
sccsida = '%W%:%E%'//char(0)
!-------------------------------------------------------------------------------------------------------------------------------
!
! Compute wind shear for higher altitudes.
! Use measured wind directions (20-200 m) at Cabauw. The empirical relation of Van Ulden & Holtslag is used for the
! whole mixing layer, also above 200 m. 
!

! Get turning angle for wind shear at reference height
shear = astat(1, 21, istab, isek)

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
! sek   : wind direction index (float), 1.0 <= sek <= 13.0
! iss   : index of wind direction sector where iwdd lies in; iss = NINT((sek)
! isekt : index of first sector for interpolation (first in clockwise direction);
!         if iwdd is in the first half of the sector, this is the sector before the sector where iwdd lies in;
!         if iwdd is in the second half of the sector, this is the sector where iwdd lies in.
!         isekt = INT(sek)
! s     : interpolation factor (0-1) for the contribution of sector is to the direction iwdd; 
!         first half of sector:                  s = 0.5 - 1; 
!         halfway sector, coming from 0 degrees: s = 1; 
!         halfway sector, going to 360 degrees:  s = 0; 
!         second half of sector:                 s = 0 - 0.5. 
!         For a parameter p, interpolation is given by p(iwdd) = s*p(is) + (1-s)*p(isekt)
! is    : index of next sector for interpolation (next in clockwise direction); is = isekt + 1
!
! iwdd   sek   iss  isekt  is   s
! 000   1.00    1    1     2   0.00
! 005   1.17    1    1     2   0.17   sector 1: 345 - 15
! 010   1.33    1    1     2   0.33
!                              
! 015   1.50    2    1     2   0.50
! 020   1.67    2    1     2   0.67
! 025   1.83    2    1     2   0.83   sector 2: 15 - 45
! 030   2.00    2    2     3   0.00
! 035   2.17    2    2     3   0.17
! 040   2.33    2    2     3   0.33
!                              
! 045   2.50    3    2     3   0.50
! 050   2.67    3    2     3   0.67   sector 3: 45 - 75
! 055   2.83    3    2     3   0.83
!                              
! ......................................................
!                              
! 325  11.83   12   11    12   0.83
! 330  12.00   12   12     1   0.00   sector 12: 315 - 345
! 335  12.17   12   12     1   0.17
! 340  12.33   12   12     1   0.33
!                              
! 345  12.50    1   12     1   0.50
! 350  12.67    1   12     1   0.67   sector 1: 345 - 15
! 355  12.83    1   12     1   0.83
! 360  13.00    1    1     2   0.00
!
sek   = FLOAT(iwdd)*NSEK/360. + 1.
iss   = NINT(sek)
isekt = INT(sek)
s     = sek - FLOAT(isekt)
is    = isekt + 1

! Correction for sector index > NSEK
IF (is .GT. NSEK) THEN
   is = is - NSEK
ENDIF
IF (isekt .GT. NSEK) THEN
   isekt = isekt - NSEK
ENDIF
IF (iss .GT. NSEK) THEN
   iss = iss - NSEK
ENDIF

RETURN
END SUBROUTINE windsek

!-------------------------------------------------------------------------------------------------------------------------------
! SUBROUTINE         : windcorr
! DESCRIPTION        : Correct wind direction sector in the case that a wind sector that does not occur.
!                      This may happen, especially when we have meteo statistics for shorter periods (e.g. month).
!                      For area sources, surrounding wind sectors have to be taken into account.
!                      Note: in subroutine call, windcorr is called with isek = isekt.
!-------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE windcorr(itra, istab, radius, disx, isek, iwdd, is, astat, iss, ispecial, phi, s)

USE Binas, only: rad2deg

! CONSTANTS
CHARACTER*512                                    :: ROUTINENAAM                ! 
PARAMETER      (ROUTINENAAM = 'windcorr')

! SUBROUTINE ARGUMENTS - INPUT
INTEGER*4, INTENT(IN)                            :: itra                       ! 
INTEGER*4, INTENT(IN)                            :: istab                      ! 
real,      INTENT(IN)                            :: radius
real,      INTENT(IN)                            :: disx
INTEGER*4, INTENT(IN)                            :: isek                       ! 
INTEGER*4, INTENT(IN)                            :: iwdd                       ! 
INTEGER*4, INTENT(IN)                            :: is                         ! 
real,      INTENT(IN)                            :: astat(NTRAJ, NCOMP, NSTAB, NSEK)

! SUBROUTINE ARGUMENTS - I/O
INTEGER*4, INTENT(INOUT)                         :: iss                        ! 

! SUBROUTINE ARGUMENTS - OUTPUT
INTEGER*4, INTENT(OUT)                           :: ispecial                   ! 
real,      INTENT(OUT)                           :: phi                        ! is not used as output
real,      INTENT(OUT)                           :: s

! LOCAL VARIABLES
INTEGER*4                                        :: iwr                        ! 

! SCCS-ID VARIABLES
CHARACTER*81                                     :: sccsida                    ! 
sccsida = '%W%:%E%'//char(0)
!-------------------------------------------------------------------------------------------------------------------------------
!
! Sector/wind direction correction, in case a wind sector does not occur and the neighbouring sector(s) do(es).
! Area sources can have more than one contributing sectors. 


! Default nothing special
ispecial = 0

! If meteo class, where source-receptor direction lies in (iss), does not occur AND area source
IF ((ABS(astat(itra, 1, istab, iss)) .LE. EPS_DELTA) .AND. (radius .GT. (0. + EPS_DELTA))) THEN

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

   ! iwr = wind direction halfway sector isek   
   iwr = (isek - 1)*360/NSEK
!   
!  Criteria for special case:
!  1. iwr < iwdd < iwr + phi (source-receptor direction lies in second half of isek
!  2. meteo class isek, which is used as first sector for interpolation, does not occur (astat(itra, 1, istab, isek) = 0)
!  Note that 1. is split into iwdd > iwr AND phi > iwdd - iwr
!  In this case, set iss (wind sector that iwdd is in) = isekt and interpolation factor s = 0
!  Note: isek = isekt here.
! 

! BUG has only effect for shorter periods; for year runs a non-occurring class does not occur ...
! Note that if ispecial = 1, the contribution of neighbouring sectors is not taken into account (see ronafhpar). 

!
   IF ((iwdd .GT. iwr) .AND. (ABS(astat(itra, 1, istab, isek)) .LE. EPS_DELTA) .AND.                                           &
     &  phi .GT. (FLOAT(iwdd - iwr) + EPS_DELTA)) THEN
      iss      = isek
      s        = 0.
      ispecial = 1
!
!  Criteria for special case:
!  1. iwr - phi < iwdd < iwr (source-receptor direction lies in first half of isek
!  2. meteo class is, which is used as second sector for interpolation, does not occur (astat(itra, 1, istab, is) = 0)
!  Note that 1 is split into iwr > iwdd AND phi > iwr - iwdd
!  In this case, set iss (wind sector that iwdd is in) = is and interpolation factor s = 1 
!

! BUG has only effect for shorter periods; for year runs a non-occurring class does not occur ...
!
   ELSE IF ((iwr .GT. iwdd) .AND. (ABS(astat(itra, 1, istab, is)) .LE. EPS_DELTA) .AND.                                        &
          &  phi .GT. (FLOAT(iwr - iwdd) + EPS_DELTA)) THEN
      iss      = is
      s        = 1.
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
SUBROUTINE interp_ctr(disx, trafst, itra, s, ids)


! CONSTANTS
CHARACTER*512                                    :: ROUTINENAAM                ! 
PARAMETER      (ROUTINENAAM = 'interp_ctr')

! SUBROUTINE ARGUMENTS - INPUT
real,      INTENT(IN)                            :: disx
real,      INTENT(IN)                            :: trafst(NTRAJ)

! SUBROUTINE ARGUMENTS - OUTPUT
INTEGER*4, INTENT(OUT)                           :: itra                       ! 
real,      INTENT(OUT)                           :: s(NTRAJ)
INTEGER*4, INTENT(OUT)                           :: ids                        ! 

! LOCAL VARIABLES
INTEGER*4                                        :: i                          ! 

! SCCS-ID VARIABLES
CHARACTER*81                                     :: sccsida                    ! 
sccsida = '%W%:%E%'//char(0)
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
! 
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
! and interpolation factor s(ids)
!
!                D(ids) - d                d - D(ids-1)
! s(ids)  = 1 - ------------------ = -------------------
!                D(ids) - D(ids-1)    D(ids) - D(ids-1)
!
! d = D(ids-1) -> s(ids) = 0
! d = D(ids)   -> s(ids) = 1
!
! s(i) = 0, for i /= ids 
!
IF (disx .LE. (trafst(2) + EPS_DELTA)) THEN
   ids = 2
    ! here D(1) is replaced by -1 km)
   s(2) = 1. - (trafst(2) - disx)/(trafst(2) + 1.)
ELSE
   s(2) = 0.
   ids = 3
   DO WHILE (ids .LT. NTRAJ .AND. disx .GE. (trafst(ids)-EPS_DELTA))
      s(ids) = 0.
      ids = ids + 1
   ENDDO
   s(ids) = 1. - (trafst(ids)-disx)/(trafst(ids)-trafst(ids-1))
ENDIF

! Interpolation factors for distance classes further away are 0
s(ids+1:NTRAJ) = 0.

! Limit interpolation factor to 1 (for distances larger than trafst(NTRAJ))
IF (s(NTRAJ) .GT. (1. + EPS_DELTA)) THEN
   s(NTRAJ) = 1.
ENDIF

RETURN
END SUBROUTINE interp_ctr

!-------------------------------------------------------------------------------------------------------------------------------
! SUBROUTINE         : interp_tra
! DESCRIPTION        : Interpolate between distance classes for specific meteo parameters (2 <= ICOMP <= 8 OR ICOMP >= 19) 
!                      and store interpolated meteo parameters into stt (same index numbering as astat)
!-------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE interp_tra(itra, s, ids, istab, iss, tal, astat, itrx, aant, stt)

! CONSTANTS
CHARACTER*512                                    :: ROUTINENAAM                ! 
PARAMETER      (ROUTINENAAM = 'interp_tra')

! SUBROUTINE ARGUMENTS - INPUT
INTEGER*4, INTENT(IN)                            :: itra                       ! 
real,      INTENT(IN)                            :: s(NTRAJ)
INTEGER*4, INTENT(IN)                            :: ids                        ! index element in s dat niet 0 is.
INTEGER*4, INTENT(IN)                            :: istab                      ! 
INTEGER*4, INTENT(IN)                            :: iss                        ! 
real,      INTENT(IN)                            :: tal(NTRAJ)
real,      INTENT(IN)                            :: astat(NTRAJ, NCOMP, NSTAB, NSEK)

! SUBROUTINE ARGUMENTS - OUTPUT
INTEGER*4, INTENT(OUT)                           :: itrx                       ! 
real,      INTENT(OUT)                           :: aant
real,      INTENT(OUT)                           :: stt(NCOMP)

! LOCAL VARIABLES
INTEGER*4                                        :: icomp                      ! 

! SCCS-ID VARIABLES
CHARACTER*81                                     :: sccsida                    ! 
sccsida = '%W%:%E%'//char(0)
!-------------------------------------------------------------------------------------------------------------------------------
!
! First initialise itrx to itra = index of distance class boundary ~nearest to the current source-receptor distance;
!
itrx = itra
!
! Interpolate between distance classes ids-1 and ids to get 
! aant = number of hours of occurrence of combination of distance class and wind direction sector 
!        for the current stability class
!
aant = (1. - s(ids))*tal(ids-1) + s(ids)*tal(ids)
!
! If meteo class is present for distance class ids and ids-1, interpolate meteo parameters 2-8 and 19-NCOMP
! between these two distance classes
!
IF ((ABS(astat(ids-1, 1, istab, iss)) .GT. EPS_DELTA) .AND. (ABS(astat(ids, 1, istab, iss)) .GT. EPS_DELTA)) THEN
   DO icomp = 2, NCOMP
      IF (.NOT. ((icomp .GT. 8) .AND. (icomp .LT. 19))) THEN
         stt(icomp) = (1. -  s(ids)) * astat(ids-1, icomp, istab, iss) + s(ids)*astat(ids, icomp, istab, iss)
      ENDIF
   ENDDO
!
! Special cases, one or both meteo classes do not occur -> no interpolation, but get value of meteo 
! parameter 2-8 or 19-NCOMP of neighbouring distance class that does exist (astat = 0);
! if both do not exist stt is undefined
!   
ELSE
   IF ((itra .EQ. ids-1) .AND. (ABS(astat(ids-1, 1, istab, iss)) .LE. EPS_DELTA)) THEN
      itrx = ids
   ELSE IF ((itra .EQ. ids) .AND. (ABS(astat(ids, 1, istab, iss)) .LE. EPS_DELTA)) THEN
      itrx = ids-1
   ELSE
      itrx = itra
   ENDIF
   DO icomp = 2, NCOMP
      IF (.NOT. ((icomp .GT. 8) .AND. (icomp .LT. 19))) THEN
         stt(icomp) = astat(itrx, icomp, istab, iss)
      ENDIF
   ENDDO
ENDIF

! Now itrx is equal to itra = index of distance class boundary ~nearest to the current source-receptor distance
! or (if meteo class itra does not exist) itra+1 or itra-1.

RETURN
END SUBROUTINE interp_tra

!-------------------------------------------------------------------------------------------------------------------------------
! SUBROUTINE         : interp_sek
! DESCRIPTION        : In deze routine worden de meteostatistiek geinterpoleerd over de windsektoren.
!                      Note that interp_sek is calles with isek = isekt.
!-------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE interp_sek(istab, iss, itrx, is, s, isek, stt, astat, xl, vw10, rb, ra4, ra50, xvglbr, xvghbr, uster_metreg_rcp,       &
                   &  temp_C, ol_metreg_rcp, h0, xloc, xl100, sp, rad, rcso2, hum, pcoef, rcnh3, rcno2, rcaer, buil, rint, shear, &
                   &  dscor, coef_space_heating, regenk)    

! CONSTANTS
CHARACTER*512                                    :: ROUTINENAAM                ! 
PARAMETER      (ROUTINENAAM = 'interp_sek')

! CONSTANTS
INTEGER*4                                        :: MENGH(NSTAB)               ! menghoogte

! SUBROUTINE ARGUMENTS - INPUT
INTEGER*4, INTENT(IN)                            :: istab                      ! 
INTEGER*4, INTENT(IN)                            :: iss                        ! 
INTEGER*4, INTENT(IN)                            :: itrx                       ! 
INTEGER*4, INTENT(IN)                            :: is                         ! 
real,      INTENT(IN)                            :: s
INTEGER*4, INTENT(IN)                            :: isek                       ! 
real,      INTENT(IN)                            :: stt(NCOMP)
real,      INTENT(IN)                            :: astat(NTRAJ, NCOMP, NSTAB, NSEK)

! SUBROUTINE ARGUMENTS - OUTPUT
real,      INTENT(OUT)                           :: xl
real,      INTENT(OUT)                           :: vw10
real,      INTENT(OUT)                           :: rb
real,      INTENT(OUT)                           :: ra4
real,      INTENT(OUT)                           :: ra50
real,      INTENT(OUT)                           :: xvglbr
real,      INTENT(OUT)                           :: xvghbr
real,      INTENT(OUT)                           :: uster_metreg_rcp
real,      INTENT(OUT)                           :: temp_C                     ! temperature at height zmet_T [C]
real,      INTENT(OUT)                           :: ol_metreg_rcp
real,      INTENT(OUT)                           :: h0
real,      INTENT(OUT)                           :: xloc
real,      INTENT(OUT)                           :: xl100
real,      INTENT(OUT)                           :: sp
real,      INTENT(OUT)                           :: rad
real,      INTENT(OUT)                           :: rcso2
real,      INTENT(OUT)                           :: hum
real,      INTENT(OUT)                           :: pcoef
real,      INTENT(OUT)                           :: rcnh3
real,      INTENT(OUT)                           :: rcno2
real,      INTENT(OUT)                           :: rcaer
real,      INTENT(OUT)                           :: buil
real,      INTENT(OUT)                           :: rint
real,      INTENT(OUT)                           :: shear
real,      INTENT(OUT)                           :: dscor(NTRAJ)
real,      INTENT(OUT)                           :: coef_space_heating         ! space heating coefficient (degree-day values in combination with a wind speed correction) [C m^1/2 / s^1/2]
real,      INTENT(OUT)                           :: regenk

! DATA
! MENGH is default value for mixing height for 6 stability classes
DATA MENGH /300, 985, 302, 537, 50, 153/

! SCCS-ID VARIABLES
CHARACTER*81                                     :: sccsida                    ! 
sccsida = '%W%:%E%'//char(0)
!-------------------------------------------------------------------------------------------------------------------------------
!
! In stt, meteo parameters are stored that have been interpolated for the distance already (2 <= ICOMP <= 8 OR ICOMP >= 19).
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
rb               = stt(4)
ra4              = stt(5) - rb
ra50             = stt(6) - rb
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
IF (astat(2, 2, istab, iss) .LE. (0. + EPS_DELTA)) THEN
   xl100 = xl ! default value at receptor
ELSE
   xl100 = astat(2, 2, istab, iss)
ENDIF

! xloc local mixing height (near source)
IF (ABS(astat(1, 2, istab, iss)) .LE. EPS_DELTA) THEN
   xloc = FLOAT(MENGH(istab))
ELSE
   xloc = astat(1, 2, istab, iss)
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
! Get interpolation factor sp for wind sector 'is' (second interpolation sector);
! if sector 'is' does not occur -> sp = 0; otherwise sp = s 
!
IF (ABS(astat(itrx, 1, istab, is)) .LE. EPS_DELTA) THEN
   sp = 0.
ELSE
   sp = s
ENDIF
!
! Interpolate meteo parameters 14-16 and 24-27 for current stability class and distance class itrx, 
! between wind direction sector isek (= isekt) and sector is
! 
rad    = (1. - sp)*astat(itrx, 14, istab, isek) + sp*astat(itrx, 14, istab, is)
hum    = (1. - sp)*astat(itrx, 24, istab, isek) + sp*astat(itrx, 24, istab, is)
pcoef  = (1. - sp)*astat(itrx, 15, istab, isek) + sp*astat(itrx, 15, istab, is)
rcso2  = (1. - sp)*astat(itrx, 16, istab, isek) + sp*astat(itrx, 16, istab, is)
rcno2  = (1. - sp)*astat(itrx, 25, istab, isek) + sp*astat(itrx, 25, istab, is)
rcnh3  = (1. - sp)*astat(itrx, 26, istab, isek) + sp*astat(itrx, 26, istab, is)
rcaer  = (1. - sp)*astat(itrx, 27, istab, isek) + sp*astat(itrx, 27, istab, is)
!
! Interpolate effective travel distances (astat(9)) for each distance class,
! between wind sectors isek (= isekt) and is;
! no correction for effective travel distance of distance class 1 (vicinity of source),
! but dscor(1) will not be used in interp_sek.
!
dscor(2:NTRAJ)=(1. - sp)*astat(2:NTRAJ, 9, istab, isek) + sp*astat(2:NTRAJ, 9, istab, is)
!
! Interpolate meteo parameters 10-11 for current stability class and distance class,
! between wind direction sectors isek and is
!
coef_space_heating = (1. - sp)*astat(itrx, 10, istab, isek) + sp*astat(itrx, 10, istab, is)
regenk             = (1. - sp)*astat(itrx, 11, istab, isek) + sp*astat(itrx, 11, istab, is)
!
! No interpolation for buil (length of rainfall period) and rint (rain intensity);
! they are not interpolated, because there may be many zeros (-> does not occur) in one of the two interpolating sectors
! and in that case interpolation is not well defined.
!
buil   = astat(itrx, 12, istab, iss)
rint   = astat(itrx, 13, istab, iss)

RETURN
END SUBROUTINE interp_sek

!-------------------------------------------------------------------------------------------------------------------------------

END SUBROUTINE ops_statparexp
