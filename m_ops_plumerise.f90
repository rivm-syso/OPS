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
!-------------------------------------------------------------------------------------------------------------------------------
module m_ops_plumerise

! module m_ops_plumerise with plume rise due to either buoyancy or momentum
! Marina Sterk and Ferd Sauter 2018-02-20

! ops_plumerise             : main routine containing the calls to different parts of the final plume rise, and the calculation of the final plume rise
! ops_plumerise_buoyancy    : determine plume rise due to buoyancy
! ops_plumerise_momentum    : determine plume rise due to momentum
! ops_plume_penetration     : determine plume penetration (fraction of plume that penetrates the mixing height)

implicit none

! T0   = reference temperature = 273.15 K = 0 C
! P0   = reference pressure = 1 atm = 101.325 kPa
real, parameter :: rho0 = 1.293 ! reference density air at pressure P0, temperature T0 (= 1.293 kg/m3)
real, parameter :: Cp0  = 1005  ! reference specific heat of air at pressure P0, temperature T0 (= 1005 J/kg/K)

contains

!------------------------------------------------------------
subroutine ops_plumerise_prelim(istab, isek, astat, hemis0, qw, D_stack, V_stack, Ts_stack, emis_horizontal, hemis1, error)

! Compute preliminary plume rise, based on stability class and preliminary wind sector (ol, uster, ... still unknown)

use m_commonconst, only: NTRAJ, NCOMP, NSTAB, NSEK
use m_ops_utils, only: is_missing
use m_error

! Input:
integer, intent(in) :: istab             ! index of stability class and preliminary wind sector
integer, intent(in) :: isek              ! index of preliminary wind sector (wind shear not yet taken into account)
real   , intent(in) :: astat(NTRAJ, NCOMP, NSTAB, NSEK) ! statistical meteo parameters
real,    intent(in) :: hemis0            ! initial emission height = stack height [m]
real,    intent(in) :: qw                ! heat content [MW]
real,    intent(in) :: D_stack           ! diameter of the stack [m]
real,    intent(in) :: V_stack           ! exit velocity of plume at stack tip [m/s]
real,    intent(in) :: Ts_stack          ! temperature of effluent from stack [K]
logical, intent(in) :: emis_horizontal   ! horizontal outflow of emission

! Output:
real, intent(out)          :: hemis1     ! emission height, including plume rise [m]
type (TError), intent(out) :: error      ! error handling record

! Local:
logical :: prelim              ! preliminary plume rise, based on stability class and preliminary wind sector (ol, uster, ... still unknown)
                               ! if prelim = true  -> ol = -999, uster = -999, z0 = -999, zmix = zmix_loc = -999
                               !                      these parameters are still unknown;
                               !                      wind profile is based on power law with coefficient based on stability class
logical :: VsDs_opt            ! include exit velocity (Vs = V_stack), stack diameter (Ds = D_stack) and effluent temperature (Ts_stack) in the emission file
real    :: dum                 ! dummy output of ops_plumerise
real    :: temp_C              ! ambient temperature at height zmet_T [C], default value

character(len = 80), parameter :: ROUTINENAAM = 'ops_plumerise_prelim'

prelim   = .true.
VsDs_opt = .not. is_missing(V_stack)
temp_C   = 12.0  ! default average value (is not a sensitive parameter for preliminary estimate)
call ops_plumerise(-999., hemis0, -999., -999., qw, VsDs_opt, D_stack, V_stack, Ts_stack, emis_horizontal, temp_C, -999., -999., &
                   hemis1, dum, error, prelim, istab, isek, astat)
! write(*,'(a,4(1x,e12.5))') 'in routine ops_plumerise_prelim a: ',hemis0,hemis1,hemis1-hemis0,-999.0

if (error%haserror) call ErrorCall(ROUTINENAAM, error)

end subroutine ops_plumerise_prelim

!------------------------------------------------------------
subroutine ops_plumerise(z0, hemis0, uster, ol, qw, VsDs_opt, D_stack, V_stack, Ts_stack, emis_horizontal, temp_C, zmix, zmix_loc, &
                         hemis1, onder, error, prelim, istab, isek, astat)

! Main routine for the different plume rise calculations

use Binas, only: T0                      ! melting point of ice [K]
use m_commonconst, only: zmet_T, NTRAJ, NCOMP, NSTAB, NSEK, EPS_DELTA
use m_error

! Input
real,    intent(in) :: z0                ! roughness length [m]
real,    intent(in) :: hemis0            ! initial emission height = stack height [m]
real,    intent(in) :: uster             ! friction velocity [m/s]
real,    intent(in) :: ol                ! Monin-Obukhov length [m]
real,    intent(in) :: qw                ! heat content [MW]
logical, intent(in) :: VsDs_opt          ! include exit velocity (Vs = V_stack), stack diameter (Ds = D_stack) and effluent temperature (Ts_stack) in the emission file
real,    intent(in) :: D_stack           ! diameter of the stack [m]
real,    intent(in) :: V_stack           ! exit velocity of plume at stack tip [m/s]
real,    intent(in) :: Ts_stack          ! temperature of effluent from stack [K]
logical, intent(in) :: emis_horizontal   ! horizontal outflow of emission
real,    intent(in) :: temp_C            ! ambient temperature at height zmet_T [C]
real,    intent(in) :: zmix              ! mixing height [m]
real,    intent(in) :: zmix_loc          ! mixing height, local scale [m]

! Output
real, intent(out)   :: hemis1            ! emission height, including plume rise [m]
real, intent(out)   :: onder             ! part of plume below mixing height
type (TError), intent(out) :: error      ! error handling record

! Input, optional:
logical, intent(in), optional :: prelim  ! preliminary plume rise, based on stability class and preliminary wind sector (ol, uster, ... still unknown)
                                         ! if prelim = true  -> ol = -999, uster = -999, z0 = -999, zmix = zmix_loc = -999
                                         !                      these parameters are still unknown;
                                         !                      wind profile is based on power law with coefficient based on stability class
                                         ! if prelim = false or not present -> istab, isek are not used
                                         !                      wind profile is computed with ops_wvprofile (logarithmic profile) using z0, ol, uster
integer, intent(in), optional :: istab   ! index of stability class and preliminary wind sector
integer, intent(in), optional :: isek    ! index of preliminary wind sector (wind shear not yet taken into account)
real   , intent(in), optional :: astat(NTRAJ, NCOMP, NSTAB, NSEK) ! statistical meteo parameters

! Local
real                :: u_stack           ! wind speed at stack height [m/s]
real                :: u_threshold       ! threshold wind speed at height z_u_threshold [m/s]
real                :: dh_buoyancy       ! plume rise due to buoyancy [m]
real                :: dh_momentum       ! plume rise due to momentum [m]
real                :: dh                ! plume rise due to either buoyancy or momentum [m]
real                :: dthetadz_stable   ! fixed potential temperature gradient dtheta/dz [K/m] for stable conditions, used for dh_buoyancy and dh_momentum
real                :: Ta_stack          ! ambient temperature at stack height [K]
real, parameter     :: z_u_threshold = 10.0 ! threshold height below which the wind speed is cut-off [m]
real                :: Ts_stack2         ! effluent temperature at stack height, but missing value replaced by computation from Qw [K]
real                :: qw2               ! heat content emission, but missing value replaced by computation from Ts [MW]
real                :: V0                ! normal volume flux [m0^3/s]
logical             :: prelim1           ! = prelim if present, otherwise false
real                :: vw10              ! wind velocity at 10 m heigth [m/s]
real                :: pcoef             ! coefficient in wind speed power law
logical             :: non_stable        ! non-stable (unstable/neutral) conditions

character(len = 80), parameter :: ROUTINENAAM = 'ops_plumerise'

! Check optional argument:
if (.not. present(prelim)) then
   prelim1 = .false.
else
   prelim1 = prelim
endif

! write(*,'(a,4(1x,e12.5))') 'in routine ops_plumerise a: ',hemis0,hemis1,hemis1-hemis0,-999.0

! Set fixed potential temperature gradient dtheta/dz for stable conditions.
! In the OPS manual (just below Eq. 4.6) it is stated that an average value of 0.006 K/m is taken as representative
! for stable situations, following TNO (1976).
! TNO (1976) Modellen voor de berekening van de verspreiding van luchtverontreiniging inclusief aanbevelingen voor de waarden van
! parameters in het lange-termijnmodel. Staatsuitgeverij, The Hague, the Netherlands.
! Just above Eq. 4.3 of the OPS manual it is stated that this is the reference to the Dutch National Model.
! However, in the manual of the NNM (March 2002), 0.006 is not used, but the profile is reviewed per 10m layer.
! For stable conditions a dtheta/dz of at least 0.005 K/m is applied.
! MS Possibly better to calculate dtheta/dz per layer as well? Also due to changing stability with height which affects plume rise?
dthetadz_stable = 0.006

! Obtain temperature at stack height.
! Use theta(z) = T(z) + Tau*z   (Tau = 9.8*10^-3 K/m = dry adiabatic lapse rate = g/Cp) (Stull 2000, Meteorology for Scientists and Engineers, Second Edition).
!
!              T(z2) - T(z1) + Tau*(z2-z1)
! dtheta/dz = ---------------------------    -->   T(z2) = dtheta/dz * (z2-z1) - Tau*(z2-z1) + T(z1);
!                      z2 - z1
! T(z1) is the temperature at z1, taken as the temperature from the meteo-file at zmet_T = 1.5m height.
Ta_stack = dthetadz_stable*(hemis0-zmet_T) - (9.8e-3)*(hemis0-zmet_T) + (temp_C + T0)

! Check for non-stable (unstable/neutral) conditions:
if (prelim1) then
   non_stable =  ( istab .lt. 5 )
else
   non_stable =  ( ol .lt. (0. - EPS_DELTA) .or. abs(ol) .gt. 50 )
endif

! 1. Compute effluent temperature Ts_stack or heat content Qw depending on input specified;
!    Ts missing -> compute Qw, Qw missing -> compute Ts:
call ops_plumerise_qw_Ts(VsDs_opt, qw, D_stack, V_stack, Ts_stack, emis_horizontal, Ta_stack, qw2, Ts_stack2, error)
if (error%haserror) goto 9999

! 2. Compute wind speed at stack height:
if (prelim1) then
   call ops_wv_powerlaw(istab,isek,astat,hemis0,u_stack,vw10,pcoef)
else
   call ops_wvprofile(z0,hemis0,uster,ol,u_stack)
endif

! 3. Determine plume rise due to buoyancy. This is including iterations to resolve the interdependency between plume rise and wind speed
if (present(prelim)) then
   call ops_plumerise_buoyancy(z0,ol,uster,non_stable,qw2,Ta_stack,dthetadz_stable,u_stack,hemis0,dh_buoyancy,prelim,istab,isek,astat)
else
   call ops_plumerise_buoyancy(z0,ol,uster,non_stable,qw2,Ta_stack,dthetadz_stable,u_stack,hemis0,dh_buoyancy)
endif

! write(*,'(a,4(1x,e12.5))') 'in routine ops_plumerise b: ',hemis0,hemis1,hemis1-hemis0,-999.0

! 4. Determine plume rise due to momentum (no momentum plume rise in case of horizontal emission):
if (VsDs_opt .and. .not. emis_horizontal) then

    ! Low stack with low wind velocity may lead to large oversestimation of plume rise ->
    ! 10 m is used as threshold for wind speed calculation (personal communication Hans Erbrink):
    if (hemis0 .lt. z_u_threshold) then
       if (prelim1) then
          call ops_wv_powerlaw(istab,isek,astat,z_u_threshold,u_threshold,vw10,pcoef)
       else
          call ops_wvprofile(z0,z_u_threshold,uster,ol,u_threshold)
       endif
       call ops_plumerise_momentum(u_threshold,D_stack,V_stack,Ts_stack2,Ta_stack,dthetadz_stable,non_stable,dh_momentum)
    else
       call ops_plumerise_momentum(u_stack,D_stack,V_stack,Ts_stack2,Ta_stack,dthetadz_stable,non_stable,dh_momentum)
    endif
else
    dh_momentum = 0.0
endif
! write(*,'(a,4(1x,e12.5))') 'in routine ops_plumerise : ',dh_buoyancy,dh_momentum

! 5. Compare plume rise due to buoyancy and momentum, which process is dominant? Adopt that plume rise.
! If buoyancy plume rise is greater than momentum plume rise, discard momentum plume rise,
! because in the parameterisation of buoyancy plume rise, momentum plume rise has been taken into account (see NNM Paarse boekje):
if (dh_buoyancy .ge. dh_momentum) then
    dh = dh_buoyancy
else
    dh = dh_momentum
endif
hemis1 = hemis0 + dh
! write(*,'(a,4(1x,e12.5))') 'in routine ops_plumerise c: ',hemis0,hemis1,hemis1-hemis0,-999.0

! 6. plume penetration
if (.not. prelim1) call ops_plume_penetration(hemis0,zmix,zmix_loc,ol,dh,hemis1,onder)
! write(*,'(a,4(1x,e12.5))') 'in routine ops_plumerise d: ',hemis0,hemis1,hemis1-hemis0,-999.0

return

9999 call ErrorCall(ROUTINENAAM, error)

end subroutine ops_plumerise

!-------------------------------------------------------------------------------------------------------------------------------
subroutine ops_plumerise_qw_Ts(VsDs_opt, qw, D_stack, V_stack, Ts_stack, emis_horizontal, Ta_stack, qw2, Ts_stack2, error)

! Compute effluent temperature Ts_stack or heat content Qw depending on input specified;
! Ts_stack missing -> compute Qw, Qw missing -> compute Ts_stack. Note that is has been checked already that either one of them is missing.
!

use Binas, only: T0, pi                  ! melting point of ice [K], pi
use m_ops_utils, only: is_missing
use m_error

! Input
logical, intent(in) :: VsDs_opt           ! include exit velocity (Vs = V_stack), stack diameter (Ds = D_stack) and effluent temperature (Ts_stack) in the emission file
real,    intent(in) :: qw                 ! heat content [MW]
real,    intent(in) :: D_stack            ! diameter of the stack [m]
real,    intent(in) :: V_stack            ! exit velocity of plume at stack tip [m/s]
real,    intent(in) :: Ts_stack           ! temperature of effluent from stack [K]
logical, intent(in) :: emis_horizontal    ! horizontal outflow of emission
real,    intent(in) :: Ta_stack           ! ambient temperature at stack height (K)

! Output:
real,    intent(out) :: Ts_stack2         ! effluent temperature at stack height, but missing value replaced by computation from Qw [K]
real,    intent(out) :: qw2               ! heat content emission, but missing value replaced by computation from Ts [MW]
type (TError), intent(out) :: error       ! error handling record

!Local:
real                 :: C1                ! help variable = rho0*Cp0*(pi*(0.5*D_stack)**2)*V_stack*T0*(1.0e-6). Needed for Ts_stack2
real                 :: V0                ! normal volume flux [m0**3/s)

character(len = 80), parameter :: ROUTINENAAM = 'ops_plumerise_qw_Ts'

! qw = rho0*Cp0*V0*(Ts - Ta)*1e-6 or 1e6*qw/(rho0*Cp0*V0) = Ts - Ta <=> Ts = Ta + 1e6*qw/(rho0*Cp0*V0)
! T0   = reference temperature = 273.15 K = 0 C
! P0   = reference pressure = 1 atm = 101.325 kPa
! rho0 = reference density air (= 1.293 kg/m3) at pressure P0, temperature T0
! Cp0  = reference specific heat of air at pressure P0, temperature T0 (= 1005 J/kg/K)
! V0   = normal volume flux (m03/s) at pressure P0, temperature T0
! Ts   = effluent temperature (K)
! Ta   = ambient temperature at stack height (K)

! write(*,*) 'ops_plumerise_qw_Ts a:',VsDs_opt,qw,Ts_stack
if (VsDs_opt) then
   if (is_missing(Ts_stack)) then

      !----------------------------------------------------------------
      ! Heat content qw given, compute effluent temperature Ts_stack2
      !----------------------------------------------------------------

      if (emis_horizontal) then
         Ts_stack2 = -999.0
      else
         if (qw .eq. 0.0) then
            Ts_stack2 = Ta_stack
         else
         ! Compute effluent temperature (not needed in case of horizontal outflow):
            ! Ts = Ta + 1e6*qw/(rho0*Cp0*V0)                 (1)
            ! V0 = (pi*(0.5*D_stack)**2)*V_stack*T0/Ts_stack (2)
            ! Substitute (2) in (1) gives Ts = Ta + f Ts <=> Ts = Ta/(1-f), with f = 1e6*qw/(rho0*Cp0*(pi*(0.5*D_stack)**2)*V_stack*T0):
            C1 = rho0*Cp0*(pi*(0.5*D_stack)**2)*V_stack*T0*(1.0e-6)
            Ts_stack2 = Ta_stack/(1.0 - qw/C1)

            ! Check:
            ! This check is not needed; next check is more stringent:
            ! if (qw .ge. C1) then
            !     CALL SetError('Computing negative effluent gas temperature Ts_stack [K] from given heat content.', &
            !                   'Probably, the given exit velocity and/or stack diameter are not in agreement with the given heat content.', error)
            ! endif
            ! Check temperature range (degrees C) (see also check in ops_read_source - check_stack_param):
            if (Ts_stack2-T0 .lt. 0.0 .or. Ts_stack2-T0 .gt. 2000.0) then  ! See also check in ops_read_source - check_stack_param
               call SetError('Computing effluent gas temperature Ts_stack from given heat content -> Ts_stack outside permitted range.',error)
               call ErrorParam('heat content [MW]',qw,error)
               call ErrorParam('diameter of the stack [m]',D_stack,error)
               call ErrorParam('exit velocity of plume at stack tip [m/s]',V_stack,error)
               call ErrorParam('ambient temperature at stack height [C]',Ta_stack-T0,error)
               call ErrorParam('lower limit effluent gas temperature [C]',0.0,error)
               call ErrorParam('<effluent gas temperature [C]>',Ts_stack2-T0,error)
               call ErrorParam('upper limit effluent gas temperature [C]',2000.0,error)
               call ErrorCall(ROUTINENAAM, error)
            endif
         endif   ! if qw = 0
      endif   ! if emis_horizontal
      qw2 = qw
   else

      !------------------------------------------------
      ! Ts_stack is given; compute heat content qw2
      !------------------------------------------------

      ! Compute normal volume flux, according to ideal gas-law (at constant pressure): V0_flux/T0 = Vs_flux/Ts, Vs_flux = pi R**2 Vs_stack
      V0 = (pi*(0.5*D_stack)**2)*V_stack*T0/Ts_stack

      ! Compute qw:
      Ts_stack2 = Ts_stack
      qw2 = rho0*Cp0*V0*(Ts_stack - Ta_stack)*1e-6
   endif
else
   ! VsDs_opt = FALSE:
   Ts_stack2 = Ts_stack ! is missing, but not used hereafter
   qw2 = qw
endif
! write(*,*) 'ops_plumerise_qw_Ts b:',VsDs_opt,qw2,Ts_stack2

end subroutine ops_plumerise_qw_Ts

!------------------------------------------------------------
subroutine ops_plumerise_buoyancy(z0, ol, uster, non_stable, qw, Ta_stack, dthetadz_stable, u_stack, hemis0, dh_buoyancy, prelim, istab, isek, astat)
!-------------------------------------------------------------------------------------------------------------------------------
!
! DESCRIPTION: This routine calculates the plume rise due to buoyancy.
!    This routine includes plume rise formulations given by Briggs(1969) and Briggs(1971).
!    This method is equal to the method used in the (old) Dutch National Model (TNO, 1976).
!                                                HvJ 960121
!    Extra iteration, because wind speed depends on plume height and vice versa.
!
!-------------------------------------------------------------------------------------------------------------------------------

use m_commonconst, only: pi, NTRAJ, NCOMP, NSTAB, NSEK, EPS_DELTA
use Binas, only: grav, T0              ! acceleration of gravity [m/s2], melting point of ice [K]

! Input
real, intent(in)   :: z0               ! roughness length [m]
real, intent(in)   :: ol               ! Monin-Obukhovlengte [m]
real, intent(in)   :: uster            ! friction velocity [m/s]
logical, intent(in):: non_stable       ! non-stable (unstable/neutral) conditions
real, intent(in)   :: qw               ! heat content (MW)
real, intent(in)   :: Ta_stack         ! ambient temperature at stack height [K]
real, intent(in)   :: dthetadz_stable  ! fixed potential temperature gradient dtheta/dz [K/m] for stable conditions, used for dh_buoyancy and dh_momentum
real, intent(in)   :: u_stack          ! wind speed at stack height [m/s]
real, intent(in)   :: hemis0           ! initial emission height = stack height [m]

! Output
real, intent(out)  :: dh_buoyancy      ! plume rise due to buoyancy [m]

! Input, optional:
logical, intent(in), optional :: prelim  ! preliminary plume rise, based on stability class and preliminary wind sector (ol, uster, ... still unknown)
                                         ! if prelim = true  -> ol = -999, uster = -999, z0 = -999, zmix = zmix_loc = -999
                                         !                      these parameters are still unknown;
                                         !                      wind profile is based on power law with coefficient based on stability class
                                         ! if prelim = false or not present -> istab, isek are not used
                                         !                      wind profile is computed with ops_wvprofile (logarithmic profile) using z0, ol, uster
integer, intent(in), optional :: istab   ! index of stability class and preliminary wind sector
integer, intent(in), optional :: isek    ! index of preliminary wind sector (wind shear not yet taken into account)
real   , intent(in), optional :: astat(NTRAJ, NCOMP, NSTAB, NSEK) ! statistical meteo parameters

! Local
real               :: f                ! stack buoyancy flux [m^4/s^3]
real               :: u_plume          ! wind speed at effective plume height, representative for the whole plume rise length [m/s]
real               :: dtdz             ! potential temperature gradient [K/m]
real               :: s                ! stability parameter [s^-2]
logical            :: prelim1          ! = prelim if present, otherwise false
real               :: vw10             ! wind velocity at 10 m heigth [m/s]
real               :: pcoef            ! coefficient in wind speed power law
character(len=1)   :: char_debug1      ! debug character (test only)

! Iteration variables
! iteration converges if |dh_buoyancy - dh_buoyancy_prev| < epsa + epsr*dh_buoyancy
integer                  :: it               ! iteration index
logical                  :: converged        ! iteration has converged
real                     :: dh_buoyancy_prev ! plume rise of previous iteration
integer, parameter       :: maxit = 10       ! maximal number of iterations
real, parameter          :: epsa = 0.1       ! absolute error tolerance (m)
real, parameter          :: epsr = 0.05      ! relative error tolerance

!-------------------------------------------------------------------------------------------------------------------------------
! MS Briggs is developed for large stacks (energy production,..); should not be used for low emissions, e.g. emissions from animal housing.

! Check optional argument:
if (.not. present(prelim)) then
   prelim1 = .false.
else
   prelim1 = prelim
endif

if ( qw .gt. (0. + EPS_DELTA)) then

   !Initialization
   u_plume = u_stack
   dh_buoyancy = 0.0
   ! if (prelim1) write(*,'(a,2(1x,e12.5))') 'ops_plumerise_buoyancy a',hemis0,u_stack

   ! f = stack buoyancy flux (4.5 in 'The OPS-model Description of OPS 4.5.0). Briggs 1982, eq. 11. Assumed that Ps/Pa = 1.
   ! f = g/(pi*0.0013*T)*qw = 9.81/(3.14*0.0013*273)*qw    ! 0.0013 = rho*cp*fac_W_to_MW = 1.293*1005*1e-6
   ! f = 8.8*qw
   f = (grav*1.0e6/(pi*rho0*Cp0*T0))*qw

   ! We want to use a wind speed that is representative for the whole plume rise length,
   ! but because we don't know the plume rise yet, we need an iteration.
   ! Initialisation for iteration:
   converged = .false.
   it        = 1
   dh_buoyancy_prev = -999.

   ! Do iteration:
   do while (.not. converged .and. it .le. maxit)

      ! plume rise for unstable or neutral conditions, L < 0 or |L| > 50 (Eq 4.3 - 4.4 in 'The OPS-model Description of OPS 4.5.0):
      ! original value plrise_nonstab_Fbsplit = 55
      if ( non_stable ) then
         if ( f .ge. 55 ) then
            dh_buoyancy = 38.8*f**0.6/u_plume                                 ! Briggs 1971 (as in the Dutch Nat. Mod.)
            ! char_debug1 = 'd'
         else
            dh_buoyancy = 21.3*f**0.75/u_plume
            ! char_debug1 = 'c'
         endif
     else
        ! Stable conditions, 0 < L < 50 (3.28 OPS report)
        ! use fixed potential temperature gradient dtheta/dz = 0.006 (K/m); is valid for conditions above mixing layer.
        ! For low emissions and stable atmospheric conditions, dtheta/dz = 0.2 K/m
        ! original value: plrise_stab_dtheta_dz = 0.006
         s    = 9.81/Ta_stack*dthetadz_stable         ! Stability parameter, Briggs (1969) Eq. 4.16.
         dh_buoyancy = 2.6*(f/(s*u_plume))**0.333     ! Briggs 1982, Eq. 59.

         ! Check with old code of routine voorlpl:
         ! if (prelim1) then
         !    ! voorlpl: dh_buoyancy = 65.*(qw/u_plume)**.333
         !    ! 2.6*(f/(s*u_plume))**0.333 = 2.6*(8.8*qw/(s*u_plume))**0.333 = 2.6*(8.8**.333)*((1/s)**.333)*(qw/u_plume)**.333
         !    write(*,'(a,7(1x,e12.5))') 'ops_plumerise_buoyancy b',hemis0,dh_buoyancy,2.6*(f/s)**0.333,65.*qw**0.333,(grav*1.0e6/(pi*rho0*Cp0*T0)),2.6*((grav*1.0e6/(pi*rho0*Cp0*T0))**.333)*((1.0/s)**.333),Ta_stack
         !    char_debug1 = 'b'
         ! endif
      endif

      ! Check for convergence:
      converged = (abs(dh_buoyancy - dh_buoyancy_prev) .lt. epsa + epsr*dh_buoyancy )

      ! Update for next iteration:
      if (.not. converged .and. it .lt. maxit) then
         ! Compute wind speed at z = h_stack + 1/2 plume_rise:
         if (prelim1) then
            call ops_wv_powerlaw(istab,isek,astat,hemis0+dh_buoyancy/2,u_plume,vw10,pcoef)
         else
            call ops_wvprofile(z0,hemis0+dh_buoyancy/2,uster,ol,u_plume)
         endif
         dh_buoyancy_prev = dh_buoyancy
      endif
      it = it + 1
   enddo
   ! if (prelim1) write(*,'(a,a1,2(1x,e12.5))') 'ops_plumerise_buoyancy ',char_debug1,hemis0,dh_buoyancy  ! char_debug1 the same as in voorlpl

   ! Check for convergence:
   ! if (.not. converged) then
   !    write(fu_err,*) ' -------------------------------------------------------'
   !    write(fu_err,*) ' WARNING, iteration in ops_plumerise has not converged'
   !    write(fu_err,*) ' plume rise                   : ', delh
   !    write(fu_err,*) ' plume rise previous iteration: ', delh_prev
   !    write(fu_err,*) ' max. number of iterations    : ', maxit
   !    write(fu_err,*) ' heat content (MW)            : ', qw
   !    write(fu_err,*) ' stack height                 : ', hs
   ! endif

ELSE
   ! Qw = 0
   dh_buoyancy = 0.0
ENDIF

end subroutine ops_plumerise_buoyancy


!------------------------------------------------------------
subroutine ops_plumerise_momentum(u_stack,D_stack,V_stack,Ts_stack,Ta_stack,dthetadz_stable,non_stable,dh_momentum)

! Subroutine ops_plumerise_momentum computes plume rise due to the momentum of a emission with a vertical exit velocity.
! Fortran-version of STACKS routine impulsstijging.dat (Hans Erbrink).
! Based on:
! C.A. Briggs, Plume Rise (1969)
!    Air resources atmospheric turbulence and diffusion laboratory
!    Environmental science services administration
!    Oak Ridge Tennessee.
!
! See also: D. Bruce Turner, Thomas Chico and Joseph A. Catalano
!       TUPOS- A MULTIPLE SOURCE GAUSSIAN DISPERSION
!       ALGORITHM USING ON-SITE TURBULENCE DATA
!       ATMOSPHERIC SCIENCES RESEARCH LARORATORY
!       OFFICE OF RESEARCH AND DEVELOPMENT
!       U. S. ENVIRONMENTAL PROTECTION AGENCY
!       RESEARCH TRIANGLE PARK, NC
! See also: ISCST3 Tech Guide
!       Gaussian Plume Air Dispersion Model
!       https://www.weblakes.com/guides/iscst3/section6/6_1_4.html  (14-2-2018)

use m_commonconst, only: EPS_DELTA
use m_ops_utils, only: is_missing

! Input:
real   , intent(in)  :: u_stack              ! wind speed at stack height [m/s]. For low sources the threshold height of 10m is applied.
real   , intent(in)  :: D_stack              ! stack internal diameter [m]
real   , intent(in)  :: V_stack              ! exit velocity of plume at stack tip [m/s]
real   , intent(in)  :: Ts_stack             ! temperature of effluent from stack [K]
real   , intent(in)  :: Ta_stack             ! ambient temperature at stack height [K]
real   , intent(in)  :: dthetadz_stable      ! fixed potential temperature gradient dtheta/dz [K/m] for stable conditions, used for dh_buoyancy and dh_momentum
logical, intent(in)  :: non_stable           ! non-stable (unstable/neutral) conditions

! Output:
real   , intent(out) :: dh_momentum          ! plume rise due to momentum [m]

! Local:
real :: dh_nonstable                         ! plume rise due to momentum in non-stable conditions (unstable/neutral) [m]
real :: dh_stable                            ! plume rise due to momentum in stable conditions [m]

if (V_stack .le. 0.0) then
   ! No momentum:
   dh_momentum = 0.0
else

   ! Plume rise due to momentum for non-stable (unstable/neutral) conditions (Briggs, 1969, Eq. 5.2)
   dh_nonstable = 3*D_stack*V_stack/u_stack

   ! Plume rise due to momentum for stable conditions:
   !                      2        2
   !                    Vs  D_stack    1/3     1/2        -1/6
   !     dh = 0.646 [ --------------- ]    (Ta)     (dTdz)
   !                      Ts  Us
   ! This originates from (Briggs 1969: Eq. 4.28, 4.19b, 4.16), see also Turner et al. (1986):
   !              Fm     1/3     -1/6              rhos       2     2     Ps*Ta       2               2              g   dtheta
   ! dh = 1.5 [ ------- ]     [s]        ;  Fm =  ------  (Vs)  (r0)  =  -------- (Vs)  0.25 (D_stack)     ;  s = [ ---  ------ ]
   !            u_stack                             rho                   P*Ts                                       Ta    dz
   ! with:
   ! Fm   = momentum flux parameter (m4/s3)
   ! rhos = density of gases emitted from stack (g/m3)
   ! rho  = average density of ambient air (g/m3)
   ! Vs   = exit velocity of plume at stack tip (m/s) ( = V_stack below)
   ! r0   = internal stack radius (m)
   ! ideal gas law: P = rho*R*Ta
   ! P    = pressure ambient air, Ps = pressure of gases emitted from the stack, R = gas constant
   ! Ta   = average absolute temperature of ambient air (K) ( = Ta_stack below)
   ! Ts   = temperature of gases emitted from the stack (K) ( = Ts_stack below)

   ! This can be rewritten to (assuming Ps/P = 1):
   !                                        2        2                                                      2        2
   !                1/3      -1/6    Ps   Vs  D_stack    1/3     1/3     1/6            -1/6              Vs  D_stack    1/3     1/2             -1/6
   ! dh = 1.5 * 0.25   * 9.81     [ ---- -------------- ]    (Ta)    (Ta)    (dtheta/dz)      =  0.646 [ -------------- ]    (Ta)     (dtheta/dz)
   !                                 P    Ts  u_stack                                                     Ts  u_stack
   !
   ! For stable conditions, the lower value of dh_nonstable and dh_stable is chosen (see also Turner et al., 1986)
   dh_stable = 0.646 * (( (V_stack**2.)*(D_stack**2.) / (Ts_stack*u_stack) )**(1./3.)) * (Ta_stack**0.5) * (dthetadz_stable**(-1./6.))
   if (dh_stable > dh_nonstable) dh_stable = dh_nonstable

   ! Set output plume rise dh_momentum, depending on stability:
   if (non_stable) then
      dh_momentum = dh_nonstable
   else
      dh_momentum = dh_stable
   endif

endif

end subroutine ops_plumerise_momentum

!------------------------------------------------------------
subroutine ops_plume_penetration(hemis0,zmix,zmix_loc,ol,dh,hemis1,onder)
!
! Subroutine to determine whether there is plume penetration.
!
use m_commonconst, only: EPS_DELTA

! Input
real, intent(in)    :: hemis0     ! initial emission height = stack height [m]
real, intent(in)    :: zmix       ! mixing height [m]
real, intent(in)    :: zmix_loc   ! mixing height, local scale [m]
real, intent(in)    :: ol         ! Monin-Obukhov length [m]
real, intent(in)    :: dh         ! plume rise due to either buoyancy or momentum [m]

! Input/Output
real, intent(inout) :: hemis1     ! emission height, including plume rise [m]

! Output
real, intent(out)   :: onder      ! part of plume below mixing height
! The emission distribution of an area source has a sigma equal to the height of the source hemis0.
! If hemis0 is close to the inversion height, the emission must be distributed over mixing layer and reservoir layer.
! last change: 21 Oct 2002
! Based on Kincaid data
!
! hemis1 < hemis0 not yet possible (only plume rise is computed here)
! hemis1 = hemis0 is possible
! onder is fraction (0-1) of plume in mixing layer ("onder"= below)
! onder = 1 -> plume completely below mixing height
! onder = 0 -> plume completely above mixing height
if( (hemis0 .gt. zmix + EPS_DELTA) .or. (hemis1 .le. hemis0 + EPS_DELTA) ) then
   onder = (zmix - hemis1)/zmix + 0.5   ! OPS
else
   onder = (zmix - hemis1)/dh + 0.5   ! Briggs (1975) and NNM
endif
!
! Temperature inversion effects in stable and unstable situations.
! In principle only applicable in situations close to the source, not if mixing height has increased much compared to the local
! mixing height.
! Laatst gewijzigd: 21 Oct 2002
!
! Stable and unstable conditions and stack < mixing height -> add extra amount plrise_ci_add_stab_unstab to onder;

if ( hemis0 .lt. zmix_loc .and. abs(ol) .lt. 100 ) then
   onder = onder + 0.35
endif

! Limit onder, such that 0 <= onder <= 1
if (onder .gt. (1. + EPS_DELTA)) then
   onder = 1.
else if (onder .LT. (0. - EPS_DELTA)) then
   onder = 0.
else
   continue
endif

! Plume centre is maximal equal to mixing haight:
if ((hemis1 .gt. (zmix + EPS_DELTA)) .and. (onder .gt. (0. + EPS_DELTA))) then
   hemis1 = zmix
endif

return

end subroutine ops_plume_penetration

end module m_ops_plumerise
