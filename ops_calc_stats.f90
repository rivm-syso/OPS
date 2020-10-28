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
! NAME                : %M%
! SCCS (SOURCE)       : %P%
! RELEASE - LEVEL     : %R% - %L%
! BRANCH - SEQUENCE   : %B% - %S%
! DATE - TIME         : %E% - %U%
! WHAT                : %W%:%E%
! AUTHOR              : OPS-support  Chris Twenh"ofel (Cap Gemini)
! FIRM/INSTITUTE      : RIVM/LLO
! LANGUAGE            : FORTRAN-77/90
! DESCRIPTION         : Calculates summary statistics for concentration and deposition.
! EXIT CODES          :
! FILES I/O DEVICES   :
! SYSTEM DEPENDENCIES : HP Fortran
! CALLED FUNCTIONS    :
! UPDATE HISTORY      :
!-------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE ops_calc_stats(nrrcp, nsubsec, frac, cpri, csec, drydep, wetdep, gemre, sdrypri, sdrysec, snatpri, snatsec, somvnpri,        &
                                &  somvnsec, vvchem, vtel, telvnpri, telvnsec, grid, conc_cf, amol21, ugmoldep, csubsec,       &
                       &  gemcpri, gemcsec, totddep, gemddep, gemddpri, gemddsec, ddrpri, ddrsec, totwdep, gemwdep,            &
                       &  gemwdpri, gemwdsec, wdrpri, wdrsec, gemprec, tottdep, gemtdep, ccr, gem_subsec)

USE m_commonconst                                                              ! EPS_DELTA only

IMPLICIT NONE

! CONSTANTS
CHARACTER*512                                    :: ROUTINENAAM                !
PARAMETER    (ROUTINENAAM = 'ops_calc_stats')

! SUBROUTINE ARGUMENTS - INPUT
INTEGER*4, INTENT(IN)                            :: nrrcp                      ! number of receptor points
INTEGER*4, INTENT(IN)                            :: nsubsec                    ! number of sub-secondary species
REAL*4,    INTENT(IN)                            :: frac(nrrcp)                ! fraction per cell inside NL
REAL*4,    INTENT(IN)                            :: cpri(nrrcp)                ! primary concentration [ug/m3]
REAL*4,    INTENT(IN)                            :: csec(nrrcp)                ! secondary concentration [ug/m3]
REAL*4,    INTENT(IN)                            :: drydep(nrrcp)              ! dry deposition
REAL*4,    INTENT(IN)                            :: wetdep(nrrcp)              ! wet deposition
REAL*4,    INTENT(IN)                            :: gemre                      ! yearly mean precipitation from meteo statistics [mm/h]
DOUBLE PRECISION, INTENT(IN)                     :: sdrypri                    !
DOUBLE PRECISION, INTENT(IN)                     :: sdrysec                    !
DOUBLE PRECISION, INTENT(IN)                     :: somvnpri                   !
DOUBLE PRECISION, INTENT(IN)                     :: somvnsec                   !
DOUBLE PRECISION, INTENT(IN)                     :: vvchem                     !
DOUBLE PRECISION, INTENT(IN)                     :: vtel                       !
DOUBLE PRECISION, INTENT(IN)                     :: telvnpri                   !
DOUBLE PRECISION, INTENT(IN)                     :: telvnsec                   !
REAL*4,    INTENT(IN)                            :: grid                       !
REAL*4,    INTENT(IN)                            :: conc_cf                    !
REAL*4,    INTENT(IN)                            :: amol21                     !
REAL*4,    INTENT(IN)                            :: ugmoldep                   !
REAL*4,    INTENT(IN)                            :: csubsec(nrrcp,nsubsec)     ! concentration of sub-secondary species [ug/m3]

! SUBROUTINE ARGUMENTS - I/O
DOUBLE PRECISION, INTENT(INOUT)                  :: snatpri                    !
DOUBLE PRECISION, INTENT(INOUT)                  :: snatsec                    !

! SUBROUTINE ARGUMENTS - OUTPUT
REAL*4,    INTENT(OUT)                           :: gemcpri                    ! grid mean for primary concentration [ug/m3]
REAL*4,    INTENT(OUT)                           :: gemcsec                    ! grid mean for secondary concentration [ug/m3]
REAL*4,    INTENT(OUT)                           :: totddep                    ! grid total dry deposition (g/s)
REAL*4,    INTENT(OUT)                           :: gemddep                    ! grid mean for dry deposition ["depeh"]
REAL*4,    INTENT(OUT)                           :: gemddpri                   ! grid mean for dry deposition of primary component ["depeh"]
REAL*4,    INTENT(OUT)                           :: gemddsec                   ! grid mean for dry deposition of secondary component ["depeh"]
REAL*4,    INTENT(OUT)                           :: ddrpri                     ! effective dry deposition velocity (primary component) [cm/s]
REAL*4,    INTENT(OUT)                           :: ddrsec                     ! effective dry deposition velocity (secondary component) [cm/s]
REAL*4,    INTENT(OUT)                           :: totwdep                    ! grid total wet deposition (g/s)
REAL*4,    INTENT(OUT)                           :: gemwdep                    ! grid mean for wet deposition ["depeh"]
REAL*4,    INTENT(OUT)                           :: gemwdpri                   ! grid mean for wet deposition of primary component ["depeh"]
REAL*4,    INTENT(OUT)                           :: gemwdsec                   ! grid mean for wet deposition of secondary component ["depeh"]
REAL*4,    INTENT(OUT)                           :: wdrpri                     ! effective wet deposition rate (primary component) [%/h]
REAL*4,    INTENT(OUT)                           :: wdrsec                     ! effective wet deposition rate (secondary component) [%/h]
REAL*4,    INTENT(OUT)                           :: gemprec                    ! grid mean yearly precipitation [mm]
REAL*4,    INTENT(OUT)                           :: tottdep                    ! grid total of total deposition (g/s)
REAL*4,    INTENT(OUT)                           :: gemtdep                    ! grid mean of total deposition ["depeh"]
REAL*4,    INTENT(OUT)                           :: ccr                        ! effective chemical conversion rate [%/h]
REAL*4,    INTENT(OUT)                           :: gem_subsec(nsubsec)        ! grid mean for concentration of sub-secondary species [ug/m3]

! LOCAL VARIABLES
REAL*4                                           :: somcsec                    ! sum of secondary concentrations [ug/m3]
REAL*4                                           :: somddep                    ! sum of dry depositions ["depeh"]
REAL*4                                           :: somwdep                    ! sum of wet depositions ["depeh"]

! LOCAL VARIABLES
REAL*4                                           :: cf                         ! conversion factor
REAL*4                                           :: somcpri                    ! sum of primary concentrations [ug/m3]
REAL*4                                           :: som_subsec(nsubsec)        ! sum of concentrations of sub-secondary species [ug/m3]
REAL*4                                           :: somfrac                    ! sum of frac
INTEGER*4                                        :: isubsec                    ! index of sub-secondary species

! SCCS-ID VARIABLES
CHARACTER*81                                     :: sccsida                    !
sccsida = '%W%:%E%'//char(0)
!-------------------------------------------------------------------------------------------------------------------------------
! Summation over grid cells
!
! Remark: for some output variables the sum is already determined in a previously called subroutine, for some others the
!         summation is done here.

somfrac    = SUM(frac(:))
somcpri    = SUM(cpri(:) * frac(:))
somcsec    = SUM(csec(:) * frac(:))
do isubsec = 1,nsubsec
   som_subsec(isubsec) = SUM(csubsec(:,isubsec) * frac(:))
enddo
somddep    = SUM(drydep(:) * frac(:))
somwdep    = SUM(wetdep(:) * frac(:))
!
! Calculation of grid averages and effective deposition rates ---
!JA* Condities wanneer berekend kan/mag worden nog inbouwen (zie oude ops_print_grid) ????
! grid = grid resolution = grid cell size
!
cf  = grid*grid*.1e-5/3600./ugmoldep/amol21

! (1) concentration
gemcpri    = somcpri/somfrac
gemcsec    = somcsec/somfrac
gem_subsec = som_subsec/somfrac

! (2) dry deposition
totddep  = somddep*cf
gemddep  = somddep/somfrac
gemddpri = sdrypri/somfrac*ugmoldep*amol21
gemddsec = sdrysec/somfrac*ugmoldep
if (somcpri .gt. 0.0) then
   ddrpri = sdrypri/somcpri/36  ! factor 36 from conversion of [ug/m2/h]/[ug/m3] to [cm/s]
else
   ddrpri = -999.0
endif
if (somcsec .gt. 0.0) then
   ddrsec = sdrysec/somcsec/36
else
   ddrsec = -999.0
endif

! (3) wet deposition
IF (ABS(somvnpri) .LE. DPEPS_DELTA) THEN
!
!  No wet deposition if there is no rain for this meteo class
!
   snatpri = 0.
   snatsec = 0.
ENDIF
totwdep  = somwdep*cf
gemwdep  = somwdep/somfrac
gemwdpri = snatpri/somfrac*ugmoldep*amol21
gemwdsec = snatsec/somfrac*ugmoldep
gemprec  = gemre*8760.

!-(4) Total deposition
tottdep = (somddep+somwdep)*cf
gemtdep = gemddep+gemwdep
!
! Calculation of effective chemical conversion rate
!
IF (ABS(vtel) > DPEPS_DELTA) THEN
  ccr = vvchem/vtel
ELSE
  ccr = vvchem
ENDIF
!
! Calculation of effective wet deposition rates for primary and secondary substance
!
IF (ABS(telvnpri) > DPEPS_DELTA) THEN
  wdrpri = somvnpri/telvnpri
ELSE
  wdrpri = somvnpri
ENDIF
IF (ABS(telvnsec) > DPEPS_DELTA) THEN
  wdrsec = somvnsec/telvnsec
ELSE
  wdrsec = somvnsec
ENDIF

RETURN
END SUBROUTINE ops_calc_stats
