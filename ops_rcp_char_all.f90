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
! DESCRIPTION        : Fill arrays with receptor characteristics:
!                      values for roughness (z0), landuse (lu), background concentration and [HNO3]/[NO3]_total ratio.
! EXIT CODES         :
! FILES AND OTHER    :
!    I/O DEVICES
! SYSTEM DEPENDENCIES: HP-Fortran
! CALLED FUNCTIONS   : amcgeo, ops_getz0, ops_getlu, ops_bgcon
! UPDATE HISTORY :
!-------------------------------------------------------------------------------------------------------------------------------
subroutine ops_rcp_char_all(icm, iopt_vchem, isec, nsubsec, xm, ym, f_z0user, z0_user, z0nlgrid, z0eurgrid, lugrid, so2bggrid, nh3bggrid, f_subsec_grid, &
                          & nrrcp, gxm, gym, lu_rcp_dom_all, z0_rcp_all, rhno3_rcp, nh3bg_rcp, so2bg_rcp, f_subsec_rcp, domlu, error)

USE m_aps
USE m_geoutils
USE m_commonconst
USE m_ops_bgcon
USE m_error

IMPLICIT NONE

! CONSTANTS
CHARACTER*512                                    :: ROUTINENAAM
PARAMETER    (ROUTINENAAM = 'ops_rcp_char_all')

! SUBROUTINE ARGUMENTS - INPUT
INTEGER*4, INTENT(IN)                            :: icm
INTEGER*4, INTENT(IN)                            :: iopt_vchem                 ! option for chemical conversion rate (0 = old OPS, 1 = EMEP)
LOGICAL*4, INTENT(IN)                            :: isec
INTEGER*4, INTENT(IN)                            :: nsubsec                    ! number of sub-secondary species
REAL*4,    INTENT(IN)                            :: xm(nrrcp)                  ! x-coordinates of receptors
REAL*4,    INTENT(IN)                            :: ym(nrrcp)                  ! y-coordinates of receptors
LOGICAL*4, INTENT(IN)                            :: f_z0user
REAL*4,    INTENT(IN)                            :: z0_user                    ! roughness length specified by user [m]
TYPE (TApsGridInt), INTENT(IN)                   :: z0nlgrid                   ! map of roughness lengths in NL [m]
TYPE (TApsGridInt), INTENT(IN)                   :: z0eurgrid                  ! map of roughness lengths in Europe [m]
TYPE (TApsGridInt), INTENT(IN)                   :: lugrid                     ! grid with land use information
TYPE (TApsGridReal), INTENT(IN)                  :: so2bggrid                  ! grid of SO2 background concentrations [ug/m3]
TYPE (TApsGridReal), INTENT(IN)                  :: nh3bggrid                  ! grid of NH3 background concentrations [ug/m3]
TYPE (TApsGridReal), INTENT(IN)                  :: f_subsec_grid              ! grids of fractions for sub-secondary species, HNO3/NO3_total, NO3_C/NO3_total, NO3_F/NO3_total [-]
INTEGER*4, INTENT(IN)                            :: nrrcp                      ! number of receptors
LOGICAL*4, INTENT(IN)                            :: domlu                      ! index of dominant land use class

! SUBROUTINE ARGUMENTS - OUTPUT
REAL*4,    INTENT(OUT)                           :: gxm(nrrcp)
REAL*4,    INTENT(OUT)                           :: gym(nrrcp)
REAL*4,    INTENT(OUT)                           :: rhno3_rcp(nrrcp)
REAL*4,    INTENT(OUT)                           :: nh3bg_rcp(nrrcp)
REAL*4,    INTENT(OUT)                           :: so2bg_rcp(nrrcp)
REAL*4,    INTENT(OUT)                           :: f_subsec_rcp(nrrcp,nsubsec)   ! fractions for sub-secondary species, HNO3/NO3_total, NO3_C/NO3_total, NO3_F/NO3_total [-]

! SUBROUTINE ARGUMENTS - OUTPUT
INTEGER*4                                        :: landuse(NLU+1)             ! land-use value at receptor
                                                                               ! landuse(1)    = index of dominant landuse
                                                                               ! landuse(lu+1) = percentage of grid cell with landuse class lu, lu = 1,NLU
                                                                               ! For locations outside lugrid, a default land use class = 1 (grass) is taken.
INTEGER*4, INTENT(INOUT)                         :: lu_rcp_dom_all(nrrcp)      ! index of dominant land use for all receptor points
REAL*4,    INTENT(INOUT)                         :: z0_rcp_all(nrrcp)          ! roughness lengths for all receptors; from z0-map or receptor file [m]
TYPE (TError), INTENT(INOUT)                     :: error

! LOCAL VARIABLES
INTEGER*4                                        :: ircp                       ! index of receptor
INTEGER*4                                        :: isubsec                    ! index of sub-secondary species
REAL*4                                           :: so2bgconc                  ! background concentratie SO2
REAL*4                                           :: nh3bgconc                  ! background concentration NH3 at receptor [ppb]
LOGICAL                                          :: z0found
INTEGER                                          :: ifield                     ! field index in f_subsec_grid

! SCCS-ID VARIABLES
CHARACTER*81                                     :: sccsida
sccsida = '%W%:%E%'//char(0)

!-------------------------------------------------------------------------------------------------------------------------------
!
! Loop over receptors
!
DO ircp = 1, nrrcp
!
! Convert receptor's RDM-coordinates to lon-lat-coordinates.
! This is needed in order to get the correct z0 from the European z0-grid (see below) and for other routines.
! Therefore the lon-lat-coordinates are also returned as output arguments.
!
  IF (IGEO .NE. 1) THEN
    CALL amc2geo(xm(ircp)/1000, ym(ircp)/1000, gxm(ircp), gym(ircp))
  ENDIF

  ! In case of a user specified z0: fix z0 at user specified value and fix land use class at 1 (grass)
  IF (f_z0user) THEN
    z0_rcp_all(ircp) = z0_user
    IF (isec) lu_rcp_dom_all(ircp) = 1
  ELSE
!
!   Get roughness length z0 [m] for receptor. If receptor lies within the NL grid, we use the z0-NL grid; otherwise the z0-EUR
!   gridvalue is returned.
!
    IF (z0_rcp_all(ircp) .LE. 0) CALL ops_getz0(xm(ircp), ym(ircp), z0nlgrid, z0eurgrid, z0_rcp_all(ircp))

    IF (isec.and.domlu) THEN
!
!     Retrieve land use information (dominant land use and percentages) at receptor; default land use class = 1 (grass) for receptor outside lugrid.
!
      IF (lu_rcp_dom_all(ircp) .LE. 0) THEN
        CALL ops_getlu(xm(ircp), ym(ircp), lugrid, landuse)
        lu_rcp_dom_all(ircp) = landuse(1)
      ENDIF
    ENDIF
  ENDIF

! For SO2, NOx, NH3:
IF (isec) THEN

    ! Get background concentrations at receptor
    CALL ops_bgcon(xm(ircp),ym(ircp),nh3bggrid, nh3bgconc)
!   Distribute NO3 and SO4 into sub-secondary species
!
!   rhno3 = ratio [HNO3]/[NO3]_total (NO3_total = HNO3+NO3_aerosol); all concentrations in ppb.
!
!                  [NH3]_background   -0.44
!   rhno3 = 0.024 (-----------------)             (6.18 OPS report FS), [NH3]_background in ppb
!                       1000
!
!   This relation has been derived with the help of a 1D chemistry model (model chemie5)
!   (fit between daily averaged HNO3 and NO3 concentrations for november and december 1989;
!   see also ops_par_chem.
!   Here we use the background NH3 concentration at the receptor.
!   Note that for background [NH3] = 0.346 ppb, rhno3_rcp = 0.024*(nh3bgconc/1000)**(-0.44) = 0.8
!             for background [NH3] < 0.346 ppb, rhno3_rcp is fixed at 0.8.
!
!   Note that we still use rhno3_rcp for computing weighed averaged Rc-values; later on, this must be replaced by f_subsec_rcp.
!   For distributing concentrations over different secondary species, we use f_subsec_rcp, which is read from file.
!
    IF (icm == 2) THEN
      rhno3_rcp(ircp)=amin1(0.024*(nh3bgconc/1000)**(-0.44),0.8)

      ! Get fractions for different sub-secondary species, HNO3/NO3_total, NO3_C/NO3_total, NO3_F/NO3_total at current receptor location:
      if (iopt_vchem .eq. 0) then

         ! File with f_subsec_grid is not present, use old OPS parameterisation rhno3 and do not split into coarse and fine:
         !WdV f_subsec_rcp(ircp,1) = rhno3_rcp(ircp)                           ! HNO3
         !WdV f_subsec_rcp(ircp,2) = (1.0 - rhno3_rcp(ircp))                   ! NO3_AEROSOL
         f_subsec_rcp(ircp,1) = (1.0 - rhno3_rcp(ircp))                   ! NO3_AEROSOL
         f_subsec_rcp(ircp,2) = rhno3_rcp(ircp)                           ! HNO3

         ! for NO3-coarse and - fine, fractions are used from BOP-report
         ! f_subsec_rcp(ircp,2) = frac_no3c_bop*(1.0 - rhno3_rcp(ircp))   ! NO3_C
         ! f_subsec_rcp(ircp,3) = frac_no3f_bop*(1.0 - rhno3_rcp(ircp))   ! NO3_F
      else
         ! Get fraction from EMEP grid:
         ! (3 fields in f_subsec_grid: HNO3/NO3_total, NO3_C/NO3_total, NO3_F/NO3_total; 4 sub species NO3_aerosol, HNO3, NO3_C, NO3_F)
         do isubsec = 2,nsubsec
            ifield = isubsec - 1
            CALL ops_bgcon(xm(ircp),ym(ircp),f_subsec_grid, f_subsec_rcp(ircp,isubsec),ifield)
         enddo

         ! Fraction NO3_aerosol / NO3_total:
         f_subsec_rcp(ircp,1) = f_subsec_rcp(ircp,3) + f_subsec_rcp(ircp,4)
      endif
    ELSE
!
!     Convert NH3 background concentration from ppb to ug/m3 (is used as such in DEPAC)
!
      nh3bg_rcp(ircp)=nh3bgconc*17/24
!
!     Get so2 background concentration at receptor
!
      CALL ops_bgcon(xm(ircp),ym(ircp),so2bggrid, so2bgconc)
!
!     Convert SO2 background concentration from ppb to ug/m3 (is used as such in DEPAC)
!
      so2bg_rcp(ircp)=so2bgconc*64./24.
    ENDIF
  ENDIF
  IF (error%debug) WRITE(*,'(3a,1x,i6,99(1x,e12.5))') trim(ROUTINENAAM),',A,',' ircp,z0_rcp_all(ircp),lu_rcp_dom_all(ircp),nh3bg_rcp(ircp): ', &
                                                                                ircp,z0_rcp_all(ircp),lu_rcp_dom_all(ircp),nh3bg_rcp(ircp)
ENDDO

RETURN

END subroutine ops_rcp_char_all
