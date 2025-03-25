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
! DESCRIPTION        : Fill arrays with receptor characteristics for all receptors:
!                      coordinates (gxm, gym), 
!                      values for roughness (z0_rcp_all),
!                      dominant land use (lu_rcp_dom_all),
!                      ratio HNO3/NO3_total (rhno3_rcp),
!                      background concentrations (nh3bg_rcp, o3bg_rcp, so2bg_rcp), 
!                      fractions for sub-secondary species f_subsec_rcp(nrrcp,nsubsec).
!-------------------------------------------------------------------------------------------------------------------------------
module m_ops_rcp_char_all

implicit none

contains

subroutine ops_rcp_char_all(icm, iopt_vchem, isec, nsubsec, xm, ym, f_z0user, z0_user, z0nlgrid, z0eurgrid, lugrid, nemcat_road, road_chem,&
                            so2bggrid, nh3bggrid, gwgrid, o3bggrid, f_subsec_grid, nrrcp, namrcp, gxm, gym, lu_rcp_dom_all, z0_rcp_all, &
                            rhno3_rcp, nh3bg_rcp, gw_rcp, o3bg_rcp, so2bg_rcp, f_subsec_rcp, domlu, error)

use m_aps
use m_geoutils
use m_commonconst_lt
use m_ops_bgcon
use m_error
use m_commonconst_lib, only: NLU
use m_ops_getz0
use m_ops_getlu

IMPLICIT NONE

! CONSTANTS
CHARACTER*512                                    :: ROUTINENAAM                ! 
PARAMETER    (ROUTINENAAM = 'ops_rcp_char_all')

! SUBROUTINE ARGUMENTS - INPUT
INTEGER,   INTENT(IN)                            :: icm                        
INTEGER,   INTENT(IN)                            :: iopt_vchem                 ! option for chemical conversion rate (0 = old OPS, 1 = EMEP)
LOGICAL,   INTENT(IN)                            :: isec                       
INTEGER,   INTENT(IN)                            :: nsubsec                    ! number of sub-secondary species                       
REAL,      INTENT(IN)                            :: xm(nrrcp)                  ! x-coordinates of receptors (m RDM)
REAL,      INTENT(IN)                            :: ym(nrrcp)                  ! y-coordinates of receptors (m RDM)
LOGICAL,   INTENT(IN)                            :: f_z0user                   
REAL,      INTENT(IN)                            :: z0_user                    ! roughness length specified by user [m]
TYPE (TApsGridInt), INTENT(IN)                   :: z0nlgrid                   ! map of roughness lengths in NL [m]
TYPE (TApsGridInt), INTENT(IN)                   :: z0eurgrid                  ! map of roughness lengths in Europe [m]
TYPE (TApsGridInt), INTENT(IN)                   :: lugrid                     ! grid with land use class information (1: dominant land use, 2:NLU+1: percentages land use class)
INTEGER,   INTENT(IN)                            :: nemcat_road                ! number of road emission categories (for vdHout NO2/NOx ratio)
LOGICAL,   INTENT(IN)							 :: road_chem					 !switch for road chemistry GTHO
TYPE (TApsGridReal), INTENT(IN)                  :: so2bggrid                  ! grid of SO2 background concentrations [ppb] (read as ug/m3, converted to ppb)
TYPE (TApsGridReal), INTENT(IN)                  :: nh3bggrid                  ! grid of NH3 background concentrations [ppb] (read as ug/m3, converted to ppb)
TYPE (TApsGridReal), INTENT(IN)                  :: gwgrid                     ! grid of gamma water values
TYPE (TApsGridReal), INTENT(IN)                  :: o3bggrid                   ! grids of O3 background concentrations per wind sector [ug/m3]
TYPE (TApsGridReal), INTENT(IN)                  :: f_subsec_grid              ! grids of fractions for sub-secondary species, HNO3/NO3_total, NO3_C/NO3_total, NO3_F/NO3_total [-]
INTEGER,   INTENT(IN)                            :: nrrcp                      ! number of receptors
CHARACTER*(*), INTENT(IN)                        :: namrcp(nrrcp)              ! receptor names 
LOGICAL,   INTENT(IN)                            :: domlu                      ! use dominant land use instead of land use percentages

! SUBROUTINE ARGUMENTS - OUTPUT
REAL,      INTENT(OUT)                           :: gxm(nrrcp)                 ! x-coordinates of receptors (lon-lat) [degrees]
REAL,      INTENT(OUT)                           :: gym(nrrcp)                 ! y-coordinates of receptors (lon-lat) [degrees] 
REAL,      INTENT(OUT)                           :: rhno3_rcp(nrrcp)           ! ratio [HNO3]/[NO3]_total at receptor [ug/m3]
REAL,      INTENT(OUT)                           :: nh3bg_rcp(nrrcp)           ! background concentration NH3 at receptor [ug/m3]
REAL,      INTENT(OUT)                           :: gw_rcp(nrrcp)              ! gamma water value at receptor
REAL,      INTENT(OUT)                           :: o3bg_rcp(NSEK,nrrcp)       ! background concentration O3 at receptor for all wind sectors [ug/m3]
REAL,      INTENT(OUT)                           :: so2bg_rcp(nrrcp)           ! background concentration SO2 at receptor [ug/m3]
REAL,      INTENT(OUT)                           :: f_subsec_rcp(nrrcp,nsubsec)   ! fractions for sub-secondary species, HNO3/NO3_total, NO3_C/NO3_total, NO3_F/NO3_total [-]

! SUBROUTINE ARGUMENTS - OUTPUT
INTEGER,   INTENT(INOUT)                         :: lu_rcp_dom_all(nrrcp)      ! dominant land use class for each receptor point
REAL,      INTENT(INOUT)                         :: z0_rcp_all(nrrcp)          ! roughness lengths for all receptors; from z0-map or receptor file [m]
TYPE (TError), INTENT(INOUT)                     :: error   

! LOCAL VARIABLES
INTEGER                                          :: landuse(NLU+1)             ! land-use value at receptor
                                                                               ! landuse(1)    = index of dominant landuse
                                                                               ! landuse(lu+1) = percentage of grid cell with landuse class lu, lu = 1,NLU
                                                                               ! For locations outside lugrid, a fatal error is triggered
INTEGER                                          :: ircp                       ! index of receptor
INTEGER                                          :: isubsec                    ! index of sub-secondary species
REAL                                             :: so2bgconc                  ! background concentratie SO2
REAL                                             :: nh3bgconc                  ! background concentration NH3 at receptor [ppb]
INTEGER                                          :: isek                       ! wind sector index
INTEGER                                          :: ifield                     ! field index in f_subsec_grid


!-------------------------------------------------------------------------------------------------------------------------------
!
! Loop over receptors
!
DO ircp = 1, nrrcp

   ! Convert receptor's RDM-coordinates to lon-lat-coordinates.
   ! This is needed in order to get the correct z0 from the European z0-grid (see below) and for other routines.
   ! Therefore the lon-lat-coordinates are also returned as output arguments.
   IF (IGEO .NE. 1) THEN
     CALL amc2geo(xm(ircp)/1000, ym(ircp)/1000, gxm(ircp), gym(ircp))
   ENDIF

   ! In case of a user specified z0: fix z0 at user specified value and fix land use class at 1 (grass):
   IF (f_z0user) THEN
     z0_rcp_all(ircp) = z0_user
     IF (isec) lu_rcp_dom_all(ircp) = 1
   ELSE
   
      ! If z0_rcp_all has not been read from the receptor file (see ops_gen_rcp), then it is 0;
      ! in this case, get roughness length z0 [m] for receptor from grid. If receptor lies within the NL grid, we use the z0-NL grid; 
      ! otherwise the z0-EUR gridvalue is returned:
      IF (z0_rcp_all(ircp) .LE. 0) THEN
         CALL ops_getz0(xm(ircp), ym(ircp), z0nlgrid, z0eurgrid, z0_rcp_all(ircp), error)
         IF (error%haserror) goto 9999
      ENDIF
      
      ! Retrieve land use information (dominant land use and percentages) at receptor; fatal error for receptor outside lugrid.
      ! landuse(1)    = index of dominant landuse
      ! landuse(lu+1) = percentage of grid cell with landuse class lu, lu = 1,NLU
      IF (isec.and.domlu) THEN
         ! If lu_rcp_dom_all has not been read from the receptor file (see ops_gen_rcp), then it is 0;
         ! in this case, get the location of the receptor in the land use grid and get land use information (dominant land use = landuse(1) and 
         ! percentages = landuse(2:NLU+1)); note: in_trajectory = .false.
         IF (lu_rcp_dom_all(ircp) .LE. 0) THEN
            CALL ops_getlu(xm(ircp), ym(ircp), .false., lugrid, landuse, error)
            IF (error%haserror) goto 9999
            lu_rcp_dom_all(ircp) = landuse(1)
         ENDIF
      ENDIF
   ENDIF




   
   ! For SO2, NOx, NH3:
   IF (isec) THEN
      IF (ANY(icm == (/icm_SO2,icm_NOx,icm_NH3/))) THEN 
         ! Get background concentrations at receptor (in_trajectory = .false):
         CALL ops_bgcon(xm(ircp), ym(ircp), .false., nh3bggrid, nh3bgconc, error) 
         IF (error%haserror) goto 9999
         ! Convert NH3 background concentration from ppb to ug/m3 (is used as such in DEPAC)
         nh3bg_rcp(ircp)=nh3bgconc*17/24
      ENDIF
      IF (ANY(icm == (/icm_SO2,icm_NH3/)))  THEN 
        ! Get so2 background concentration at receptor  (in_trajectory = .false):
         CALL ops_bgcon(xm(ircp),ym(ircp), .false., so2bggrid, so2bgconc, error)
         IF (error%haserror) goto 9999
        ! Convert SO2 background concentration from ppb to ug/m3 (is used as such in DEPAC)
         so2bg_rcp(ircp)=so2bgconc*64./24. 
      ENDIF
      IF (ANY(icm == (/icm_NOx/)))   THEN
      
         ! NOx 

         ! Get background concentrations of ozone at receptor (in_trajectory = .false);
         ! note that o3bggrid and o3bg_rcp are in ug/m3
         if (nemcat_road .gt. 0 .and. road_chem) then
            do isek = 1,NSEK 
               CALL ops_bgcon(xm(ircp), ym(ircp), .false., o3bggrid, o3bg_rcp(isek,ircp), error, isek) 
               IF (error%haserror) goto 9999
            enddo
         endif
         
         ! Distribute NO3 and SO4 into sub-secondary species
         ! rhno3_rcp = ratio [HNO3]/[NO3]_total (NO3_total = HNO3+NO3_aerosol) for receptor; all concentrations in ppb.
         !
         !                      [NH3]_background - 0.44
         !   rhno3_rcp = 0.024 (-------------------------)             (6.18 OPS report FS), [NH3]_background in ppb
         !                               1000
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
         rhno3_rcp(ircp)=amin1(0.024*(nh3bgconc/1000)**(-0.44),0.8) 
        
         ! Get fractions for different sub-secondary species, HNO3/NO3_total, NO3_C/NO3_total, NO3_F/NO3_total at current receptor location:
         IF (iopt_vchem .eq. 0) THEN
         
            ! File with f_subsec_grid is not present, use old OPS parameterisation rhno3_rcp and do not split into coarse and fine:
            f_subsec_rcp(ircp,1) = (1.0 - rhno3_rcp(ircp))                   ! NO3_AEROSOL
            f_subsec_rcp(ircp,2) = rhno3_rcp(ircp)                           ! HNO3
         
            ! for NO3-coarse and - fine, fractions are used from BOP-report 
            ! f_subsec_rcp(ircp,2) = frac_no3c_bop*(1.0 - rhno3_rcp(ircp))   ! NO3_C
            ! f_subsec_rcp(ircp,3) = frac_no3f_bop*(1.0 - rhno3_rcp(ircp))   ! NO3_F
         ELSE
            ! Get fraction from EMEP grid  (in_trajectory = .false):
            ! (3 fields in f_subsec_grid: HNO3/NO3_total, NO3_C/NO3_total, NO3_F/NO3_total; 4 sub species NO3_aerosol, HNO3, NO3_C, NO3_F)
            do isubsec = 2,nsubsec
               ifield = isubsec - 1 
               CALL ops_bgcon(xm(ircp), ym(ircp), .false., f_subsec_grid, f_subsec_rcp(ircp,isubsec), error, ifield)
               IF (error%haserror) goto 9999
            enddo
            
            ! Fraction NO3_aerosol / NO3_total:
            f_subsec_rcp(ircp,1) = f_subsec_rcp(ircp,3) + f_subsec_rcp(ircp,4)  
         ENDIF  ! iopt_vchem .eq. 0
      ENDIF
      IF (icm == icm_NH3)  THEN 
         CALL ops_bgcon(xm(ircp),ym(ircp), .false., gwgrid, gw_rcp(ircp), error)
         IF (error%haserror) goto 9999
      ENDIF
   ENDIF  ! IF (isec)
   IF (error%debug) write(*,'(3a,2(1x,i6,";"),99(1x,e12.5,";"))') & 
      trim(ROUTINENAAM),',A; ','ircp; lu_rcp_dom_all(ircp); z0_rcp_all(ircp); nh3bg_rcp(ircp); o3bg_rcp(1,ircp); o3bg_rcp(2,ircp); o3bg_rcp(3,ircp); o3bg_rcp(4,ircp); o3bg_rcp(5,ircp); o3bg_rcp(6,ircp); o3bg_rcp(7,ircp); o3bg_rcp(8,ircp); o3bg_rcp(9,ircp); o3bg_rcp(10,ircp); o3bg_rcp(11,ircp); o3bg_rcp(12,ircp); ', &
                                ircp, lu_rcp_dom_all(ircp), z0_rcp_all(ircp), nh3bg_rcp(ircp), o3bg_rcp(:,ircp)
ENDDO ! Loop over receptor points

RETURN

9999 CALL ErrorCall(ROUTINENAAM, error)
CALL ErrorParam('receptor number', ircp, error)
CALL ErrorParam('receptor name', trim(namrcp(ircp)), error)

END subroutine ops_rcp_char_all

end module m_ops_rcp_char_all
