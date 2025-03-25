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
! DESCRIPTION        : Prepares values for landuse and roughness for one receptorpoint.
!-------------------------------------------------------------------------------------------------------------------------------
module m_ops_rcp_char_1

implicit none

contains

SUBROUTINE ops_rcp_char_1(isec, ircp, intpol, gxm_rcp, gym_rcp, cs, z0_metreg, xreg, yreg, astat, z0_metreg_user,    &
                       &  spgrid, x_rcp, y_rcp, lugrid, domlu, perc, lu_rcp_per_user_all, lu_rcp_dom_all, f_z0user, z0_rcp_all, &
                       &  uurtot, z0_metreg_rcp, lu_rcp_per, lu_rcp_dom, z0_rcp, error)

use m_commonconst_lt
USE m_commonfile
USE m_error
USE m_aps
USE m_geoutils
USE m_commonconst_lib, only: NLU

IMPLICIT NONE

! CONSTANTS
CHARACTER*512                                    :: ROUTINENAAM                ! 
PARAMETER    (ROUTINENAAM = 'ops_rcp_char_1')

! SUBROUTINE ARGUMENTS - INPUT
LOGICAL,   INTENT(IN)                            :: isec                        
INTEGER,   INTENT(IN)                            :: ircp                   
INTEGER,   INTENT(IN)                            :: intpol                 ! 
REAL,      INTENT(IN)                            :: gxm_rcp                ! x-coordinaat van receptorpunt ircp (lola)
REAL,      INTENT(IN)                            :: gym_rcp                ! y-coordinaat van receptorpunt ircp (lola)
REAL,      INTENT(IN)                            :: cs(:,:,:,:,:)          ! cs(NTRAJ, NCOMP, NSTAB, NSEK, NMETREG)
REAL,      INTENT(IN)                            :: z0_metreg(NMETREG)     ! roughness lengths of NMETREG meteo regions; scale < 50 km [m]
REAL,      INTENT(IN)                            :: xreg(NMETREG)          ! array met x-coordinaat van meteo-regios
REAL,      INTENT(IN)                            :: yreg(NMETREG)          ! array met y-coordinaat van meteo-regio's
REAL,      INTENT(IN)                            :: z0_metreg_user         ! roughness length of user specified meteo region [m]
INTEGER,   INTENT(IN)                            :: spgrid
REAL,      INTENT(IN)                            :: x_rcp                  ! x-coordinate receptor (RDM)
REAL,      INTENT(IN)                            :: y_rcp                  ! y-coordinate receptor (RDM)
TYPE (TApsGridInt), INTENT(IN)                   :: lugrid                 ! grid with land use class information (1: dominant land use, 2:NLU+1: percentages land use class)
LOGICAL,   INTENT(IN)                            :: domlu                  ! use dominant land use instead of land use percentages
LOGICAL,   INTENT(IN)                            :: perc                   ! 
REAL,      INTENT(IN)                            :: lu_rcp_per_user_all(:) ! percentage of landuse for receptor ircp, user defined in receptor file
INTEGER,   INTENT(IN)                            :: lu_rcp_dom_all         ! dominant land use class for each receptor point
LOGICAL,   INTENT(IN)                            :: f_z0user                   
REAL,      INTENT(IN)                            :: z0_rcp_all             ! roughness length for receptor ircp; from z0-map or receptor file [m]

! SUBROUTINE ARGUMENTS - I/O
REAL,      INTENT(INOUT)                         :: astat(NTRAJ,NCOMP,NSTAB,NSEK)  ! Only modified if intpol == 0
REAL,      INTENT(INOUT)                         :: uurtot                     ! total number of hours from meteo statistics

! SUBROUTINE ARGUMENTS - OUTPUT
REAL,      INTENT(OUT)                           :: z0_metreg_rcp              ! roughness length at receptor; interpolated from meteo regions [m]
INTEGER,   INTENT(OUT)                           :: lu_rcp_dom                 ! dominant landuse class for receptor
REAL,      INTENT(OUT)                           :: lu_rcp_per(NLU)            ! percentages of landuse classes at receptor points
REAL,      INTENT(OUT)                           :: z0_rcp                     ! roughness length at receptor; from z0-map or receptor file [m]
TYPE (TError)                                    :: error  

! LOCAL VARIABLES
INTEGER                                          :: lu_rcp_per_int(NLU)        ! percentages of landuse classes at receptor points
INTEGER                                          :: lu
LOGICAL                                          :: iscell                     ! whether point is inside masker grid


!-------------------------------------------------------------------------------------------------------------------------------
! Initialisation:
lu_rcp_per    = 0.
z0_metreg_rcp = 0

!---------------------------------------------------------------------
! Get roughness lengths
!---------------------------------------------------------------------

! Get roughness length at receptor:
z0_rcp = z0_rcp_all

! Select the three nearest climatological regions and interpolate between them in order to compute z0_metreg_rcp
! Note: intpol = 0 -> interpolate    
IF (intpol.EQ.0) THEN
  CALL reginpo(gxm_rcp, gym_rcp, cs, z0_metreg, xreg, yreg, z0_metreg_rcp, uurtot, astat, error)
  IF (error%haserror) GOTO 9999
ELSE
  ! Get z0 of user specified meteo region:
  z0_metreg_rcp = z0_metreg_user
ENDIF

!---------------------------------------------------------------------
! Get percentages landuse lu_rcp_per for this receptor point,
! either from a land use grid file or from the receptor file.
! 
! Table. Options -perc, -domlu and their effect on the type of land use for deposition calculations. 
! fixed         -> use fixed land use (read from control file, e,g, ROUGHNESS 0.05 2 -> z0 = 0.05m, land use = arable land)
! fractions     -> use fraction of each land use class (deposition is weighed averaged of each deposition at different land use classes)
! dominant      -> use dominant land use class (fractions are not used)
! map           -> use land use map
! receptor file -> use land use data from receptor file. 
! ----------------------------------------------------------------------------------------------
! |               | land use at receptor, |  land use at receptor,   |  land use at trajectory |
! |               | receptor grid         |  defined in a file       |                         |
! |               | spgrid = 0,1          |  spgrid = 2,3            |                         |
! |--------------------------------------------------------------------------------------------|
! | ROUGGNESS > 0 | fixed                 |  fixed                   |  fixed                  |
! |               | fractions-map         |  fractions-map           |  fractions-map          |
! | -perc         | fractions-map         |  fractions-receptor file |  fractions-map          |
! | -domlu        | dominant-map          |  dominant-map            |  dominant-map           |
! | -perc -domlu  | dominant-map          |  dominant-receptor file  |  dominant-map           |
! ----------------------------------------------------------------------------------------------

IF (ANY(spgrid == (/0,1/))) THEN

   ! spgrid = 0: regular grid of receptors, NL
   ! spgrid = 1: rectangular regular grid of receptors, user defined 

   IF (.not.domlu) THEN
   
      ! do not use dominant land use, but percentages land use.
      ! Fill lu_rcp_per with info from standard grid with landuse info
      ! The first aps-grid contains dominant landuse, so we start with the second
      IF (isec .and. .not. f_z0user) THEN
         DO lu=2,NLU+1
            CALL GridValue(x_rcp/1000, y_rcp/1000, lugrid, lu_rcp_per_int(lu-1), iscell, lu)
         ENDDO     
         lu_rcp_per = float(lu_rcp_per_int)
      ENDIF
   ! ELSE  domlu -> use dominant land use -> lu_rcp_per is filled later on (see if (domlu))
   ENDIF
ELSE
   
   ! spgrid = 2: receptors at specific locations, read from file
   ! spgrid = 3: receptors at user specific regular grid, not necessarily rectangular, read from file
   
   IF (.not.perc) THEN
   
      ! User did not specify percentages landuse in rcp-file.
      ! Fill lu_rcp_per with info from standard grid with landuse info
   
      IF (isec .and. .not. f_z0user) THEN
         DO lu=2,NLU+1
            CALL GridValue(x_rcp/1000, y_rcp/1000, lugrid, lu_rcp_per_int(lu-1), iscell, lu)
         ENDDO 
         lu_rcp_per = float(lu_rcp_per_int)
      ENDIF
   ELSE
   
      ! Fill lu_rcp_per with land use precentages read from rcp-file:
      DO lu=1,NLU
         lu_rcp_per(lu) = lu_rcp_per_user_all(lu)
      ENDDO
   ENDIF
ENDIF

!------------------------------------------------------------
! Set dominant landuse class lu_rcp_dom at receptor
!------------------------------------------------------------
IF (f_z0user) THEN
   ! If z0 is user specified, then dominant land use is grass: 
   lu_rcp_dom = 1
ELSE  
   ! If z0 not user specified, then use dominant land use class from receptor file (first choice) or land use grid:
   lu_rcp_dom = lu_rcp_dom_all
ENDIF

!------------------------------------------------------------------------
! Overrule percentages landuse lu_rcp_per at receptor for option -domlu
!------------------------------------------------------------------------
! If option for dominant land use is set, set landuse percentage of dominant land use class to 100:
IF (domlu) THEN
  lu_rcp_per             = 0.0
  lu_rcp_per(lu_rcp_dom) = 100.0
ENDIF

!-------------------------------------------------------------------------------------------------------------
! If no landuse information available in standard grid or rcp-file, we asume it's grass  
!-------------------------------------------------------------------------------------------------------------
IF (sum(lu_rcp_per(1:NLU)) .le. 0) THEN
  lu_rcp_per    = 0.0
  lu_rcp_per(1) = 100.0
ENDIF   

if (error%debug) write(*,'(3a,1x,i6,";",99(1x,e12.5,";"))') trim(ROUTINENAAM),',A; ', &
   'ircp; z0_rcp; lu_rcp_per(lu=1); lu_rcp_per(lu=2); lu_rcp_per(lu=3); lu_rcp_per(lu=4); lu_rcp_per(lu=5); lu_rcp_per(lu=6); lu_rcp_per(lu=7); lu_rcp_per(lu=8); lu_rcp_per(lu=9); ',&
    ircp, z0_rcp, lu_rcp_per

RETURN

9998 CALL ErrorParam('error in land use grid','xx',error)
9999 CALL ErrorCall(ROUTINENAAM, error)
RETURN

END SUBROUTINE ops_rcp_char_1

!-------------------------------------------------------------------------------------------------------------------------------
! SUBROUTINE         : reginpo
! DESCRIPTION        : Determine the three meteo statistics regions nearest to location (x,y) and interpolate
!                      parameters between these three regions. Interpolated parameters are stored in astat.
!-------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE reginpo(x, y, cs, z0_metreg, xreg, yreg, z0_metreg_xy, uurtot, astat, error)

use m_commonconst_lt, only: NMETREG, NTRAJ, EPS_DELTA, NCOMP, NSTAB, NSEK
USE Binas, only: deg2rad
USE m_error, only: TError, SetError, ErrorParam, ErrorCall

! CONSTANTS
CHARACTER*512                                    :: ROUTINENAAM                ! 
PARAMETER      (ROUTINENAAM = 'reginpo')

! meteo parameters for which no interpolation has to be done when one of the interpolants has a zero frequency of occurrence  
INTEGER,   PARAMETER :: NONZERO(NCOMP) = (/         &                          
             0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1/)                     
!            1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27

! If NONZERO = 1 -> no interpolation has to be done when one of the interpolants has a zero frequency of occurrence;
!                   in this case the weighing coefficients ss1 are used, which are zero if the class does not occur
! If NONZERO = 0 -> interpolate anyhow, even if one of the interpolants has a zero frequency of occurrence;
!                   in this case the weighing coefficients s1 are used, which are always non-zero
!
! NONZERO = 0 ('interpolate anyhow') for parameters 1, 17, 18:
!        1. number of hours for which a certain meteo class (stability class + wind direction class) has occurred [-]
!        17. distribution of stability classes over day, source oriented [-]
!        18. distribution of stability classes over day, receptor oriented [-]

INTEGER,   PARAMETER                             :: NMETREG_NEAREST = 3        ! number of nearest meteo regions that are used in the interpolation

! SUBROUTINE ARGUMENTS - INPUT
REAL,      INTENT(IN)                            :: x                          ! x-coordinate (longitude; degrees)
REAL,      INTENT(IN)                            :: y                          ! y-coordinate (latitude; degrees)
REAL,      INTENT(IN)                            :: cs(:,:,:,:,:)              ! cs(NTRAJ, NCOMP, NSTAB, NSEK, NMETREG) 
REAL,      INTENT(IN)                            :: z0_metreg(NMETREG)         ! roughness lengths of NMETREG meteo regions; scale < 50 km [m]
REAL,      INTENT(IN)                            :: xreg(NMETREG)              ! x-coordinate region centre (longitude; degrees)
REAL,      INTENT(IN)                            :: yreg(NMETREG)              ! y-coordinate region centre (latitude; degrees)

! SUBROUTINE ARGUMENTS - OUTPUT
REAL,      INTENT(OUT)                           :: z0_metreg_xy               ! roughness length at (x,y), interpolated from meteo regions [m]
REAL,      INTENT(OUT)                           :: uurtot                     ! total number of hours from meteo statistics
                                                                               ! Untouched if intpol!=0, INTENT(OUT) if intpol==0
REAL,      INTENT(OUT)                           :: astat(NTRAJ, NCOMP, NSTAB, NSEK) ! 
TYPE (TError), INTENT(INOUT)                     :: error                      ! error handling record

! LOCAL VARIABLES
INTEGER                                          :: ireg                       ! index of meteo region
INTEGER                                          :: ireg_nearest               ! index of nearest meteo region
INTEGER                                          :: icomp                      ! index of meteo component (parameter)
INTEGER                                          :: istab                      ! index of stability class
INTEGER                                          :: imin                       ! index of nearest region
INTEGER                                          :: itraj                      ! index of distance class
INTEGER                                          :: isek                       ! index of wind sector, isek = 1,NSEK
REAL                                             :: a                          ! Set a = cos(y); needed in computation of distance
                                                                               ! dx = (x2 - x1)*cos(y) for geographical coordinates

REAL                                             :: r                          ! distance region - receptor
REAL                                             :: rmin                       ! distance nearest region - receptor
REAL                                             :: s                          ! sum of s1()
REAL                                             :: ss                         ! 
REAL                                             :: rr                         ! 
REAL                                             :: rrtot                      ! 
INTEGER                                          :: i1(NMETREG_NEAREST)        ! indices of three regions nearest to the receptor
REAL                                             :: r1(NMETREG_NEAREST)        ! distance of three nearest regions - receptor
REAL                                             :: s1(NMETREG_NEAREST)        ! weighing coefficients (inverse distances) for nearest meteo regions (non-zero if meteo class does not occur)
REAL                                             :: ss1(NMETREG_NEAREST)       ! weighing coefficients (inverse distances) for nearest meteo regions (zero if meteo class does not occur)
INTEGER                                          :: is_metreg_near3(NMETREG)   ! belongs to nearest 3 meteo regions (1/0)

!-------------------------------------------------------------------------------------------------------------------------------
! Set a = cos(y); needed in computation of dx = (x2 - x1)*cos(y) for geographical coordinates
a = COS(y*deg2rad)

! Initialise i1 (meaning that no region has been found) and sums:
i1           = 0
s            = 0.
z0_metreg_xy = 0.

! Loop over 3 nearest regions:
DO ireg_nearest = 1, NMETREG_NEAREST
  
   ! Initialise minimal distance at 'HUGE'
   rmin = 100000.

   ! Loop over all regions:
   DO ireg = 1, NMETREG
!
!     Check whether any index of nearest regions i1 has already been assigned to ireg (in a previous loop over ireg_nearest);
!     if so, skip this region.
!
      IF (.NOT. ANY(i1 .EQ. ireg)) THEN 

         ! Compute distance region - receptor
         r = ((x - xreg(ireg))*a)**2 + (y - yreg(ireg))**2

         ! Adjust minimal distance rmin and index to nearest region imin
         IF (r .LT. (rmin - EPS_DELTA)) THEN
            rmin = r
            imin = ireg
         ENDIF
      ENDIF
   ENDDO
!
!  Set i1 = index to nearest region, r1 = minimal distance region - receptor, s1 = inverse minimal distance and
!  add inverse distance to som, for this nearest region. Inverse distances are used as weighing coefficients for
!  interpolation of z0 over the nearest regions:
   i1(ireg_nearest) = imin 
   r1(ireg_nearest) = rmin
   s1(ireg_nearest) = 1./(rmin + .01)
   s                = s + s1(ireg_nearest)
   z0_metreg_xy     = z0_metreg_xy + s1(ireg_nearest)*z0_metreg(imin)                                     ! 940907
ENDDO

IF (ABS(s) .LE. EPS_DELTA) THEN
   s = .01
ENDIF

! Compute weighed average over nearest meteo regions (weighing coefficients are the inverse minimal distances)
z0_metreg_xy = z0_metreg_xy / s

! Write integer array with 0 (meteo region not used in interpolation or 1 (meteo region used in interpolation) (test):
if (error%debug) then
   is_metreg_near3 = 0
   DO ireg = 1, NMETREG
      IF (ANY(i1 .EQ. ireg)) is_metreg_near3(ireg) = is_metreg_near3(ireg) + 1
   ENDDO
   write(*,'(a,a,2(1x,e12.5,";"),6(1x,i2,";"),3(1x,e12.5,";"),3(1x,i4,";"))' ) &
        trim(ROUTINENAAM),'; x; y; is_metreg_near3; s1/s; i1; ',x,y,is_metreg_near3,s1/s,i1
endif

! Loop over all {distance,stability,wind sector} classes
DO itraj = 1, NTRAJ
  DO istab = 1, NSTAB
    DO isek = 1, NSEK
!
!     Compute ss1 = weighing coefficients (inverse distances) for nearest meteo regions
!     and ss = sum of all weighing coefficients
!
      ss = 0.
      DO ireg_nearest = 1, NMETREG_NEAREST 

        ! If {distance,stability,wind sector} class does not occur for the current nearest region,
        ! ss1 = 0, otherwise ss1 = s1
        IF (cs(itraj, 1, istab, isek, i1(ireg_nearest)).LE.EPS_DELTA) THEN
          ss1(ireg_nearest) = 0.
        ELSE
          ss1(ireg_nearest) = s1(ireg_nearest)
        ENDIF
        ss = ss + ss1(ireg_nearest)
      ENDDO

      IF (ABS(ss) .LE. EPS_DELTA) THEN
        ss = .01
      ENDIF
!
!     Compute weighing coefficients rr = ss1(ireg_nearest)/ss; interpolate and assign interpolated parameters to astat.
!
      DO icomp = 1, NCOMP
        rrtot = 0
        DO ireg_nearest = 1, NMETREG_NEAREST 

          ! If NONZERO = 1 -> no interpolation has to be done when one of the interpolants has a zero frequency of occurrence;
          !                   in this case the weighing coefficients ss1 are used, which are zero if the class does not occur
          ! If NONZERO = 0 -> interpolate anyhow, even if one of the interpolants has a zero frequency of occurrence;
          !                   in this case the weighing coefficients s1 are used, which are always non-zero
          IF (NONZERO(icomp) .EQ. 1) THEN
            rr = ss1(ireg_nearest) / ss
          ELSE
            rr = s1(ireg_nearest) / ss
          ENDIF
          rrtot = rrtot + rr * cs(itraj, icomp, istab, isek, i1(ireg_nearest))
        ENDDO
        astat(itraj, icomp, istab, isek) = rrtot
      ENDDO
    ENDDO
  ENDDO
ENDDO

! uurtot = total number of hours over {distance class=1,stability, wind sector} classes
uurtot = SUM(astat(1,1,:NSTAB,:NSEK))
!
! Check whether z0_metreg_xy is positive. If not generate an error
!
IF (z0_metreg_xy <= (0. + EPS_DELTA)) THEN
  CALL SetError('Negative z0 value encountered', error)
  CALL ErrorParam('z0_metreg_xy', z0_metreg_xy, error)
  CALL ErrorParam('x', x, error)
  CALL ErrorParam('y', y, error)
  CALL ErrorCall(ROUTINENAAM, error)
ENDIF

RETURN
END SUBROUTINE reginpo

end module m_ops_rcp_char_1
