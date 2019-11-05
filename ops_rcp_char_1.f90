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
! LANGUAGE           : FORTRAN-77/90
! DESCRIPTION        : Prepares values for landuse and roughness for one receptorpoint.
! EXIT CODES         :
! FILES AND OTHER    :
!    I/O DEVICES
! SYSTEM DEPENDENCIES: HP-Fortran
! CALLED FUNCTIONS   :
! UPDATE HISTORY :
!-------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE ops_rcp_char_1(ircp, nrrcp, intpol, gxm_rcp, gym_rcp, cs, z0_metreg, xreg, yreg, i1, astat, z0_metreg_user,            &
                       &  spgrid, x_rcp, y_rcp, lugrid, domlu, perc, lu_rcp_per_user_all, lu_rcp_dom_all, f_z0user, z0_rcp_all, &
                       &  uurtot, z0_metreg_rcp, lu_rcp_per, lu_rcp_dom, z0_rcp, error)

USE m_commonconst
USE m_commonfile
USE m_error
USE m_aps
USE m_geoutils

IMPLICIT NONE

! CONSTANTS
CHARACTER*512                                    :: ROUTINENAAM                ! 
PARAMETER    (ROUTINENAAM = 'ops_rcp_char_1')

! SUBROUTINE ARGUMENTS - INPUT
INTEGER*4, INTENT(IN)                            :: ircp                        
INTEGER*4, INTENT(IN)                            :: nrrcp   
INTEGER*4, INTENT(IN)                            :: intpol                     ! 
REAL*4,    INTENT(IN)                            :: gxm_rcp                    ! array met x-coordinaat van receptorpunten (lola)
REAL*4,    INTENT(IN)                            :: gym_rcp                    ! array met y-coordinaat van receptorpunten (lola)
REAL*4,    INTENT(IN)                            :: cs(NTRAJ, NCOMP, NSTAB, NSEK, NMETREG) ! 
REAL*4,    INTENT(IN)                            :: z0_metreg(NMETREG)         ! roughness lengths of NMETREG meteo regions; scale < 50 km [m]
REAL*4,    INTENT(IN)                            :: xreg(NMETREG)              ! array met x-coordinaat van meteo-regios
REAL*4,    INTENT(IN)                            :: yreg(NMETREG)              ! array met y-coordinaat van meteo-regio's
REAL*4,    INTENT(IN)                            :: z0_metreg_user             ! roughness length of user specified meteo region [m]
INTEGER*4, INTENT(IN)                            :: spgrid
REAL*4,    INTENT(IN)                            :: x_rcp                      ! array met x-coordinaat van receptorpunten (RDM)
REAL*4,    INTENT(IN)                            :: y_rcp                      ! array met y-coordinaat van receptorpunten (RDM)
TYPE (TApsGridInt), INTENT(IN)                   :: lugrid                     ! grid with land use information
LOGICAL*4, INTENT(IN)                            :: domlu                      ! index of dominant land use class
LOGICAL*4, INTENT(IN)                            :: perc                           ! 
INTEGER,   INTENT(IN)                            :: lu_rcp_per_user_all(nrrcp,NLU) ! percentage of landuse for all receptors, used defined in receptor file
INTEGER*4, INTENT(IN)                            :: lu_rcp_dom_all(nrrcp)      ! land use at receptor points
LOGICAL*4, INTENT(IN)                            :: f_z0user                   
REAL*4,    INTENT(IN)                            :: z0_rcp_all(nrrcp)                 ! roughness lengths for all receptors; from z0-map or receptor file [m]
! SUBROUTINE ARGUMENTS - I/O
INTEGER*4, INTENT (INOUT)                        :: i1(NTRAJ-1)                ! 
REAL*4,    INTENT(INOUT)                         :: astat(NTRAJ,NCOMP,NSTAB,NSEK) ! 
! SUBROUTINE ARGUMENTS - OUTPUT
REAL*4,    INTENT(OUT)                           :: uurtot                     ! 
REAL*4,    INTENT(OUT)                           :: z0_metreg_rcp              ! roughness length at receptor; interpolated from meteo regions [m]
INTEGER*4, INTENT(OUT)                           :: lu_rcp_dom                 ! dominant landuse class for receptor
REAL*4,    INTENT(OUT)                           :: lu_rcp_per(NLU)            ! percentages of landuse classes at receptor points
REAL*4,    INTENT(OUT)                           :: z0_rcp                     ! roughness length at receptor; from z0-map [m]
TYPE (TError)                                    :: error  

! LOCAL VARIABLES
INTEGER*4                                        :: lu_rcp_per_int(NLU)        ! percentages of landuse classes at receptor points
INTEGER                                          :: lu
LOGICAL                                          :: iscell                     ! whether point is inside masker grid

! SCCS-ID VARIABLES
CHARACTER*81                                     :: sccsida                    ! 
sccsida = '%W%:%E%'//char(0)

!-------------------------------------------------------------------------------------------------------------------------------
z0_rcp        = z0_rcp_all(ircp)
lu_rcp_per    = 0.
z0_metreg_rcp = 0
!
! Select the three nearest climatological regions and interpolate between them in order to compute z0_metreg_rcp
! Note: intpol = 0 -> interpolate    
!
IF (intpol.EQ.0) THEN
  CALL reginpo(gxm_rcp, gym_rcp, cs, z0_metreg, xreg, yreg, i1, z0_metreg_rcp, uurtot, astat, error)
  IF (error%haserror) GOTO 9999
ELSE
  ! Get z0 of user specified meteo region:
  z0_metreg_rcp = z0_metreg_user
ENDIF
!
! Retreive landuse and roughness values for this receptorpoint.
!
IF (ANY(spgrid == (/0,1/))) THEN
  IF (.not.domlu) THEN
!
! ---   Fill lu_rcp_per array with info from standard grid with landuse info
! ---   The first aps-grid contains dominant landuse so we start with the second
!
    DO lu=2,NLU+1
      CALL GridValue(x_rcp/1000, y_rcp/1000, lugrid, lu_rcp_per_int(lu-1), iscell, lu)
    ENDDO     
    lu_rcp_per = float(lu_rcp_per_int)
  ENDIF
ELSE
  IF (.not.perc) THEN
!
! ---   User did not specify percentages landuse in rcp-file.
! ---   Fill lu_rcp_per array with info from standard grid with landuse info
!
    DO lu=2,NLU+1
      CALL GridValue(x_rcp/1000, y_rcp/1000, lugrid, lu_rcp_per_int(lu-1), iscell, lu)
    ENDDO     
    lu_rcp_per = float(lu_rcp_per_int)
  ELSE
!
! ---   User specified percentages landuse in rcp-file.
! ---   Fill lu_rcp_per array with info read from rcp-file
!
    DO lu=1,NLU
      lu_rcp_per(lu) = float(lu_rcp_per_user_all(ircp,lu))
    ENDDO
  ENDIF
ENDIF
!
!   Set roughness length and dominant landuse class at receptor (user specified)
!
IF (f_z0user) THEN
  lu_rcp_dom = 1
ELSE  
!
!     Set roughness length and dominant landuse class at receptor (not user specified)
!
  lu_rcp_dom = lu_rcp_dom_all(ircp)
ENDIF
!
! If option for dominant land use is set, set landuse percentage of dominant land use class to 100:
!
IF (domlu) THEN
  lu_rcp_per             = 0.0
  lu_rcp_per(lu_rcp_dom) = 100.0
ENDIF
!
!  IF no landuse information available in standard grid or rcp-file, we asume it's grass
!
IF (sum(lu_rcp_per(1:NLU)) .le. 0) THEN
  lu_rcp_per    = 0.0
  lu_rcp_per(1) = 100.0
ENDIF        

9999 CALL ErrorCall(ROUTINENAAM, error)
RETURN

CONTAINS

!-------------------------------------------------------------------------------------------------------------------------------
! SUBROUTINE         : reginpo, contained in ops_tra_char
! DESCRIPTION        : Determine the three meteo statistics regions nearest to location (x,y) and interpolate
!                      parameters between these three regions. Interpolated parameters are stored in astat.
!-------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE reginpo(x, y, cs, z0_metreg, xreg, yreg, i1, z0_metreg_xy, uurtot, astat, error)

! CONSTANTS
CHARACTER*512                                    :: ROUTINENAAM                ! 
PARAMETER      (ROUTINENAAM = 'reginpo')

INTEGER*4                                        :: NONZERO(NCOMP)             ! meteo parameters for which no interpolation has to
                                                                               ! be done when one of the interpolants has a zero
                                                                               ! frequency of occurrence

! SUBROUTINE ARGUMENTS - INPUT
REAL*4,    INTENT(IN)                            :: x                          ! x-coordinate (longitude; degrees)
REAL*4,    INTENT(IN)                            :: y                          ! y-coordinate (latitude; degrees)
REAL*4,    INTENT(IN)                            :: cs(NTRAJ, NCOMP, NSTAB, NSEK, NMETREG) ! 
REAL*4,    INTENT(IN)                            :: z0_metreg(NMETREG)         ! roughness lengths of NMETREG meteo regions; scale < 50 km [m]
REAL*4,    INTENT(IN)                            :: xreg(NMETREG)              ! x-coordinate region centre (longitude; degrees)
REAL*4,    INTENT(IN)                            :: yreg(NMETREG)              ! y-coordinate region centre (latitude; degrees)

! SUBROUTINE ARGUMENTS - I/O
INTEGER*4, INTENT(INOUT)                         :: i1(NTRAJ-1)                ! indices of three regions nearest to the receptor

! SUBROUTINE ARGUMENTS - OUTPUT
REAL*4,    INTENT(OUT)                           :: z0_metreg_xy               ! roughness length at (x,y), interpolated from meteo regions [m]
REAL*4,    INTENT(OUT)                           :: uurtot                     ! 
REAL*4,    INTENT(OUT)                           :: astat(NTRAJ, NCOMP, NSTAB, NSEK) ! 
TYPE (TError), INTENT(OUT)                       :: error                      ! error handling record

! LOCAL VARIABLES
INTEGER*4                                        :: ireg                       ! index of region
INTEGER*4                                        :: icomp                      ! index of meteo component (parameter)
INTEGER*4                                        :: istab                      ! index of stability class
INTEGER*4                                        :: imin                       ! index of nearest region
INTEGER*4                                        :: itraj                      ! index of distance class
INTEGER*4                                        :: isek                       ! index of wind sector
REAL*4                                           :: a                          ! Set a = cos(y); needed in computation of distance
                                                                               ! dx = (x2 - x1)*cos(y) for geographical coordinates

REAL*4                                           :: r                          ! distance region - receptor
REAL*4                                           :: rmin                       ! distance nearest region - receptor
REAL*4                                           :: s                          ! sum of s1()
REAL*4                                           :: ss                         ! 
REAL*4                                           :: rr                         ! 
REAL*4                                           :: rrtot                      ! 
REAL*4                                           :: r1(NTRAJ-1)                ! distance of three nearest regions - receptor
REAL*4                                           :: s1(NTRAJ-1)                ! inverse distance = 1/r1()
REAL*4                                           :: ss1(NTRAJ-1)               ! 

! DATA
!            1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27
DATA NONZERO/0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1/ 
!
! If NONZERO = 1 -> no interpolation has to be done when one of the interpolants has a zero frequency of occurrence;
!                   in this case the weighing coefficients ss1 are used, which are zero if the class does not occur
! If NONZERO = 0 -> interpolate anyhow, even if one of the interpolants has a zero frequency of occurrence;
!                   in this case the weighing coefficients s1 are used, which are always non-zero
!
! NONZERO = 0 ('interpolate anyhow') for parameters 1, 17, 18:
!        1. number of hours for which a certain meteo class (stability class + wind direction class) has occurred [-]
!        17. distribution of stability classes over day, source oriented [-]
!        18. distribution of stability classes over day, receptor oriented [-]

! SCCS-ID VARIABLES
CHARACTER*81                                     :: sccsida                    ! 
sccsida = '%W%:%E%'//char(0)
!-------------------------------------------------------------------------------------------------------------------------------
! Set a = cos(y); needed in computation of dx = (x2 - x1)*cos(y) for geographical coordinates
a = COS(y/CONV)

! Initialise sums:
s      = 0.
z0_metreg_xy = 0.

! Loop over 3 nearest regions



!
DO itraj = 1, NTRAJ - 1 
  
   ! Initialise minimal distance at 'HUGE'
   rmin = 100000.

   ! Loop over all regions:
   DO ireg = 1, NMETREG
!
!     Check whether any index of nearest regions i1 has already been assigned to ireg (in a previous loop over ireg_nearest);
!     if so, skip this region.
!
      IF (.NOT. ANY(i1(1:itraj-1).EQ.ireg)) THEN 

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
!  interpolation of z0 over the nearest regions.
!
   i1(itraj)    = imin 
   r1(itraj)    = rmin
   s1(itraj)    = 1./(r1(itraj) + .01)
   s            = s + s1(itraj)
   z0_metreg_xy = z0_metreg_xy + s1(itraj)*z0_metreg(imin)                                     ! 940907
ENDDO

IF (ABS(s) .LE. EPS_DELTA) THEN
   s = .01
ENDIF

! Compute weighed average over nearest meteo regions (weighing coefficients are the inverse minimal distances)
z0_metreg_xy = z0_metreg_xy / s

! Loop over all {distance,stability,wind sector} classes
DO itraj = 1, NTRAJ
  DO istab = 1, NSTAB
    DO isek = 1, NSEK
!
!     Compute ss1 = weighing coefficients (inverse distances) for nearest meteo regions
!     and ss = sum of all weighing coefficients
!
      ss = 0.
      DO ireg = 1, NTRAJ-1 

        ! If {distance,stability,wind sector} class does not occur for the current nearest region,
        ! ss1 = 0, otherwise ss1 = s1
        IF (cs(itraj, 1, istab, isek, i1(ireg)).LE.EPS_DELTA) THEN
          ss1(ireg) = 0.
        ELSE
          ss1(ireg) = s1(ireg)
        ENDIF
        ss = ss + ss1(ireg)
      ENDDO

      IF (ABS(ss) .LE. EPS_DELTA) THEN
        ss = .01
      ENDIF
!
!     Compute weighing coefficients rr = ss1(ireg/ss); interpolate and assign interpolated parameters to astat.
!
      DO icomp = 1, NCOMP
        rrtot = 0
        DO ireg = 1, NTRAJ-1 

          ! If NONZERO = 1 -> no interpolation has to be done when one of the interpolants has a zero frequency of occurrence;
          !                   in this case the weighing coefficients ss1 are used, which are zero if the class does not occur
          ! If NONZERO = 0 -> interpolate anyhow, even if one of the interpolants has a zero frequency of occurrence;
          !                   in this case the weighing coefficients s1 are used, which are always non-zero
          IF (NONZERO(icomp) .EQ. 1) THEN
            rr = ss1(ireg) / ss
          ELSE
            rr = s1(ireg) / ss
          ENDIF
          rrtot = rrtot + rr * cs(itraj, icomp, istab, isek, i1(ireg))
        ENDDO
        astat(itraj, icomp, istab, isek) = rrtot
      ENDDO
    ENDDO
  ENDDO
ENDDO

! uurtot = total number of hours for {distance class=1,stability, wind sector} classes
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

END SUBROUTINE ops_rcp_char_1
    
