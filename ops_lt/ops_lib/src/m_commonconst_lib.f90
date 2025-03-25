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
! DESCRIPTION        : Defines common parameters, values, etc. stripped version for library
!-------------------------------------------------------------------------------------------------------------------------------
MODULE m_commonconst_lib

USE Binas, only: pi                                                     

! CONSTANTS (dimensions of meteo input)
INTEGER,   PARAMETER                             :: NSEK        = 12                   ! number of wind sectors
INTEGER,   PARAMETER                             :: NSTAB       = 6                    ! number of stability classes 
INTEGER,   PARAMETER                             :: NTRAJ       = 4                    ! number of distance classes
INTEGER,   PARAMETER                             :: NCOMP       = 27                   ! number of components in meteo input (from METPRO)
INTEGER,   PARAMETER                             :: NMETREG     = 6                    ! number of meteo regions

! CONSTANTS (component identifiers), use id's from earlier version of DEPAC
INTEGER,   PARAMETER                             :: i_SO2       = 1
INTEGER,   PARAMETER                             :: i_NO2       = 2
INTEGER,   PARAMETER                             :: i_NO        = 3
INTEGER,   PARAMETER                             :: i_NH3       = 4
INTEGER,   PARAMETER                             :: i_HNO3      = 5
INTEGER,   PARAMETER                             :: i_O3        = 6


! CONSTANTS (dimensions of distributions, building effect table)
INTEGER,   PARAMETER                             :: MAXDISTR    = 999999               ! maximal number of distributions (for particle size or emission variation)    
INTEGER,   PARAMETER                             :: ncolBuildingEffectTable = 5        ! 1st column corresponds to distance from building. 2-5 correspond to different building types
! 
! CONSTANTS - miscellaneous
REAL,   PARAMETER                                :: zmet_T      = 1.5                  ! reference height for temperature measurements [m]
REAL,   PARAMETER                                :: zmet_u      = 10.0                 ! reference height for wind speed measurements [m]
INTEGER,   PARAMETER                             :: MISVALNUM   = -9999                ! missing value
REAL                                             :: r4_for_tiny                        ! help variable to define EPS_DELTA
DOUBLE PRECISION                                             :: r8_for_tiny                        ! help variable to define DEPS_DELTA
REAL,      PARAMETER                             :: EPS_DELTA   = tiny(r4_for_tiny)    ! tiny number (real)
DOUBLE PRECISION,      PARAMETER                             :: DPEPS_DELTA = tiny(r8_for_tiny)    ! tiny number (double precision)
REAL,      PARAMETER                             :: HUMAX       = 500.                 ! maximal plume height [m]      
integer, parameter                               :: nlu         = 9                    ! number of landuse types, used in DEPAC

END MODULE m_commonconst_lib
