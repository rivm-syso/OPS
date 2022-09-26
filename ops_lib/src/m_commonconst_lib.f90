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
INTEGER*4, PARAMETER                             :: NSEK        = 12                   ! number of wind sectors
INTEGER*4, PARAMETER                             :: NSTAB       = 6                    ! number of stability classes 
INTEGER*4, PARAMETER                             :: NTRAJ       = 4                    ! number of distance classes
INTEGER*4, PARAMETER                             :: NCOMP       = 27                   ! number of components in meteo input (from METPRO)
INTEGER*4, PARAMETER                             :: NMETREG     = 6                    ! number of meteo regions

! CONSTANTS (dimensions of distributions, building effect table)
INTEGER*4, PARAMETER                             :: MAXDISTR    = 999999               ! maximal number of distributions (for particle size or emission variation)    
INTEGER*4, PARAMETER                             :: ncolBuildingEffectTable = 5        ! 1st column corresponds to distance from building. 2-5 correspond to different building types
! 
! CONSTANTS - miscellaneous
REAL*4, PARAMETER                                :: zmet_T      = 1.5                  ! reference height for temperature measurements [m]
REAL*4, PARAMETER                                :: zmet_u      = 10.0                 ! reference height for wind speed measurements [m]
INTEGER*4, PARAMETER                             :: MISVALNUM   = -9999                ! missing value
REAL*4                                           :: r4_for_tiny                        ! help variable to define EPS_DELTA
REAL*8                                           :: r8_for_tiny                        ! help variable to define DEPS_DELTA
REAL*4,    PARAMETER                             :: EPS_DELTA   = tiny(r4_for_tiny)    ! tiny number (real)
REAL*8,    PARAMETER                             :: DPEPS_DELTA = tiny(r8_for_tiny)    ! tiny number (double precision)
REAL*4,    PARAMETER                             :: HUMAX       = 500.                 ! maximal plume height [m]      
integer, parameter                               :: nlu         = 9                    ! number of landuse types, used in DEPAC

END MODULE m_commonconst_lib
