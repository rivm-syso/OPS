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
! DESCRIPTION        : Defines common parameters, values, etc.
!-------------------------------------------------------------------------------------------------------------------------------
MODULE m_commonconst_lt

use m_commonconst_lib
! m_commonconst_lib contains the following variables (see m_commonconst_lib.f90 for actual settings)
! INTEGER,   PARAMETER    :: NSEK        = 12                   ! number of wind sectors
! INTEGER,   PARAMETER    :: NSTAB       = 6                    ! number of stability classes
! INTEGER,   PARAMETER    :: NTRAJ       = 4                    ! number of distance classes
! INTEGER,   PARAMETER    :: NCOMP       = 27                   ! number of components in meteo input (from METPRO)
! INTEGER,   PARAMETER    :: NMETREG     = 6                    ! number of meteo regions
! INTEGER,   PARAMETER    :: MAXDISTR    = 999999               ! maximal number of distributions (for particle size or emission variation)
! INTEGER,   PARAMETER    :: ncolBuildingEffectTable = 5        ! 1st column = distance from building. 2-5 = different building types
! REAL,   PARAMETER       :: zmet_T      = 1.5                  ! reference height for temperature measurements [m]
! INTEGER,   PARAMETER    :: MISVALNUM   = -9999                ! missing value
! REAL                    :: r4_for_tiny                        ! help variable to define EPS_DELTA
! DOUBLE PRECISION                    :: r8_for_tiny                        ! help variable to define DEPS_DELTA
! REAL,      PARAMETER    :: EPS_DELTA   = tiny(r4_for_tiny)    ! tiny number (real)
! DOUBLE PRECISION,      PARAMETER    :: DPEPS_DELTA = tiny(r8_for_tiny)    ! tiny number (double precision)
! REAL,      PARAMETER    :: HUMAX       = 500.                 ! maximal plume height [m]
! END m_commonconst_lib

USE Binas, only: pi

INTEGER,   PARAMETER                             :: NUNIT       = 6                    ! number of units for deposition
INTEGER,   PARAMETER                             :: NHRBLOCKS   = 12                   ! number of two-hour blocks in a day
INTEGER,   PARAMETER                             :: NPARTCLASS  = 6                    ! number of particle size classes
INTEGER,   PARAMETER                             :: NMONTH      = 12                   ! number of months in a year
INTEGER,   PARAMETER                             :: NKLIGEB     = 8                    ! number of climate regions in NL (KLIGEB << klimaatgebieden = climate regions)
INTEGER,   PARAMETER                             :: LSBUF       = 4000                 ! size of buffer for reading emissions
INTEGER,   PARAMETER                             :: NBRMAX      = 10                   ! maximal number of emission sources for which there is emission data written to print file
INTEGER,   PARAMETER                             :: NCATMAX     = 199                  ! maximal number of emission categories
INTEGER,   PARAMETER                             :: NLANDMAX    = 50                   ! maximal number of emission countries (land << country)
INTEGER,   PARAMETER                             :: NBGMAPS     = 5                    ! number of background maps for reference years
INTEGER,   PARAMETER                             :: MAXROW      = 9999                 ! maximal number of rows in receptor grid
INTEGER,   PARAMETER                             :: MAXCOL      = 9999                 ! maximal number of columns in receptor grid
INTEGER,   PARAMETER                             :: DISTMIN     = 5000                 ! mimimum distance in meters for which a result will be calculated (if mindist=.true.)
INTEGER,   PARAMETER                             :: DISTMAX     = 25000                ! maximum distance in meters for which a result will be calculated (if maxdist=.true.)

! CONSTANTS - overige
REAL                                             :: z0_FACT_NL  = 10000.               ! default factor for conversion of z0_nl gridvalue to meters
REAL                                             :: z0_FACT_EUR = 10000.               ! default factor for conversion of z0_eur gridvalue to meters


INTEGER,   PARAMETER                             :: IGEO        = 0                    ! 1 -> Geographical coordinates lon-lat [degrees]; 0 -> RDM coordinates [m]
INTEGER,   PARAMETER                             :: FIRSTYEAR   = 1977                 ! first year, used for interpolating background maps
INTEGER,   PARAMETER                             :: NYEARS      =   47                 ! number of years for interpolating background maps
INTEGER,   PARAMETER                             :: FUTUREYEAR  = 2025                 ! future year, from which prognosis chemical maps are used
CHARACTER*8,  PARAMETER                          :: MODVERSIE   = '5.3.1.0'            ! model version OPS-LT
CHARACTER*20, PARAMETER                          :: RELEASEDATE = '19 dec 2024'        ! release date

!
! CONSTANTS - Data
!
INTEGER                                          :: NACHTZOMER(NSTAB, NTRAJ)           ! relative occurrences (%) of nighttime hours in summer (for each stability class and distance class) ("NACHT" = night, "ZOMER" = summer)
INTEGER                                          :: NACHTWINTER(NSTAB, NTRAJ)          ! relative occurrences (%) of nighttime hours in winter (for each stability class and distance class) ("NACHT" = night)
REAL                                             :: DISPH(NSTAB)                       ! coefficients for vertical dispersion coefficient sigma_z; sigma_z = dispg*x**disph
REAL                                             :: STOKES(NPARTCLASS)                 ! Sedimentation velocity (m/s) needed for plume descent in case of heavy particles, for each particle class
REAL                                             :: SCWINTER(NSTAB)                    ! variation in NO2/NOx ratio (relative to stability class S2) for each stability class (only in winter)
REAL                                             :: nox_no2_beta(2)                    ! coefficient in conversion NO2 = beta(1)*log(NOx) + beta(2)
CHARACTER*10                                     :: CNAME(3,5)                         ! names of substances (primary, secondary, second secondary, deposited, name in DEPAC)
CHARACTER*10                                     :: CNAME_SUBSEC(4)                    ! names of sub-secondary species (HNO3, NO3_C, NO3_F)
CHARACTER*10                                     :: UNITS(2)                           ! units for concentration
CHARACTER*10                                     :: DEPUNITS(NUNIT)                    ! units for deposition
CHARACTER*40                                     :: KLIGEB(NKLIGEB)                    ! climate regions in NL (KLIGEB << klimaatgebieden = climate regions)

! Set coefficients in conversion function NO2 = beta1*log(NOx) + beta2;
! based on LML-measurements in 1993
DATA nox_no2_beta  /8.6, -12.4/

! Set relative occurrences (%) of nighttime hours in summer and winter
! (for each stability class and distance class) ("NACHT" = night, "ZOMER" = summer)
! NACHTZOMER, NACHTWINTER: see OPS-doc/chem, bookmark table_no2_nox.
DATA NACHTZOMER  /0, 0, 61, 61, 100, 98, 17, 17, 68, 68, 63, 83, 43, 43, 44, 44, 42, 44, 43, 43, 44, 44, 42, 44/
DATA NACHTWINTER /0 , 0 , 66, 66, 100, 99, 25, 25, 71, 71, 77, 92, 62, 64, 74, 63, 64, 63, 62, 74, 74, 63, 64, 63/

! Set coefficients for vertical dispersion coefficient; sigma_z = dispg*x**disph
! (For DISPG, see ops_main DATA statements)
DATA DISPH       /.82,.82,.76,.76,.67,.76/

! Sedimentation velocity (m/s) needed for plume descent in case of heavy particles, for each particle class.
! Sedimentation velocity depends on particle size according to Stokes law; see ops_conc_ini
DATA STOKES      /0., 0., 0.0003, 0.0012, 0.0055, 0.047/

! Set SCWINTER (variation in NO2/NOx ratio (relative to stability class S2) for each stability class (only in winter))
! see OPS-doc/chem, bookmark table_no2_nox. [0.47 0.47 0.62 0.69 0.39 0.58] /0.58 = [0.81  0.81  1.19  1.03  0.67  1.00]
DATA SCWINTER    /.81, .81, 1.07, 1.19, .67, 1.0/

! Declaration of the naming convention used for SO2, NOx and NH3
! CNAME(:,1): name of primary substance
! CNAME(:,2): name of first secondary substance
! CNAME(:,3): name of second secondary substance (currently only for NOx we have two secondary substances)
! CNAME(:,4): deposited substance
! CNAME(:,5): primary substance name used in DEPAC
DATA CNAME       /'SO2', 'NOx'     , 'NH3',  &
               &  'SO4', 'NO3+HNO3', 'NH4',  &
               &  '   ', 'NO3'     , '   ',  &  ! not used anymore -> CNAME_SUBSEC
               &  'SOx', 'NOy'     , 'NHx',  &
               &  'SO2', 'NO2'     , 'NH3'   /

! CNAME_SUBSEC is defined in ops_read_ctr
! DATA CNAME_SUBSEC /'HNO3', 'NO3_C', 'NO3_F' /   ! HNO3, NO3_coarse (in PM10-PM2.5), NO3_fine (in PM2.5)
! DATA CNAME_SUBSEC /'HNO3', 'NO3_AER' /          ! HNO3, NO3_aerosol (in PM10)

! Units for concentration and deposition
DATA UNITS       /'ug/m3', 'ug/m3_NO2'/
DATA DEPUNITS    /' mmol/m2/s', ' g/m2/s   ', ' mol/ha/y ', ' kg/ha/y  ', ' mmol/m2/y', ' g/m2/y   '/

! meteo regions (KLIGEB << klimaatgebieden = climate regions)
DATA KLIGEB      /'The_Netherlands                    ',                    &
               &  'N-Holland, N-Friesland, N-Groningen',                    &
               &  'Randstad, W-Brabant, E-Zeeland     ',                    &
               &  'Drente, S-Friesland, S-Groningen   ',                    &
               &  'W-Zeeland, ZH-Islands              ',                    &
               &  'Mid-Brabant, Veluwe, Twente        ',                    &
               &  'S-Limburg, E-Brabant, Achterhoek   ',                    &
               &  'Special_climatological_datafile    '/                       ! always the last one

integer, parameter :: icm_SO2 = 1, icm_NOx=2, icm_NH3=3, icm_PM=24
END MODULE m_commonconst_lt
