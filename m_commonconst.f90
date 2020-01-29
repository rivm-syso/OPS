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
! MODULE             : m_commonconst
! FILENAME           : %M%
! SCCS(SOURCE)       : %P%
! RELEASE - LEVEL    : %R% - %L%
! BRANCH - SEQUENCE  : %B% - %S%
! DATE - TIME        : %E% - %U%
! WHAT               : %W%:%E%
! AUTHOR             : Martien de Haan (ARIS)
! FIRM/INSTITUTE     : RIVM/LLO
! LANGUAGE           : FORTRAN-F90
! DESCRIPTION        : Defines common parameters, values, etc.
! EXIT CODES         :
! FILES AND OTHER    :
!    I/O DEVICES
! SYSTEM DEPENDENCIES: HP Fortran
! CALLED FUNCTIONS   :
! UPDATE HISTORY :
!-------------------------------------------------------------------------------------------------------------------------------
MODULE m_commonconst

USE Binas, only: pi                                                     
           
INTEGER*4, PARAMETER                             :: NUNIT       = 6                    ! number of units for deposition
INTEGER*4, PARAMETER                             :: NMETREG     = 6                    ! number of meteo regions
INTEGER*4, PARAMETER                             :: NSEK        = 12                   ! number of wind sectors
INTEGER*4, PARAMETER                             :: NSTAB       = 6                    ! number of stability classes 
INTEGER*4, PARAMETER                             :: NTRAJ       = 4                    ! number of distance classes
INTEGER*4, PARAMETER                             :: NCOMP       = 27                   ! number of components in meteo input (from METPRO)
INTEGER*4, PARAMETER                             :: NHRBLOCKS   = 12                   ! number of two-hour blocks in a day
INTEGER*4, PARAMETER                             :: NPARTCLASS  = 6                    ! number of particle size classes
INTEGER*4, PARAMETER                             :: NMONTH      = 12                   ! number of months in a year
INTEGER*4, PARAMETER                             :: NKLIGEB     = 8                    ! number of climate regions in NL (KLIGEB << klimaatgebieden = climate regions)
INTEGER*4, PARAMETER                             :: LSBUF       = 4000                 ! size of buffer for reading emissions
INTEGER*4, PARAMETER                             :: NBRMAX      = 10                   ! maximal number of emission sources for which there is emission data written to print file
INTEGER*4, PARAMETER                             :: NCATMAX     = 199                  ! maximal number of emission categories
INTEGER*4, PARAMETER                             :: NLANDMAX    = 50                   ! maximal number of emission countries (land << country)
INTEGER*4, PARAMETER                             :: NBGMAPS     =  5                   ! number of background maps
INTEGER*4, PARAMETER                             :: NYEARS      = 41                   ! number of years for interpolating backgground maps
INTEGER*4, PARAMETER                             :: MAXDISTR    = 9999                 ! maximal number of distributions (for particle size or emission variation)    
INTEGER*4, PARAMETER                             :: MAXROW      = 9999                 ! maximal number of rows in receptor grid
INTEGER*4, PARAMETER                             :: MAXCOL      = 9999                 ! maximal number of columns in receptor grid
INTEGER*4, PARAMETER                             :: NLU         = 9                    ! number of landuse classes
INTEGER*4, PARAMETER                             :: ncolBuildingEffectTable = 5        ! 1st column corresponds to distance from building. 2-5 correspond to different building types

! CONSTANTS - overige
REAL*4                                           :: z0_FACT_NL  = 10000.               ! default factor for conversion of z0_nl gridvalue to meters
REAL*4                                           :: z0_FACT_EUR = 10000.               ! default factor for conversion of z0_eur gridvalue to meters
                                                                                       
REAL*4, PARAMETER                                :: zmet_T      = 1.5                  ! reference height for temperature measurements [m]

INTEGER*4, PARAMETER                             :: IGEO        = 0                    ! 1 -> Geographical coordinates lon-lat [degrees]; 0 -> RDM coordinates [m]  
INTEGER*4, PARAMETER                             :: MISVALNUM   = -9999                ! missing value
INTEGER*4, PARAMETER                             :: FIRSTYEAR   = 1977                 ! first year, used for interpolating background maps
INTEGER*4, PARAMETER                             :: FUTUREYEAR  = 2020                 ! future year, used for interpolating background maps
REAL*4                                           :: r4_for_tiny                        ! help variable to define EPS_DELTA
REAL*8                                           :: r8_for_tiny                        ! help variable to define DEPS_DELTA
REAL*4,    PARAMETER                             :: EPS_DELTA   = tiny(r4_for_tiny)    ! tiny number (real)
REAL*8,    PARAMETER                             :: DPEPS_DELTA = tiny(r8_for_tiny)    ! tiny number (double precision)
! REAL*4,    PARAMETER                             :: PI          = 3.14159265
REAL*4,    PARAMETER                             :: HUMAX       = 500.                 ! maximal plume height [m]      
CHARACTER*8,  PARAMETER                          :: MODVERSIE   = '4.6.2.5'            ! model version OPS-LT
CHARACTER*20, PARAMETER                          :: RELEASEDATE = '06 dec 2019'        ! release date

!
! CONSTANTS - Data
!
INTEGER*4                                        :: NACHTZOMER(NSTAB, NTRAJ)           ! relative occurrences (%) of nighttime hours in summer (for each stability class and distance class) ("NACHT" = night, "ZOMER" = summer)
INTEGER*4                                        :: NACHTWINTER(NSTAB, NTRAJ)          ! relative occurrences (%) of nighttime hours in winter (for each stability class and distance class) ("NACHT" = night)
REAL*4                                           :: DISPH(NSTAB)                       ! coefficients for vertical dispersion coefficient sigma_z; sigma_z = dispg*x**disph  
REAL*4                                           :: STOKES(NPARTCLASS)                 ! Sedimentation velocity (m/s) needed for plume descent in case of heavy particles, for each particle class
REAL*4                                           :: SCWINTER(NSTAB)                    ! variation in NO2/NOx ratio (relative to stability class S2) for each stability class (only in winter)
REAL*4                                           :: cf_so2(NBGMAPS)                    ! correction factors for the difference between model output and measurements for SO2
REAL*4                                           :: cf_nox(NBGMAPS)                    ! correction factors for the difference between model output and measurements for NOx
REAL*4                                           :: cf_nh3(NBGMAPS)                    ! correction factors for the difference between model output and measurements for NH3
REAL*4                                           :: tf_so2(NYEARS + 1)                 ! trendfactors for SO2: concentration in year T, relative to the concentration in reference year
REAL*4                                           :: tf_no2(NYEARS + 1)                 ! trendfactors for NO2: concentration in year T, relative to the concentration in reference year
REAL*4                                           :: tf_nh3(NYEARS + 1)                 ! trendfactors for NH3: concentration in year T, relative to the concentration in reference year
REAL*4                                           :: nox_no2_beta(2)                    ! coefficient in conversion NO2 = beta(1)*log(NOx) + beta(2)
CHARACTER*10                                     :: CNAME(3,5)                         ! names of substances (primary, secondary, second secondary, deposited, name in DEPAC)
CHARACTER*10                                     :: UNITS(2)                           ! units for concentration
CHARACTER*10                                     :: DEPUNITS(NUNIT)                    ! units for deposition
CHARACTER*40                                     :: KLIGEB(NKLIGEB)                    ! climate regions in NL (KLIGEB << klimaatgebieden = climate regions)

!
! Set coefficients in conversion function NO2 = beta1*log(NOx) + beta2;
! based on LML-measurements in 1993 
!
DATA nox_no2_beta  /8.6, -12.4/

! Set relative occurrences (%) of nighttime hours in summer and winter
! (for each stability class and distance class) ("NACHT" = night, "ZOMER" = summer)

!
DATA NACHTZOMER  /0, 0, 61, 61, 100, 98, 17, 17, 68, 68, 63, 83, 43, 43, 44, 44, 42, 44, 43, 43, 44, 44, 42, 44/
DATA NACHTWINTER /0 , 0 , 66, 66, 100, 99, 25, 25, 71, 71, 77, 92, 62, 64, 74, 63, 64, 63, 62, 74, 74, 63, 64, 63/
!
! Set coefficients for vertical dispersion coefficient; sigma_z = dispg*x**disph  
! (For DISPG, see ops_main DATA statements)
DATA DISPH       /.82,.82,.76,.76,.67,.76/ 
!
! Sedimentation velocity (m/s) needed for plume descent in case of heavy particles, for each particle class.  
! Sedimentation velocity depends on particle size according to Stokes law; see ops_conc_ini
DATA STOKES      /0., 0., 0.0003, 0.0012, 0.0055, 0.047/ 
!
! Set SCWINTER (variation in NO2/NOx ratio (relative to stability class S2) for each stability class (only in winter))
! see OPS-doc/chem, bookmark table_no2_nox. [0.47 0.47 0.62 0.69 0.39 0.58] /0.58 = [0.81  0.81  1.19  1.03  0.67  1.00]
!
DATA SCWINTER    /.81, .81, 1.07, 1.19, .67, 1.0/
!
! Correction factors for the difference between model output and measurements
! These correction factors are given for 4 historical years and one year in
! the future. The correction factor for the future year is by definition equal
! to the correction factor for the last historical year. 
!
DATA cf_so2      / 1.04, 0.96, 0.69, 0.72, 0.72 /
DATA cf_nox      / 0.93, 0.94, 0.77, 0.94, 0.94 /
DATA cf_nh3      / 0.83, 0.83, 0.94, 1.12, 1.12 /
!
! Trendfactors: concentration in year T, relative to the concentration in reference year
! - period 1977 upto 1989 relative to 1984
! - period 1990 upto 1999 relative to 1994
! - period 2000 upto 2006 relative to 2005
! - period 2007 upto -    relative to 2012
! The trendfactor for the year in the future is by definition equal to 1.
!
DATA tf_so2      /1.11,1.39,1.79,1.27,1.19,1.20,0.94,1.00,1.04,1.02,1.10,0.62,0.65,    &  ! 1977 t/m 1989 (ref=1984)
               &                 1.60,1.70,1.46,1.33,1.00,0.86,1.02,0.78,0.63,0.50,    &  ! 1990 t/m 1999 (ref=1994)
               &            1.33,1.15,1.20,1.14,1.01,1.00,0.96,                        &  ! 2000 t/m 2006 (ref=2005)
               &            1.72,1.85,1.75,1.44,1.14,1.00,0.98,1.10,0.81,0.61,0.58,1.00/ ! 2007 t/m 2017 plus future (ref=2012)

DATA tf_no2      /0.91,0.92,1.03,0.93,0.92,1.00,0.93,1.00,1.05,0.93,0.88,0.81,0.94,    &  ! 1977 t/m 1989 (ref=1984)
               &                 1.12,1.19,1.06,1.02,1.00,0.94,1.04,1.03,0.91,0.86,    &  ! 1990 t/m 1999 (ref=1994)
               &            1.07,1.05,1.06,1.17,1.06,1.00,0.99,                        &  ! 2000 t/m 2006 (ref=2005)
               &            1.09,1.15,1.12,1.06,1.04,1.00,0.94,0.90,0.84,0.88,0.89,1.00/  ! 2007 t/m 2017 plus future (ref=2012)

DATA tf_nh3      /1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,    &  ! 1977 t/m 1989 (ref=1984)
               &                 1.00,1.00,1.00,1.01,1.00,0.97,0.97,1.03,0.75,0.85,    &  ! 1990 t/m 1999 (ref=1994)
               &            0.94,1.00,0.83,1.04,0.84,1.00,1.08,                        &  ! 2000 t/m 2006 (ref=2005)
               &            0.86,0.84,0.98,1.00,1.10,1.00,1.00,1.05,0.93,1.04,1.04,1.00/  ! 2007 t/m 2017 plus future (ref=2012)
!
! Declaration of the naming convention used for SO2, NOx and NH3
! CNAME(:,1): name of primary substance
! CNAME(:,2): name of first secondary substance
! CNAME(:,3): name of second secondary substance (currently only for NOx we have two secondary substances)
! CNAME(:,4): deposited substance
! CNAME(:,5): primary substance name used in DEPAC
!
DATA CNAME       /'SO2', 'NOx'     , 'NH3',  &
               &  'SO4', 'NO3+HNO3', 'NH4',  &
               &  '   ', 'NO3'     , '   ',  &
               &  'SOx', 'NOy'     , 'NHx',  &
               &  'SO2', 'NO2'     , 'NH3'   /
!
! Units for concentration and deposition
!
DATA UNITS       /'ug/m3', 'ug/m3 NO2'/
DATA DEPUNITS    /' mmol/m2/s', ' g/m2/s   ', ' mol/ha/y ', ' kg/ha/y  ', ' mmol/m2/y', ' g/m2/y   '/

!
! meteo regions (KLIGEB << klimaatgebieden = climate regions)
!
DATA KLIGEB      /'The Netherlands                    ',                    &
               &  'N-Holland, N-Friesland, N-Groningen',                    & 
               &  'Randstad, W-Brabant, E-Zeeland     ',                    &
               &  'Drente, S-Friesland, S-Groningen   ',                    &
               &  'W-Zeeland, ZH-Islands              ',                    &
               &  'Mid-Brabant, Veluwe, Twente        ',                    &
               &  'S-Limburg, E-Brabant, Achterhoek   ',                    &
               &  'Special climatological datafile    '/                       ! always the last one

END MODULE m_commonconst
