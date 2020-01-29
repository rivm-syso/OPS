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
! USAGE              :
! AUTHOR             :
! FIRM/INSTITUTE     : RIVM/LLO
! LANGUAGE           : FORTRAN-90
! DESCRIPTION        : Check existence and generate full file names of those files that have not been explicitly defined 
!                      in the control file: files for diurnal variation, particle size distribution, z0-Europe, NL-mask,
!                      meteo statistics. Note: meteo statisctics file names are generated in ops_read_meteo.
! CHANGES            : 
! EXIT CODES         :
! REFERENCE          :
! FILES AND OTHER    :
!   I/O DEVICES
! SYSTEM DEPENDENCIES: HP-Fortran
! CALLED FUNCTIONS   : chkexist
!-------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE ops_gen_fnames(gasv, spgrid, intpol, error)

USE m_error
USE m_fileutils
USE m_string
USE m_commonconst
USE m_commonfile

IMPLICIT NONE

! CONSTANTS
CHARACTER*512                                    :: ROUTINENAAM                
PARAMETER    (ROUTINENAAM = 'ops_gen_fnames')

! SUBROUTINE ARGUMENTS - INPUT
LOGICAL,   INTENT(IN)                            :: gasv                       
INTEGER*4, INTENT(IN)                            :: spgrid                     
INTEGER*4, INTENT(IN)                            :: intpol                     

! SUBROUTINE ARGUMENTS - OUTPUT
TYPE (TError), INTENT(OUT)                       :: error                      ! Error handling record

! LOCAL VARIABLES
INTEGER*4                                        :: teller                     ! region index
INTEGER*4                                        :: naamlengte                 ! length of name meteostatistics file
CHARACTER*512                                    :: helpfile                   ! meteostatistics file name, includes region number

! SCCS-ID VARIABLES
CHARACTER*81                                     :: sccsida                    ! 
sccsida = '%W%:%E%'//char(0)
!-------------------------------------------------------------------------------------------------------------------------------
! Standard file for diurnal variations of emissions
CALL MakeCommonPath(DVFILE, dvnam, error)

! Standard file for particle size distributions 
IF (.NOT.gasv) THEN
  CALL MakeCommonPath(PSDFILE, psdnam, error)
ENDIF

! Mask NL
IF (spgrid == 0) THEN
  CALL MakeCommonPath(BASEMASK, masknam, error)
ENDIF
!
! z0-data for Europe
!
CALL MakeCommonPath(Z0EURFILE, z0eurnam, error)
!
! Test existence of meteostatistics files for interpolated meteo. The existence of the file for non-interpolated meteo
! (intpol /= 0) has already been checked when reading the control file. Note: intpol = 0 -> interpolated meteo !!!
! Name meteo statistics file for interpolated meteo is f.ex. a001101c.001.
! File names are constructed anew in ops_read_meteo.
!
IF (intpol == 0) THEN
  naamlengte=LEN_TRIM(kname)-1
  CALL copystrpart(kname, 1, naamlengte, helpfile, error)
  CALL appendstring(helpfile, '000', error)
  IF (error%haserror) GOTO 999

  DO teller = 1,NMETREG
    WRITE (helpfile(naamlengte+1:naamlengte+3),'(i3.3)') teller
    IF (.NOT.chkexist(helpfile,error)) GOTO 999
  ENDDO
ENDIF

RETURN
!
! Error situation: This was the routine calling before the error occurred.
!
999 CALL ErrorCall(ROUTINENAAM, error)

END SUBROUTINE ops_gen_fnames
