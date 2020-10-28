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
!
! NAME                  : %M%
! SCCS (SOURCE)         : %P%
! RELEASE - LEVEL       : %R% - %L%
! BRANCH -SEQUENCE      : %B% - %S%
! DATE - TIME           : %E% - %U%
! WHAT                  : %W%:%E%
! AUTHOR                : OPS-support 
! FIRM/INSTITUTE        : RIVM/LLO
! LANGUAGE              : FORTRAN-77/90
! DESCRIPTION           : Read source file with emissions.
!                         Emissions are read from a source file and emissions for selected emission categories and countries 
!                         are then copied to a scratch file (line for line);
!                         emission parameters that lie outside a specified range agenerate an error.
! EXIT CODES            :
! FILES AND OTHER       :
!   I/O DEVICES
! SYSTEM DEPENDENCIES   : HP-Fortran
! CALLED FUNCTIONS      : flrs, ops_check, sysread
! UPDATE HISTORY        :
!-------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE ops_read_source(icm, gasv, ncatsel, catsel, nlandsel, landsel, presentcode, numbron, building_present1, error)

USE m_error
USE m_commonfile, only: fu_scratch, fu_bron
USE m_commonconst, only: EPS_DELTA, MAXDISTR 
USE m_ops_emis
USE m_ops_building

IMPLICIT NONE

! CONSTANTS
CHARACTER*512                                    :: ROUTINENAAM               
PARAMETER    (ROUTINENAAM = 'ops_read_source')

! SUBROUTINE ARGUMENTS - INPUT
INTEGER*4, INTENT(IN)                            :: icm                        ! component nummer
LOGICAL,   INTENT(IN)                            :: gasv                       ! component is gasuous     
INTEGER*4, INTENT(IN)                            :: ncatsel                    ! number of selected emission categories
INTEGER*4, INTENT(IN)                            :: catsel(*)                  ! selected emission categories
INTEGER*4, INTENT(IN)                            :: nlandsel                   ! number of selected emission countries
INTEGER*4, INTENT(IN)                            :: landsel(*)                 ! selected emission countries
LOGICAL,   INTENT(IN)                            :: presentcode(MAXDISTR,4)    ! which distribution codes are present 
                                                                               ! presentcode(:,1): diurnal variations
                                                                               ! presentcode(:,2): particle size distributions
                                                                               ! presentcode(:,3): user-defined diurnal variation
                                                                               ! presentcode(:,4): user-defined particle size distributions

! SUBROUTINE ARGUMENTS - OUTPUT
! Note: emission parameters are written to scratch file and are not part of the output arguments
INTEGER*4, INTENT(OUT)                           :: numbron                    ! number of (selected) sources
LOGICAL,   INTENT(OUT)                           :: building_present1          ! at least one building is present in the source file 
TYPE (TError), INTENT(OUT)                       :: error                      ! Error handling record

! LOCAL VARIABLES
INTEGER*4                                        :: ibtg                       ! diurnal emission variation code read from emission record
INTEGER*4                                        :: nrec                       ! record number of source file
INTEGER*4                                        :: mm                         ! source identification number
INTEGER*4                                        :: iland                      ! country/area code read from emission record
INTEGER*4                                        :: idgr                       ! particle size distribution code read from emission record
INTEGER*4                                        :: ibroncat                   ! emission category code read from emission record
INTEGER*4                                        :: ierr                       ! error value
LOGICAL*4                                        :: end_of_file                ! end of file has been reached
INTEGER*4                                        :: brn_version                ! version of emission file
REAL*4                                           :: qob                        ! emission strength read from emission record [g/s] 
REAL*4                                           :: qww                        ! heat content read from emission record [MW] 
REAL*4                                           :: hbron                      ! emission height read from emission record [m] 
REAL*4                                           :: diameter                   ! diameter area source read from emission record (NOT stack diameter) [m] 
REAL*4                                           :: szopp                      ! deviation emission height for area source = initial sigma_z [m] 
REAL*4                                           :: x                          ! x coordinate of source location (RDM [m])               
REAL*4                                           :: y                          ! y coordinate of source location (RDM [m])
LOGICAL                                          :: country_selected           ! emission country has been selected
LOGICAL                                          :: category_selected          ! emission category has been selected
LOGICAL                                          :: VsDs_opt                   ! read stack parameters Ds/Vs/Ts from source file
REAL                                             :: D_stack                    ! diameter of the stack [m]
REAL                                             :: V_stack                    ! exit velocity of plume at stack tip [m/s]
REAL                                             :: Ts_stack                   ! temperature of effluent from stack [K]
LOGICAL                                          :: emis_horizontal            ! horizontal outflow of emission
type(Tbuilding)                                  :: building                   ! structure with building parameters
LOGICAL                                          :: check_psd                  ! check whether particle size distribution has been read

!-------------------------------------------------------------------------------------------------------------------------------
 50 FORMAT (i4, 2f9.0, es12.3, f9.3, f6.1, f8.0, f6.1, 3e12.5, l2, 4i4, 4f9.3) ! format for writing to scratch (RDM; includes D_stack, V_stack, Ts_stack, building parameters, possibly -999). Also possible -999 for qw

! Initialisation:
end_of_file = .FALSE.
building_present1 = .FALSE.

! Read file header (lines starting with !) and determine BRN-VERSION (= version of brn-file; brn << bron = source):
! no BRN-VERSION header -> fixed format
! BRN-VERSION 0         -> fixed format
! BRN-VERSION 1         -> free format
! BRN-VERSION 2         -> free format, include stack parameters D_stack, V_stack, Ts_stack.
! BRN-VERSION 3         -> free format, add parameter building%type with respect to BRN-VERSION 2 - NOT SUPPORTED ANYMORE
! BRN-VERSION 4         -> free format, add parameters building%length, building%width, building%height, building%orientation with respect to BRN-VERSION 2
call ops_emis_read_header(fu_bron, brn_version, VsDs_opt, nrec, numbron, error)
IF (error%haserror) GOTO 9999

! Read source file until end of file:
DO WHILE (.NOT. end_of_file)

   ! Do not check particle size distribution for gaseous component:
   check_psd = (.not. gasv)

   ! Read emission record and check whether parameters are within range:
   call ops_emis_read_annual1(fu_bron, icm, check_psd, presentcode, brn_version, VsDs_opt, nrec, numbron, building_present1, &
               mm, x, y, qob, qww, hbron, diameter, szopp, D_stack, V_stack, Ts_stack, emis_horizontal, building, ibtg, ibroncat, iland, idgr, end_of_file, error)
   IF (error%haserror) GOTO 9999

   IF (.NOT. end_of_file) THEN

      ! Copy valid (emission > 0) and selected sources to scratch file:
      IF (qob .GT. EPS_DELTA) THEN
    
         country_selected  = any((landsel(1:nlandsel) .eq. 0) .OR. (iland    .eq. landsel(1:nlandsel)))
         category_selected = any((catsel(1:ncatsel)   .eq. 0) .OR. (ibroncat .eq. catsel(1:ncatsel)))
       
         IF (country_selected .AND. category_selected) THEN
            WRITE (fu_scratch, 50) mm,x,y,qob,qww, hbron, diameter, szopp, D_stack, V_stack, Ts_stack, emis_horizontal, ibtg, ibroncat, iland, idgr, building%length, building%width, building%height, building%orientation
            numbron = numbron+1
         ENDIF
      ENDIF
   ENDIF
ENDDO
REWIND (fu_scratch)

RETURN

9999 CALL ErrorParam('numbron', numbron, error)
CALL ErrorParam('nrec', nrec, error)
CALL ErrorCall(ROUTINENAAM, error)

END SUBROUTINE ops_read_source
