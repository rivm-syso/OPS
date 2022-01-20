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

module m_ops_landuse

! module for land use, roughness length etc.

implicit none

contains

!----------------------------------------------------------------------------
subroutine ops_read_z0_landuse(isec, z0nlgrid, z0eurgrid, lugrid, error)

! Read maps for roughness length and land use (if needed)

use m_aps
use m_commonfile, only: z0file, z0eurnam, lufile

! Input:
logical, intent(in)                              :: isec                       ! secondary component present (in this case a land use map is needed)

! Output
TYPE (TApsGridInt), intent(out)                  :: z0nlgrid                   ! map of roughness lengths in NL [m]
TYPE (TApsGridInt), intent(out)                  :: lugrid                     ! grid with land use class information (1: dominant land use, 2:NLU+1: percentages land use class)
TYPE (TApsGridInt), intent(out)                  :: z0eurgrid                  ! map of roughness lengths in Europe [m]

! Input/output:
TYPE (TError), intent(inout)                     :: error                      ! error handling structure

! CONSTANTS
CHARACTER*512                                    :: ROUTINENAAM                ! 
PARAMETER    (ROUTINENAAM = 'ops_read_z0_landuse')

! write(*,'(a,a)') 'File for z0 NL   : ',trim(z0file)
! write(*,'(a,a)') 'File for z0 EU   : ',trim(z0eurnam)
! write(*,'(a,a)') 'File for land use: ',trim(lufile)

! Read rougness length for NL:
CALL ReadAps(z0file, 'z0 grid NL', z0nlgrid, error)
IF (error%haserror) GOTO 9999 ! GOTO error handling at end of program

! Read rougness length for Europe:
CALL ReadAps(z0eurnam, 'z0 grid Europe', z0eurgrid, error)
IF (error%haserror) GOTO 9999 ! GOTO error handling at end of program

! Read land use information.
! Note: for primary components other than acidifying components (which have secondary components),
! there is no relation between land use and deposition available.
IF (isec) THEN
  CALL ReadAps(lufile, 'land use grid', lugrid, error)
  IF (error%haserror) GOTO 9999 ! GOTO error handling at end of program
  
     !write(*,'(/,/,a,/,/)') 'FS TEST land use (urban) > 50% -> land use = 100% urban'
     !do iy = lugrid%gridheader%nrrow,1,-1
     !   do ix = 1,lugrid%gridheader%nrcol
     !      ! urban : ilu = 7
     !      if (lugrid%value(ix,iy,1+7) > 50) then
     !         lugrid%value(ix,iy,1)   = 7    ! dominant land use
     !         lugrid%value(ix,iy,:)   = 0
     !         lugrid%value(ix,iy,1+7) = 100
     !      endif
     !   enddo
     !   ! write(*,'(999(1x,i4))') lugrid%value(:,iy,1+7)
     !enddo
ENDIF

RETURN

9999 CALL ErrorCall(ROUTINENAAM, error)

end subroutine ops_read_z0_landuse

end module m_ops_landuse
