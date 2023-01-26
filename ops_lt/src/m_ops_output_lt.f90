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

module m_ops_output_lt

! module with output subroutines specific for OPS-LT

! The parout variables are used to output maps of extra parameters e.g. Ra, Rb or other. In order to compute these maps, we define NSEK sources
! around each receptor (one for each wind sector) at a distance parout_disx. After summing all parameters with weighing factors the fraction
! of occurence of the wind direction/stability class, we get an averaged map for sources at the defined distance.
! Note that this can only run with an emission file with one point source, defining the characteristics of a 'typical source' 
! for which maps are created; concentration/deposition results of such a run should not be used. 
! Sub sources and sub receptors are switched off.
!
! Writing of parout variables is used for debugging purposes and is set by changing the value of parout_write in ops_main. Normally,
! writing parout variables is switched off. 

implicit none

contains

!-----------------------------------------------------------------------------
subroutine ops_parout_circle(ircp, xm, ym, parout_disx, subbron, nsbuf, nbron, numbron, bnr, bx, by, bdiam, bsterkte, & 
                             bwarmte, bhoogte, bsigmaz, bD_stack, bV_stack, bTs_stack, & 
                             bemis_horizontal, bbuilding, btgedr, bdegr, bqrv, bqtr, bcatnr, blandnr)

! Define NSEK sources in a circle around the current receptor point. These are used to make maps of the parout-variables.
! The NSEK sources are located in the middle of each wind direction sector.

use Binas, only: deg2rad 
USE m_commonconst_lt, only: LSBUF, NSEK
USE m_ops_building

! Input:
integer, intent(in) :: ircp                      ! index of receptor
real,    intent(in) :: xm(:)                     ! x-coordinates of receptors (m RDM)
real,    intent(in) :: ym(:)                     ! y-coordinates of receptors (m RDM)
real,    intent(in) :: parout_disx               ! source receptor distance used when computing parout variables

! Input/output:
logical, intent(inout)                           :: subbron                   ! use sub sources (and sub receptors) if needed
integer, intent(inout)                           :: nsbuf                     ! number of sources in buffer  
integer, intent(inout)                           :: bnr(LSBUF)                ! source number
integer, intent(inout)                           :: bx(LSBUF)                 ! x-coordinate of source [m RDM]
integer, intent(inout)                           :: by(LSBUF)                 ! y-coordinate of source [m RDM]
real,    intent(inout)                           :: bdiam(LSBUF)              ! diameter of area source [m]
real,    intent(inout)                           :: bsterkte(LSBUF)           ! emission strngth [g/s]
real,    intent(inout)                           :: bwarmte(LSBUF)            ! heat content [MW]
real,    intent(inout)                           :: bhoogte(LSBUF)            ! emission height [m]
real,    intent(inout)                           :: bsigmaz(LSBUF)            ! spread of emission height for area source [m]
real,    intent(inout)                           :: bD_stack(LSBUF)           ! diameter of the stack [m]
real,    intent(inout)                           :: bV_stack(LSBUF)           ! exit velocity of plume at stack tip [m/s]
real,    intent(inout)                           :: bTs_stack(LSBUF)          ! temperature of effluent from stack [K]            
logical,   intent(inout)                         :: bemis_horizontal(LSBUF)   ! horizontal outflow of emission
type(Tbuilding), intent(inout)                   :: bbuilding(LSBUF)          ! array with structures with building parameters
integer, intent(inout)                           :: btgedr(LSBUF)
integer, intent(inout)                           :: bdegr(LSBUF)                
real,    intent(inout)                           :: bqrv(LSBUF)                 
real,    intent(inout)                           :: bqtr(LSBUF)                 
integer, intent(inout)                           :: bcatnr(LSBUF)               
integer, intent(inout)                           :: blandnr(LSBUF)              
integer, intent(inout)                           :: nbron
integer, intent(inout)                           :: numbron

! Local
integer :: isek        ! index of wind sector
real    :: wd_sek      ! wind direction of middle of wind sector [degrees]
                          
! All source parameters (except coordinates) get the same value for the NSEK sources as defined in the emission file for 1 source
! and are the same for all receptors:
if (ircp .eq. 1) then
   ! Check whether the emission file contains only one source:
   if (nsbuf .ne. 1) then
      write(*,*) 'error for parout parameters (only for debugging purposes)'
      write(*,*) 'you can use parout-parameters only if there is one source on the emission file'
      stop
   endif

   ! No sub receptors, sub sources:
   subbron = .false.
   
   ! Set number of sources:
   nsbuf   = NSEK
   nbron   = NSEK
   numbron = NSEK
   
   ! Check for point source:
   if (bdiam(1) .ne. 0.0) then
      write(*,*) 'error for parout parameters (only for debugging purposes)'
      write(*,*) 'you can use parout-parameters only for a point source in the emission file'
      stop
   endif
   
   ! Set parameters for NSEK sources (here we assume that NSEK < LSBUF):
   bdiam(1:NSEK)     = bdiam(1)
   bsterkte(1:NSEK)  = bsterkte(1)
   bwarmte(1:NSEK)   = bwarmte(1)
   bhoogte(1:NSEK)   = bhoogte(1)
   bsigmaz(1:NSEK)   = bsigmaz(1)
   bD_stack(1:NSEK)  = bD_stack(1)
   bV_stack(1:NSEK)  = bV_stack(1)
   bTs_stack(1:NSEK) = bTs_stack(1)
   bemis_horizontal(1:NSEK) = bemis_horizontal(1)
   bbuilding(1:NSEK) = bbuilding(1)
   btgedr(1:NSEK)    = btgedr(1)
   bdegr(1:NSEK)     = bdegr(1)
   bqrv(1:NSEK)      = bqrv(1)
   bqtr(1:NSEK)      = bqtr(1)
   bcatnr(1:NSEK)    = bcatnr(1)
   blandnr(1:NSEK)   = blandnr(1)
endif

! Define coordinates of NSEK sources in a circle around the receptor:
do isek = 1,NSEK
   wd_sek = (isek - 1)*360.0/NSEK
   bnr(isek) = isek
   bx(isek)  = nint(xm(ircp) + parout_disx*sin(wd_sek*deg2rad))
   by(isek)  = nint(ym(ircp) + parout_disx*cos(wd_sek*deg2rad))
   !if (ircp .eq. 1) then
   !   write(*,'(a,3(1x,i8),3(1x,e14.7))') 'FS test 12 sources: ',isek, bx(isek),by(isek),xm(ircp),ym(ircp),wd_sek
   !endif
enddo

end subroutine ops_parout_circle

!-----------------------------------------------------------------------------
subroutine ops_parout_fill(nparout, ra_rcp_4, rb_rcp, rc_eff_rcp_4, percvk, &
                           parout_val, parout_name, parout_unit)

! Fill parout-arrays with values

! Input:
integer, intent(in)                              :: nparout        ! number of extra output parameters (besides concentration, deposition)
real, intent(in)                                 :: ra_rcp_4       ! aerodynamic resistance at receptor, 4 m height [s/m]
real, intent(in)                                 :: rb_rcp         ! boundary layer resistance at receptor [s/m]
real, intent(in)                                 :: rc_eff_rcp_4   ! effective canopy resistance at receptor, using Ra at 4 m height [s/m]
real, intent(in)                                 :: percvk         ! fraction of occurrence of (wind direction/stability/distance) class

! Input/output:
real,    intent(inout)                           :: parout_val(nparout)  ! values for extra output parameters for current receptor

! Output:
character(len= *), intent(out)                   :: parout_name(nparout)       ! names of extra output parameters                      
character(len= *), intent(out)                   :: parout_unit(nparout)       ! units of extra output parameters                      

! Create maps of parameters, for one specific meteo class. 
parout_val(1) = parout_val(1) + (1.0/ra_rcp_4)*percvk    ; parout_name(1) = '1/Ra,z=4m';     parout_unit(1) = 'm/s'
parout_val(2) = parout_val(2) + (1.0/rb_rcp)*percvk      ; parout_name(2) = '1/Rb';          parout_unit(2) = 'm/s'
parout_val(3) = parout_val(3) + (1.0/rc_eff_rcp_4)*percvk; parout_name(3) = '1/Rc_eff,z=4m'; parout_unit(3) = 'm/s'
parout_val(4) = parout_val(4) + (3.0)*percvk             ; parout_name(4) = 'check = 3';     parout_unit(4) = '-'

IF (nparout .ne. 4) THEN
   write(*,*) 'internal programming error; inconsistent value of NPAROUT = ',nparout
   stop
ENDIF

end subroutine ops_parout_fill

end module m_ops_output_lt
