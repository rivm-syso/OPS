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
module m_tst_ops_read_ctr

implicit none

contains

subroutine tst_ops_read_ctr_loss

! Test reading LOSS options
 
use no_pfunit
use m_ops_tdo_proc, only: Tdo_proc
use m_commonfile, only: fu_input, IOB_STDOUT
use m_error
use m_ops_read_ctr, only: ops_read_ctr_loss

implicit none

logical        :: idep                       ! if true -> chemistry, dry deposition, wet deposition switched on
logical        :: idep_ref                   ! reference value for idep (see input file tst_m_ops_read_ctr.in)
TYPE(Tdo_proc) :: do_proc                    ! option to switch on/off depletion for chemistry, dry deposition, wet deposition
TYPE(Terror)   :: error                      ! error structure
logical        :: loss_ref(4,19)             ! reference values for do_proc in the same order as the input file tst_m_ops_read_ctr.in
integer        :: i                          ! index of lines read from file
character(len = 100) :: msg                  ! message 
character(len = 200) :: errmsg_ref           ! reference error message 

! Open file:
open(fu_input,FILE = './level_1/resources/tst_m_ops_read_ctr1.in');
rewind(fu_input)

loss_ref(:,1)  = (/ .false., .false., .false., .false. /)
loss_ref(:,2)  = (/ .false., .false., .false., .true.  /)   
loss_ref(:,3)  = (/ .false., .false., .true. , .false. /)
loss_ref(:,4)  = (/ .false., .false., .true. , .true.  /)
loss_ref(:,5)  = (/ .false., .true. , .false., .false. /)
loss_ref(:,6)  = (/ .false., .true. , .false., .true.  /)
loss_ref(:,7)  = (/ .false., .true. , .true. , .false. /)
loss_ref(:,8)  = (/ .false., .true. , .true. , .true.  /)
loss_ref(:,9)  = (/ .true. , .false., .false., .false. /)
loss_ref(:,10) = (/ .true. , .false., .false., .true.  /)   
loss_ref(:,11) = (/ .true. , .false., .true. , .false. /)
loss_ref(:,12) = (/ .true. , .false., .true. , .true.  /)
loss_ref(:,13) = (/ .true. , .true. , .false., .false. /)
loss_ref(:,14) = (/ .true. , .true. , .false., .true.  /)
loss_ref(:,15) = (/ .true. , .true. , .true. , .false. /)
loss_ref(:,16) = (/ .true. , .true. , .true. , .true.  /)
loss_ref(:,17) = .false.
loss_ref(:,18) = .true. 
loss_ref(:,19) = .false.

! Template message
!      123456789012345678901234567890
msg = 'line    of input file; test '

! Loop over input file:
i = 0
do while (error%message .ne. 'Unexpected end-of-file')
   
   ! Reset error:
   error%haserror = .false.
   
   ! Read value of LOSS:
   call ops_read_ctr_loss(do_proc, idep, error)
   
   i = i + 1
   ! write(*,*) 'line ',i,' idep, do_proc: ', idep, '=== ', do_proc%chem, do_proc%depl_drydep, do_proc%depl_wetdep, do_proc%grad_drydep
   
   if (i .le. 19) then
      ! Reference valuwe of idep (see input file):
      if (i .eq. 17 .or. i .eq. 19) then
         idep_ref = .false.
      else
         idep_ref = .true.
      endif
      
      ! Check idep:
      write(msg(6:7),'(i2)') i
      write(msg(29:),'(a)') 'idep';          
      call assertEqual(idep_ref,idep,msg,__LINE__,__FILE__)

      ! Check do_proc:
      !write(*,*) '                   loss_ref     : ', idep, '=== ', loss_ref(:,i)
      write(msg(29:),'(a)') 'do_proc_chem    ';  
      call assertEqual(loss_ref(1,i),do_proc%chem,        msg,__LINE__,__FILE__)
      write(msg(29:),'(a)') 'do_proc_depl_drydep   ';
      call assertEqual(loss_ref(2,i),do_proc%depl_drydep, msg,__LINE__,__FILE__)
      write(msg(29:),'(a)') 'do_proc_depl_wetdep   ';
      call assertEqual(loss_ref(3,i),do_proc%depl_wetdep, msg,__LINE__,__FILE__)
      write(msg(29:),'(a)') 'do_proc_grad_drydep ';
      call assertEqual(loss_ref(4,i),do_proc%grad_drydep, msg,__LINE__,__FILE__)
      
   else
      ! Check error messages:
      if (i .eq. 20) then
         errmsg_ref = 'If all loss processes are switched off by idep, you cannot switch a specific process on; special OPS-users only'
      elseif (i .eq. 21) then
         errmsg_ref = 'Logical number should be 0 or 1'
      elseif (i .eq. 22) then
         errmsg_ref = 'Incorrect values for settings of do_proc in LOSS: only 0 or 1 allowed; special OPS-users only'
      endif
      if (error%message .ne. 'Unexpected end-of-file') then
         !write(*,*) '========== error ================'
         !CALL WriteError(IOB_STDOUT, error)
         call assertEqual(error%message,errmsg_ref,'check error message',__LINE__,__FILE__)
      endif
   endif
enddo

end subroutine tst_ops_read_ctr_loss

!---------------------------------------------------------------------------
subroutine tst_ops_read_ctr_roads

use no_pfunit
use m_ops_read_ctr
use m_commonfile, only: fu_input, IOB_STDOUT
use m_error

INTEGER      :: nemcat_road                ! number of road emission categories (for vdHout NO2/NOx ratio)
logical		:: road_chem
logical		:: road_disp
INTEGER      :: emcat_road(3)              ! list of road emission categories (for vdHout NO2/NOx ratio)
type(Terror) :: error

character(len = 100) :: str                ! string to write to input file

! Open file:
open(fu_input,FILE = './level_1/resources/tst_m_ops_read_ctr2.in');

! Check input file with ROADS option:
rewind(fu_input)
str = 'ROADS     3100 3200 3300'; write(fu_input,*) str; !write(*,'(/," === ",a," === ")') trim(str)
str = 'ROADSopt   0 0'; write(fu_input,*) str; !write(*,'(/," === ",a," === ")') trim(str)
rewind(fu_input)
error%haserror = .false.; emcat_road = -999.0; nemcat_road = -999 
call ops_read_ctr_roads(nemcat_road, road_chem, road_disp, emcat_road, error)
call assertEqual(3,nemcat_road,'assert nemcat_road = 3',__LINE__,__FILE__)
call assertEqual((/ 3100, 3200, 3300/),emcat_road,'assert emcat_road',__LINE__,__FILE__)

! Check input file with ROADS option - out of range:
rewind(fu_input)
str = 'ROADS     12345 3100 3200 3300'; write(fu_input,*) str; !write(*,'(/," === ",a," === ")') trim(str)
str = 'ROADSopt   0 0'; write(fu_input,*) str; !write(*,'(/," === ",a," === ")') trim(str)
rewind(fu_input)
error%haserror = .false.; emcat_road = -999.0; nemcat_road = -999 
call ops_read_ctr_roads(nemcat_road, road_chem, road_disp, emcat_road, error)
call assertEqual(.true.,error%haserror,'assert ROADS - out of range',__LINE__,__FILE__)

! Check input file with other input line than ROADS:
rewind(fu_input)
str = 'RECEPTYPE 2'; write(fu_input,*) str; !write(*,'(/," === ",a," === ")') trim(str)
rewind(fu_input)
error%haserror = .false.; emcat_road = -999.0; nemcat_road = -999 
call ops_read_ctr_roads(nemcat_road, road_chem, road_disp, emcat_road, error)
call assertEqual(0,nemcat_road,'assert nemcat_road = 0',__LINE__,__FILE__)
call assertEqual(.false.,error%haserror,'assert missing line with ROADS ',__LINE__,__FILE__)

! Check input file with ROADS 'empty':
rewind(fu_input)
str = 'ROADS'; write(fu_input,*) str; !write(*,'(/," === ",a," === ")') trim(str)
str = 'ROADSopt   0 0'; write(fu_input,*) str; !write(*,'(/," === ",a," === ")') trim(str)
rewind(fu_input)
error%haserror = .false.; emcat_road = -999.0; nemcat_road = -999 
call ops_read_ctr_roads(nemcat_road, road_chem, road_disp, emcat_road, error)
call assertEqual(0,nemcat_road,'assert ROADS "empty"',__LINE__,__FILE__)
call assertEqual(.false.,error%haserror,'assert empty value after ROADS ',__LINE__,__FILE__)

! Check input file with ROADS 0:
rewind(fu_input)
str = 'ROADS     0'; write(fu_input,*) str; !write(*,'(/," === ",a," === ")') trim(str)
str = 'ROADSopt   0 0'; write(fu_input,*) str; !write(*,'(/," === ",a," === ")') trim(str)
rewind(fu_input)
error%haserror = .false.; emcat_road = -999.0; nemcat_road = -999 
call ops_read_ctr_roads(nemcat_road, road_chem, road_disp, emcat_road, error)
call assertEqual(1,nemcat_road,'assert ROADS 0',__LINE__,__FILE__)
call assertEqual(0,emcat_road(1),'assert ROADS 0',__LINE__,__FILE__)

close(fu_input)

return

9999 CALL WriteError(IOB_STDOUT, error)

end subroutine tst_ops_read_ctr_roads


!---------------------------------------------------------------------------
subroutine tst_ops_read_ctr_year

use no_pfunit
use m_ops_read_ctr
use m_error
use m_commonfile, only: IOB_STDOUT, fu_input

implicit none

integer                     :: year                   ! year for chemical maps 
logical                     :: chem_meteo_prognosis   ! use meteo prognosis in chemistry maps
type(Terror)                :: error                  ! error structure

integer                     :: i                      ! index of record in input file
integer                     :: i_end                  ! index of last record in input file
integer                     :: year_ref(100)          ! reference values for year
logical                     :: cmp_ref(100)           ! reference values for chem_meteo_prognosis
character(len = 40)         :: msg                    ! message for assertEqual

! Open fu_input:
open(fu_input,file='./level_1/resources/tst_m_ops_read_ctr3.in')
rewind(fu_input)

! Write test records and fill reference data:
i = 0
write(fu_input,'(a)') 'YEAR';                             i = i + 1; year_ref(i) = -9999; cmp_ref(i) = .false.
write(fu_input,'(a)') 'YEAR 2018';                        i = i + 1; year_ref(i) = 2018; cmp_ref(i) = .false.
write(fu_input,'(a)') 'YEAR 2018 prognosis';              i = i + 1; year_ref(i) = 2018; cmp_ref(i) = .true.
write(fu_input,'(a)') 'YEAR 2018 prognosis ! comment';    i = i + 1; year_ref(i) = 2018; cmp_ref(i) = .true.
                                                                                     
! Erroneous input:                                                                   
write(fu_input,'(a)') 'YEARX 2018';                       i = i + 1; year_ref(i) = -9999; cmp_ref(i) = .false.
write(fu_input,'(a)') 'YEAR 2018 prognose';               i = i + 1; year_ref(i) = -9999; cmp_ref(i) = .false.
write(fu_input,'(a)') 'YEAR 2018 prognose ! prognosis';   i = i + 1; year_ref(i) = -9999; cmp_ref(i) = .false.
rewind(fu_input)
i_end = 1

! Read and check records:
do i = 1,i
   msg = ' === test record     === '
   write(msg(18:19),'(i2.2)') i
   !write(*,*) msg

   ! Call read routine:
   call ops_read_ctr_year(year,chem_meteo_prognosis,error)

   ! Assert results:
   if (year_ref(i) .ne. -9999) then
      if (error%haserror) goto 9999
      !write(*,*) 'year, chem_meteo_prognosis: ',year,chem_meteo_prognosis
      call assertEqual(year_ref(i), year,msg // ' year',__LINE__,__FILE__)
      call assertEqual(cmp_ref(i), chem_meteo_prognosis,msg // ' chem_meteo_prognosis',__LINE__,__FILE__)
   else
      ! Check whether erroneous input is trapped (reset haserror each time):
      call assertEqual(.true., error%haserror,msg // ' error',__LINE__,__FILE__)   
      error%haserror = .false. 
   endif
enddo

return

9999 call WriteError(IOB_STDOUT,error)

end subroutine tst_ops_read_ctr_year


end module m_tst_ops_read_ctr

program p_tst_ops_read_ctr

use no_pfunit
use m_tst_ops_read_ctr
implicit none

call tst_ops_read_ctr_loss
call tst_ops_read_ctr_roads
call tst_ops_read_ctr_year
call conclusion

end program p_tst_ops_read_ctr
