program test_error
use no_pfunit
use m_commonfile, only: IOB_STDOUT
use m_error
implicit none
   TYPE (TError)               :: error
   TYPE (TErrorCall),  POINTER :: caller
   TYPE (TErrorParam), POINTER :: param
   integer                     :: unit
   character(len=100) :: pname, pvalue
   if (.false.) then
      open(newunit = unit, file = 'uit')
   else 
      unit = IOB_STDOUT
   end if

   call set_error1('An error message', error)
   call set_error2('Another error message', 'extra info', error)
   call write_error(unit, error) 
   call ErrorCall('deeper into the calculation', error)
   call write_error(unit, error) 
   error%haserror = .false.
   call set_error2('Another error message', 'extra info', error)
   call ErrorCall('main calculation', error)
   call ErrorCall('deeper into the calculation', error)
   call ErrorCall('deeper yet', error)
   call ErrorParam('error int array', (/1, 2, 3 /), error)
   call ErrorParam('string array', (/ 'een ', 'twee', '    ', 'vier'/), error)
   call ErrorParam('empty string', ' ', error)
   call ErrorParam(' ', ' ', error)
   call ErrorParam(' ', 'waarde', error)
   call ErrorParam('error int ', 12, error)
   call ErrorParam('error real ', 12.3456, error)
   call ErrorParam('error logical ', .false., error)
   call ErrorParam('error logical ', .true., error)
   call ErrorParam('error logical ', 'met', .false., error)
   call ErrorParam('error logical ', '  zonder', .true., error)
   call ErrorParam('error logical ', ' ', .true., error)
   call ErrorParam('error real array ', (/ 12.3456, 8.45, 98.76 /) , error)
   call write_error(unit, error) 
   do
      caller=>error%callroutines 
      call assertTrue(associated(caller),'There should be call-tree info')
      if (.not. associated(caller)) exit
      call assertEqual(caller%routinename,'main calculation', 'call-stack starts with "main calculation"')

      caller=>caller%nextcall
      call assertTrue(associated(caller),'The call-tree should be at least 2 long')
      if (.not. associated(caller)) exit
      call assertEqual(caller%routinename,"deeper into the calculation", 'call-stack level 2 is "deeper into the calculation"')

      caller=>caller%nextcall
      call assertTrue(associated(caller),'The call-tree should be at least 3 long')
      if (.not. associated(caller)) exit
      call assertEqual(caller%routinename,"deeper yet", 'call-stack level 3 is "deeper yet"')

      caller=>caller%nextcall
      call assertFalse(associated(caller),'The call-tree should be at least 3 long')

      param => error%firstparam
      pname = "error int array"
      pvalue = "1 2 3"
      call assertTrue(associated(param),'There should be parameter info') 
      if (.not. associated(param)) exit
      call assertEqual(param%paramname,pname, 'param 1 is called "'//trim(pname)//'"')
      call assertEqual(param%stringvalue,pvalue, 'param 1 is "'//trim(pvalue)//'"')

      param => param%nextparam
      pname = "string array" 
      pvalue = " / een / twee / / vier"
      call assertTrue(associated(param),'There should be at least 2 parameters')
      if (.not. associated(param)) exit
      call assertEqual(param%paramname,pname, 'param 2 is called "'//trim(pname)//'"')
      call assertEqual(param%stringvalue,pvalue, 'param 2 is "'//trim(pvalue)//'"')

      param => param%nextparam
      pname = "empty string" 
      pvalue = " "
      call assertTrue(associated(param),'There should be at least 3 parameters')
      if (.not. associated(param)) exit
      call assertEqual(param%paramname,pname, 'param 3 is called "'//trim(pname)//'"')
      call assertEqual(param%stringvalue,pvalue, 'param 3 is "'//trim(pvalue)//'"')

      param => param%nextparam
      pname = " " 
      pvalue = " "
      call assertTrue(associated(param),'There should be at least 4 parameters')
      if (.not. associated(param)) exit
      call assertEqual(param%paramname,pname, 'param 4 is called "'//trim(pname)//'"')
      call assertEqual(param%stringvalue,pvalue, 'param 4 is "'//trim(pvalue)//'"')

      param => param%nextparam
      pname = " " 
      pvalue = "waarde"
      call assertTrue(associated(param),'There should be at least 5 parameters')
      if (.not. associated(param)) exit
      call assertEqual(param%paramname,pname, 'param 5 is called "'//trim(pname)//'"')
      call assertEqual(param%stringvalue,pvalue, 'param 5 is "'//trim(pvalue)//'"')

      param => param%nextparam
      pname = "error int" 
      pvalue = "12"
      call assertTrue(associated(param),'There should be at least 6 parameters')
      if (.not. associated(param)) exit
      call assertEqual(param%paramname,pname, 'param 6 is called "'//trim(pname)//'"')
      call assertEqual(param%stringvalue,pvalue, 'param 6 is "'//trim(pvalue)//'"')

      param => param%nextparam
      pname = "error real" 
      pvalue = "1.2346E+01"
      call assertTrue(associated(param),'There should be at least 7 parameters')
      if (.not. associated(param)) exit
      call assertEqual(param%paramname,pname, 'param 7 is called "'//trim(pname)//'"')
      call assertEqual(param%stringvalue,pvalue, 'param 7 is "'//trim(pvalue)//'"')

      param => param%nextparam
      pname = "error logical" 
      pvalue = ".FALSE."
      call assertTrue(associated(param),'There should be at least 8 parameters')
      if (.not. associated(param)) exit
      call assertEqual(param%paramname,pname, 'param 8 is called "'//trim(pname)//'"')
      call assertEqual(param%stringvalue,pvalue, 'param 8 is "'//trim(pvalue)//'"')

      param => param%nextparam
      pname = "error logical" 
      pvalue = ".TRUE."
      call assertTrue(associated(param),'There should be at least 9 parameters')
      if (.not. associated(param)) exit
      call assertEqual(param%paramname,pname, 'param 9 is called "'//trim(pname)//'"')
      call assertEqual(param%stringvalue,pvalue, 'param 9 is "'//trim(pvalue)//'"')

      param => param%nextparam
      pname = "error logical" 
      pvalue = "met"
      call assertTrue(associated(param),'There should be at least 10 parameters')
      if (.not. associated(param)) exit
      call assertEqual(param%paramname,pname, 'param 10 is called "'//trim(pname)//'"')
      call assertEqual(param%stringvalue,pvalue, 'param 10 is "'//trim(pvalue)//'"')

      param => param%nextparam
      pname = "error logical" 
      pvalue = "zonder"
      call assertTrue(associated(param),'There should be at least 11 parametersa')
      if (.not. associated(param)) exit
      call assertEqual(param%paramname,pname, 'param 11 is called "'//trim(pname)//'"')
      call assertEqual(param%stringvalue,pvalue, 'param 11 is "'//trim(pvalue)//'"')

      param => param%nextparam
      pname = "error logical" 
      pvalue = " "
      call assertTrue(associated(param),'There should be at least 12 parametersa')
      if (.not. associated(param)) exit
      call assertEqual(param%paramname,pname, 'param 12 is called "'//trim(pname)//'"')
      call assertEqual(param%stringvalue,pvalue, 'param 12 is "'//trim(pvalue)//'"')

      param => param%nextparam
      pname = "error real array" 
      pvalue = "1.2346E+01  8.4500E+00  9.8760E+01"
      call assertTrue(associated(param),'There should be at least 13 parametersa')
      if (.not. associated(param)) exit
      call assertEqual(param%paramname,pname, 'param 13 is called "'//trim(pname)//'"')
      call assertEqual(param%stringvalue,pvalue, 'param 13 is "'//trim(pvalue)//'"')

      param => param%nextparam
      call assertFalse(associated(param),'There should be no more than 13 parametersa')
      exit
   end do

   call conclusion()
end program test_error
