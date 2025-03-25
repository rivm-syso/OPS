module m_tst_ops_get_arg
implicit none
contains
   subroutine tst_ops_get_arg()
      use no_pfunit
      use m_ops_get_arg, only: TKwargs, ops_get_arg
      use m_error

      integer   :: diag
      logical   :: subbron
      logical   :: domlu
      logical   :: varz
      logical   :: perc
      logical   :: mindist
      logical   :: maxdist
      logical   :: class_output
      character(len = 512) :: varin_file
      integer   :: nthreads
      logical   :: allow_sigz0_point_source   ! allow initial sigma for point sources

      type(TKwargs) :: kwargs
      type(TError) :: error

      call ops_get_arg(kwargs, error)
      !    diag, subbron, domlu, varz, perc, mindist, maxdist, class_output, &
      !    varin_file, nthreads, allow_sigz0_point_source, error)

      ! Note that arguments are specified in OpsLibFunctions.cmake/CreateUnitTests:
      ! set(tst_args  -i ./level_1/resources/tst_ctrl.in -nosub -domlu -varz -perc -mindist -maxdist -classoutput -varinfile tst.txt -v -nthreads 11 -allow_sigz0_point_source)
      ! Note: -r quits the subroutine directly!
      call assertFalse(kwargs%subbron                 ,"command line argument -nosub")
      call assertTrue (kwargs%domlu                   ,"command line argument -domlu")
      call assertTrue (kwargs%varz                    ,"command line argument -varz")
      call assertTrue (kwargs%perc                    ,"command line argument -perc")
      call assertTrue (kwargs%mindist                 ,"command line argument -mindist")
      call assertTrue (kwargs%maxdist                 ,"command line argument -maxdist")
      call assertTrue (kwargs%class_output            ,"command line argument -classoutput")
      call assertEqual(kwargs%varin_file,"tst.txt"    ,"command line argument -varinfile")
      ! call assertEqual(kwargs%diag,1                  ,"command line argument -r")
      call assertEqual(kwargs%diag,2                  ,"command line argument -v")
      call assertEqual(kwargs%nthreads,11             ,"command line argument -nthreads nthreads")
      call assertTrue (kwargs%allow_sigz0_point_source,"command line argument -allow_sigz0_point_source")
   end subroutine tst_ops_get_arg
end module m_tst_ops_get_arg

program p_tst_ops_get_arg
   use m_tst_ops_get_arg
   use no_pfunit
   implicit none
   call tst_ops_get_arg()
   call conclusion()
end program p_tst_ops_get_arg
