module m_test_diurnal_perturbation
   use m_commonconst_lt, only: NHRBLOCKS
   use m_diurnal_perturbation, only: scale_dverl
   use no_pfunit

   implicit none

   ! Create some simple variation distributions.
   integer, parameter :: DV = 4
   integer, parameter :: dverl_init(NHRBLOCKS, DV) = reshape((/ &
      10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, &
       0,  0,  0,  0,  0,  0, 10, 10, 10, 10, 10, 10, &
      10, 10, 10, 10, 10, 10, 30, 30, 30, 30, 30, 30, &
       1,  1,  1,  1,  1,  1, 21, 21, 21, 21, 21, 21  &
   /), shape(dverl_init))

contains
   subroutine test_no_change()
      ! Case 1: Set index to 0.5; all distributions must remain unchanged.

      real :: dverl(NHRBLOCKS, DV)
      integer :: i_hrblock, i_dv

      dverl = dverl_init

      call scale_dverl(dverl, 0.5, DV)

      do i_hrblock = 1, NHRBLOCKS
         do i_dv = 1, DV
            call assertEqual( &
               real(dverl(i_hrblock, i_dv)), &
               real(dverl_init(i_hrblock, i_dv)), &
               "Comparing unchanged dverl cells.", __LINE__, __FILE__ &
            )
         enddo
      enddo
   end subroutine

   subroutine test_scale_up()
      ! Case 2: Scale values up maximally. Uniform distributions and distributions
      ! that have zeroes in them, should remain unchanged.
      real, parameter :: dverl_ref(NHRBLOCKS, DV) = reshape((/ &
         10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, &
          0,  0,  0,  0,  0,  0, 10, 10, 10, 10, 10, 10, &
          0,  0,  0,  0,  0,  0, 40, 40, 40, 40, 40, 40, &
          0,  0,  0,  0,  0,  0, 22, 22, 22, 22, 22, 22  &
      /), shape(dverl_init))

      real :: dverl(NHRBLOCKS, DV)
      integer :: i_hrblock, i_dv

      dverl = dverl_init

      call scale_dverl(dverl, 1.0, dv)

      do i_hrblock = 1, NHRBLOCKS
         do i_dv = 1, DV
            call assertEqual( &
               real(dverl(i_hrblock, i_dv)), &
               real(dverl_ref(i_hrblock, i_dv)), &
               "Comparing amplified dverl cells.", __LINE__, __FILE__ &
            )
         enddo
      enddo
   end subroutine

   subroutine test_scale_down()
      ! Case 3: Scale values down maximally.
      ! Again, uniform distributions should remain unchanged.
      ! Since symmetry is forced onto the variability of each distribution,
      ! distributions that contain weights of zero are unchanging as well.
      real, parameter :: dverl_ref(NHRBLOCKS, DV) = reshape((/ &
         10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, &
          5,  5,  5,  5,  5,  5,  5,  5,  5,  5,  5,  5, &
         20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, &
         11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11  &
      /), shape(dverl_init))

      real :: dverl(NHRBLOCKS, DV)
      integer :: i_hrblock, i_dv

      dverl = dverl_init

      call scale_dverl(dverl, 0.0, dv)

      do i_hrblock = 1, NHRBLOCKS
         do i_dv = 1, DV
            call assertEqual( &
               real(dverl(i_hrblock, i_dv)), &
               real(dverl_ref(i_hrblock, i_dv)), &
               "Comparing amplified dverl cells.", __LINE__, __FILE__ &
            )
         enddo
      enddo
   end subroutine
end module

program p_test_diurnal_perturbation
   use m_test_diurnal_perturbation
   use no_pfunit

   implicit none

   call test_no_change()
   call test_scale_up()
   call test_scale_down()

   call conclusion()
end program
