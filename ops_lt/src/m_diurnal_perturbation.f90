!
! The function `scale_dverl` is used to randomly scale a diurnal distribution
! in m_ops_read_emis::ops_read_emis for use in sensitivity analyses.
!
module m_diurnal_perturbation
   implicit none

contains
   subroutine scale_dverl(dverl, scale_index, dv)
      ! Scale a diurnal distribution without losing normalisation.
      ! A scaling index (or q-value) is transformed to a scaling factor using
      ! the quantile function of the scaling probability distribution (see
      ! comment below). A `scale_index` of 0.5 results in no change to the
      ! distribution.
      use m_commonconst_lt, only: NHRBLOCKS

      integer, intent(in) :: dv  ! Number of diurnal distributions.
      real, intent(inout) :: dverl(NHRBLOCKS, dv)  ! Array containing diurnal
      ! distribution weights.
      real, intent(in) :: scale_index  ! Scale index between 0 and 1, where 0
      ! pulls the distribution closer to a uniform distribution and 1 pulls
      ! the distribution's extremes (peaks and valleys) apart. Can also be
      ! interpreted as a percentile of the scale's distribution.

      integer :: i_distr  ! Distribution index.
      real :: pivot  ! Mean distribution value around which weights are scaled.
      real :: minimum_weight  ! Smallest weight in distribution.
      real :: upper_bound  ! Upper scaling limit.
      real :: scale_factor  ! Actual applied scaled.

      do i_distr = 1,dv
         pivot = sum(dverl(:,i_distr)) / NHRBLOCKS
         minimum_weight = minval(dverl(:,i_distr))

         ! If the mean equals the minimum, scaling is not possible, so this
         ! distribution is skipped.
         if (pivot == minimum_weight) then
            cycle
         endif

         ! The upper scaling bound is de maximum value by which the diurnal
         ! distribution can be scaled without ending up with negative values.
         ! At this scale, the smallest weight becomes 0.
         upper_bound = pivot / (pivot - minimum_weight)

         ! The scale factor distribution is defined as a piecewise linear function
         ! where the probability falls of as the scale factor diverges from 1
         ! and with the median fixed at 1.
         !
         ! p(x) ^       *
         !      |       :\
         !    1 +      .o \
         !      |    ." :  \
         !      |  ."   :   \
         !      |."     :    \
         !      +――――――――――――――> x
         !      0       1     φ

         ! Evaluate the quantile function of the distribution described above.
         if (scale_index < 0.5) then
            ! The outcome lies below the median (scaling down).
            scale_factor = sqrt(2 * scale_index)
         else
            ! The outcome lies above the median (scaling up).
            scale_factor = upper_bound - sqrt(2 * (1 - upper_bound) ** 2 * (1 - scale_index))
         endif

         ! Scaling the distribution around a 'pivot' is equivalent to a linear
         ! interpolation between the original distribution and its mean value.
         ! Scaling up is then equivalent to extrapolating.
         dverl(:,i_distr) = max(0., scale_factor * dverl(:,i_distr) + (1.0 - scale_factor) * pivot)
      enddo
   end subroutine
end module
