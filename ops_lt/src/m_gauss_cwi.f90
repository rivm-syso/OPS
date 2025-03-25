module m_gauss_cwi

! gauss_cwi is copied from ops_support


implicit none

contains

!--------------------------------------------------------------------------------------------------------------------
subroutine gauss_cwi(q,hemis2,x,z,u,sigz,zi,vs,sedfac,c)

! Calculates CrossWind Integrated concentration [g/m2], using the Gaussian plume formulas.

use binas, only : twopi

! Input:
real,    intent(in)  :: q                  ! source strength of emission [g/s]
real,    intent(in)  :: hemis2             ! emission height (including plume rise and plume descent due to sedimentation) [m]
real,    intent(in)  :: x                  ! source-receptor distance [m]
real,    intent(in)  :: z                  ! receptor height [m]
real,    intent(in)  :: u                  ! wind speed [m/s]
real,    intent(in)  :: sigz               ! vertical dispersion length [m]
real,    intent(in)  :: zi                 ! mixing height [m]
real,    intent(in)  :: vs                 ! settling velocity [m/s]
real,    intent(in)  :: sedfac             ! sedimentation factor
                                           ! sedfac is based on the ratio of sigma_z and d_descent = distance the plume has descended
                                           ! and describes the relative importance of sedimentation vs. vertical dispersion.
! Output:
real,    intent(out) :: c                  ! crosswind integrated concentration at receptor distance x and height z, for a source with Q g/s source strength [g/m2]

! Local:
real    :: e                               ! help variable to sum the contributions of the reflections
real    :: rr                              ! = 2*sigz*sigz [m2]
integer :: i                               ! index of reflection at the surface
real    :: a                               ! 1 - sedfac
real    :: sqrt_twopi = sqrt(twopi)        ! sqrt(2 pi)

if (vs .eq. 0.0) then
   
   ! Settling velocity = 0 -> gases or fine particles:
   
   if (z .le. zi) then
      
      ! In mixing layer:
      if (sigz .lt. 1.6*zi) then
      
         ! Gaussian part of plume:
         ! c = q/(sqrt(2*pi)*u*sigz)*
         ! +     (exp(-((     z-hemis2)**2)/(2*sigz**2)) + exp(-((     z+hemis2)**2)/(2*sigz**2))+
         ! +      exp(-((2*zi-hemis2+z)**2)/(2*sigz**2)) + exp(-((2*zi+hemis2+z)**2)/(2*sigz**2))+
         ! +      exp(-((z-2*zi-hemis2)**2)/(2*sigz**2)) + exp(-((z-2*zi+hemis2)**2)/(2*sigz**2)))
         ! ops_st_conc_gauss1, v11.0.6
         e  = 0.0
         rr = 2*sigz*sigz ! [m2]

         ! Loop over reflections (surf = earth surface, mix = mixing height)
         ! ---------------------------------------------------------------
         ! | index | (virtual) | reflections   | (virtual) | reflections |
         ! |       |  stack 1  |               |  stack 2  |             |
         ! |-------|-----------|---------------|-----------}-------------|
         ! | -1    | -2*zi-h   | surf-mix-surf | -2*zi+h   | mix-surf    |
         ! |  0    |      -h   | surf          |       h   | -           |
         ! |  1    |  2*zi-h   | mix           |  2*zi+h   | surf-mix    |
         ! ---------------------------------------------------------------
         
         ! More reflections are possible by extending the range of i, e.g. do i = -2,2.
         ! The extra effort does not improve accuracy significantly.
         do i = -1,1
            e = e + exp(-((z+2*i*zi-hemis2)**2)/rr) + exp(-((z+2*i*zi+hemis2)**2)/rr) ! vertical dispersion evaluated at z = z_rcp [-]
            ! write(*,'(a,1x,i6,99(1x,e12.5))') 'gauss_cwi: ',i,z,zi,sigz,hemis2,exp(-((z+2*i*zi-hemis2)**2)/rr)+ exp(-((z+2*i*zi+hemis2)**2)/rr),e
         enddo
         
         ! Cross wind integrated concentration:
         c = q/(sqrt_twopi*u*sigz)*e ! [(g/s)/(m/s m)[ = [g/m2]

      else
        ! sigma_z large -> fully mixed; 
        ! Cross wind integrated concentration:
        c = q/(zi*u) ! [(g/s)/(m/s m)[ = [g/m2]
      endif
   else
      ! Above mixing layer:
      c = 0.0
   endif
else
   ! Coarse particles:

   ! sedfac = 1     -> sedimentation predominant       -> a = 0     -> neglect reflection term
   ! 0 < sedfac < 1 -> both processes important        -> 0 < a < 1 -> use fraction of reflection term
   ! sedfac = 0     -> vertical dispersion predominant -> a = 1     -> use full reflection term
   a = 1.0 - sedfac
   ! a = -a ! 2007
   ! sigz = sqrt(sigz*sigz+(vs*x/u/2.5)**2)

   ! Cross wind integrated concentration [g/m2]:
   c  = q/(sqrt_twopi*u*sigz)*(exp(-((z-hemis2)**2)/(2*sigz*sigz)) + a*exp(-((z+hemis2)**2)/(2*sigz*sigz)))
endif

end subroutine gauss_cwi

end module m_gauss_cwi
