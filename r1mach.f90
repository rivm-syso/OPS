real function r1mach( k )

! return smallest difference with 1, from below (k=3) or from above (k=4)
! Gerard Cats, 25 August 2020

implicit none
real t  
real r(4)
save r
data r /0,0,0,0/
integer k
if ( r(k) > 0 ) then
   r1mach = r(k)
   return
endif
t = 1
if ( k == 3 ) then
   do while ( 1 - t < 1 )
      t = t/2
      r(3) = t
   enddo
   r(k) = r(k) * 2
else if ( k == 4 ) then
   do while ( 1 + t > 1 )
      t = t/2
      r(4) = t
   enddo
   r(k) = r(k) * 2
else
   print *, "r1mach called with a non-programmed argument, k = " , k
   error stop
endif
r1mach = r(k)
end
