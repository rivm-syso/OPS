module m_random
Use iso_fortran_env, only : int64, real32
implicit none

    ! MMIX by Donald Knuth
    integer(kind=int64), parameter :: a=1664525, c=1013904223, m=int(2,int64)**32
    integer(kind=int64), save      :: seed = 0

contains

    function i64random()
       integer(kind=int64) :: i64random
       seed = mod(m+mod(a*seed + c, m),m)
       i64random = seed
    end function i64random

    function r32random()
       real(kind=real32) :: r32random
       r32random = i64random() / real(m,kind=real32)
    end function r32random

    subroutine set_seed(s)
      integer(int64), intent(in) :: s
      seed = s
    end subroutine set_seed

    function get_seed()
      integer(int64) :: get_seed
      get_seed = seed
    end function get_seed
     
end module m_random

