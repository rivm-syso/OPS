FUNCTION INUM(s)

IMPLICIT NONE
INTEGER :: INUM
character*(*) :: s
integer i
read(s,*) i
inum = i
END FUNCTION INUM
