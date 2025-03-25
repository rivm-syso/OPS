    subroutine subroutine_name(val1, val2, message, lineno, filename) 
    typ, intent(in) :: val1(:,:), val2(:,:)
    character(len=*), intent(in), optional :: message
    integer,          intent(in), optional :: lineno
    character(len=*), intent(in), optional :: filename
    integer :: maxerr
    character(len=512) :: msg
    character(len=1), parameter :: eol = new_line('a')
    integer :: i, j
    integer :: locerr_i,locerr_j ! indices where maximal error occurs
    logical :: success
       success = compare_dimensions(shape(val1),shape(val2))
       if (.not. success .or. size(val1)==0) return

       locerr_i = 1
       locerr_j = 1
       do i = 1,size(val1,1)
          do j = 1,size(val1,2)
             if (.not. abs(val1(i,j)-val2(i,j)) < abs(val1(locerr_i,locerr_j)-val2(locerr_i,locerr_j))) then
                locerr_i = i
                locerr_j = j
             end if
          end do
       end do

       maxerr = val1(locerr_i,locerr_j) - val2(locerr_i,locerr_j)

       write(msg,'(20(a,i0))') &
                               'checking whether'//eol// &
                               '     maxabs(val1-val2) = abs(val1(',locerr_i,locerr_j,')-val2(',locerr_i,locerr_j,'))'//eol// &
                               '                       = abs(',val1(locerr_i,locerr_j),'-',val2(locerr_i,locerr_j),')'//eol// &
                               '                       = abs(',maxerr,') = 0'
       call print_result(message, lineno, filename, msg, success=abs(maxerr)==0)
    end subroutine subroutine_name
#   undef  subroutine_name   
#   undef  typ

