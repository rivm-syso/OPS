    subroutine subroutine_name(val1, val2, message, lineno, filename) 
    typ, intent(in) :: val1(:), val2(:)
    character(len=*), intent(in), optional :: message
    integer,          intent(in), optional :: lineno
    character(len=*), intent(in), optional :: filename
    integer :: maxerr
    character(len=512) :: msg
    character(len=1), parameter :: eol = new_line('a')
    integer :: i, locerr
    logical :: success
       success = compare_dimensions(shape(val1),shape(val2))
       if (.not. success .or. size(val1)==0) return

       locerr = 1
       do i = 1,size(val1)
          if (.not. abs(val1(i)-val2(i)) < abs(val1(locerr)-val2(locerr))) then
             locerr = i
          end if
       end do

       maxerr = val1(locerr)-val2(locerr)

       write(msg,'(20(a,i0))') &
                               'checking whether'//eol// &
                               '     maxabs(val1-val2) = abs(val1(',locerr,')-val2(',locerr,'))'//eol// &
                               '                       = abs(',val1(locerr),'-',val2(locerr),')'//eol// &
                               '                       = abs(',maxerr,') = 0'
       call print_result(message, lineno, filename, msg, success=abs(maxerr)==0)
    end subroutine subroutine_name
#   undef  subroutine_name   
#   undef  typ

