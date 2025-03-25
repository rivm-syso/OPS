

    subroutine subroutine_name(val1, val2, message, lineno, filename)
    typ, intent(in) :: val1, val2
    character(len=*), intent(in), optional :: message
    integer,          intent(in), optional :: lineno
    character(len=*), intent(in), optional :: filename
    character(len=512) :: msg
       
       write(msg,my_fmt) printline
       call print_result(message, lineno, filename, msg, success = val1 compare val2)
    end subroutine subroutine_name

#   undef  subroutine_name   
#   undef  typ
#   undef  printline
#   undef  my_fmt
#   undef  compare


