module m_tst_string

implicit none

contains

subroutine tst_string

use no_pfunit
use m_error, only: TError
use m_string
implicit none
   type(Terror) :: error
   character(len=100) :: string
   character(len=3) :: short_string

   call copystrpart('"Look up!", she said.', 2, 8, short_string, error)
   call assertTrue(error%haserror,'intentional error from copystrpart',__LINE__,__FILE__)
   error%haserror=.false. ! reset haserror
   call copystrpart('"Look up!", she said.', 2, 8, string, error)
   call assertFalse(error%haserror,'error from copystrpart',__LINE__,__FILE__)
   call assertEqual('Look up', string, 'Result from copystrpart',__LINE__,__FILE__)

   call copystring('"Look up!", she said.', short_string, error)
   call assertTrue(error%haserror,'intentional error from copystring',__LINE__,__FILE__)
   error%haserror=.false. ! reset haserror
   call copystring('"Look up!", she said.', string, error)
   call assertFalse(error%haserror,'error from copystring',__LINE__,__FILE__)
   call assertEqual('"Look up!", she said.', string, 'Result from copystring')

   short_string = 'a'
   string = '"Please"'
   call appendstrpart('"Look up!", she said.', 11, 21, short_string, error)
   call assertTrue(error%haserror,'intentional error from appendstrpart',__LINE__,__FILE__)
   error%haserror=.false. ! reset haserror
   call appendstrpart('"Look up!", she said.', 11, 21, string, error)
   call assertFalse(error%haserror,'error from appendstrpart',__LINE__,__FILE__)
   call assertEqual('"Please", she said.', string, 'Result from appendstrpart',__LINE__,__FILE__)

   short_string = 'ab'
   string = '"Please"'
   call appendstring(short_string,', she said.', error)
   call assertTrue(error%haserror,'intentional error from appendstring',__LINE__,__FILE__)
   error%haserror=.false. ! reset haserror
   call appendstring(string, ', she said.', error)
   call assertFalse(error%haserror,'error from appendstring',__LINE__,__FILE__)
   call assertEqual('"Please", she said.', string, 'Result from appendstring',__LINE__,__FILE__)

   short_string = 'ab'
   string = 'You can only have'
   call appendinteger(short_string, 12, error)
   call assertTrue(error%haserror,'intentional error from appendinteger',__LINE__,__FILE__)
   error%haserror = .false.
   call appendinteger(string, 12, error)
   call assertFalse(error%haserror,'error from appendinteger',__LINE__,__FILE__)
   call assertEqual('You can only have12', string, 'Result from appendinteger',__LINE__,__FILE__)


   short_string = 'ab'
   string = '"", she said.'
   call insertstrpart(2, "sometimes I take the long way home", 13, 29, short_string, error)
   call assertTrue(error%haserror,'intentional error from insertstrpart',__LINE__,__FILE__)
   error%haserror = .false.
   call insertstrpart(2, "sometimes I take the long way home", 13, 29, string, error)
   call assertFalse(error%haserror,'error from insertstrpart',__LINE__,__FILE__)
   call assertEqual('"take the long way", she said.', string, 'Result from insertstrpart',__LINE__,__FILE__)
   string = ', she said.'
   call insertstrpart(1, "sometimes I take the long way home", 13, 29, string, error)
   call assertFalse(error%haserror,'error from insertstrpart',__LINE__,__FILE__)
   call assertEqual('take the long way, she said.', string, 'Result from insertstrpart',__LINE__,__FILE__)
   string = ', she said.'
   call insertstrpart(1, "sometimes I take the long way home", 13, 12, string, error)
   call assertFalse(error%haserror,'error from insertstrpart',__LINE__,__FILE__)
   call assertEqual(', she said.', string, 'Result from insertstrpart',__LINE__,__FILE__)

   short_string = 'ab'
   string = '"", she said.'
   call insertstring("take the long way", 2, short_string, error)
   call assertTrue(error%haserror,'intentional error from insertstring',__LINE__,__FILE__)
   error%haserror = .false.
   call insertstring("take the long way", 2, string, error)
   call assertFalse(error%haserror,'error from insertstring',__LINE__,__FILE__)
   call assertEqual('"take the long way", she said.', string, 'Result from insertstring',__LINE__,__FILE__)

   short_string = 'ab'
   string = '"You can only have ", she said.'
   call insertinteger(12, 2, short_string, error)
   call assertTrue(error%haserror,'intentional error from insertinteger',__LINE__,__FILE__)
   error%haserror = .false.
   call insertinteger(12, 20, string, error)
   call assertFalse(error%haserror,'error from insertinteger',__LINE__,__FILE__)
   call assertEqual('"You can only have 12", she said.', string, 'Result from insertinteger',__LINE__,__FILE__)

   short_string = 'ab'
   string = 'contents will be erased'
   call mergestrpart("I don't like the rain any more", 14, 22, &
                     'When it is done, it will stop. Don''t you think?', 18, 30, short_string, error)
   call assertTrue(error%haserror,'intentional error from mergestrpart',__LINE__,__FILE__)
   error%haserror = .false.
   call mergestrpart("I don't like the rain any more", 14, 22, &
                     'When it is done, it will stop. Don''t you think?', -1, 6, string, error)
   call assertTrue(error%haserror,'intentional error from mergestrpart',__LINE__,__FILE__)
   error%haserror = .false.
   call mergestrpart("I don't like the rain any more", 14, 22, &
                     'When it is done, it will stop. Don''t you think?', 18, 100, string, error)
   call assertTrue(error%haserror,'intentional error from mergestrpart',__LINE__,__FILE__)
   error%haserror = .false.
   call mergestrpart("I don't like the rain any more", 14, 22, &
                     'When it is done, it will stop. Don''t you think?', 18, 12, string, error)
   call assertTrue(error%haserror,'intentional error from mergestrpart',__LINE__,__FILE__)
   error%haserror = .false.
   call mergestrpart("I don't like the rain any more", 14, 22, &
                     'When it is done, it will stop. Don''t you think?', 20, 30, string, error)
   call assertFalse(error%haserror,'error from mergestrpart',__LINE__,__FILE__)
   call assertEqual('the rain will stop.', string, 'Result from mergestrpart',__LINE__,__FILE__)

   short_string = 'ab'
   string = 'contents will be erased'
   call mergestring("the rain", ' will stop.', short_string, error)
   call assertTrue(error%haserror,'intentional error from mergestring',__LINE__,__FILE__)
   error%haserror = .false.
   call mergestring("the rain", ' will stop.', string, error)
   call assertFalse(error%haserror,'error from mergestring',__LINE__,__FILE__)
   call assertEqual('the rain will stop.', string, 'Result from mergestring',__LINE__,__FILE__)

   call assertEqual(6, string_count_words("words don't come easy to me."),'from string_count_words',__LINE__,__FILE__)

end subroutine tst_string

!-----------------------------------------------------
subroutine tst_LoCase

USE no_pfunit
use m_string, only: LoCase, UpCase

IMPLICIT NONE

! Test conversion to lower/upper case:
call assertEqual('abcdefghijklmnopqrstuvwxyz' ,LoCase('ABCDEFGHIJKLMNOPQRSTUVWXYZ' ),'test LoCase 1',__LINE__,__FILE__)
call assertEqual('abcd!',LoCase('ABCD!'),'test LoCase 2',__LINE__,__FILE__)
call assertEqual('abcd' ,LoCase('AbCd' ),'test LoCase 3',__LINE__,__FILE__)
                                                        
call assertEqual('ABCDEFGHIJKLMNOPQRSTUVWXYZ' ,UpCase('abcdefghijklmnopqrstuvwxyz' ),'test UpCase 1',__LINE__,__FILE__)
call assertEqual('ABCD!',UpCase('abcd!'),'test UpCase 2',__LINE__,__FILE__)
call assertEqual('ABCD' ,UpCase('AbCd' ),'test UpCase 3',__LINE__,__FILE__)

end subroutine tst_LoCase

end module m_tst_string

!-----------------------------------------------------
program p_tst_string

use no_pfunit
use m_tst_string

implicit none

call tst_string
call tst_LoCase
call conclusion

end program p_tst_string
