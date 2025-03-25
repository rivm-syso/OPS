
module m_test_test_log_call
implicit none
contains
   subroutine test_test_log_call()
   use no_pfunit
       integer         :: log_call_stat, i1, i2, i3, i4, i5, fid
       real, parameter :: tol = 1e-5
        integer, allocatable :: input_int1(:)
        integer, allocatable :: input_int2(:,:)
        integer, allocatable :: output_int1(:)
        integer, allocatable :: ref_output_int1(:)
        integer, allocatable :: output_int2(:,:)
        integer, allocatable :: ref_output_int2(:,:)
        integer, allocatable :: in_inout_int1(:)
        integer, allocatable :: in_inout_int2(:,:)
        real, allocatable :: in_inout_real1(:)
        real, allocatable :: in_inout_real2(:,:)
        real, allocatable :: in_inout_real3(:,:,:)
        real, allocatable :: in_inout_real4(:,:,:,:)
        real, allocatable :: in_inout_real5(:,:,:,:,:)
        real, allocatable :: output_real1(:)
        real, allocatable :: ref_output_real1(:)
        real, allocatable :: output_real2(:,:)
        real, allocatable :: ref_output_real2(:,:)
        real, allocatable :: output_real3(:,:,:)
        real, allocatable :: ref_output_real3(:,:,:)
        real, allocatable :: output_real4(:,:,:,:)
        real, allocatable :: ref_output_real4(:,:,:,:)
        real, allocatable :: output_real5(:,:,:,:,:)
        real, allocatable :: ref_output_real5(:,:,:,:,:)
        real, allocatable :: input_real1(:)
        real, allocatable :: input_real2(:,:)
        real, allocatable :: input_real3(:,:,:)
        real, allocatable :: input_real4(:,:,:,:)
        real, allocatable :: input_real5(:,:,:,:,:)
        logical :: out_bool1(3)
        real :: ref_out_bool1(3) = (/ &
           .true., .true., .true./)
        real :: in_bool1(3) = (/ &
           .true., .true., .true./)
        real :: in_inout_bool1(3) = (/ &
           .true., .true., .true./)
        character(len=6), allocatable :: out_string1(:)
        character(len=6), allocatable :: ref_out_string1(:)
        character(len=6), allocatable :: in_string1(:)
        character(len=6), allocatable :: in_inout_string1(:)
        allocate(input_int1(1010), stat=log_call_stat)
        call AssertEqual(log_call_stat,0,"allocation of 'input_int1'", __LINE__, __FILE__)
        open(newunit=fid, file="./level_1/resources/integer1d_pid_1.int", status="old", action="read", iostat=log_call_stat)
        call AssertEqual(log_call_stat,0,"opening 'integer1d_pid_1.int' for reading variable input_int1", __LINE__, __FILE__)
        do i1 = 1,1010
           read(fid,*) input_int1(i1)
        end do
        close(unit=fid, iostat=log_call_stat)
        call AssertEqual(log_call_stat,0,"closing 'integer1d_pid_1.int' for reading variable input_int1", __LINE__, __FILE__)
        allocate(input_int2(101,10), stat=log_call_stat)
        call AssertEqual(log_call_stat,0,"allocation of 'input_int2'", __LINE__, __FILE__)
        open(newunit=fid, file="./level_1/resources/int2d_pid_1.real", status="old", action="read", iostat=log_call_stat)
        call AssertEqual(log_call_stat,0,"opening 'int2d_pid_1.real' for reading variable input_int2", __LINE__, __FILE__)
        do i2 = 1,10
           do i1 = 1,101
              read(fid,*) input_int2(i1, i2)
           end do
        end do
        close(unit=fid, iostat=log_call_stat)
        call AssertEqual(log_call_stat,0,"closing 'int2d_pid_1.real' for reading variable input_int2", __LINE__, __FILE__)
        allocate(ref_output_int1(1010), stat=log_call_stat)
        call AssertEqual(log_call_stat,0,"allocation of 'ref_output_int1'", __LINE__, __FILE__)
        allocate(output_int1(1010), stat=log_call_stat)
        call AssertEqual(log_call_stat,0,"allocation of 'output_int1'", __LINE__, __FILE__)
        open(newunit=fid, file="./level_1/resources/integer1d_pid_2.int", status="old", action="read", iostat=log_call_stat)
        call AssertEqual(log_call_stat,0,"opening 'integer1d_pid_2.int' for reading variable output_int1", __LINE__, __FILE__)
        do i1 = 1,1010
           read(fid,*) ref_output_int1(i1)
        end do
        close(unit=fid, iostat=log_call_stat)
        call AssertEqual(log_call_stat,0,"closing 'integer1d_pid_2.int' for reading variable output_int1", __LINE__, __FILE__)
        allocate(ref_output_int2(101,10), stat=log_call_stat)
        call AssertEqual(log_call_stat,0,"allocation of 'ref_output_int2'", __LINE__, __FILE__)
        allocate(output_int2(101,10), stat=log_call_stat)
        call AssertEqual(log_call_stat,0,"allocation of 'output_int2'", __LINE__, __FILE__)
        open(newunit=fid, file="./level_1/resources/int2d_pid_2.real", status="old", action="read", iostat=log_call_stat)
        call AssertEqual(log_call_stat,0,"opening 'int2d_pid_2.real' for reading variable output_int2", __LINE__, __FILE__)
        do i2 = 1,10
           do i1 = 1,101
              read(fid,*) ref_output_int2(i1, i2)
           end do
        end do
        close(unit=fid, iostat=log_call_stat)
        call AssertEqual(log_call_stat,0,"closing 'int2d_pid_2.real' for reading variable output_int2", __LINE__, __FILE__)
        allocate(inout_int1(1010), stat=log_call_stat)
        call AssertEqual(log_call_stat,0,"allocation of 'inout_int1'", __LINE__, __FILE__)
        open(newunit=fid, file="./level_1/resources/integer1d_pid_3.int", status="old", action="read", iostat=log_call_stat)
        call AssertEqual(log_call_stat,0,"opening 'integer1d_pid_3.int' for reading variable inout_int1", __LINE__, __FILE__)
        do i1 = 1,1010
           read(fid,*) in_inout_int1(i1)
        end do
        close(unit=fid, iostat=log_call_stat)
        call AssertEqual(log_call_stat,0,"closing 'integer1d_pid_3.int' for reading variable inout_int1", __LINE__, __FILE__)
        allocate(inout_int2(101,10), stat=log_call_stat)
        call AssertEqual(log_call_stat,0,"allocation of 'inout_int2'", __LINE__, __FILE__)
        open(newunit=fid, file="./level_1/resources/int2d_pid_3.real", status="old", action="read", iostat=log_call_stat)
        call AssertEqual(log_call_stat,0,"opening 'int2d_pid_3.real' for reading variable inout_int2", __LINE__, __FILE__)
        do i2 = 1,10
           do i1 = 1,101
              read(fid,*) in_inout_int2(i1, i2)
           end do
        end do
        close(unit=fid, iostat=log_call_stat)
        call AssertEqual(log_call_stat,0,"closing 'int2d_pid_3.real' for reading variable inout_int2", __LINE__, __FILE__)
        allocate(inout_real1(1008), stat=log_call_stat)
        call AssertEqual(log_call_stat,0,"allocation of 'inout_real1'", __LINE__, __FILE__)
        open(newunit=fid, file="./level_1/resources/real1d_pid_1.real", status="old", action="read", iostat=log_call_stat)
        call AssertEqual(log_call_stat,0,"opening 'real1d_pid_1.real' for reading variable inout_real1", __LINE__, __FILE__)
        do i1 = 1,1008
           read(fid,*) in_inout_real1(i1)
        end do
        close(unit=fid, iostat=log_call_stat)
        call AssertEqual(log_call_stat,0,"closing 'real1d_pid_1.real' for reading variable inout_real1", __LINE__, __FILE__)
        allocate(inout_real2(11,100), stat=log_call_stat)
        call AssertEqual(log_call_stat,0,"allocation of 'inout_real2'", __LINE__, __FILE__)
        open(newunit=fid, file="./level_1/resources/real2d_pid_1.real", status="old", action="read", iostat=log_call_stat)
        call AssertEqual(log_call_stat,0,"opening 'real2d_pid_1.real' for reading variable inout_real2", __LINE__, __FILE__)
        do i2 = 1,100
           do i1 = 1,11
              read(fid,*) in_inout_real2(i1, i2)
           end do
        end do
        close(unit=fid, iostat=log_call_stat)
        call AssertEqual(log_call_stat,0,"closing 'real2d_pid_1.real' for reading variable inout_real2", __LINE__, __FILE__)
        allocate(inout_real3(20,20,3), stat=log_call_stat)
        call AssertEqual(log_call_stat,0,"allocation of 'inout_real3'", __LINE__, __FILE__)
        open(newunit=fid, file="./level_1/resources/real3d_pid_1.real", status="old", action="read", iostat=log_call_stat)
        call AssertEqual(log_call_stat,0,"opening 'real3d_pid_1.real' for reading variable inout_real3", __LINE__, __FILE__)
        do i3 = 1,3
           do i2 = 1,20
              do i1 = 1,20
                 read(fid,*) in_inout_real3(i1, i2, i3)
              end do
           end do
        end do
        close(unit=fid, iostat=log_call_stat)
        call AssertEqual(log_call_stat,0,"closing 'real3d_pid_1.real' for reading variable inout_real3", __LINE__, __FILE__)
        allocate(inout_real4(5,20,3,4), stat=log_call_stat)
        call AssertEqual(log_call_stat,0,"allocation of 'inout_real4'", __LINE__, __FILE__)
        open(newunit=fid, file="./level_1/resources/real4d_pid_1.real", status="old", action="read", iostat=log_call_stat)
        call AssertEqual(log_call_stat,0,"opening 'real4d_pid_1.real' for reading variable inout_real4", __LINE__, __FILE__)
        do i4 = 1,4
           do i3 = 1,3
              do i2 = 1,20
                 do i1 = 1,5
                    read(fid,*) in_inout_real4(i1, i2, i3, i4)
                 end do
              end do
           end do
        end do
        close(unit=fid, iostat=log_call_stat)
        call AssertEqual(log_call_stat,0,"closing 'real4d_pid_1.real' for reading variable inout_real4", __LINE__, __FILE__)
        allocate(inout_real5(1,20,3,4,5), stat=log_call_stat)
        call AssertEqual(log_call_stat,0,"allocation of 'inout_real5'", __LINE__, __FILE__)
        open(newunit=fid, file="./level_1/resources/real5d_pid_1.real", status="old", action="read", iostat=log_call_stat)
        call AssertEqual(log_call_stat,0,"opening 'real5d_pid_1.real' for reading variable inout_real5", __LINE__, __FILE__)
        do i5 = 1,5
           do i4 = 1,4
              do i3 = 1,3
                 do i2 = 1,20
                    do i1 = 1,1
                       read(fid,*) in_inout_real5(i1, i2, i3, i4, i5)
                    end do
                 end do
              end do
           end do
        end do
        close(unit=fid, iostat=log_call_stat)
        call AssertEqual(log_call_stat,0,"closing 'real5d_pid_1.real' for reading variable inout_real5", __LINE__, __FILE__)
        allocate(ref_output_real1(1008), stat=log_call_stat)
        call AssertEqual(log_call_stat,0,"allocation of 'ref_output_real1'", __LINE__, __FILE__)
        allocate(output_real1(1008), stat=log_call_stat)
        call AssertEqual(log_call_stat,0,"allocation of 'output_real1'", __LINE__, __FILE__)
        open(newunit=fid, file="./level_1/resources/real1d_pid_2.real", status="old", action="read", iostat=log_call_stat)
        call AssertEqual(log_call_stat,0,"opening 'real1d_pid_2.real' for reading variable output_real1", __LINE__, __FILE__)
        do i1 = 1,1008
           read(fid,*) ref_output_real1(i1)
        end do
        close(unit=fid, iostat=log_call_stat)
        call AssertEqual(log_call_stat,0,"closing 'real1d_pid_2.real' for reading variable output_real1", __LINE__, __FILE__)
        allocate(ref_output_real2(11,100), stat=log_call_stat)
        call AssertEqual(log_call_stat,0,"allocation of 'ref_output_real2'", __LINE__, __FILE__)
        allocate(output_real2(11,100), stat=log_call_stat)
        call AssertEqual(log_call_stat,0,"allocation of 'output_real2'", __LINE__, __FILE__)
        open(newunit=fid, file="./level_1/resources/real2d_pid_2.real", status="old", action="read", iostat=log_call_stat)
        call AssertEqual(log_call_stat,0,"opening 'real2d_pid_2.real' for reading variable output_real2", __LINE__, __FILE__)
        do i2 = 1,100
           do i1 = 1,11
              read(fid,*) ref_output_real2(i1, i2)
           end do
        end do
        close(unit=fid, iostat=log_call_stat)
        call AssertEqual(log_call_stat,0,"closing 'real2d_pid_2.real' for reading variable output_real2", __LINE__, __FILE__)
        allocate(ref_output_real3(20,20,3), stat=log_call_stat)
        call AssertEqual(log_call_stat,0,"allocation of 'ref_output_real3'", __LINE__, __FILE__)
        allocate(output_real3(20,20,3), stat=log_call_stat)
        call AssertEqual(log_call_stat,0,"allocation of 'output_real3'", __LINE__, __FILE__)
        open(newunit=fid, file="./level_1/resources/real3d_pid_2.real", status="old", action="read", iostat=log_call_stat)
        call AssertEqual(log_call_stat,0,"opening 'real3d_pid_2.real' for reading variable output_real3", __LINE__, __FILE__)
        do i3 = 1,3
           do i2 = 1,20
              do i1 = 1,20
                 read(fid,*) ref_output_real3(i1, i2, i3)
              end do
           end do
        end do
        close(unit=fid, iostat=log_call_stat)
        call AssertEqual(log_call_stat,0,"closing 'real3d_pid_2.real' for reading variable output_real3", __LINE__, __FILE__)
        allocate(ref_output_real4(5,20,3,4), stat=log_call_stat)
        call AssertEqual(log_call_stat,0,"allocation of 'ref_output_real4'", __LINE__, __FILE__)
        allocate(output_real4(5,20,3,4), stat=log_call_stat)
        call AssertEqual(log_call_stat,0,"allocation of 'output_real4'", __LINE__, __FILE__)
        open(newunit=fid, file="./level_1/resources/real4d_pid_2.real", status="old", action="read", iostat=log_call_stat)
        call AssertEqual(log_call_stat,0,"opening 'real4d_pid_2.real' for reading variable output_real4", __LINE__, __FILE__)
        do i4 = 1,4
           do i3 = 1,3
              do i2 = 1,20
                 do i1 = 1,5
                    read(fid,*) ref_output_real4(i1, i2, i3, i4)
                 end do
              end do
           end do
        end do
        close(unit=fid, iostat=log_call_stat)
        call AssertEqual(log_call_stat,0,"closing 'real4d_pid_2.real' for reading variable output_real4", __LINE__, __FILE__)
        allocate(ref_output_real5(1,20,3,4,5), stat=log_call_stat)
        call AssertEqual(log_call_stat,0,"allocation of 'ref_output_real5'", __LINE__, __FILE__)
        allocate(output_real5(1,20,3,4,5), stat=log_call_stat)
        call AssertEqual(log_call_stat,0,"allocation of 'output_real5'", __LINE__, __FILE__)
        open(newunit=fid, file="./level_1/resources/real5d_pid_2.real", status="old", action="read", iostat=log_call_stat)
        call AssertEqual(log_call_stat,0,"opening 'real5d_pid_2.real' for reading variable output_real5", __LINE__, __FILE__)
        do i5 = 1,5
           do i4 = 1,4
              do i3 = 1,3
                 do i2 = 1,20
                    do i1 = 1,1
                       read(fid,*) ref_output_real5(i1, i2, i3, i4, i5)
                    end do
                 end do
              end do
           end do
        end do
        close(unit=fid, iostat=log_call_stat)
        call AssertEqual(log_call_stat,0,"closing 'real5d_pid_2.real' for reading variable output_real5", __LINE__, __FILE__)
        allocate(input_real1(1008), stat=log_call_stat)
        call AssertEqual(log_call_stat,0,"allocation of 'input_real1'", __LINE__, __FILE__)
        open(newunit=fid, file="./level_1/resources/real1d_pid_3.real", status="old", action="read", iostat=log_call_stat)
        call AssertEqual(log_call_stat,0,"opening 'real1d_pid_3.real' for reading variable input_real1", __LINE__, __FILE__)
        do i1 = 1,1008
           read(fid,*) input_real1(i1)
        end do
        close(unit=fid, iostat=log_call_stat)
        call AssertEqual(log_call_stat,0,"closing 'real1d_pid_3.real' for reading variable input_real1", __LINE__, __FILE__)
        allocate(input_real2(11,100), stat=log_call_stat)
        call AssertEqual(log_call_stat,0,"allocation of 'input_real2'", __LINE__, __FILE__)
        open(newunit=fid, file="./level_1/resources/real2d_pid_3.real", status="old", action="read", iostat=log_call_stat)
        call AssertEqual(log_call_stat,0,"opening 'real2d_pid_3.real' for reading variable input_real2", __LINE__, __FILE__)
        do i2 = 1,100
           do i1 = 1,11
              read(fid,*) input_real2(i1, i2)
           end do
        end do
        close(unit=fid, iostat=log_call_stat)
        call AssertEqual(log_call_stat,0,"closing 'real2d_pid_3.real' for reading variable input_real2", __LINE__, __FILE__)
        allocate(input_real3(20,20,3), stat=log_call_stat)
        call AssertEqual(log_call_stat,0,"allocation of 'input_real3'", __LINE__, __FILE__)
        open(newunit=fid, file="./level_1/resources/real3d_pid_3.real", status="old", action="read", iostat=log_call_stat)
        call AssertEqual(log_call_stat,0,"opening 'real3d_pid_3.real' for reading variable input_real3", __LINE__, __FILE__)
        do i3 = 1,3
           do i2 = 1,20
              do i1 = 1,20
                 read(fid,*) input_real3(i1, i2, i3)
              end do
           end do
        end do
        close(unit=fid, iostat=log_call_stat)
        call AssertEqual(log_call_stat,0,"closing 'real3d_pid_3.real' for reading variable input_real3", __LINE__, __FILE__)
        allocate(input_real4(5,20,3,4), stat=log_call_stat)
        call AssertEqual(log_call_stat,0,"allocation of 'input_real4'", __LINE__, __FILE__)
        open(newunit=fid, file="./level_1/resources/real4d_pid_3.real", status="old", action="read", iostat=log_call_stat)
        call AssertEqual(log_call_stat,0,"opening 'real4d_pid_3.real' for reading variable input_real4", __LINE__, __FILE__)
        do i4 = 1,4
           do i3 = 1,3
              do i2 = 1,20
                 do i1 = 1,5
                    read(fid,*) input_real4(i1, i2, i3, i4)
                 end do
              end do
           end do
        end do
        close(unit=fid, iostat=log_call_stat)
        call AssertEqual(log_call_stat,0,"closing 'real4d_pid_3.real' for reading variable input_real4", __LINE__, __FILE__)
        allocate(input_real5(1,20,3,4,5), stat=log_call_stat)
        call AssertEqual(log_call_stat,0,"allocation of 'input_real5'", __LINE__, __FILE__)
        open(newunit=fid, file="./level_1/resources/real5d_pid_3.real", status="old", action="read", iostat=log_call_stat)
        call AssertEqual(log_call_stat,0,"opening 'real5d_pid_3.real' for reading variable input_real5", __LINE__, __FILE__)
        do i5 = 1,5
           do i4 = 1,4
              do i3 = 1,3
                 do i2 = 1,20
                    do i1 = 1,1
                       read(fid,*) input_real5(i1, i2, i3, i4, i5)
                    end do
                 end do
              end do
           end do
        end do
        close(unit=fid, iostat=log_call_stat)
        call AssertEqual(log_call_stat,0,"closing 'real5d_pid_3.real' for reading variable input_real5", __LINE__, __FILE__)
        inout_bool1 = in_inout_bool1
        allocate(ref_out_string1(101), stat=log_call_stat)
        call AssertEqual(log_call_stat,0,"allocation of 'ref_out_string1'", __LINE__, __FILE__)
        allocate(out_string1(101), stat=log_call_stat)
        call AssertEqual(log_call_stat,0,"allocation of 'out_string1'", __LINE__, __FILE__)
        allocate(in_string1(101), stat=log_call_stat)
        call AssertEqual(log_call_stat,0,"allocation of 'in_string1'", __LINE__, __FILE__)
        allocate(inout_string1(101), stat=log_call_stat)
        call AssertEqual(log_call_stat,0,"allocation of 'inout_string1'", __LINE__, __FILE__)
        call test_log_call( input_int1, input_int2, output_int1, output_int2, output_real1, output_real2, &
            output_real3, output_real4, output_real5, input_real1, input_real2, input_real3, input_real4, &
            input_real5, out_bool1, in_bool1, out_string1, in_string1,)
        call assertEqual( ref_output_int1, output_int1, "output_int1", __LINE__, __FILE__)
        call assertEqual( ref_output_int2, output_int2, "output_int2", __LINE__, __FILE__)
        call assertEqual( ref_output_real1, output_real1, tol, "output_real1", __LINE__, __FILE__)
        call assertEqual( ref_output_real2, output_real2, tol, "output_real2", __LINE__, __FILE__)
        call assertEqual( ref_output_real3, output_real3, tol, "output_real3", __LINE__, __FILE__)
        call assertEqual( ref_output_real4, output_real4, tol, "output_real4", __LINE__, __FILE__)
        call assertEqual( ref_output_real5, output_real5, tol, "output_real5", __LINE__, __FILE__)
        call assertEqual( ref_out_bool1, out_bool1, "out_bool1", __LINE__, __FILE__)
        call assertEqual( ref_out_string1, out_string1, "out_string1", __LINE__, __FILE__)
   end subroutine test_test_log_call
end module m_test_test_log_call

program p_test_test_log_call
use m_test_test_log_call
use no_pfunit
implicit none
   call test_test_log_call()
   call conclusion()
end program p_test_test_log_call
