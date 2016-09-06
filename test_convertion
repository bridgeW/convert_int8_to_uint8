program convertion
use nrtype
implicit none
integer(I1B) :: testdata = -120
write(*,*)'The maximum of 1-byte integer:', huge(testdata)
write(*,*)convert_int8_to_uint8(testdata)


contains
        integer(I4B) function convert_uint16_to_dec ( Array2bytes )
        use nrtype 
        implicit none
        integer(I1B), dimension(:), intent(in) :: Array2bytes
        convert_uint16_to_dec = &
            convert_int8_to_uint8(Array2Bytes(1)) * 2**8   + &
            convert_int8_to_uint8(Array2Bytes(2))
        end function convert_uint16_to_dec
!BL
        integer(I4B) function convert_uint24_to_dec ( Array3bytes )
        use nrtype 
        implicit none
        integer(I1B), dimension(:), intent(in) :: Array3bytes
        convert_uint24_to_dec = &
            convert_int8_to_uint8(Array3Bytes(1)) * 2**16  + &
            convert_int8_to_uint8(Array3Bytes(2)) * 2**8   + &
            convert_int8_to_uint8(Array3Bytes(3))
        end function convert_uint24_to_dec
!BL
        integer(I6B) function convert_uint32_to_dec ( Array4bytes )
        use nrtype 
        implicit none
        integer(I1B), dimension(:), intent(in) :: Array4bytes
        convert_uint32_to_dec = &
            convert_int8_to_uint8(Array4Bytes(1)) * 2**24  + &
            convert_int8_to_uint8(Array4Bytes(2)) * 2**16  + &
            convert_int8_to_uint8(Array4Bytes(3)) * 2**8   + &
            convert_int8_to_uint8(Array4Bytes(4))
        end function convert_uint32_to_dec
!BL
        integer(I2B) function convert_int8_to_uint8 ( int8 )
        !/*
        !    I = convert_int8_unit8(X) 
        !   converts the elements of the signed 8-bit INTEGER array X into 
        !   unsigned 8-bit integers. The signed 8-bit INTEGER range from -128
        !   to 127, i.e., -2**7 to 2**7-1.
        !   X must be a signed 8-bit integer numeric object). The values
        !   of a UINT8 range from 0 to 255. Values outside this range saturate
        !   on overflow, namely they are mapped to 0 or 255 if they are outside
        !   the range. 
        !
        !   Author: WANG Qiao, wangqiaochn@163.com, 20160903
        !*/
    
        use nrtype
    
        implicit none

        integer(I1B), parameter :: minINT8 = -128
        integer(I1B), parameter :: maxINT8 =  127
        integer(I1B), parameter :: minUINT8 = 0
        integer(I2B), parameter :: maxUINT8 = 255
    
        ! integer(I2B) :: convert_int8_to_uint8
        integer(I1B), intent(IN):: int8
    
    
        if ( int8 >= minINT8 .and. int8 <= maxINT8 ) then
        
            if ( int8 >= 0 ) then
                convert_int8_to_uint8 = int8
            else
                convert_int8_to_uint8 = int8 + maxUINT8 + 1
            endif
        else
            write(*,*)'OVERFLOW Warning: Input number is not a signed 8-bit value.'
            stop
        endif
            
        end function convert_int8_to_uint8
!BL
        integer(I4B) function convert_int16_to_uint16 ( int16 )
        !/*
        !    I = convert_int16_unit16(X) 
        !   converts the elements of the signed 8-bit INTEGER array X into 
        !   unsigned 16-bit integers. The signed 16-bit INTEGER range from 
        !   -32768 to 32767, i.e., -2**15 to 2**15-1.
        !   X can be any numeric object, such as a REAL (SP or DP). The values
        !   of a UINT16 range from 0 to 65535. Values outside this range saturate
        !   on overflow, namely they are mapped to 0 or 65535 if they are outside
        !   the range. REAL (SP or DP) values are rounded to the nearest UINT16
        !   value on conversion.
        !
        !   Author: WANG Qiao, wangqiaochn@163.com, 20160903
        !*/
    
        use nrtype
    
        implicit none

        integer(I2B), parameter :: minINT16 = -32768
        integer(I2B), parameter :: maxINT16 =  32767
        integer(I1B), parameter :: minUINT16 = 0
        integer(I4B), parameter :: maxUINT16 = 65535
    
        integer(I2B), intent(IN):: int16
    
    
        if ( int16 >= minINT16 .and. int16 <= maxINT16 ) then
        
            if ( int16 >= 0 ) then
                convert_int16_to_uint16 = int16
            else
                convert_int16_to_uint16 = int16 + maxUINT16 + 1
            endif
        else
            write(*,*)'OVERFLOW Warning: Input number is not a signed 16-bit value.'
            stop
        endif
            
        end function convert_int16_to_uint16
!BL
        integer(I6B) function convert_int32_to_uint32 ( int32 )
        !/*
        !    I = convert_int32_unit32(X) 
        !   converts the elements of the signed 32-bit INTEGER array X into 
        !   unsigned 32-bit integers. The signed 32-bit INTEGER range from
        !   -2147483648 to 2147483647, i.e., -2**31 to 2**31-1.
        !   X must be a singed 32-bit integer numeric object. The values
        !   of a UINT32 range from 0 to 2**32-1. Values outside this range saturate
        !   on overflow, namely they are mapped to 0 or 2**32-1 if they are outside
        !   the range. 
        !
        !   Author: WANG Qiao, wangqiaochn@163.com, 20160903
        !*/
    
        use nrtype
    
        implicit none

        integer(I6B), parameter :: minINT32 = -2147483648
        integer(I6B), parameter :: maxINT32 =  2147483647
        integer(I1B), parameter :: minUINT32 = 0
        integer(I6B), parameter :: maxUINT32 = 4294967295
    
        integer(I4B), intent(IN):: int32
    
    
        if ( int32 >= minINT32 .and. int32 <= maxINT32 ) then
        
            if ( int32 >= 0 ) then
                convert_int32_to_uint32 = int32
            else
                convert_int32_to_uint32 = int32 + maxUINT32 + 1
            endif
        else
            write(*,*)'OVERFLOW Warning: Input number is not a signed 8-bit value.'
            stop
        endif
            
        end function convert_int32_to_uint32
!BL

end program convertion
