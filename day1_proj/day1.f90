program day1
    implicit none

    ! Values
    character(len=1) :: sign
    character(len=200) :: line
    character(len=199) :: prenum
    integer :: number, ios, frequency = 0
    

    ! Open file as identifier 1
    open(1, file = 'day1_input.txt', iostat=ios, status = 'old') 
    if ( ios /= 0 ) stop "Error opening file"

    ! Loop through values line by line
    do
        read(1, *,iostat=ios) line
        if (ios /= 0) exit

        ! Break sign off of front of line
        sign = trim(line(1:1))

        ! Get number payload and convert to integer
        prenum = trim(line(2:199))
        read(prenum, *) number

        ! Perform integer additional/subtraction
        if (sign == '+') then
            frequency = frequency + number
        else
            frequency = frequency - number
        end if
    end do

    write(*,*) frequency

end program day1