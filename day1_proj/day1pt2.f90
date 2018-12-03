program day1pt2
    use dynamicArray

    implicit none

    ! Values
    character(26) :: alpha
    character(len=1) :: sign
    character(len=200) :: line
    character(len=199) :: prenum
    integer :: i, number, ios, fileSize, frequency = 0
    integer, dimension(:), allocatable :: previousFreqs

    ! Populate array with alphabet
    do i = 1, 26
        alpha(i:i) = achar(iachar('a') + i - 1)
    end do


    ! Open file as identifier 1
    open(1, file = 'day1_input.txt', iostat=ios, status = 'old') 
    if ( ios /= 0 ) stop "Error opening file"

    allocate(previousFreqs(1))

    ! Loop through values line by line
    do
        ! If we hit the EOF, loop back to top and keep going
        read(1, *,iostat=ios) line
        if (ios /= 0) then 
            rewind(1)
            cycle
        end if

        ! Break sign off of front of line
        sign = trim(line(1:1))

        ! Get number payload and convert to integer
        prenum = trim(line(2:199))
        read(prenum, *) number

        if (sign == '+') then
            frequency = frequency + number
        else
            frequency = frequency - number
        end if

        if (any(previousFreqs == frequency)) then
            exit
        end if

        call addElementToArray(frequency, previousFreqs)
    end do

    write(*,*) frequency
    deallocate(previousFreqs)

end program day1pt2
