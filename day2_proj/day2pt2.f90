program day2pt2
    
    use dynamicArray

    implicit none

    ! Values
    integer :: checksum = 0
    character(len=26) :: line, answer, swap1, swap2
    integer :: i= 0, j= 0, k=0, ios= 0, diffCount = 0
    character(len=26), allocatable, dimension(:) :: previousLines

    ! Open file as identifier 1
    open(1, file = 'day2_input.txt', iostat=ios, status = 'old') 
    if ( ios /= 0 ) stop "Error opening file"

    ! Read first line and init array
    read(1, *,iostat=ios) line
    if (ios /= 0) stop
    previousLines = [line]

    ! Loop through values line by line
    mainloop: do
        read(1, *,iostat=ios) line
        if (ios /= 0) exit

        ! Take each value read, and loop through previous values starting at first one
        do i = 1, size(previousLines)
            k = 1
            diffCount = 0

            ! For each value compare each character, increment diffcounter if so
            do j= 1, len(line)
                if (line(k:j) /= previousLines(i)(k:j)) then
                    diffCount = diffCount + 1
                end if
                k = k + 1
            end do

            ! Found a diff ==1, pull all chars that match from both values
            if (diffCount == 1) then
                k = 1
                do j= 1, len(line)
                    if (line(k:j) /= previousLines(i)(k:j)) then
                        answer = trim(line(1:j-1))//trim(line(j+1:len(line)))
                    end if
                    k = k + 1
                end do
            end if
        end do

        call addElementToArray(line, previousLines)

    end do mainloop

   print *, answer

end program day2pt2
