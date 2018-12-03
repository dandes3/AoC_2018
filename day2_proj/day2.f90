program day2
    implicit none

    ! Values
    integer :: i= 0, j= 0, ios= 0
    character(len=200) :: line
    integer, dimension(26) :: occ
    character(len=1), dimension(26) :: alpha
    integer :: twoSum = 0, threeSum = 0, twoSumTotal = 0, threeSumTotal = 0, checksum = 0

    ! Populate array with alphabet
    do i = 1, 26
        alpha(i) = achar(iachar('a') + i -1)
    end do

    ! Open file as identifier 1
    open(1, file = 'day2_input.txt', iostat=ios, status = 'old') 
    if ( ios /= 0 ) stop "Error opening file"

    ! Loop through values line by line
    mainloop: do

        read(1, *,iostat=ios) line
        if (ios /= 0) exit
        
        twoSum = 0
        threeSum = 0

        ! Get number of matches for each letter of alphabet
        do i = 1, 26
            occ(i) = countSubString(line, alpha(i))
        end do

        ! Add number of matches for each amount to local vals
        do j = 1, 26
            if (occ(j) == 2) then 
                twoSum = twoSum + 1
            end if  
            if (occ(j) == 3) then
                threeSum = threeSum + 1
            end if
        end do

        ! Increment total if any matches found
        if (twoSum > 0) then
            twoSumTotal = twoSumTotal + 1
        end if
        if (threeSum > 0) then
            threeSumTotal = threeSumTotal + 1
        end if

    end do mainloop

    checksum = twoSumTotal * threeSumTotal
    print*,  checksum

contains 

! Take in string and substring, return number of times
!  substring appears inside the string.
function countsubstring(s1, s2) result(c)
  character(*), intent(in) :: s1, s2
  integer :: c, p, posn
 
  c = 0
  if(len(s2) == 0) return
  p = 1
  do 
    posn = index(s1(p:), s2)
    if(posn == 0) return
    c = c + 1
    p = p + posn + len(s2) - 1
  end do
end function

end program day2
