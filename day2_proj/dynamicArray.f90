module dynamicArray
    contains

    ! Takes an element and array, checks if array is already allocated
    !  and if not allocates. If it is, creates new array at size + 1, 
    !  copies old array to it, and moves allocation to the new array.
    subroutine addElementToArray(element, array)
        implicit none

        integer :: i, isize
        integer :: element
        integer, dimension(:), allocatable :: array
        integer, dimension(:), allocatable :: newArray

        if (allocated(array)) then
            isize = len(array)
            allocate(newArray(isize+1))
            do i=1, isize
                newArray(i) = array(i)
            end do
            newArray(isize+1) = element
            deallocate(array)

            call move_alloc(newArray, array)

        else
            allocate(array(1))
            array(1) = element
        end if
    end subroutine addElementToArray
end module dynamicArray