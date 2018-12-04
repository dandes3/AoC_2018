module dynamicArray
    contains

    ! Takes an element and array, checks if array is already allocated
    !  and if not allocates. If it is, creates new array at size + 1, 
    !  copies old array to it, and moves allocation to the new array.
    subroutine addElementToArray(element, array)
        implicit none

        integer :: i
        character(len=26) :: element
        character(len=26), allocatable, dimension(:)  :: array
        character(len=26), allocatable, dimension(:) :: newArray

        if (allocated(array)) then
            newArray = [array, element]
            deallocate(array)
            call move_alloc(newArray, array)

        else
            allocate(array(1))
            array(1) = element
        end if
    end subroutine addElementToArray
end module dynamicArray