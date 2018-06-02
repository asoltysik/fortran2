program main
    use matmul
    implicit none

    character (len = 16) :: maxSizeString
    integer (kind = 4) :: maxSize, conversionRes, i

    call GET_COMMAND_ARGUMENT(1, maxSizeString)
    read(maxSizeString, *, iostat=conversionRes) maxSize

    if (conversionRes .NE. 0) then
        WRITE (*, *) 'Error while converting to integer.'
        stop 1
    end if
    
    do i = 1, maxSize, 5
        call doMultiply(i)
    end do

    contains
    subroutine doMultiply(matrixSize)
        implicit none
        integer (kind = 4), intent(in) :: matrixSize
        real (kind = 8), allocatable :: first(:, :), second(:, :), out(:, :)
        real :: startTime, endTime
        integer (kind = 4) :: ret

        allocate(first(matrixSize, matrixSize))
        allocate(second(matrixSize, matrixSize))
        allocate(out(matrixSize, matrixSize))

        first = 13.7623
        second = 69.12321

        call CPU_TIME(startTime)
        call mm(first, second, out, ret)
        call CPU_TIME(endTime)

        WRITE (*, *) matrixSize, ',', (endTime - startTime)

        deallocate(first)
        deallocate(second)
        deallocate(out)

    end subroutine

end program main