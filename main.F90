program main
    use matmul
    implicit none

    real (kind = 8) :: first(1,1)
    real (kind = 8) :: second(1 ,1)
    real (kind = 8) :: out(1,1)
    integer (kind = 4) :: ret

    first(1, 1) = 3
    second(1, 1) = 7
    
    call mm(first, second, out, ret)

    WRITE (*, *) out(1, 1)

end program