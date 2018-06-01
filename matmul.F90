#define OPT_1 1
#define OPT_2 1

module matmul
  implicit none
contains
subroutine mm(first, second, multiply, status)
  implicit none
  real (kind = 8), intent(in)     :: first(:,:)     ! pierwsza macierz
  real (kind = 8), intent(in)     :: second(:,:)    ! druga macierz
  real (kind = 8), intent(out)    :: multiply(:,:)  ! macierz wynikowa
  integer (kind = 4), intent(out) :: status         ! kod błędu, 0 gdy OK

  integer (kind = 4) :: firstRows, secondRows, firstCols, secondCols, i, j, k, ichunk, jj, kk

  multiply   = 0.d0
  status     = 0

  firstRows  = size(first, 1)
  secondRows = size(second, 1)
  firstCols  = size(first, 2)
  secondCols = size(second, 2)

  if (firstCols .NE. secondRows) then
    status = -1
    return
  end if

#if OPT_2
    WRITE (*, *) 'USING CACHE OPTIMIZATION'
    ichunk = 512
    do jj = 1, firstRows, ichunk
        do kk = 1, secondCols, ichunk

          do j = jj, min(jj + ichunk - 1, firstRows)
              do k = kk, min(kk + ichunk - 1, secondCols)
#if OPT_1
                WRITE (*, *) 'USING DOT PRODUCT OPTIMIZATION'
                multiply(j,k)=dot_product(first(j,:), second(:,k))
#else
                do i = 1, firstCols
                    multiply(j, k) = multiply(j, k) + first(j, i) * second(i, k)
                end do
#endif
              end do
          end do

        end do
    end do
#else
  do i = 1, firstRows
    do j = 1, secondCols
#if OPT_1
        WRITE (*, *) 'USING DOT PRODUCT OPTIMIZATION'
        multiply(i, j) = dot_product(first(i, :), second(:, j))
#else
      do k = 1, firstCols
        multiply(i, j) = multiply(i, j) + (first(i, k) * second(k, j))
      end do
#endif
    end do
  end do
#endif



end subroutine

end module

