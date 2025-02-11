program prgm_01_03
    implicit none
    integer, parameter :: inFileUnitA=10, inFileUnitB=11
    integer :: errorFlag, i
    real, dimension(3,3) :: matrixInA, matrixInB, matrixProduct
    character(len=128) :: fileNameA, fileNameB

    ! Prompt user for input file names
    write(*,*) 'What is the name of the first input data file?'
    read(*,*) fileNameA

    write(*,*) 'What is the name of the second input data file?'
    read(*,*) fileNameB

    ! Open and read the first matrix
    open(unit=inFileUnitA, file=trim(fileNameA), status='old', iostat=errorFlag)
    if (errorFlag .ne. 0) then
        write(*,*) 'There was a problem opening the first input file.'
        goto 999
    endif

    do i = 1, 3
        read(inFileUnitA, *) matrixInA(i,1), matrixInA(i,2), matrixInA(i,3)
    enddo
    close(inFileUnitA)

    ! Open and read the second matrix
    open(unit=inFileUnitB, file=trim(fileNameB), status='old', iostat=errorFlag)
    if (errorFlag .ne. 0) then
        write(*,*) 'There was a problem opening the second input file.'
        goto 999
    endif

    do i = 1, 3
        read(inFileUnitB, *) matrixInB(i,1), matrixInB(i,2), matrixInB(i,3)
    enddo
    close(inFileUnitB)

    ! Print both matrices
    call PrintMatrix3x3(matrixInA)
    call PrintMatrix3x3(matrixInB)

    ! Compute the matrix product
    matrixProduct = matmul(matrixInA, matrixInB)

    ! Print the resulting matrix product
    call PrintMatrix3x3(matrixProduct)

999 continue
end program prgm_01_03


subroutine PrintMatrix3x3(matrix)
    implicit none
    real, dimension(3,3), intent(in) :: matrix
    integer :: i

    ! Format for printing
1000 format(3(2x,f5.1))

    ! Print header
    write(*,*) 'Printing Matrix'

    ! Print the matrix row-wise
    do i = 1, 3
        write(*, 1000) matrix(i,1), matrix(i,2), matrix(i,3)
    end do

    return
end subroutine PrintMatrix3x3

