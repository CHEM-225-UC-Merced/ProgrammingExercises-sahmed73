program prgm_01_01
    implicit none
    integer, parameter :: inFileUnitA=10
    integer :: errorFlag, i
    real, dimension(3,3) :: matrixInA
    character(len=128) :: fileNameA

    ! Prompt user for input file name
    write(*,*) 'What is the name of the input data file?'
    read(*,*) fileNameA

    ! Open the file
    open(unit=inFileUnitA, file=trim(fileNameA), status='old', iostat=errorFlag)
    if (errorFlag .ne. 0) then
        write(*,*) 'There was a problem opening the input file.'
        goto 999
    endif

    ! Read matrix row-wise (fixes column-wise issue)
    do i = 1, 3
        read(inFileUnitA, *) matrixInA(i,1), matrixInA(i,2), matrixInA(i,3)
    enddo

    ! Close the file
    close(inFileUnitA)

    ! Print the matrix
    call PrintMatrix3x3(matrixInA)

999 continue
end program prgm_01_01


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