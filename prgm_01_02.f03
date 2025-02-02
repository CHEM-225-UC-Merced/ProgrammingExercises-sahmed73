program prgm_01_02
        implicit none
        real,dimension(3,3) :: matrixA, matrixB

        write(*,*)' What is the name of the first input data file?'
        call ReadMatrix3x3(matrixA)
        write(*,*)' What is the name of the second input data file?'
        call ReadMatrix3x3(matrixB)

        call PrintMatrix3x3(matrixA)
        call PrintMatrix3x3(matrixB)

end program prgm_01_02

subroutine ReadMatrix3x3(matrix)
        implicit none
        integer, parameter :: inFileUnit=10
        integer :: errorFlag,i
        real,dimension(3,3),intent(out) :: matrix
        character(len=128) :: fileName

        read(*,*) fileName
        open(unit=inFileUnit, file=trim(fileName), status='old', iostat=errorFlag)

        if(errorFlag .ne. 0) then
                write(*,*)' There was a problem opening the input file.'
                goto 999
        endif

        do i = 1,3
                read(inFileUnit,*) matrix(1,i),matrix(2,i),matrix(3,i)
        enddo

        close(inFileUnit)
    999 continue
end subroutine ReadMatrix3x3

subroutine PrintMatrix3x3(matrix)
       implicit none
       real,dimension(3,3),intent(in)::matrix
       integer::i

       1000 format(3(2x,f5.1))
       
        write(*,*)' Printing Matrix'

        do i = 1, 3
            write(*, 1000) matrix(i, 1), matrix(i, 2), matrix(i, 3)
        end do
        return
end subroutine PrintMatrix3x3

