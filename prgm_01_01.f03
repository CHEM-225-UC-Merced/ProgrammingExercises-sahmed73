program prgm_01_01
        implicit none
        integer, parameter :: inFileUnitA=10
        integer :: errorFlag,i
        real,dimension(3,3) :: matrixInA
        character(len=128) :: fileNameA

        write(*,*)' What is the name of the input data file?'
        read(*,*) fileNameA

        open(unit=inFileUnitA, file=trim(fileNameA), status='old', iostat=errorFlag)
        if(errorFlag .ne. 0) then
                write(*,*)' There was a problem opening the input file.'
                goto 999
        endif

        do i = 1,3
                read(inFileUnitA,*) matrixInA(1,i),matrixInA(2,i),matrixInA(3,i)
        enddo
        
        close(inFileUnitA)

        call PrintMatrix3x3(matrixInA)

    999 continue
end program prgm_01_01


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

