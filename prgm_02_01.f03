Program prgm_02_01

  Implicit None
  Integer, Parameter :: IIn=10
  Integer :: IError, NDim, i
  Real, Dimension(:), Allocatable :: Array_Input
  Real, Dimension(:,:), Allocatable :: Matrix
  Character(Len=256) :: FileName

  ! Read the input file name from the command line
  Call Get_Command_Argument(1, FileName)
  Open(Unit=IIn, File=TRIM(FileName), Status='OLD', IOStat=IError)
  If (IError /= 0) Then
    Write(*,*) 'Error opening input file.'
    STOP
  End If

  ! Read matrix dimension and allocate memory
  Read(IIn,*) NDim
  Allocate(Array_Input(NDim*NDim), Matrix(NDim, NDim))

  ! Read packed matrix data
  Do i = 1, NDim * NDim
    Read(IIn,*) Array_Input(i)
  End Do
  Close(IIn)

  ! Convert and print matrix (Row-wise unpacking)
  Write(*,*) 'The matrix expanded according to a row-wise linear packed format:'
  Call Packed2Matrix_RowWise(NDim, NDim, Array_Input, Matrix)
  Call Print_Matrix_Full_Real(Matrix, NDim, NDim)

  ! Convert and print matrix (Column-wise unpacking)
  Write(*,*) 'The matrix expanded according to a column-wise linear packed format:'
  Call Packed2Matrix_ColumnWise(NDim, NDim, Array_Input, Matrix)
  Call Print_Matrix_Full_Real(Matrix, NDim, NDim)

End Program prgm_02_01


Subroutine Packed2Matrix_ColumnWise(M, N, ArrayIn, AMatOut)
  Implicit None
  Integer, Intent(In) :: M, N
  Real, Dimension(N*M), Intent(In) :: ArrayIn
  Real, Dimension(M, N), Intent(Out) :: AMatOut
  Integer :: i, j, k

  k = 1
  Do j = 1, N
    Do i = 1, M
      AMatOut(i, j) = ArrayIn(k)
      k = k + 1
    End Do
  End Do

End Subroutine Packed2Matrix_ColumnWise


Subroutine Packed2Matrix_RowWise(M, N, ArrayIn, AMatOut)
  Implicit None
  Integer, Intent(In) :: M, N
  Real, Dimension(N*M), Intent(In) :: ArrayIn
  Real, Dimension(M, N), Intent(Out) :: AMatOut
  Integer :: i, j, k

  k = 1
  Do i = 1, M
    Do j = 1, N
      AMatOut(i, j) = ArrayIn(k)
      k = k + 1
    End Do
  End Do

End Subroutine Packed2Matrix_RowWise


Subroutine Print_Matrix_Full_Real(Matrix, M, N)
  Implicit None
  Integer, Intent(In) :: M, N
  Real, Dimension(M, N), Intent(In) :: Matrix
  Integer :: i, j

  Do i = 1, M
    Do j = 1, N
      Write(*,'(F10.6)', Advance='No') Matrix(i, j)
    End Do
    Write(*,*)
  End Do

End Subroutine Print_Matrix_Full_Real
