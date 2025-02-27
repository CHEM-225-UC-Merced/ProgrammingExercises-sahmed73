Program prgm_02_02

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
  Allocate(Array_Input((NDim*(NDim+1))/2), Matrix(NDim, NDim))

  ! Read packed symmetric matrix data
  Do i = 1, (NDim*(NDim+1))/2
    Read(IIn,*) Array_Input(i)
  End Do
  Close(IIn)

  ! Convert and print matrix (Lower-triangle unpacking)
  Write(*,*) 'The matrix loaded (column-wise) lower-tri packed:'
  Call SymmetricPacked2Matrix_LowerPac(NDim, Array_Input, Matrix)
  Call Print_Matrix_Full_Real(Matrix, NDim, NDim)

  ! Convert and print matrix (Upper-triangle unpacking)
  Write(*,*) 'The matrix loaded (column-wise) upper-tri packed:'
  Call SymmetricPacked2Matrix_UpperPac(NDim, Array_Input, Matrix)
  Call Print_Matrix_Full_Real(Matrix, NDim, NDim)

End Program prgm_02_02


Subroutine SymmetricPacked2Matrix_LowerPac(N, ArrayIn, AMatOut)
  Implicit None
  Integer, Intent(In) :: N
  Real, Dimension((N*(N+1))/2), Intent(In) :: ArrayIn
  Real, Dimension(N, N), Intent(Out) :: AMatOut
  Integer :: i, j, k

  k = 1
  Do j = 1, N
    Do i = j, N
      AMatOut(i, j) = ArrayIn(k)
      AMatOut(j, i) = ArrayIn(k)  ! Symmetric property
      k = k + 1
    End Do
  End Do

End Subroutine SymmetricPacked2Matrix_LowerPac


Subroutine SymmetricPacked2Matrix_UpperPac(N, ArrayIn, AMatOut)
  Implicit None
  Integer, Intent(In) :: N
  Real, Dimension((N*(N+1))/2), Intent(In) :: ArrayIn
  Real, Dimension(N, N), Intent(Out) :: AMatOut
  Integer :: i, j, k

  k = 1
  Do i = 1, N
    Do j = i, N
      AMatOut(i, j) = ArrayIn(k)
      AMatOut(j, i) = ArrayIn(k)  ! Symmetric property
      k = k + 1
    End Do
  End Do

End Subroutine SymmetricPacked2Matrix_UpperPac


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
