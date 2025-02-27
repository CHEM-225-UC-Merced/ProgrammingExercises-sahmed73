Program prgm_02_03

  Implicit None
  Integer :: IIn=10, IError, NDim, i
  Real, Dimension(:), Allocatable :: Array_Input, EVals, Temp_Vector
  Real, Dimension(:,:), Allocatable :: Matrix, EVecs, Temp_Matrix
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
  Allocate(EVals(NDim), EVecs(NDim, NDim), Temp_Vector(3*NDim))
  Allocate(Temp_Matrix(NDim, NDim))

  ! Read packed symmetric matrix data
  Do i = 1, (NDim*(NDim+1))/2
    Read(IIn,*) Array_Input(i)
  End Do
  Close(IIn)

  ! Convert and print matrix (Lower-triangle unpacking)
  Write(*,*) 'The matrix loaded (column) lower-triangle packed:'
  Call SymmetricPacked2Matrix_LowerPac(NDim, Array_Input, Matrix)
  Call Print_Matrix_Full_Real(Matrix, NDim, NDim)

  ! Compute Eigenvalues and Eigenvectors using LAPACK SSPEV
  Call SSPEV('V', 'L', NDim, Array_Input, EVals, EVecs, NDim, Temp_Vector, IError)
  If (IError /= 0) Then
    Write(*,*) 'Failure in SSPEV.'
    STOP
  End If

  ! Print Eigenvalues
  Write(*,*) 'EVals:'
  Call Print_Matrix_Full_Real(RESHAPE(EVals, (/1, NDim/)), 1, NDim)

  ! Print Eigenvectors
  Write(*,*) 'EVecs:'
  Call Print_Matrix_Full_Real(EVecs, NDim, NDim)

End Program prgm_02_03


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
