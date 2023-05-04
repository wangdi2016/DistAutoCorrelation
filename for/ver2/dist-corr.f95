      PROGRAM distance_autocorrelation

! Distance autocorrelation 

      implicit none

!     Declare variables
!     PARAMETER (N=2666)
!     PARAMETER (M=2666)
!     PARAMETER (L=1000)
      REAL, DIMENSION(:), allocatable :: P1, W1, P2, W2
      REAL, DIMENSION(:), allocatable :: OCCURENCE
      INTEGER :: I, J, IMAX, N, M, L, DIST
      CHARACTER(len=50) :: FILENAME1, FILENAME2, FILENAME3
      INTEGER :: FILE_UNIT1, FILE_UNIT2, FILE_UNIT3, ierr, stat

      CHARACTER(LEN=*), PARAMETER  :: FMT2 = "(I5, F9.3)"
      CHARACTER(LEN=100) :: BUFFER

! Assign unit for read and write files
      FILE_UNIT1=10
      FILE_UNIT2=20
      FILE_UNIT3=30

!     IMAX=1000

! Read command line arguments
!     call get_command_argument(1, FILENAME1)
!     call get_command_argument(2, FILENAME2)
!     call get_command_argument(3, FILENAME3)

      call getarg(1,buffer) 
      read(buffer,*) FILENAME1

      call getarg(2,buffer)   
      read(buffer,*) FILENAME2

      call getarg(3,buffer)   
      read(buffer,*) FILENAME3

      call getarg(4,buffer)   
      read(buffer,*) N

      call getarg(5,buffer)
      read(buffer,*) M

      call getarg(6,buffer)
      read(buffer,*) L

!     Prompt the user to enter the filename
!     WRITE(*,*) 'Enter the filenames: '
!     READ(*,*) FILENAME1, FILENAME2, FILENAME3
!     WRITE(*,*) 'Enter the number of lines filename1(N), filename2(M), filename3(L):'
!     READ(*,*) N, M, L

      IMAX=L

! Allocate memory for the position array and the distance and correlation arrays
      allocate(P1(N))
      allocate(W1(N))
      allocate(P2(M))
      allocate(W2(M))
      allocate(OCCURENCE(L))

!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!CC   Open file1                                    CCC
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      OPEN(FILE_UNIT1, FILE=FILENAME1, STATUS='OLD', ACTION='read', IOSTAT=ierr)
      IF (ierr.NE.0) THEN
        WRITE(*,*) 'Error: could not open file1'
        STOP
      END IF
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!CC   Read the data from the file1 into the array   CCC
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      READ(FILE_UNIT1,*)
! 
!     DO I=1,N
      DO I=1,size(P1)
        READ(FILE_UNIT1,*,IOSTAT=stat) P1(I), W1(I) 
        IF(IS_IOSTAT_END(stat)) EXIT
!        IF (stat.NE.0) EXIT
      END DO
!     Close the file1
      CLOSE(FILE_UNIT1)
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!CC   Open file2                                    CCC
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      OPEN(FILE_UNIT2, FILE=FILENAME2, STATUS='OLD', ACTION='read', IOSTAT=ierr)
      IF (ierr.NE.0) THEN
        WRITE(*,*) 'Error: could not open file2'
        STOP
      END IF
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!CC   Read the data from the file2 into the array   CCC
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      READ(FILE_UNIT2,*)
! 
!     DO I=1,M
      DO I=1,size(P2)
         READ(FILE_UNIT2,*,IOSTAT=stat) P2(I), W2(I) 
         IF(IS_IOSTAT_END(stat)) EXIT
!        IF (stat.NE.0) EXIT
      END DO
!     Close the file2
      CLOSE(FILE_UNIT2)

!     Print the contents of the array
!     WRITE(*,*) 'The array are:'
!     DO I=1,M
!        WRITE(*,*) P1(I), W1(I), P2(I), W2(I)
!     END DO

!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!CC   Calculate Distance and Occurance              CCC
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      DO I=1,N
         DO J=1,M
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!CC         Calculate Distance                      CCC
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
            DIST = P2(J) - P1(I) + 1

            IF ((DIST .GE. 0) .AND. (DIST .LE. IMAX)) THEN
                 OCCURENCE(DIST) = OCCURENCE(DIST) + W1(I) * W2(J)
            END IF

         END DO
      END DO  

!CC
!CC Write output array
!CC
      OPEN(FILE_UNIT3, FILE=FILENAME3, status='replace', action='write', iostat=ierr)
      WRITE(FILE_UNIT3,*) "Dist      DAC"
      DO I=1,IMAX
        WRITE(FILE_UNIT3,FMT2) I, OCCURENCE(I)
      END DO

! Deallocate memory
      deallocate(P1)
      deallocate(W1)
      deallocate(P2)
      deallocate(W2)
      deallocate(OCCURENCE)

      WRITE(*,*) '== Done =='

end program distance_autocorrelation
