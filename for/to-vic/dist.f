      PROGRAM READ_FILE
C     Declare variables
      PARAMETER (N=50000)
      PARAMETER (M=50000)
      PARAMETER (L=2000)

      REAL X(N,2), Y(M,2), OCCURENCE(L)
C      REAL P1(N), W1(N), P2(M), W2(M), OCCURENCE(L)
      INTEGER I, J, STATUS, L, IMAX, NC, MC, DIST
      CHARACTER*20 FILENAME1, FILENAME2, FILENAME3
      INTEGER FILE_UNIT1, FILE_UNIT2, FILE_UNIT3

      FILE_UNIT1=10
      FILE_UNIT2=20
      FILE_UNIT3=30

      NC=0
      MC=0

C     set IMAX = L; L is the number of lines for output
      IMAX=L

C     Prompt the user to enter the input filename1
      WRITE(*,*) 'Enter the filename1: '
      READ(*,*) FILENAME1

C     Prompt the user to enter the input filename2
      WRITE(*,*) 'Enter the filename2: '
      READ(*,*) FILENAME2

C     Prompt the user to enter the output filename
      WRITE(*,*) 'Enter the output filename: '
      READ(*,*) FILENAME3

CCCCCCCCCCCCCCCCCCCCCCCCCCCC 
C     Open the file1
      OPEN(FILE_UNIT1, FILE=FILENAME1, STATUS='OLD', IOSTAT=STATUS)
      IF (STATUS.NE.0) THEN
        WRITE(*,*) 'Error: could not open file'
        STOP
      END IF

C     Skip header line
      READ(FILE_UNIT1,*)
C     Read the data from the file into the array
      DO I=1,N
C     set NC to count lines read
        NC=NC+1
        READ(FILE_UNIT1,*,IOSTAT=STATUS) X(I,1), X(I,2)
        IF (STATUS.NE.0) EXIT
      END DO
C     Close the file
      CLOSE(FILE_UNIT1)
CCCCCCCCCCCCCCCCCCCCCCCCCCCC 
C     Open the file2
      OPEN(FILE_UNIT2, FILE=FILENAME2, STATUS='OLD', IOSTAT=STATUS)
      IF (STATUS.NE.0) THEN
        WRITE(*,*) 'Error: could not open file'
        STOP
      END IF

C     Skip header line
      READ(FILE_UNIT2,*)
C     Read the data from the file into the array
      DO I=1,M
C     set MC to count lines read
        MC=MC+1
        READ(FILE_UNIT2,*,IOSTAT=STATUS) Y(I,1), Y(I,2)
        IF (STATUS.NE.0) EXIT
      END DO
C     Close the file
      CLOSE(FILE_UNIT2)
CCCCCCCCCCCCCCCCCCCCCCCCCCCC 

C     Print the contents of the array
C     WRITE(*,*) 'The contents of the array are:'
C     DO I=1,NC-1
C       WRITE(*,*) X(I,1), X(I,2), Y(I,1), Y(I,2)
C     END DO

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CC   Calculate Distance and Occurance              CCC
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      DO I=1,NC-1
         DO J=1,MC-1
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CC         Calculate Distance                      CCC
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
            DIST = Y(J,1) - X(I,1) + 1
      
            IF ((DIST .GE. 0) .AND. (DIST .LE. IMAX)) THEN
                 OCCURENCE(DIST) = OCCURENCE(DIST) + X(I,2) * Y(J,2)
            END IF

         END DO
      END DO  

CC   
CC Write output array
CC
      OPEN(FILE_UNIT3, FILE=FILENAME3, status='replace', action='write')
      WRITE(FILE_UNIT3,*) "Dist      DAC"
      DO I=1,IMAX
        WRITE(FILE_UNIT3,'(I5, F12.3)') I, OCCURENCE(I)
      END DO

      END

