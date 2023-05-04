      PROGRAM distance_autocorrelation
        
      INTEGER I, J, K, EOF
      CHARACTER*20 FNAME1, FNAME2,FNAME3
      
      REAL P(10000,2), W(10000,2)
        
      WRITE(*,*) 'Enter input file 1:'
      READ(*,*) FNAME1
      OPEN(10,FILE=FNAME1,STATUS='OLD')
        
      WRITE(*,*) 'Enter input file 2:'
      READ(*,*) FNAME2
      OPEN(20,FILE=FNAME2,STATUS='OLD')
        
C      WRITE(*,*) 'Enter output file:'
C      READ 'A', FNAME3
C      OPEN(30,FILE=FNAME3,STATUS='OLD')

C        
      READ(15,*,IOSTAT=EOF)
      I=1
        
   15 IF (EOF .GE. 0) THEN
         READ(10, *, IOSTAT=EOF) P(I,1), P(I,2)   
         I = I + 1
      END IF
      GO TO 15
      
      READ(25,*,IOSTAT=EOF)
      I=1
        
   25 IF (EOF .GE. 0) THEN
         READ(20, *, IOSTAT=EOF) W(I,1), W(I,2)   
         I = I + 1
      END IF
      GO TO 25
      
      WRITE(*,*) P(5,1), P(5,2)
      WRITE(*,*) W(6,1), W(6,2)
      
      CLOSE(30)
      CLOSE(20)
      CLOSE(10)
      END     
