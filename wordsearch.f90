
MODULE MODWORDS
INTEGER, PARAMETER :: COL=8
INTEGER, PARAMETER :: iwrd=8
Character(len=1) SPACE
 
 
character(len=14) :: swords(iwrd),TEMP
CHARACTER(LEN=1)  :: BOARD(COL,COL)
CHARACTER(LEN=1)  :: USED(26)
CHARACTER(LEN=26) :: CHARS
INTEGER :: IUSED
LOGICAL :: LBOARD(COL,COL)
data space/" "/
data swords/"bat","anmol","cat","song","carpenter","school","book","milk"/
END MODULE
 
program words
USE MODWORDS
implicit none
integer i
 
CALL INIT
CALL SHOW
 
PRINT *, USED(1:IUSED-1)
end program
 
 
SUBROUTINE SHOW
USE MODWORDS
 
DO I=1,COL
  DO J = 1,COL
    WRITE(*,'(A2)', ADVANCE='NO') BOARD(J,I)
  END DO
   WRITE(*,*)
END DO
 
WRITE(*,'(4(A14))') (SWORDS)
!CALL SHOW2
END 
 
 
SUBROUTINE SHOW2
USE MODWORDS
 
DO I=1,COL
  DO J = 1,COL
    WRITE(*,'(L2)', ADVANCE='NO') LBOARD(J,I)
  END DO
   WRITE(*,*)
END DO
 
 
END 
 
 
 
SUBROUTINE INIT
USE MODWORDS
 
CHARS='ABCDEFGHIJKLMNOPQRSTUVWXYZ'
USED=SPACE
IUSED=1
LBOARD =.FALSE.
! WLEN=LEN_TRIM(SWORDS(1))
 BOARD=SPACE
!call show
CALL FILLBOARD
 
END
 
 
FUNCTION ISUSEDORNOT(CHAR) RESULT(STER)
USE MODWORDS
LOGICAL STER
CHARACTER(LEN=1) CHAR
STER=.FALSE.
DO I=1,IUSED
 IF(USED(I) == CHAR) THEN
    STER =.TRUE.
    EXIT
 END IF
END DO
 
END FUNCTION
 
SUBROUTINE FILLBOARD
USE MODWORDS
REAL A,B(2)
INTEGER WLEN,X,Y
LOGICAL CHECKOK,SS,ISUSEDORNOT
!XXXXXXXXXX
!Y   4  3  2
!Y   5  %  1
!Y   6  7  8
!
CALL random_seed
 
DO I=1,iwrd
 WLEN=LEN_TRIM(SWORDS(I))
 do2:  do
 do1:  DO 
    call random_number(B)
    X=INT(B(1)*COL)+1
    Y=INT(B(2)*COL)+1
    call random_number(A)
    NUM= INT(A*8)+1
!    print*, x,y,num,wlen
    IF(.NOT. LBOARD(X,Y)) EXIT do1
   
  END DO  do1
   
   
!  print *,x,y,wlen,num
  SS= CHECKOK(X,Y,WLEN,NUM,I)
  IF(SS) THEN
  LBOARD(X,Y) =.TRUE.
  TEMP=SWORDS(I)
     BOARD(X,Y)=TEMP(1:1)
     IF(.NOT. ISUSEDORNOT(TEMP(1:1))) THEN
       USED(IUSED)=TEMP(1:1)
       IUSED=IUSED+1
     END IF  
!     print *, temp
    CALL FILLB(X,Y,WLEN,I,NUM)
      
!    call show
!    print *, swords(i),i,num
!    read *,a
  exit do2
     
  END IF
   
  end do do2
END DO
 
DO I=1,COL
  DO J=1,COL
    IF(BOARD(I,J) == SPACE) THEN
      call random_number(A)
      NUM=INT(A*(IUSED-1))+1
      BOARD(I,J)=USED(NUM)
      LBOARD(I,J)=.TRUE.
    END IF  
     
  END DO
 END DO
END 
 
SUBROUTINE FILLB(X,Y,WLEN,II,IDIR)
USE MODWORDS
 
INTEGER X,Y,WLEN,II,IDIR
LOGICAL ISUSEDORNOT
 
SELECT CASE(IDIR)
  
 CASE (1)
   XLIM=1
   YLIM=0
 
 CASE (2)
   XLIM=1
   YLIM=-1
 
 CASE (3)
   XLIM=0
   YLIM=-1
    
 CASE (4)
   XLIM=-1
   YLIM=-1
 
 CASE (5)
   XLIM=-1
   YLIM=0
 
 CASE (6)
   XLIM=-1
   YLIM=1
 
 CASE (7)
   XLIM=0
   YLIM=1
 
 CASE (8)
   XLIM=1
   YLIM=1
  
 END SELECT
  
 DO I=2,WLEN
    X=X+XLIM
    Y=Y+YLIM
    BOARD(X,Y)=TEMP(I:I)
    LBOARD(X,Y) =.TRUE.
    IF(.NOT. ISUSEDORNOT(TEMP(I:I))) THEN
       USED(IUSED)=TEMP(I:I)
       IUSED=IUSED+1
    END IF
 END DO
 
END
 
FUNCTION CHECKOK(X,Y,WLEN,IDIR,IWORD) RESULT(SRET)
USE MODWORDS 
 LOGICAL SRET
 INTEGER X,Y,WLEN,IDIR,IWORD
 INTEGER XLIM,YLIM,X1,Y1,XLIM1,YLIM1,I
  
 
! 1(1,0)  2(1,-1) 3(0,-1) 4(-1,-1)
! 5(-1,0) 6(-1,1) 7(0,1) 8(1,1)
  
 SRET=.TRUE.
 SELECT CASE(IDIR)
  
 CASE (1)
   XLIM=1
   YLIM=0
 
 CASE (2)
   XLIM=1
   YLIM=-1
 
 CASE (3)
   XLIM=0
   YLIM=-1
    
 CASE (4)
   XLIM=-1
   YLIM=-1
 
 CASE (5)
   XLIM=-1
   YLIM=0
 
 CASE (6)
   XLIM=-1
   YLIM=1
 
 CASE (7)
   XLIM=0
   YLIM=1
 
 CASE (8)
   XLIM=1
   YLIM=1
  
 END SELECT
 XLIM1=XLIM
 YLIM1=YLIM
  
 XLIM=X+XLIM*WLEN
 YLIM=Y+YLIM*WLEN
! print *, 'xlim ',xlim,ylim,sret
 IF(((XLIM .GT. COL) .or. (XLIM .LE. 0)) .or.  ((YLIM .GT. COL) .or. (YLIM .LE. 0))) THEN
   SRET=.FALSE.
   RETURN
 END IF
  
 IF(.NOT. SRET) RETURN
  
 TEMP=SWORDS(IWORD)
 X1=X
 Y1=Y
! print *,'x y ',x,y,wlen,idir,xlim1,ylim1,sret
 DO I=2,WLEN
   X1=X1+XLIM1
   Y1=Y1+YLIM1
!   IF((TEMP(I:I) == BOARD(X1,Y1)) .OR. LBOARD(X1,Y1)) THEN
    IF(LBOARD(X1,Y1)) THEN
      SRET=.FALSE.
!      print *, 'heelo ',temp,x1,y1
      RETURN
   END IF   
 END DO
  
  
END FUNCTION