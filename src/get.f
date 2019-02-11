      REAL FUNCTION RGET(VAR)
      CHARACTER VAR(80)
      DO 10 I=1,80
         IF(VAR(I).EQ.'^') GO TO 14
10    CONTINUE
14    WRITE(0,100) (VAR(J),J=1,I-1)
100   FORMAT(' *Enter ',80A1)
      READ(5,*) RGET
      RETURN
      END
C
      DOUBLE PRECISION FUNCTION DGET(VAR)
      CHARACTER VAR(80)
      DO 10 I=1,80
         IF(VAR(I).EQ.'^') GO TO 14
10    CONTINUE
 14   WRITE(0,100) (VAR(J),J=1,I-1)
100   FORMAT(' *Enter ',80A1)
      READ(5,*) DGET
      RETURN
      END
C
      INTEGER FUNCTION IGET(VAR)
      CHARACTER VAR(80)
      DO 10 I=1,80
         IF(VAR(I).EQ.'^') GO TO 14
10    CONTINUE
 14   WRITE(0,100) (VAR(J),J=1,I-1)
100   FORMAT(' *Enter ',80A1)
      READ(5,*) IGET
      RETURN
      END
C
      LOGICAL FUNCTION LGET(VAR)
      CHARACTER*1 VAR(80),ANSWER
      DO 10 I=1,80
         IF(VAR(I).EQ.'^') GO TO 14
10    CONTINUE
14    WRITE(0,100) (VAR(J),J=1,I-1)
100   FORMAT(' *',80A1,'  (y or n) ')
      READ(5,1000) ANSWER
1000  FORMAT(1a)
      IF(ANSWER.EQ.'Y' .or. ANSWER.EQ.'y') THEN
         LGET=.TRUE.
      ELSE IF (ANSWER.EQ.'N' .or. ANSWER.EQ.'n') THEN
         LGET=.FALSE.
      ELSE
         WRITE(0,200)
200      FORMAT(' --> Enter y or n, please: ')
         GO TO 14
      ENDIF
      RETURN
      END
C
      CHARACTER*1 FUNCTION CGET(VAR)
      CHARACTER*1 VAR(80)
      DO 10 I=1,80
         IF(VAR(I).EQ.'^') GO TO 14
10    CONTINUE
 14   WRITE(0,100) (VAR(J),J=1,I-1)
100   FORMAT(' *Enter ',80A1)
      READ(5,1000) CGET
1000  FORMAT(A1)
      RETURN
      END
C
      SUBROUTINE SGET(VAR, ANSWER)
      CHARACTER*1 VAR(80)
      CHARACTER*(*) ANSWER
      DO 10 I=1,80
         IF(VAR(I).EQ.'^') GO TO 14
10    CONTINUE
 14   WRITE(0,100) (VAR(J),J=1,I-1)
100   FORMAT(' *Enter ',80A1)
      READ(5,1000) ANSWER
1000  FORMAT(A)
      RETURN
      END
