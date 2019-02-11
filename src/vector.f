C SUBPROGRAMS FOR VECTOR MANIPULATION.
C
C DOT PRODUCT.
C
      DOUBLE PRECISION FUNCTION DOT(A,B)
      REAL*8 A(3),B(3)
      DOT=A(1)*B(1)+A(2)*B(2)+A(3)*B(3)
      RETURN
      END
C
C CROSS PRODUCT.
C
      SUBROUTINE CROSS(A,B,C)
      REAL*8 A(3),B(3),C(3)
      C(1)=A(2)*B(3)-A(3)*B(2)
      C(2)=A(3)*B(1)-A(1)*B(3)
      C(3)=A(1)*B(2)-A(2)*B(1)
      RETURN
      END
C
C ADDITION.
C
      SUBROUTINE VADD(A,B,C)
      REAL*8 A(3),B(3),C(3)
      C(1)=A(1)+B(1)
      C(2)=A(2)+B(2)
      C(3)=A(3)+B(3)
      RETURN
      END
C
C Subtraction.
C
      SUBROUTINE VSUB(A,B,C)
      REAL*8 A(3),B(3),C(3)
      C(1)=A(1)-B(1)
      C(2)=A(2)-B(2)
      C(3)=A(3)-B(3)
      RETURN
      END
C
C SCALAR MULTIPLICATION.
C
      SUBROUTINE MSCALR(A,X,C)
      REAL*8 A(3),X,C(3)
      C(1)=X*A(1)
      C(2)=X*A(2)
      C(3)=X*A(3)
      RETURN
      END
C
C NORMALIZATION.
C
      SUBROUTINE NORM(A)
      IMPLICIT REAL*8 (A-Z)
      REAL*8 A(3),C(3),X,Dot
      X=DOT(A,A)
      IF(X.NE.0.0D0) THEN
         X=1.0D0/DSQRT(X)
         CALL MSCALR(A,X,C)
         CALL EQUATE(A,C)
      ENDIF
      RETURN
      END
C
C EQUATE A TO B.
C
      SUBROUTINE EQUATE(A,B)
      REAL*8 A(3),B(3)
      A(1)=B(1)
      A(2)=B(2)
      A(3)=B(3)
      RETURN
      END
C
C IS A VECTOR NON-ZERO to within DZERO?
C
      LOGICAL FUNCTION NONZIP(A)
      IMPLICIT REAL*8 (A-Z)
      REAL*8 A(3)
      COMMON/UTILTY/DZERO, XNULL(4), PI
      NONZIP=DOT(A,A) .GT. DZERO**2
      RETURN
      END
C
C IS A VECTOR ZERO to within DZERO?
C
      LOGICAL FUNCTION ZIP(A)
      IMPLICIT REAL*8 (A-Z)
      REAL*8 A(3)
      COMMON/UTILTY/DZERO, XNULL(4), PI
      ZIP=DOT(A,A) .LE. DZERO**2
      RETURN
      END
C
C CONVERT FROM POLAR TO CARTESIAN.
C
      SUBROUTINE UNIT(T,P,X)
      IMPLICIT REAL*8 (A-Z)
      REAL*8 X(3)
      COMMON/UTILTY/DZERO, XNULL(4), PI
      TH=T*PI/180.0D0
      PH=P*PI/180.0D0
      SINTH=DSIN(TH)
      X(1)=SINTH*DCOS(PH)
      X(2)=SINTH*DSIN(PH)
      X(3)=DCOS(TH)
      CALL NORM(X)
      RETURN
      END
C
C FIND THE COSINE OF THE ANGLE BETWEEN TWO VECTORS.
C
      DOUBLE PRECISION FUNCTION VCOS(A,B)
      REAL*8 A(3),B(3),WORK1(3),WORK2(3),Dot
      CALL EQUATE(WORK1,A)
      CALL EQUATE(WORK2,B)
      CALL NORM(WORK1)
      CALL NORM(WORK2)
      VCOS=DOT(WORK1,WORK2)
      RETURN
      END
C
C FIND THE ANGLE BETWEEN TWO VECTORS.
C
      DOUBLE PRECISION FUNCTION ANGLE(A,B)
      IMPLICIT REAL*8 (A-Z)
      REAL*8 A(3),B(3)
      COMMON/UTILTY/DZERO, XNULL(4), PI
      CC=VCOS(A,B)
      if(cc.gt.1.0d0 .and. cc.lt.1.001d0) cc=1.0d0
      ANGLE=DACOS(CC)*180.0D0/PI
      RETURN
      END
C
C See if two vectors are equivalant to within DZERO.
C
      LOGICAL FUNCTION VEQ(A,B)
      IMPLICIT REAL*8 (A-Z)
      REAL*8 A(3),B(3)
      COMMON/UTILTY/DZERO, XNULL(4), PI
      VEQ = ( DABS(A(1)-B(1)) .LT. DZERO .AND.
     &        DABS(A(2)-B(2)) .LT. DZERO .AND.
     &        DABS(A(3)-B(3)) .LT. DZERO )
      RETURN
      END

