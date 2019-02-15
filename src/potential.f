C***********************************************************************
C
C POTENTIAL ENERGY FOR TWO-EXPONENTIAL OR ZBL OR EXPONENTIAL+GAUSSIAN
C
C
C CAK 19jan95 - error in VR(R,NV) fixed
C
      DOUBLE PRECISION FUNCTION DVDR(R,NV)
      IMPLICIT REAL*8 (A-H,O-Z)
      integer charge(10)
      REAL*8 POTPAR(30),PIMPAR(10)
      COMMON/POTPAR/POTPAR,PIMPAR,IPOT,IIMPOT
      COMMON/CHARGE/CHARGE
      DATA ESQR/14.398D0/
C  ZBL SCREENING FUNCTION.
      PHI(X)=.1818D0*DEXP(-3.2D0*X)
     1   +.2802D0*DEXP(-.4029D0*X)
     2   +0.02817D0*DEXP(-.2016D0*X)
     3   +0.5099D0*DEXP(-0.9423D0*X)
C  DERIVATIVE OF SCREENING FUNCTION WRT X.
      PPRIME(X)=-.58176D0*DEXP(-3.2D0*X)
     1   -.112893D0*DEXP(-.4029D0*X)
     2   -0.005679D0*DEXP(-.2016D0*X)
     3   -0.48048D0*DEXP(-0.9423D0*X)
C
      IF(IPOT.EQ.1) THEN
C TWO-EXPONENTIAL POTENTIAL
         INDEX=4*(NV-1)
         A1=POTPAR(INDEX+1)
         B1=POTPAR(INDEX+2)
         A2=POTPAR(INDEX+3)
         B2=POTPAR(INDEX+4)
         DVDR=-B1*A1*DEXP(-B1*R)-B2*A2*DEXP(-B2*R)
      ELSE IF(IPOT.EQ.2) THEN
C ZBL TYPE POTENTIAL
C
         zion=potpar(1)
         SCREEN=POTPAR(NV+1)
c
         Zatom=1.0D0*CHARGE(NV)
         IF(R.EQ.0.0D0) THEN
            DVDR=0.0D0
         ELSE
            X=R/SCREEN
            PP=PHI(X)
            PPP=PPRIME(X)
            ZZ=Zatom*Zion
            DVDR=ZZ*ESQR/(R*R)*(PPP*X-PP)
         ENDIF
      ELSE IF(IPOT.EQ.3) THEN
C EXPONENTIAL PLUS GAUSSIAN POTENTIAL
         INDEX=4*(NV-1)
         A1=POTPAR(INDEX+1)
         B1=POTPAR(INDEX+2)
         A2=POTPAR(INDEX+3)
         B2=POTPAR(INDEX+4)
         DVDR=-B1*A1*DEXP(-B1*R)-2.0D0*B2*A2*R*DEXP(-B2*R*R)
      ELSE
         write(0,*) 'unacceptable value for ipot'
         stop
      ENDIF
      RETURN
      END
C****************************************************
      DOUBLE PRECISION FUNCTION VR(R,NV)
      IMPLICIT REAL*8 (A-H,O-Z)
      REAL*8 POTPAR(30),PIMPAR(10)
      integer charge(10)
      COMMON/POTPAR/POTPAR,PIMPAR,IPOT,IIMPOT
      COMMON/CHARGE/CHARGE
      DATA ESQR/14.398D0/
C  ZBL SCREENING FUNCTION.
      PHI(X)=.1818D0*DEXP(-3.2D0*X)
     1   +.2802D0*DEXP(-.4029D0*X)
     2   +0.02817D0*DEXP(-.2016D0*X)
     3   +0.5099D0*DEXP(-0.9423D0*X)
C  DERIVATIVE OF SCREENING FUNCTION WRT X.
      PPRIME(X)=-.58176D0*DEXP(-3.2D0*X)
     1   -.112893D0*DEXP(-.4029D0*X)
     2   -0.005679D0*DEXP(-.2016D0*X)
     3   -0.48048D0*DEXP(-0.9423D0*X)
C
      IF(IPOT.EQ.1) THEN
C TWO-EXPONENTIAL POTENTIAL
         INDEX=4*(NV-1)
         A1=POTPAR(INDEX+1)
         B1=POTPAR(INDEX+2)
         A2=POTPAR(INDEX+3)
         B2=POTPAR(INDEX+4)
         VR=A1*DEXP(-B1*R)+A2*EXP(-B2*R)
      ELSE IF(IPOT.EQ.2) THEN
C ZBL TYPE POTENTIAL
         INDEX=2*(NV-1)
C
         zion=potpar(1)
         SCREEN=POTPAR(index+2)
c
         Zatom=1.0D0*CHARGE(NV)
         IF(R.EQ.0.0D0) THEN
            VR=0.0D0
         ELSE
            X=R/SCREEN
            PP=PHI(X)
            ZZ=Zatom*Zion
C
C  this potential is wrong.  Changed 19jan95 by CAK
C            VR=ZZ*ESQR/(R*R)*PP
            VR=ZZ*ESQR/R*PP
         ENDIF
      ELSE IF(IPOT.EQ.3) THEN
C EXPONENTIAL PLUS GAUSSIAN POTENTIAL
         INDEX=4*(NV-1)
         A1=POTPAR(INDEX+1)
         B1=POTPAR(INDEX+2)
         A2=POTPAR(INDEX+3)
         B2=POTPAR(INDEX+4)
         VR=A1*DEXP(-B1*R)+A2*EXP(-B2*R*R)
      ELSE
         write(0,*) 'unacceptable value for ipot'
         stop
      ENDIF
      RETURN
      END

C***********************************************************************
C
C IMAGE CHARGE POTENTIAL
C
      DOUBLE PRECISION FUNCTION VIMZ(Z)
      REAL*8 Z,ZMIN,VMIN,ESQR,POTPAR(30),PIMPAR(10)
      COMMON/POTPAR/POTPAR,PIMPAR,IPOT,IIMPOT
      DATA ESQR/14.398D0/
C E**2=HBAR*C*ALPHA=1973/137 EV-A.
      IF(IIMPOT.EQ.1) THEN
         ZMIN=PIMPAR(1)
         VMIN=PIMPAR(2)
         IF(Z.GT.ZMIN) THEN
            VIMZ=-.25D0*ESQR/DSQRT((Z-ZMIN)*(Z-ZMIN)
     1                    +(.25D0*ESQR/VMIN)*(.25D0*ESQR/VMIN))
         ELSE
            VIMZ=-VMIN
         ENDIF
      ELSE
         write(0,*) 'unacceptable value for iimpot'
         stop
      ENDIF
      RETURN
      END
C
      DOUBLE PRECISION FUNCTION DVIMDZ(Z)
      REAL*8 Z,ZMIN,VMIN,ESQR,POTPAR(30),PIMPAR(10)
      COMMON/POTPAR/POTPAR,PIMPAR,IPOT,IIMPOT
      DATA ESQR/14.398D0/
C E**2=HBAR*C*ALPHA=1973/137 EV-A.
      IF(IIMPOT.EQ.1) THEN
         ZMIN=PIMPAR(1)
         VMIN=PIMPAR(2)
         IF(Z.GT.ZMIN) THEN
            DVIMDZ=(Z-ZMIN)*(Z-ZMIN)+(.25D0*ESQR/VMIN)*(.25D0*ESQR/VMIN)
            DVIMDZ=.25D0*ESQR*(Z-ZMIN)/((DVIMDZ)**1.5)
         ELSE
            DVIMDZ=0.0D0
         ENDIF
      ELSE
         write(0,*) 'unacceptable value for ipot'
         stop
      ENDIF
      RETURN
      END
C************************************
      SUBROUTINE POTSET
      CHARACTER*70 POTENT
      CHARACTER*36 POTTEMP
      REAL*8 POTPAR(30),PIMPAR(10)
      COMMON/POTPAR/POTPAR,PIMPAR,IPOT,IIMPOT
      COMMON/POTENT/POTENT
      IF(IPOT.EQ.1) THEN
         POTTEMP='TWO-EXPONENTIAL POTENTIAL'
      ELSE IF(IPOT.EQ.2) THEN
         POTTEMP='ZIEGLER-BIERSACK-LITTMARK POTENTIAL'
      ELSE IF(IPOT.EQ.3) THEN
         POTTEMP='EXPONENTIAL+GAUSSIAN POTENTIAL'
      ENDIF
      IF(IIMPOT.EQ.1) THEN
         POTENT=POTTEMP//' WITH FLAT IMAGE'
      ELSE IF(IIMPOT.EQ.2) THEN
         POTENT=POTTEMP//' WITH CORRUGATED IMAGE'
      ENDIF
      RETURN
      END
C***********************************************************************
C
C DERIVATIVE OF INDUCED DIPLOE POTENTIAL ENERGY WITH RESPECT TO R AS A
C FUNCTION OF R.
C
      DOUBLE PRECISION FUNCTION DVINDR(R)
      IMPLICIT REAL*8 (A-H,O-Z)
      REAL*8 POTPAR(30),PIMPAR(10)
      COMMON/POTPAR/POTPAR,PIMPAR,IPOT,IIMPOT
C
C NOTE THAT PIMPAR(2), UNLIKE OTHER POTENTIAL PARAMETERS, IS NOT ENTERED
C BY THE USER, BUT CALCULATED IN THE CORRUGATED POTENTIAL INITIALIZATION
      IF(IIMPOT.EQ.2) THEN
         ALPHR=PIMPAR(1)*R
         DVINDR= 1.0D0 +ALPHR +ALPHR**2/2.0D0 +ALPHR**3/6.0D0
         DVINDR= 1.0D0 -EXP(-ALPHR)*DVINDR
         DVINDR= ALPHR**4*EXP(-ALPHR)/6.0D0 -4.0D0*DVINDR
         DVINDR= -PIMPAR(2)*DVINDR /R**5
      ELSE
         write(0,*) 'unacceptable value for ipot'
         stop
      ENDIF
      RETURN
      END
