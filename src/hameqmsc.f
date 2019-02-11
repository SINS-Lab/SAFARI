             SUBROUTINE HAMEQ(X,Y,Z,PX,PY,PZ,XAT,YAT,ZAT,
     1                          PXAT,PYAT,PZAT,NPART,ii)
C

c handles correlated atom-atom forces
C EVALUATE HAMILTON'S EQUATIONS FOR NPART PARTICLES.
C
C
      IMPLICIT REAL*8 (A-H,O-Z)
      INTEGER*4 KALL
C
      REAL*8 X,Y,Z,PX,PY,PZ,XAT(100),YAT(100),ZAT(100),
     1                          PXAT(100),PYAT(100),PZAT(100)
        REAL*8 XLAT(100,3),SPRING(10,3)
      REAL*8 DX,DY,DZ,DPX,DPY,DPZ,DXAT(100),DYAT(100),DZAT(100),
     1                          DPXAT(100),DPYAT(100),DPZAT(100)
      REAL*8 MION,MASS(10)
        REAL*8 MION1,MASS1(10),MINVV
      REAL*8 F
C               F IS A FORCE.
      logical pt
      REAL*8 XA,YA,ZA
C           XA IS THE ION-ATOM DISPLACEMENT.
      INTEGER TYPEAT(100)
       LOGICAL IMAGE
        real*8 atomk,rneigh
c rneigh is nearest-neighbor separation squared
        logical corr,near
c if corr eq true, use correlated springs to compute atom motion
C
      COMMON/MASS/MASS,MION,TYPEAT
        COMMON/MINV/MASS1,MION1
      COMMON/HAM/DX,DY,DZ,DPX,DPY,DPZ,DXAT,DYAT,DZAT,DPXAT,DPYAT,DPZAT
        COMMON/LAT/XLAT
        COMMON/K/SPRING
        COMMON/KALL/KALL
        COMMON/IMAGE/IMAGE
        common/corr/atomk,rneigh,corr
        common/vtot/vtot
        KALL=KALL+1
        vtot=0.0d0
C
C
C DX/DT = P/M
            DX=PX*MION1
            DY=PY*MION1
            DZ=PZ*MION1
C

C LOOP THROUGH ATOMS. SUM UP THE FORCES ON THE ATOMS TO GET THE
C FORCE ON THE ION.
C     DPX IS THE X COMPONENT OF THE FORCE (DPX/DT) ON THE ION.
      DPX=0.0D0
      DPY=0.0D0
      DPZ=0.0D0
CVDIR ASSUME COUNT(6)
      DO 111 L=1,NPART
          DPXAT(L)=0.0D0
          DPYAT(L)=0.0D0
          DPZAT(L)=0.0D0
 111  CONTINUE
      DO 100 J=1,NPART
            NAT=TYPEAT(J)
            MINVV=MASS1(NAT)
C           DX/DT FOR SURFACE ATOMS.
            DXAT(J)=PXAT(J)*MINVV
            DYAT(J)=PYAT(J)*MINVV
            DZAT(J)=PZAT(J)*MINVV
C           F=DP/DT=-DV/DX.
C           XA(1) IS THE X COMPONENT OF THE DISTANCE BETWEEN ION AND ATO
            XA=XAT(J)-X
            YA=YAT(J)-Y
            ZA=ZAT(J)-Z
c
c
c
C               RR IS THE SQUARE OF THE DISTANCE TO THE ION.
C      WRITE(6,*) 'J= ',J
C      WRITE(6,*) 'XA = ',XA
            RR=XA*XA+YA*YA+ZA*ZA
C           F IS A COMPONENT OF THE FORCE ON THE ION.
C               IF ION AND ATOM ARE AT THE SAME PLACE, F=0 BY SYMMETRY.
                IF(RR.EQ.0.0D0) THEN
                 F=0.0D0
                 DPXAT(J)=DPXAT(J)+F
                 DPYAT(J)=DPYAT(J)+F
                 DPZAT(J)=DPZAT(J)+F
                ELSE
C                       USE DV/DX=(DV/DR**2)*DR**2/DX = (DV/DR**2)*2X.
                        DDR=-2.0D0*DDVDR(RR,NAT)
                        F=DDR*XA
                DPXAT(J)=DPXAT(J)+F
                        DPX=DPX-F
                        F=DDR*YA
                DPYAT(J)=DPYAT(J)+F
                        DPY=DPY-F
                        F=DDR*ZA
                DPZAT(J)=DPZAT(J)+F
                        DPZ=DPZ-F
                endif
c compute energy if ii.eq.2
        if(ii.eq.2) then
            vtot=vtot+VV(rr,nat)
        endif
 9999           if(corr) then
c Now include spring forces
c if atomk.eq.0d0 then use einstein springs
                 if(atomk.le.1.d-30) then
c use einstein spring
              dpxat(j)=dpxat(j)-spring(nat,1)*(xat(j)-xlat(j,1))
              dpyat(j)=dpyat(j)-spring(nat,2)*(yat(j)-xlat(j,2))
              dpzat(j)=dpzat(j)-spring(nat,3)*(zat(j)-xlat(j,3))
                 else
            DO 400 I=J+1,NPART
c R0 is the equillibrium spacing
            R0=(XLAT(J,1)-XLAT(I,1))**2+(XLAT(J,2)-XLAT(I,2))**2
     &         +(XLAT(J,3)-XLAT(I,3))**2
            if(r0 .le. rneigh) then
             RR0=dsqrt(R0)
             near=.true.
	    else
	     near=.false.
            endif
c
c now, the magnitude of the force on i due to j is equal to
c the force on j due to i
c the force magnitude is
            IF(NEAR) THEN
c need distance between i and j
         rrat=dsqrt((xat(j)-xat(i))**2+(yat(j)-yat(i))**2+
     &              (zat(j)-zat(i))**2)
c compute magnitude of force and include division by rr
c here atomk is the spring constant between the neighbors
          fmag=atomk*(RR0/rrat - 1.0d0)
c compute force components
             DPXJ=fmag*(xat(j)-xat(i))
             DPYJ=fmag*(yat(j)-yat(i))
             DPZJ=fmag*(zat(j)-zat(i))
             DPXAT(J)=DPXAT(J)+DPXJ
             DPYAT(J)=DPYAT(J)+DPYJ
             DPZAT(J)=DPZAT(J)+DPZJ
             DPXAT(I)=DPXAT(I)-DPXJ
             DPYAT(I)=DPYAT(I)-DPYJ
             DPZAT(I)=DPZAT(I)-DPZJ
            ENDIF
 400   CONTINUE
       endif
       endif
100   CONTINUE
        IF(IMAGE) THEN
C     INCLUDE IMAGE CHARGE FORCE.
         DPZ=DPZ-DDVDZ(Z)
       ENDIF
C
      RETURN
      END
C
C***********************************************************************
C
      DOUBLE PRECISION FUNCTION VV(RR,NV)
C
C COMPUTE THE POTENTIAL ENERGY OF AN ION AND AN ATOM BY LOOKING IT UP
C IN THE TABLE TV AND INTERPOLATING IF POSSIBLE, OR BY CALCULATING IT
C BY BRUTE FORCE.
C RR IS THE SQUARE OF THE DISTANCE TO THE ION.
C
        PARAMETER (NTABLE=30000)
      IMPLICIT REAL*8 (A-H,O-Z)
      REAL*8 TDVDR(NTABLE,3),TDIMDZ(NTABLE),VVTAB(NTABLE,3)
      COMMON/TABLES/TDVDR,TDIMDZ,RRMIN,RRSTEP,ZMIN,ZSTEP,NTAB
       COMMON/STPINV/STPINV
       COMMON/VVTAB/VVTAB
C
      IF(NTAB.EQ.0 .OR.RR.LT.RRMIN.OR.RR.GT.RRMIN+(NTAB-1)*RRSTEP) THEN
            VV=V(DSQRT(RR),NV)
      ELSE
C           INTERPOLATE IN THE TABLE.
            WHERE=(RR-RRMIN)*STPINV+1.0D0
            NR=INT(WHERE)
            FRACT=WHERE-NR
            VV=(1.0D0-FRACT)*VVTAB(NR,NV)+FRACT*VVTAB(NR+1,NV)
      ENDIF
      RETURN
      END
C
C
C***********************************************************************
C
      DOUBLE PRECISION FUNCTION VVIM(Z)
C
C COMPUTE THE POTENTIAL ENERGY OF AN ION DUE TO BULK EFFECTS BY LOOKING
C IN THE TABLE TVIM AND INTERPOLATING, IF POSSIBLE, OR BY CALCULATING IT
C BY BRUTE FORCE.
C
        PARAMETER (NTABLE=30000)
      IMPLICIT REAL*8 (A-H,O-Z)
      REAL*8 TDVDR(NTABLE,3),TDIMDZ(NTABLE)
      COMMON/TABLES/TDVDR,TDIMDZ,RMIN,RSTEP,ZMIN,ZSTEP,NTAB
       COMMON/ZINV/ZINV
C
            VVIM=VIM(Z)
      RETURN
      END
C
C
C***********************************************************************
C
      DOUBLE PRECISION FUNCTION DDVDZ(Z)
C
C COMPUTE THE DERIVATIVE OF THE POTENTIAL ENERGY OF AN ION DUE TO BULK E
C BY LOOKING IT UP IN THE TABLE TDIMDZ AND INTERPOLATING, IF POSSIBLE,
C OR BY CALCULATING IT BY BRUTE FORCE.
C
        PARAMETER (NTABLE=30000)
      IMPLICIT REAL*8 (A-H,O-Z)
      REAL*8 TDVDR(NTABLE,3),TDIMDZ(NTABLE)
      COMMON/TABLES/TDVDR,TDIMDZ,RMIN,RSTEP,ZMIN,ZSTEP,NTAB
       COMMON/ZINV/ZINV
C
      IF(NTAB.EQ.0 .OR. Z.LT.ZMIN .OR. Z.GT.ZMIN+(NTAB-1)*ZSTEP) THEN
C           DVIMDZ(Z) IS NOT IN THE TABLE.
            DDVDZ=DVIMDZ(Z)
      ELSE
C           INTERPOLATE IN THE TABLE.
C           WHERE=(Z-ZMIN)/ZSTEP+1.0D0
            WHERE=(Z-ZMIN)*ZINV+1.0D0
            NZ=INT(WHERE)
            FRACT=WHERE-NZ
            DDVDZ=(1.0D0-FRACT)*TDIMDZ(NZ)+FRACT*TDIMDZ(NZ+1)
      ENDIF
      RETURN
      END
C
C
C***********************************************************************
C
      DOUBLE PRECISION FUNCTION DDVDR(RR,NV)
C
C COMPUTE THE DERIVATIVE WRT R**2 OF ION-ATOM POTENTIAL ENERGY BY LOOKIN
C IN THE TABLE TDVDR AND INTERPOLATING, IF POSSIBLE, OR BY CALCULATING I
C BY BRUTE FORCE.
C
        PARAMETER (NTABLE=30000)
      IMPLICIT REAL*8 (A-H,O-Z)
      REAL*8 TDVDR(NTABLE,3),TDIMDZ(NTABLE)
      COMMON/TABLES/TDVDR,TDIMDZ,RRMIN,RRSTEP,ZMIN,ZSTEP,NTAB
       COMMON/STPINV/STPINV
C
      IF(NTAB.EQ.0 .OR. RR.LT.RRMIN .OR.
     1          RR.GT.RRMIN+(NTAB-1)*RRSTEP) THEN
C           DVDR(RR) IS NOT IN THE TABLE.
                DDVDR=DVDR2(RR,NV)
      ELSE
C           INTERPOLATE IN THE TABLE.
C           WHERE=(RR-RRMIN)/RRSTEP+1.0D0
            WHERE=(RR-RRMIN)*STPINV+1.0D0
            NR=INT(WHERE)
            FRACT=WHERE-NR
            DDVDR=(1.0D0-FRACT)*TDVDR(NR,NV)+FRACT*TDVDR(NR+1,NV)
      ENDIF
      RETURN
      END
C
C
C***********************************************************************
C
        DOUBLE PRECISION FUNCTION V2(RR,NV)
C
C COMPUTE THE ION-ATOM POTENTIAL AS A FUNCTION OF R**2.
C
        REAL*8 RR,V
        V2=V(DSQRT(RR),NV)
        RETURN
        END
C
C***********************************************************************
C
        DOUBLE PRECISION FUNCTION DVDR2(RR,NV)
C
C COMPUTE THE DERIVATIVE WRT R**2 OF THE ION ATOM POTENTIAL, AS A FUNCTI
C OF R**2.
        REAL*8 RR,R,DVDR
        IF(RR.NE.0.0D0) THEN
                R=DSQRT(RR)
                DVDR2=DVDR(R,NV)*.5D0/R
        ELSE
                DVDR2=0.0D0
        ENDIF
        RETURN
        END
C
C
C***********************************************************************
      SUBROUTINE TABLE(F,XMIN,XSTEP,N,FTAB,NV)
C
C ROUTINE TO CREATE A TABLE OF N VALUES OF THE FUNCTIONS
C F AND G STARTING AT XMIN AND INCREMENTING BY XSTEP.
C
      IMPLICIT REAL*8 (A-H,O-Z)
      REAL*8 FTAB(N)
      EXTERNAL F
C
      X=XMIN
        IF(NV.NE.0) THEN
        DO 10 I=1,N
                FTAB(I)=F(X,NV)
                X=X+XSTEP
10              CONTINUE
        ELSE
        DO 15 I=1,N
                FTAB(I)=F(X)
                X=X+XSTEP
15              CONTINUE
        ENDIF
      RETURN
      END
