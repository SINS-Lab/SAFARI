      SUBROUTINE HAMEQ(X,Y,Z,PX,PY,PZ,XAT,YAT,ZAT,
     1                          PXAT,PYAT,PZAT,NPART,LCALCV)
* X,Y,Z,PX,PY,PZ are the positions and momenta of the projectile.
* XAT, YAT, ZAT, PXAT, PYAT, PZAT are arrays of
* positions and momenta of the lattice.
* NPART is the number of particles in the lattice under consideration.
* LCALCV is a flag for whether to calculate total potential energy.

c handles correlated atom-atom forces
C EVALUATE HAMILTON'S EQUATIONS FOR NPART PARTICLES.
C
C
      IMPLICIT REAL*8 (A-H,O-Z)
      INCLUDE "params.txt"
C
      REAL*8 X,Y,Z,PX,PY,PZ,
     1       XAT(NPARTMAX),YAT(NPARTMAX),ZAT(NPARTMAX),
     2       PXAT(NPARTMAX),PYAT(NPARTMAX),PZAT(NPARTMAX)

      REAL*8 MION,MASS(NTYPEMAX)
      REAL*8 MION1,MASS1(NTYPEMAX),MINVV
      REAL*8 F
C            F IS A FORCE.
      REAL*8 XA,YA,ZA
C            XA IS THE ION-ATOM DISPLACEMENT.
      INTEGER TYPEAT(NPARTMAX)
      LOGICAL IMAGE
      REAL*8 LVR2,LDVDR2,LINDR2
      real*8 atomk,rneigh
c rneigh is nearest-neighbor separation squared
      logical corr,near
c if corr eq true, use correlated springs to compute atom motion
C
      COMMON/MASS/MASS,MION,TYPEAT
        COMMON/MINV/MASS1,MION1
      COMMON/HAM/DX,DY,DZ,DPX,DPY,DPZ,
     1           DXAT(NPARTMAX),DYAT(NPARTMAX),DZAT(NPARTMAX),
     2           DPXAT(NPARTMAX),DPYAT(NPARTMAX),DPZAT(NPARTMAX)
      COMMON/LAT/XLAT(NPARTMAX,3)
      COMMON/K/SPRING(NTYPEMAX,3)
      COMMON/KALL/KALL
      COMMON/IMAGE/IMAGE
      COMMON/POTPAR/POTPAR(30),PIMPAR(10),IPOT,IIMPOT
      common/corr/atomk,rneigh,corr
      common/vtot/vtot
C
      KALL=KALL+1
      vtot=0.0d0
C
C
C     DX/DT = P/M
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

C        DX/DT FOR SURFACE ATOMS.
         DXAT(J)=PXAT(J)*MINVV
         DYAT(J)=PYAT(J)*MINVV
         DZAT(J)=PZAT(J)*MINVV
C        F=DP/DT=-DV/DX.
C        XA IS THE X COMPONENT OF THE DISTANCE BETWEEN ION AND ATO
         XA=XAT(J)-X
         YA=YAT(J)-Y
         ZA=ZAT(J)-Z
c
C        RR IS THE SQUARE OF THE DISTANCE TO THE ION.
         RR=XA*XA+YA*YA+ZA*ZA

C        F IS A COMPONENT OF THE FORCE ON THE ION.
C        IF ION AND ATOM ARE AT THE SAME PLACE, F=0 BY SYMMETRY.
         IF(RR.NE.0.0D0) THEN
C           USE DV/DX=(DV/DR**2)*DR**2/DX = (DV/DR**2)*2X.
            DDR=-2.0D0*LDVDR2(RR,NAT)
            IF(IMAGE .AND. IIMPOT.EQ.2) DDR=DDR-2.0D0*LINDR2(RR)
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
         IF(IMAGE .AND. IIMPOT.EQ.2) THEN
            XLA=XLAT(J,1)-X
            YLA=XLAT(J,2)-Y
            ZLA=XLAT(J,3)-Z
            RR0=XLA**2 +YLA**2 +ZLA**2
            DDR=-2.0D0*LINDR2(RR0)
            DPX=DPX-DDR*XLA
            DPY=DPY-DDR*YLA
            DPZ=DPZ-DDR*ZLA
         ENDIF
c compute energy if LCALCV.eq.1
         if(LCALCV.eq.1 ) then
            vtot=vtot+LVR2(rr,nat)
         endif
         if(corr) then
c Now include spring forces
c if atomk.eq.0d0 then use einstein springs
            if(atomk.le.1.d-30) then
c use einstein spring
               dpxat(j)=dpxat(j)-spring(nat,1)*(xat(j)-xlat(j,1))
               dpyat(j)=dpyat(j)-spring(nat,2)*(yat(j)-xlat(j,2))
               dpzat(j)=dpzat(j)-spring(nat,3)*(zat(j)-xlat(j,3))
            else
               DO 400 I=J+1,NPART
c RR0 is the equillibrium spacing squared
                  RR0=(XLAT(J,1)-XLAT(I,1))**2+(XLAT(J,2)-XLAT(I,2))**2
     &                 +(XLAT(J,3)-XLAT(I,3))**2
                  if(RR0 .le. rneigh) then
                     R0=dsqrt(RR0)
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
                     rrat=dsqrt((xat(j)-xat(i))**2+(yat(j)-yat(i))**2
     &                      +(zat(j)-zat(i))**2)
c compute magnitude of force and include division by rr
c here atomk is the spring constant between the neighbors
                     fmag=atomk*(R0/rrat - 1.0d0)
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
 400           CONTINUE
            endif
         endif
100   CONTINUE
      IF(IMAGE) THEN
C        INCLUDE IMAGE CHARGE FORCE.
         CALL FIM(X,Y,Z,FX,FY,FZ)
         DPX=DPX+FX
         DPY=DPY+FY
         DPZ=DPZ+FZ
         VTOT = VTOT +VIM(X,Y,Z)
      ENDIF
C
      RETURN
      END
C
C***********************************************************************
C
      DOUBLE PRECISION FUNCTION LVR2(RR,NV)
C
C COMPUTE THE POTENTIAL ENERGY OF AN ION AND AN ATOM BY LOOKING IT UP
C IN THE TABLE TV AND INTERPOLATING IF POSSIBLE, OR BY CALCULATING IT
C BY BRUTE FORCE.
C RR IS THE SQUARE OF THE DISTANCE TO THE ION.
C
      IMPLICIT REAL*8 (A-H,O-Z)
      INCLUDE "params.txt"
      COMMON/TABLES/TDVDR2(NTABLE,3),TDIMDZ(NTABLE),TINDR2(NTABLE),
     &              RRMIN,RRSTEP,ZMIN,ZSTEP,NTAB
      COMMON/STPINV/STPINV
      COMMON/TVR2/TVR2(NTABLE,3)
C
      IF(NTAB.EQ.0 .OR. RR.LT.RRMIN .OR.
     &                   RR.GT.RRMIN+(NTAB-1)*RRSTEP) THEN
         LVR2=VR(DSQRT(RR),NV)
      ELSE
C        INTERPOLATE IN THE TABLE.
         WHERE=(RR-RRMIN)*STPINV+1.0D0
         NR=INT(WHERE)
         FRACT=WHERE-NR
         LVR2=(1.0D0-FRACT)*TVR2(NR,NV)+FRACT*TVR2(NR+1,NV)
      ENDIF
      RETURN
      END
C
C
C***********************************************************************
C
      DOUBLE PRECISION FUNCTION VIM(X,Y,Z)
C
C COMPUTE THE POTENTIAL ENERGY OF AN ION DUE TO BULK EFFECTS BY CALLING
C VIMCOR FOR THE CORRUGATED POTENTIAL, OR VIM Z FOR THE FLAT ONE.
C
      IMPLICIT REAL*8 (A-H,O-Z)
      integer qion
      COMMON/POTPAR/POTPAR(30),PIMPAR(10),IPOT,IIMPOT
      common/projectile/qion
C
      IF(IIMPOT.EQ.1) THEN
         VIM=VIMZ(Z) * qion
      ELSE IF(IIMPOT.EQ.2) THEN
         VIM=VIMCOR(0,X,Y,Z) * qion
      ELSE
         write(0,*) 'unacceptable value for iimpot'
         stop
      ENDIF
      RETURN
      END
C
C
C***********************************************************************
C
      DOUBLE PRECISION FUNCTION LDIMDZ(Z)
C
C COMPUTE THE DERIVATIVE OF THE POTENTIAL ENERGY OF AN ION DUE TO BULK
C BY LOOKING IT UP IN THE TABLE TDIMDZ AND INTERPOLATING, IF POSSIBLE,
C OR BY CALCULATING IT BY BRUTE FORCE.
C
      IMPLICIT REAL*8 (A-H,O-Z)
      INCLUDE "params.txt"
      COMMON/TABLES/TDVDR2(NTABLE,3),TDIMDZ(NTABLE),TINDR2(NTABLE),
     &              RRMIN,RRSTEP,ZMIN,ZSTEP,NTAB
      COMMON/ZINV/ZINV
C
      IF(NTAB.EQ.0 .OR. Z.LT.ZMIN .OR. Z.GT.ZMIN+(NTAB-1)*ZSTEP) THEN
C        DVIMDZ(Z) IS NOT IN THE TABLE.
         LDIMDZ=DVIMDZ(Z)
      ELSE
C        INTERPOLATE IN THE TABLE.
C        WHERE=(Z-ZMIN)/ZSTEP+1.0D0
         WHERE=(Z-ZMIN)*ZINV+1.0D0
         NZ=INT(WHERE)
         FRACT=WHERE-NZ
         LDIMDZ=(1.0D0-FRACT)*TDIMDZ(NZ)+FRACT*TDIMDZ(NZ+1)
      ENDIF
      RETURN
      END
C
C
C***********************************************************************
C
      DOUBLE PRECISION FUNCTION LDVDR2(RR,NV)
C
C COMPUTE THE DERIVATIVE WRT R**2 OF ION-ATOM POTENTIAL ENERGY BY LOOKIN
C IN THE TABLE TDVDR2 AND INTERPOLATING, IF POSSIBLE, OR BY CALCULATING I
C BY BRUTE FORCE.
C
      IMPLICIT REAL*8 (A-H,O-Z)
      INCLUDE "params.txt"
      COMMON/TABLES/TDVDR2(NTABLE,3),TDIMDZ(NTABLE),TINDR2(NTABLE),
     &              RRMIN,RRSTEP,ZMIN,ZSTEP,NTAB
      COMMON/STPINV/STPINV
C
      IF(NTAB.EQ.0 .OR. RR.LT.RRMIN .OR.
     1          RR.GT.RRMIN+(NTAB-1)*RRSTEP) THEN
C          DVDR(RR) IS NOT IN THE TABLE.
         LDVDR2=DVDR2(RR,NV)
      ELSE
C          INTERPOLATE IN THE TABLE.
         WHERE=(RR-RRMIN)*STPINV+1.0D0
         NR=INT(WHERE)
         FRACT=WHERE-NR
         LDVDR2=(1.0D0-FRACT)*TDVDR2(NR,NV)+FRACT*TDVDR2(NR+1,NV)
      ENDIF
      RETURN
      END
C
C
C***********************************************************************
C
      DOUBLE PRECISION FUNCTION VR2(RR,NV)
C
C COMPUTE THE ION-ATOM POTENTIAL AS A FUNCTION OF R**2.
C
      REAL*8 RR,VR
      VR2=VR(DSQRT(RR),NV)
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
         DVDR2=DVDR(R,NV)*0.5D0/R
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
C ROUTINE TO CREATE A TABLE OF N VALUES OF THE FUNCTION
C F STARTING AT XMIN AND INCREMENTING BY XSTEP.
C NV specifies the atom type.
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
10       CONTINUE
      ELSE
         DO 15 I=1,N
            FTAB(I)=F(X)
            X=X+XSTEP
15       CONTINUE
      ENDIF
      RETURN
      END
C
C
C***********************************************************************
C
      DOUBLE PRECISION FUNCTION LINDR2(RR)
C
C COMPUTE THE DERIVATIVE WRT R**2 OF ION-ATOM INDUCED DIPOLE POTENTIAL
C ENERGY BY LOOKING IN THE TABLE TINDR2 AND INTERPOLATING, IF POSSIBLE,
C OR BY CALCULATING IT BY BRUTE FORCE.
C
      IMPLICIT REAL*8 (A-H,O-Z)
      INCLUDE "params.txt"
      COMMON/TABLES/TDVDR2(NTABLE,3),TDIMDZ(NTABLE),TINDR2(NTABLE),
     &              RRMIN,RRSTEP,ZMIN,ZSTEP,NTAB
      COMMON/STPINV/STPINV
C
      IF(NTAB.EQ.0 .OR. RR.LT.RRMIN .OR.
     1          RR.GT.RRMIN+(NTAB-1)*RRSTEP) THEN
C          DVINDR2(RR) IS NOT IN THE TABLE.
         LINDR2=DINDR2(RR)
      ELSE
C          INTERPOLATE IN THE TABLE.
         WHERE=(RR-RRMIN)*STPINV+1.0D0
         NR=INT(WHERE)
         FRACT=WHERE-NR
         LINDR2=(1.0D0-FRACT)*TINDR2(NR)+FRACT*TINDR2(NR+1)
      ENDIF
      RETURN
      END
C
C
C***********************************************************************
C
      SUBROUTINE FIM(X,Y,Z,FX,FY,FZ)
C
C COMPUTE THE FORCE ON AN ION AT X,Y,Z DUE TO BULK EFFECTS BY CALLING
C FIMCOR FOR CORRUGATED POTENTIAL OR DVIMDZ FOR FLAT POTENTIAL.
C
      IMPLICIT REAL*8 (A-H,O-Z)
      REAL*8 LDIMDZ
      COMMON/POTPAR/POTPAR(30),PIMPAR(10),IPOT,IIMPOT
C
      IF(IIMPOT.EQ.1) THEN
         FX=0.0D0
         FY=0.0D0
         FZ=-LDIMDZ(Z)
      ELSE IF(IIMPOT.EQ.2) THEN
         CALL FIMCOR(0,X,Y,Z,FX,FY,FZ)
      ELSE
         write(0,*) 'unacceptable value for iimpot'
         stop
      ENDIF
      RETURN
      END
C
C***********************************************************************
C
      DOUBLE PRECISION FUNCTION DINDR2(RR)
C
C COMPUTE THE DERIVATIVE WRT R**2 OF THE ION ATOM POTENTIAL, AS A FUNCTI
C OF R**2.
      REAL*8 RR,R,DVINDR
      IF(RR.NE.0.0D0) THEN
         R=DSQRT(RR)
         DINDR2=DVINDR(R)*0.5D0/R
      ELSE
         DINDR2=0.0D0
      ENDIF
      RETURN
      END
