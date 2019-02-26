      SUBROUTINE TRAJ(DELT,XDIST,YDIST,ZDIST,NPART,NPART0)
C
C THIS VERSION SUPPORTS TRAJECTORY PLOTTING.
C
C SUBROUTINE TO PERFORM PREDICTOR-CORRECTOR INTEGRATION OF
C HAMILTON'S EQUATIONS. THE ALGORITHM COMES FROM BARBARA GARRISON.
C THE ARGUMENTS ARE:
C     DELT              TIME STEP.
C     XDIST,YDIST,ZDIST DISTANCE FROM INITIAL POINT THROUGH WHICH ION IS
C                                       TO BE MOVED IN EACH DIRECTION.
C     NPART             NUMBER OF SURFACE ATOMS TO BE INCLUDED.
C ARGUMENTS PASSED IN COMMON/RK/ ARE:
C     X0(3)             INITIAL POSTION OF ION.
C     X1(3)             FINAL POSITION OF ION.
C     P0(3)             INITIAL MOMENTUM OF ION.
C     P1(3)             FINAL MOMENTUM OF ION.
C     XAT0(3,NPARTMAX)  ARRAY OF INITIAL SURFACE ATOM POSITIONS.
C     PAT0(3,NPARTMAX)  ARRAY OF INITIAL SURFACE ATOM MOMENTA.
C     XAT1,ETC.
C     PAT1,ETC.
C
C HAMILTON'S EQUATIONS ARE EVALUATED BY SUBROUTINE HAMEQ, WHICH
C TAKES ION AND ATOM POSITIONS X,Y,Z,XAT,YAT,ZAT AND THE CORRESPONDING
C MOMENTA AND RETURNS THE DERIVATIVES DX,DY,DZ, ETC.
C THE ALGORITHM IS:
C       1. STARTING WITH THE CURRENT POSITION AND MOMEMTUM (R AND P)
C          AT TIME T, COMPUTE A PREDICTED POSITION AND MOMENTUM AT TIME
C          T+DELT.
C               RP= R + P/M*DELT + .5*(DP/DT)/M*DELT**2.
C
C       2. EVALUATE THE FORCES AT THE PREDICTED POSITION AND THE VELOCIT
C          AT THE ORIGINAL POSITION IN ONE CALL TO HAMEQ.
C
C       3. GET THE MOMENTUM AT THE CORRECTED POSITION USING THE AVERAGE
C          THE FORCES AT THE INITIAL AND PREDICTED POSITIONS.
C               PC = P + .5*DELT/M*(DP/DT+DP/DT)
C
C       4. GET THE CORRECTED POSITION USING THE CORRECTED MOMENTUM AND
C          THE AVERAGE FORCE.
C               NOTE THAT ONLY THE POSITIONS ARE CORRECTED.
C
C       5. CHECK THAT THE DIFFERENCE BETWEEN THE PREDICTED AND CORRECTED
C          POSITIONS IS NOT TOO LARGE.
C
C
      IMPLICIT REAL*8 (A-H,O-Z)
      INCLUDE "params.txt"
C
C DISTANCE TO MOVE.
      REAL*8 XDIST,YDIST,ZDIST
C INITIAL CONDITIONS.
      REAL*8 X0(3),XAT0(NPARTMAX,3)
      REAL*8 P0(3),PAT0(NPARTMAX,3)
C INTERMEDIATE RESULTS.
C       CURRENT.
      REAL*8 X,Y,Z,XAT(NPARTMAX),YAT(NPARTMAX),ZAT(NPARTMAX)
      REAL*8 PX,PY,PZ,PXAT(NPARTMAX),PYAT(NPARTMAX),PZAT(NPARTMAX)
C       PREDICTED.
      REAL*8 XP,YP,ZP,XATP(NPARTMAX),YATP(NPARTMAX),ZATP(NPARTMAX)
      REAL*8 PXP,PYP,PZP,PXATP(NPARTMAX),PYATP(NPARTMAX),PZATP(NPARTMAX)
C     ERRORS -- DIFFERENCE BETWEEN PREDICTED AND CORRECTED POSITIONS.
      REAL*8 EX,EY,EZ,EXAT(NPARTMAX),EYAT(NPARTMAX),EZAT(NPARTMAX)
C FINAL RESULTS.
      REAL*8 X1(3),XAT1(NPARTMAX,3)
      REAL*8 P1(3),PAT1(NPARTMAX,3)
C DERIVATIVES.
C       THESE ARE IN COMMON/HAM/ AND ARE RETURNED BY HAMEQ.
      REAL*8 DX,DY,DZ,DXAT(NPARTMAX),DYAT(NPARTMAX),DZAT(NPARTMAX)
      REAL*8 DPX,DPY,DPZ,DPXAT(NPARTMAX),DPYAT(NPARTMAX),DPZAT(NPARTMAX)
C       CURRENT.
      REAL*8 DPXX,DPYY,DPZZ,
     &       DPXXAT(NPARTMAX),DPYYAT(NPARTMAX),DPZZAT(NPARTMAX)
C ERROR CHECKING.
      REAL*8 ERRMAX,ERR,CHANGE
C               ERR IS A DIFFERENCE BETWEEN PREDICTED AND CORRECTED COOR
C               ERRMAX IS THE LARGEST ERR DURING A TIME STEP.
C               CHANGE IS THE FACTOR BY WHICH DELT IS CHANGED AFTER ERRM
      REAL*8 DEMAX,DEMIN,ABSERR
C               DEMAX IS USED HERE TO DETERMINE CHANGE AND IS NOT AN ERR
C               DEMIN IS NOT USED.
C               ABSERR IS THE ABSOLUTE ERROR TOLERANCE IN ANGSTROMS.
C               CHANGE IS GIVEN BY
C                       CHANGE=(ABSERR/ERRMAX)**DEMAX
C
C ION STATUS.
      LOGICAL STUCK,BURIED
      REAL*8 SENRGY,BDIST
C               STUCK IF ION TURNS BACK TOWARDS THE SURFACE.
C               BURIED IF ION EMERGES OUT THE BACK.
C
C SWITCHES.
      LOGICAL RECOIL
C               ALLOW SURFACE ATOMS TO RECOIL.
C
      LOGICAL PLOTAT
      INTEGER PLOT
C
      REAL*8 MION,MASS(NTYPEMAX)
      REAL*8 MION1,MASS1(NTYPEMAX),M1
      INTEGER TYPEAT(NPARTMAX)
      INTRINSIC DABS,MAX
C
      COMMON/RK/X0,X1,P0,P1,XAT0,XAT1,PAT0,PAT1
      COMMON/HAM/DX,DY,DZ,DPX,DPY,DPZ,DXAT,DYAT,DZAT,DPXAT,DPYAT,DPZAT
      COMMON/NRG/DEMAX,DEMIN,ABSERR,DELLOW,DELT0
      COMMON/MASS/MASS,MION,TYPEAT
       COMMON/MINV/MASS1,MION1
      COMMON/FLAGS/STUCK,BURIED
      COMMON/STATS/DELMIN,NCALLS,NSTEPS,TIME
      COMMON/SWITCH/PLOT,PLOTAT,RECOIL
C
      COMMON/CUTOFF/SENRGY,BDIST
      common/depth/depth
      common/vtot/vtot
C INITIALIZE.
C     WRITE(6,*) 'P0(1)= ',P0(1)
C     WRITE(6,*) 'X0(1)= ',X0(1)
      X=X0(1)
      Y=X0(2)
      Z=X0(3)
      PX=P0(1)
      PY=P0(2)
      PZ=P0(3)
CVDIR ASSUME COUNT(6)
      DO 6 J=1,NPART
         XAT(J)=XAT0(J,1)
         YAT(J)=XAT0(J,2)
         ZAT(J)=XAT0(J,3)
         PXAT(J)=PAT0(J,1)
         PYAT(J)=PAT0(J,2)
         PZAT(J)=PAT0(J,3)
6     CONTINUE
      IF(DELT.LT.DELMIN) DELMIN=DELT
C
C
1     CONTINUE
C       GET TIME DERIVATIVES.
      CALL HAMEQ(X,Y,Z,PX,PY,PZ,XAT,YAT,ZAT,
     1                  PXAT,PYAT,PZAT,NPART,0)
C
C       PREDICT, AND STORE CURRENT DERIVATIVES.
C     WRITE(6,*) DPX,DPY,DPZ
C     WRITE(6,*) 'DX= ',DX, 'DPX= ',DPX
      XP=X+DELT*(DX+.5*DELT*MION1*DPX)
      YP=Y+DELT*(DY+.5*DELT*MION1*DPY)
      ZP=Z+DELT*(DZ+.5*DELT*MION1*DPZ)
      DPXX=DPX
      DPYY=DPY
      DPZZ=DPZ
      IF(RECOIL) THEN
CVDIR ASSUME COUNT(6)
         DO 20 I=1,NPART
            M1=MASS1(TYPEAT(I))
            XATP(I)=XAT(I)+DELT*(DXAT(I)+.5*DELT*M1*DPXAT(I))
            YATP(I)=YAT(I)+DELT*(DYAT(I)+.5*DELT*M1*DPYAT(I))
            ZATP(I)=ZAT(I)+DELT*(DZAT(I)+.5*DELT*M1*DPZAT(I))
            DPXXAT(I)=DPXAT(I)
            DPYYAT(I)=DPYAT(I)
            DPZZAT(I)=DPZAT(I)
20       CONTINUE
      ENDIF
C
C       GET FORCES AT PREDICTED POINT.  Final 1 puts total potential
C       energy in VTOT
      CALL HAMEQ(XP,YP,ZP,PX,PY,PZ,XATP,YATP,ZATP,
     1                  PXAT,PYAT,PZAT,NPART,1)
C
C       COMPUTE MOMENTUM (USING AVERAGE) FORCE AND DIFFERENCE BETWEEN
C       PREDICTED AND CORRECTED POSITIONS.
C       FIND MAXIMUM DIFFERENCE (ERRMAX).
      ERRMAX=0.
      PXP=PX+.5*DELT*(DPX+DPXX)
      PYP=PY+.5*DELT*(DPY+DPYY)
      PZP=PZ+.5*DELT*(DPZ+DPZZ)

      IF((DPX.EQ.0).AND.(DPY.EQ.0).AND.(DPZ.EQ.0))THEN
        WRITE(6,*) PX, PY, PZ, X, Y, Z
        GOTO 666
      ENDIF

      EX=.25*DELT*DELT*MION1*(DPX-DPXX)
      EY=.25*DELT*DELT*MION1*(DPY-DPYY)
      EZ=.25*DELT*DELT*MION1*(DPZ-DPZZ)
      ERR=DABS(EX)
      ERRMAX=DMAX1(ERR,ERRMAX)
      ERR=DABS(EY)
      ERRMAX=DMAX1(ERR,ERRMAX)
      ERR=DABS(EZ)
      ERRMAX=DMAX1(ERR,ERRMAX)
      IF(RECOIL) THEN
CVDIR ASSUME COUNT(6)
         DO 30 I=1,NPART
            M1=MASS1(TYPEAT(I))
            PXATP(I)=PXAT(I)+.5*DELT*(DPXAT(I)+DPXXAT(I))
            PYATP(I)=PYAT(I)+.5*DELT*(DPYAT(I)+DPYYAT(I))
            PZATP(I)=PZAT(I)+.5*DELT*(DPZAT(I)+DPZZAT(I))
            EXAT(I)=.25*DELT*DELT*M1*(DPXAT(I)-DPXXAT(I))
            EYAT(I)=.25*DELT*DELT*M1*(DPYAT(I)-DPYYAT(I))
            EZAT(I)=.25*DELT*DELT*M1*(DPZAT(I)-DPZZAT(I))
            ERR=DABS(EXAT(I))
C           IF(ERR.GT.ERRMAX) ERRMAX=ERR
            ERRMAX=DMAX1(ERR,ERRMAX)
            ERR=DABS(EYAT(I))
C           IF(ERR.GT.ERRMAX) ERRMAX=ERR
            ERRMAX=DMAX1(ERR,ERRMAX)
            ERR=DABS(EZAT(I))
C           IF(ERR.GT.ERRMAX) ERRMAX=ERR
            ERRMAX=DMAX1(ERR,ERRMAX)
30       CONTINUE
      ENDIF
      IF(ERRMAX.NE.0.0D0) THEN
         CHANGE=(ABSERR/ERRMAX)**DEMAX
         IF(CHANGE.GE.2.) CHANGE=2.
         IF(CHANGE.GT.1. .AND. CHANGE.LT.2.) CHANGE=1.
c do the step over if a large change in time step is required and
c we are not at the minimum allowed time step already
         IF(CHANGE.LT..2 .AND. DELT.GT.DELLOW) THEN
c do step over
            delt=change*delt
            if(delt.lt.DELLOW) delt=DELLOW
            go to 1
         ENDIF
      ELSE
         CHANGE=2.0
      ENDIF
C
C COMPUTE CORRECTED POSITIONS.
      X=XP+EX
      Y=YP+EY
      Z=ZP+EZ
c     WRITE(6,*) X,Y,Z
      if(z.lt.depth) then
         depth=z
      endif
      PX=PXP
      PY=PYP
      PZ=PZP
C     WRITE(6,*) 'PX= ',PX
      IF(RECOIL) THEN
CVDIR ASSUME COUNT(6)
         DO 50 I=1,NPART
            XAT(I)=XATP(I)+EXAT(I)
            YAT(I)=YATP(I)+EYAT(I)
            ZAT(I)=ZATP(I)+EZAT(I)
            PXAT(I)=PXATP(I)
            PYAT(I)=PYATP(I)
            PZAT(I)=PZATP(I)
50       CONTINUE
      ENDIF
C
C PLOT TRAJECTORY, IF DESIRED. (ONLY FOR ION PROGRAM.)
      nsteps=nsteps+1
      time=time+delt
      IF(PLOT.GT.0) THEN
          write(PLOT,*) (1 + NPART0)
          write(PLOT,*) time
          write(PLOT,*) 'A',X, Y, Z, PX, PY, PZ, MION

          DO 55 I=1,NPART0
            write(PLOT,*) 'B',XAT(I), YAT(I), ZAT(I),
     &                     PXAT(I), PYAT(I), PZAT(I), MASS((TYPEAT(I)))
55       CONTINUE
      ENDIF
C
c it will be buried if it penetrates deeper than -bdist
c it will be stuck if , in traj, its k.e. plus potential energy falls
c below some threshold.
      ETEST=0.5*mion1*(PX*PX+PY*PY+PZ*PZ)+vtot
      IF(ETEST.LT.SENRGY) THEN
         STUCK=.TRUE.
         RETURN
      ENDIF
      IF(Z.LT.-BDIST) THEN
         BURIED=.TRUE.
         RETURN
      ENDIF
C
C FIND NEW TIME STEP.
      DELT=DELT*CHANGE
      IF(DELT.GT.DELT0) THEN
         DELT=DELT0
      ELSE IF(DELT.LT.DELLOW) THEN
         DELT=DELLOW
      ENDIF
C GONE FAR ENOUGH?
      IF(DABS(X-X0(1)).GE.XDIST .OR. DABS(Y-X0(2)).GE.YDIST .OR.
     1                  DABS(Z-X0(3)).GE.ZDIST) THEN
         X1(1)=X
         X1(2)=Y
         X1(3)=Z
         P1(1)=PX
         P1(2)=PY
         P1(3)=PZ
CVDIR ASSUME COUNT(6)
         DO 60 I=1,NPART
            XAT1(I,1)=XAT(I)
            XAT1(I,2)=YAT(I)
            XAT1(I,3)=ZAT(I)
            PAT1(I,1)=PXAT(I)
            PAT1(I,2)=PYAT(I)
            PAT1(I,3)=PZAT(I)
60       CONTINUE
         RETURN
      ENDIF
C
C TAKE ANOTHER STEP.
c     n=n+1
c     write(34,*) n,delt
      GO TO 1

666   CONTINUE
      WRITE(0,6666)
6666  FORMAT(' ???ERROR IN TRAJ???')
      STOP

      END
