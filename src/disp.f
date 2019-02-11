      Subroutine DISP
C
C DISPLAY DATA.
C
      PARAMETER (NLINES=20)
      PARAMETER (NARR=70000)
      INTEGER TPOINT,LEVEL(NARR),LREAD(10000)
      INTEGER TOTAL
      REAL*4 XTRAJ(NARR),YTRAJ(NARR),ztraj(narr)
      REAL*4 XREAD(10000),YREAD(10000),zread(10000)
      REAL*4 ENRGY(NARR),THETA(NARR),PHI(NARR),AREA(NARR)
      REAL*4 EREAD(10000),TREAD(10000),PREAD(10000),AREAD(10000)
      REAL*4 EMIN,EMAX
      LOGICAL RDATA
      Logical facc
      CHARACTER*1 ANSWER,B
      EXTERNAL RGET, IGET, LGET, CGET
      LOGICAL LGET
      CHARACTER*1 CGET
      Common/facc/facc
C
      facc=.true.
      TOTAL=0
      IF( LGET('Restrict energy range?^') ) THEN
         EMIN = RGET('Min. energy^')
         EMAX = RGET('Max. energy^')
      ELSE
         EMIN = -1.0e6
         EMAX = 1.0e6
      ENDIF
      IF( LGET('Restrict level?^') ) THEN
         LMIN = IGET('Min. level^')
         LMAX = IGET('Max. level^')
      ELSE
         LMIN = 1
         LMAX = 20
      ENDIF
      if(rdata(narr,tpoint,xread,yread,zread,eread,tread,pread,
     &        aread,lread,7)) then
         llk=11
      endif
20    IF(RDATA(NARR,TPOINT,XREAD,YREAD,zread,EREAD,TREAD,PREAD,
     &        AREAD,LREAD,7)) THEN
40       IF(TOTAL+TPOINT.LE.NARR) THEN
            DO 10 I=1,TPOINT
               IF((EREAD(I).LE.EMAX).AND.(EREAD(I).GE.EMIN) .AND.
     &             (LREAD(I).LE.LMAX .AND. LREAD(I).GE.LMIN)) THEN
                  J=J+1
                  K=K+1
                  XTRAJ(J)=XREAD(I)
                  YTRAJ(J)=YREAD(I)
                  ztraj(j)=zread(i)
                  ENRGY(J)=EREAD(I)
                  THETA(J)=TREAD(I)
                  PHI(J)=PREAD(I)
                  AREA(J)=AREAD(I)
                  LEVEL(J)=LREAD(I)
               ENDIF
10          CONTINUE
            TOTAL=TOTAL+K
            K=0
            GO TO 20
         ELSE
            WRITE(6,900) J
900         FORMAT(1X,I5,' TRAJECTORIES READ')
            GOTO 30
         ENDIF
      ENDIF
30    N=1
1     CONTINUE
      M=N+NLINES-1
      IF(M.GT.TOTAL) M=TOTAL
      WRITE(6,1000) (I,XTRAJ(I),YTRAJ(I),ZTRAJ(I),ENRGY(I),THETA(I),
     &   PHI(I),I=N,M)
1000  FORMAT(12X,'X',8X,'Y',8X,'Zmin',7X,'ENERGY',6X,'THETA',7X,'PHI'
     &          /(I4,3(1X,F10.6),3(1X,E11.4)))
      ANSWER=CGET('Q=QUIT; P=SKIP PAGE; S=SKIP 100; N=SPECIFY LINE;')
      IF(ANSWER.EQ.'Q' .or. answer.eq.'q') THEN
         return
      ELSE IF(ANSWER.EQ.'P' .or. answer.eq.'p') THEN
         N=M+NLINES+1
      ELSE IF(ANSWER.EQ.'S' .or. answer.eq.'s') THEN
         N=M+101
      ELSE IF(ANSWER.EQ.'N' .or. answer.eq.'n') THEN
         N = IGET('trajectory number^')
         IF(N.LE.0) N=1
      ELSE
         N=M+1
      ENDIF
      IF(N.LT.TOTAL) GO TO 1
      return
      END
