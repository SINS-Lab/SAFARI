      Subroutine IMPACT
C
C FIND THE IMPACT PARAMETERS FOR TRAJECTORIES WITH CERTAIN FINAL ANGLES
C AND ENERGIES.
C
C 5 Oct 93 - Craig A. Keller - modified to accept multiple input files
C
      PARAMETER (NAR=200000)
      implicit real*8 (a-h,o-z)
      REAL*4 XTRAJ(NAR),YTRAJ(NAR),ztraj(nar)
      REAL*4 XREAD(NAR),YREAD(NAR),zread(nar)
      REAL*4 ENRGY(NAR),THETA(NAR),PHI(NAR),AREA(NAR)
      REAL*4 EREAD(NAR),TREAD(NAR),PREAD(NAR),AREAD(NAR)
      INTEGER LEVEL(NAR)
      INTEGER LREAD(NAR)
      INTEGER TPOINT
      LOGICAL lval
      LOGICAL RDATA,FACC,noshar
      external rdata
      CHARACTER*1 ANSWER,schar
      REAL*8 EMIN,EMAX,ESIZE,ASIZE
      REAL*4 Z1,THETA0,PHI0
      logical trdata
      character*25 dname,sname
      real*8 dparam(10),dcirc(4,4),dseg(4,4),xdtect(3)
      EXTERNAL CGET, LGET,INSIDE
      LOGICAL LGET, INSIDE, answer1
      CHARACTER*1 CGET
C
      COMMON/RESOLV/EMIN,EMAX,ESIZE,ASIZE
      COMMON/FACC/FACC
      common/dname/ind,dname
      common/dparam/dparam,ndtect
      common/dlimit/dcirc,dseg,xdtect
C
      FACC=.true.
      Lunit=7
1111  format(a1)
C Read the detector parameters from the Safari file
      if (RDATA(nar,tpoint,xread,yread,zread,eread,tread,
     &       pread,aread,lread,Lunit)) then
         ijji=1
      endif
C
      IF(LGET('Change detector parameters?^')) call dchang
C set up detector
      call dsetup(ndtect)
c
C
 6344 format(l1)
      NFOUND=0
5     IF(RDATA(NAR,TPOINT,XREAD,YREAD,zread,
     1                  EREAD,TREAD,PREAD,AREAD,LREAD,Lunit)) THEN
C
         DO 100 I=1,TPOINT
            tt=1.0d0*tread(i)
            pp=1.0d0*pread(i)
            IF(EREAD(I).GE.EMIN .AND. EREAD(I).LE.EMAX .AND.
     1              INSIDE(tt,pp)) THEN
               NFOUND=NFOUND+1
               XTRAJ(NFOUND)=XREAD(I)
               YTRAJ(NFOUND)=YREAD(I)
               ztraj(nfound)=zread(i)
               ENRGY(NFOUND)=EREAD(I)
               THETA(NFOUND)=TREAD(I)
               PHI(NFOUND)=PREAD(I)
               AREA(NFOUND)=AREAD(I)
               LEVEL(NFOUND)=LREAD(I)
            ENDIF
100      CONTINUE
         GO TO 5
      ENDIF


      write(6,*) 'another file (t/f)?'
      read(5,699) answer1
 699  format(l1)
      if(answer1) then
         rewind(Lunit)
         facc=.true.
         write(6,*) 'enter filename'
         read(5,988) sname
 988     format(a25)
         iii=index(sname,' ')
         Lunit=Lunit+1
         open(unit=Lunit,form='unformatted',
     &        file=sname(1:iii-1)//'.undata')
         if(rdata(NAR,TPOINT,XREAD,YREAD,zread,
     &        EREAD,TREAD,PREAD,AREAD,LREAD,Lunit)) then
c go at it again
            go to 5
         endif
      endif

C
      IF(NFOUND.EQ.0) THEN
         WRITE(6,1900)
1900     FORMAT(' NO TRAJECTORIES FOUND.')
         STOP
      ENDIF
      IF(LGET('Write results in .impact file?^')) THEN
         OPEN(UNIT=11,FORM='FORMATTED',File=dname(1:ind)//'.impact')
         write(11,2100) nfound
2100     FORMAT(1X,'TRAJECTORIES FOUND = ',I6,'.'/'      X        Y  ',
     1     '   Z min       E         theta        phi        weight')
         do 555 i=1,nfound
            WRITE(11,2201) XTRAJ(i),YTRAJ(i),ZTRAJ(i),
     &                     ENRGY(i),THETA(i),PHI(i),area(i)
 555     continue
2201     format(1x,3(f8.4,1x),1p,4(e11.4,1x))
      ENDIF
8002  FORMAT(1X,6(F10.5,','),D13.6)
      CLOSE(11)
c
C PRINT TRAJECTORIES.
      WRITE(6,2000) NFOUND
2000  FORMAT(/1X,I6,' TRAJECTORIES HAVE BEEN FOUND.'/
     1  8X,'X',8X,'Y',7X,'ENERGY',6X,'THETA',7X,'PHI',9X,'WEIGHT',
     2  7X,'LEVEL'/)
C
      J=1
      NWRITE=0
C
200   CONTINUE
      WRITE(6,2200) J,XTRAJ(J),YTRAJ(J),ENRGY(J),
     1                          THETA(J),PHI(J),AREA(J),LEVEL(J)
2200  FORMAT(1X,I5,2(1X,F8.4),4(1X,E11.4),1X,I3)
      J=J+1
      IF(J.GT.NFOUND) Return
      IF(J-NWRITE.GT.20) THEN
         NWRITE=J
         ANSWER=CGET('Q=QUIT; M=MORE:^')
         IF(ANSWER.EQ.'Q' .or. answer.eq.'q') Go to 444
      ENDIF
      GO TO 200
 444  Rewind(7)
      Return
      END
