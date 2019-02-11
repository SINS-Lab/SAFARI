      Subroutine GLEAN
C
c eliminate stuck and buried trajectories from undata file
C 21dec94 - Craig A. Keller
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
      character*8 ud1
      REAL*8 EMIN,EMAX,ESIZE,ASIZE
      REAL*4 Z1,THETA0,PHI0
      logical trdata
      character*25 dname,sname,ddname
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
      Nunit=20
      ud1='g.undata'
1111  format(a1)
c open file for output
      ddname=dname(1:ind)//ud1
      open(unit=20,status='new',form='unformatted',file=ddname,
     &      err=666)
C Read the detector parameters from the Safari file
      if (RDATA(nar,tpoint,xread,yread,zread,eread,tread,
     &       pread,aread,lread,Lunit)) then
         ijji=1
      endif
C
c write the detector parameters to new Safari file
         WRITE(NUNIT) EMIN,EMAX,ESIZE,ASIZE
         WRITE(NUNIT) NDTECT
         WRITE(NUNIT) (DPARAM(I),I=1,5)
         WRITE(NUNIT) (DPARAM(I),I=6,10)
c
C
 6344 format(l1)
      ntotal=0
5     IF(RDATA(NAR,TPOINT,XREAD,YREAD,zread,
     1                  EREAD,TREAD,PREAD,AREAD,LREAD,Lunit)) THEN
C
         NFOUND=0
         DO 100 I=1,TPOINT
            tt=1.0d0*tread(i)
            pp=1.0d0*pread(i)
            IF(EREAD(I).GE.-1.0) THEN
               NFOUND=NFOUND+1
               ntotal=ntotal+1
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
         write(NUNIT) nfound
         do 109 NREAD=1, NFOUND
            write(NUNIT) XTRAJ(NREAD), YTRAJ(NREAD),
     &           LEVEL(NREAD)
            write(NUNIT) ztraj(NREAD)
            write(NUNIT) ENRGY(NREAD), THETA(NREAD),
     &           PHI(NREAD), AREA(NREAD)
 109     CONTINUE
         GO TO 5
      ENDIF


      write(6,*) 'another file (t/f)?  Same detector is assumed'
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
      IF(NTOTAL.EQ.0) THEN
         WRITE(6,1900)
1900     FORMAT(' NO TRAJECTORIES FOUND.')
         STOP
      ENDIF
      return

 444  Rewind(7)
      Return
 666  continue
      write(6,*) 'could not open file ',dname
      return
      END
