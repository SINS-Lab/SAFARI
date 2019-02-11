      Subroutine SPCT
C
C 23Mar07 Changint the unit numbers back for now, I think the 
C         move from 7 to 8 led to a special version of Detect 
C         used for trapping simulations only
C
C 28Oct98 Output unit changed to 7, lunit changed to 8 to 
C         eliminate crossover for multiple files  CSosolik
C
C COMPILE A SPECTRUM OF P1 INTEGRATING OVER P2 AND P3.
C
      PARAMETER (NRAY=40000)
      PARAMETER (NPTS=200)
      PARAMETER (NTAB=500)
      PARAMETER (NWIDTH=6)
C               NPTS IS THE NUMBER OF POINTS IN THE SPECTRUM PLOT.
C               NTAB IS THE NUMBER OF ENTRIES IN THE TABULATED GAUSSIAN
C               NWIDTH IS THE NUMBER OF GAUSSIAN WIDTHS COVERED BY THE T
      REAL*4 ENRGY(NRAY),THETA(NRAY),PHI(NRAY),AREA(NRAY)
      REAL*4 XTRAJ(NRAY),YTRAJ(NRAY),ztraj(nray)
      INTEGER LEVEL(NRAY)
      REAL*4 X(NRAY),XMIN,XMAX
      REAL*8 EEMIN,EEMAX,EESIZE,AASIZE
      INTEGER INTRAJ(NRAY)
      real*8 dot
      REAL*8 EMIN,EMAX,ESIZE,ASIZE
      REAL*8 SPECT(NPTS),GS(NTAB)
      REAL*8 SUM,STEP,WINV
      INTEGER TPOINT
      INTEGER NFOUND,I
      REAL*8 TOTAL
      REAL*8 VECT(3),VFORW(3),FORW
      LOGICAL INSIDE
      LOGICAL RDATA
      logical FACC
      CHARACTER*1 SCHAR
      Character*25 dname,addon,sname
      REAL*4 XSAVE(200000),ASAVE(200000)
      LOGICAL SCALE
      EXTERNAL RDATA
      LOGICAL ANSWER
      real*8 dparam(10)
      COMMON/RESOLV/EMIN,EMAX,ESIZE,ASIZE
      COMMON/FACC/FACC
      common/dname/ind,dname
      common/dparam/dparam,ndtect
C GAUSSIAN OF UNIT AREA AND WIDTH 1./WINV.
      GAUSS(XX,WINV)=EXP(-XX*XX*2.*WINV*WINV)*WINV*0.7978845608
C
      atot=0.0
      nfound=0
      total=0
      lunit=7
      rewind(lunit)
C
      FACC=.true.
C Read the detector parameters from the Safari file
C Although you will have to specify the detector anyway, ha! ha!
      If( Rdata(nray,tpoint,xtraj,ytraj,ztraj,enrgy,theta,phi,
     &      area,level,lunit)) then
         ijji=1
      Endif
c4    Write(6,*) 'Change detector parameters? (y/n)'
c     read(5,1111) schar
c     if(schar.eq.'y') then
4        call dchang
         eemin=emin
         eemax=emax
         eesize=esize
         aasize=asize
c     endif
C set up detector
      call dsetup(ndtect)
C
C WHICH SPECTRUM?
      refl=0.0
      write(6,*) 'which spectrum (e,t,p) ?'
      READ(5,1111) SCHAR
      IF(SCHAR.EQ.'t') FORW=DGET('FORWARD PHI DIRECTION?')
      if(schar.eq.'t') addon='.theta'
      if(schar.eq.'e') addon='.energy'
      if(schar.eq.'p') addon='.phi'
      open(unit=8,form='formatted',file=dname(1:ind)//addon)
1111  FORMAT(A1)
C
C INITIALIZE.
      DO 10 I=1,NPTS
         SPECT(I)=0.
10    CONTINUE
C GET TRAJECTORIES.
      XMIN=1.0D22
      XMAX=-1.0D22
      NPOINT=0
22    IF(RDATA(NRAY,TPOINT,XTRAJ,YTRAJ,ztraj,ENRGY,THETA,PHI,
     1             AREA,LEVEL,lunit)) THEN
         TOTAL=TOTAL+TPOINT
         DO 15 I=1,TPOINT
            IF(SCHAR.EQ.'d' .or. schar.eq.'e') THEN
               X(I)=ENRGY(I)
            ELSE IF(SCHAR.EQ.'t') THEN
               CALL UNIT(90.0D0,FORW,VFORW)
               CALL UNIT(1.0d0*THETA(I),1.0d0*PHI(I),VECT)
               IF(DOT(VECT,VFORW).GE.0.0D0) THEN
                  X(I)=THETA(I)
               ELSE
                  X(I)=-THETA(I)
               ENDIF
            ELSE IF(SCHAR.EQ.'p') THEN
               X(I)=PHI(I)
            ELSE
               WRITE(6,*) ' CONFUSION AND LACK OF COMMUNICATION.'
               STOP
            ENDIF
7666        FORMAT(L1)
            IF(ENRGY(I).GE.EEMIN .AND. ENRGY(I).LE.EEMAX
     1             .AND. INSIDE(1.0d0*THETA(I),1.0d0*PHI(I))) THEN
               NFOUND=NFOUND+1
               XSAVE(NFOUND)=X(I)
               ASAVE(NFOUND)=AREA(I)
               refl=refl+xsave(nfound)*asave(nfound)
               atot=atot+asave(nfound)
3232           FORMAT(1X,2(F11.5,','),E15.6,',')
               IF(X(I).GT.XMAX) XMAX=X(I)
               IF(X(I).LT.XMIN) XMIN=X(I)
            ENDIF
15       CONTINUE
         GO TO 22
      ENDIF
      write(6,*) 'another file (t/f)?'
      read(5,699) answer
 699  format(l1)
      if(answer) then
         rewind(lunit)
         facc=.true.
         write(6,*) 'enter filename'
         read(5,988) sname
 988     format(a25)
         iii=index(sname,' ')
         lunit=lunit+1
         open(unit=lunit,form='unformatted',
     &        file=sname(1:iii-1)//'.undata')
         if(rdata(nray,tpoint,xtraj,ytraj,ztraj,
     &        enrgy,theta,phi,area,level,lunit)) then
c go at it again
            go to 22
         endif
      endif
      WRITE(6,9393) TOTAL
9393  FORMAT(1X,E10.5,' TRAJECTORIES READ.')
      IF(NFOUND.EQ.0) THEN
         WRITE(6,*) ' NO TRAJECTORIES FOUND IN DETECTOR.'
         STOP
      ENDIF
      if(schar.eq.'d') then
         write(6,4444) refl/atot
 4444    format(1x,'energy reflected was ',f12.6)
         return
      endif
C
C       TOTAL WEIGHT OF SPECTRUM.
      SUM=0.
C
C
C       INVERSE WIDTH OF GAUSSIAN.
      IF(SCHAR.EQ.'e') THEN
         WINV=1./Eesize
         RES=EeSIZE
      ELSE
         WINV=1./aASIZE
         RES=aASIZE
      ENDIF
C
C ELBOW ROOM.
      XMIN=XMIN-3.*RES
      XMAX=XMAX+3.*RES
C
C       DISTANCE BETWEEN PLOT POINTS.
      STEP=(XMAX-XMIN)/NPTS
C
C COMPUTE THE GAUSSIAN.
      DO 27 I=1,NTAB
         GS(I)=GAUSS((I-1)*RES*NWIDTH/NTAB,WINV)
27    CONTINUE
C       NUMBER OF TRAJECTORIES USED.
C
C COMPILE SPECTRUM.
      DO 30 I=1,NFOUND
         SUM=SUM+ASAVE(I)
         DO 20 J=1,NPTS
            XX=ABS(XMIN+(J-1)*STEP-XSAVE(I))
            INDX=NINT(1+XX*NTAB*WINV/NWIDTH)
            IF(INDX.LE.NTAB) THEN
               SPECT(J)=SPECT(J)+ASAVE(I)*GS(INDX)
            ELSE
               SPECT(J)=SPECT(J)+ASAVE(I)*GAUSS(XX,WINV)
            ENDIF
20       CONTINUE
30    CONTINUE
      
C
C WRITE SPECTRUM.
      WRITE(8,2000) (XMIN+(I-1)*STEP,SPECT(I),I=1,NPTS)
2000  FORMAT(1X,F16.11,',  ',F16.11,',')
C
      WRITE(6,2010) SUM,NFOUND
2010  FORMAT(' SPECTRUM WRITTEN TO FILE '/
     1  ' TOTAL WEIGHT ',E12.5/
     2  1X,I6,' TRAJECTORIES INCLUDED.'/)
C
      rewind(lunit)
      close(8)
      Return
      END
