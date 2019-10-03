C***********************************************************************
C
C IBM VERSIONS OF INPUTS OUTPUT AND CRYSTL.
C
C MODIFIED TO ACCOMODATE THE ULTIMATE DETECTOR.
C
C WRITES UNFORMATTED OUTPUT TO UNIT 13.
C
C WRITES NO FORMATTED OUTPUT.
C
C READS THE VARIABLE NWRITE.
C
C***********************************************************************
C
      SUBROUTINE INPUTS
C
C GET INPUT FOR SAFARI.
C
      IMPLICIT REAL*8 (A-H,O-Z)
      INCLUDE "params.txt"
      REAL*8 MASS(NTYPEMAX),MION,MASS1(NTYPEMAX),MION1
      INTEGER TYPEAT(NPARTMAX),TYPBAS(NBASISMAX)
      character*2 SYMION
      character*2 unk(NTYPEMAX)
      INTEGER NPAR
      INTEGER PLOT
      LOGICAL PBC,RECOIL,START,PLOTAT
      CHARACTER*70 POTENT
      LOGICAL IMAGE
      REAL*8 SENRGY,BDIST
      INTEGER NITER
C CHAIN VARIABLES
      INTEGER NUMCHA
C
COMMON!!
      COMMON/BEAM/E0,THETA0,PHI0
      COMMON/MASS/MASS,MION,TYPEAT
      common/symbols/SYMION,unk
      COMMON/MINV/MASS1,MION1
      COMMON/XTAL/AX,AY,XBASIS(3,NBASISMAX),TYPBAS,NBASIS
      COMMON/NRG/DEMAX,DEMIN,ABSERR,DELLOW,DELT0
      COMMON/SWITCH/PLOT,PLOTAT,RECOIL
      COMMON/TABLES/TDVDR2(NTABLE,3),TDIMDZ(NTABLE),TINDR2(NTABLE),
     &       RRMIN,RRSTEP,ZMIN,ZSTEP,NTAB
      COMMON/RESOLV/EMIN,EMAX,ESIZE,ASIZE
      COMMON/DPARAM/DPARAM(10),NDTECT
      COMMON/OTHER/Z1,MAXDIV,MINDIV,FAX,FAY,START,NPART
      COMMON/PBC/PBC,NWRITX,NWRITY
      COMMON/POTENT/POTENT
      COMMON/POTPAR/POTPAR(30),PIMPAR(10),IPOT,IIMPOT
      COMMON/TEMP/TEMP
      COMMON/RANDOM/SEED,NITER
      COMMON/STPINV/STPINV
      COMMON/ZINV/ZINV
      COMMON/IMAGE/IMAGE
      COMMON/CUTOFF/SENRGY,BDIST
      common/s0/s0
      COMMON/CHAIN/XSTART,YSTART,XSTEP,YSTEP,NUMCHA
      COMMON/ZMESH/ZMAX(0:NTYPEMAX),NZ(NTYPEMAX),NBZ
      COMMON/GMESH/GMAX(0:NTYPEMAX),NG(NTYPEMAX),NBG
      COMMON/CORPAR/TOL,GTOL,A,B,D,VIMCOS,VIMSIN
      COMMON/UTILTY/DZERO, XNULL(4), PI
C     write(0,*) 'Pi in gsafio = ',pi
C
C
C
C GET BEAM PARAMETERS
      READ(9,*) E0,THETA0,PHI0,MION,SYMION
      IF(START) WRITE(10,4000) E0,THETA0,PHI0,MION
4000  FORMAT('     E0 ',D15.8/' THETA0 ',D15.8/'   PHI0 ',
     1           D15.8/'   MASS ',D15.8/)
      THETA0=THETA0*PI/180.D0
      PHI0=PHI0*PI/180.D0
      MION1=1./MION
C
C GET DETECTOR PARAMETERS.
      READ(9,*) EMIN,EMAX,ESIZE,ASIZE
      READ(9,*) NDTECT
      READ(9,*) (DPARAM(I),I=1,4)
c dparam(5-10) are not used
      do 111 l=5,10
         dparam(l)=0.0d0
111   continue
      IF(START) WRITE(10,4010)
     1  EMIN,EMAX,ESIZE,ASIZE,NDTECT,(DPARAM(I),I=1,10)
4010  FORMAT(' DETECTOR PARAMETERS:'/
     1   5X,'ENERGY RANGE ',D12.5,' TO ',D12.5/
     2   5X,'ENERGY RESOLUTION ',D12.5/
     3   5X,'ANGLE  RESOLUTION ',D12.5,' DEGREES'/
     4   5X,'DETECTOR TYPE ',I4/ 5X,'DETECTOR PARAMETERS:'/
     5   5(1X,F7.2)/5(1X,F7.2))
      IF(EMIN.GE.EMAX) GO TO 666
C
C GET INTEGRATION PARAMETERS.
      READ(9,*) DELLOW,DELT0
        IF(START) WRITE(10,4020) DELLOW,DELT0
4020    FORMAT(' TIME STEP RANGE ',D10.3,' TO ',D10.3)
      READ(9,*) DEMAX,DEMIN,ABSERR
        IF(START) WRITE(10,4040) DEMAX,ABSERR
4040    FORMAT(' ERRORS   EXPONENT=',F6.3,5X,'ABSERR=',D10.3)
      READ(9,*) NPART
        IF(START) WRITE(10,4050) NPART
4050    FORMAT(' NUMBER OF SURFACE ATOMS = ',I5)
c       IF(NPART.GT.9*NBASIS) THEN
c          WRITE(6,*) ' NPART=',NPART,'  NBASIS=',NBASIS
c          GO TO 666
c       ENDIF
      READ(9,*) RECOIL
        IF(START) WRITE(10,4051) RECOIL
4051    FORMAT(' SURFACE RECOIL IS ',L1)
      READ(9,*) Z1
        IF(START) WRITE(10,4060) Z1
4060    FORMAT(' INTERACTION HEIGHT Z1 = ',D13.6)
C
C TABULATE POTENTIALS.
      READ(9,*) NTAB
        IF(NTAB.GT.NTABLE) NTAB=NTABLE
        IF(START) WRITE(10,4080) NTAB
4080    FORMAT(' NO. OF TABLE ENTRIES=',I6/6X,'MIN',12X,'STEP')
      READ(9,*) RRMIN,RRSTEP
        STPINV=1./RRSTEP
        IF(START) WRITE(10,4090) RRMIN,RRSTEP
4090    FORMAT(' R**2 ',D13.6,2X,D13.6)
      READ(9,*) ZMIN,ZSTEP
        ZINV=1./ZSTEP
        IF(START) WRITE(10,4100) ZMIN,ZSTEP
4100    FORMAT(4X,'Z ',D13.6,2X,D13.6)
C
      READ(9,*) MAXDIV,MINDIV
       IF(MAXDIV.EQ.MINDIV.AND.MAXDIV.EQ.1) THEN
          READ(9,*) NUMCHA
          READ(9,*) XSTART, XSTEP
          READ(9,*) YSTART, YSTEP
          WRITE(10,6755) NUMCHA
6755      FORMAT(1X,'NUMBER OF CHAIN TRAJS = ',I6)
          WRITE(10,6756) XSTART,XSTEP
6756      FORMAT(1X,'XSTART= ',F8.6, ' XSTEP= ',E14.6)
          WRITE(10,6757) YSTART,YSTEP
6757      FORMAT(1X,'YSTART= ',F8.6,' YSTEP= ',E14.6)
       ENDIF
      READ(9,*) NWRITX,NWRITY
        IF(MAXDIV.GT.MXDIV) MAXDIV=MXDIV
        IF(MINDIV.LT.1) MINDIV=1
        IF(START) WRITE(10,4105) MAXDIV,MINDIV,NWRITX,NWRITY
4105    FORMAT(' GRID DIVISIONS   MAX = ',I4,5X,'MIN = ',I4,
     1              5X,' NWRITX = ',I4,' NWRITY= ',I4)
        IF(MINDIV.GT.MAXDIV) GO TO 666
C
      READ(9,*) FAX,FAY
        IF(START) WRITE(10,4106) FAX,FAY
4106    FORMAT(' FRACTION OF CELL    FAX=',F5.3,3X,'FAY=',F5.3)
        PBC=FAX.EQ.1. .AND. FAY.EQ.1. .AND. NWRITX.EQ.1
     &    .AND. NWRITY.EQ.1
C
C
      READ(9,*) NPAR, IPOT
        IF(NPAR.GT.30) THEN
           WRITE(0,*) ' TOO MANY POTENTIAL PARAMETERS!!!'
           GO TO 666
        ENDIF
      READ(9,*) (POTPAR(I),I=1,NPAR)
      READ(9,*) NIMPAR, IIMPOT
        IF(NPAR.GT.10) THEN
           WRITE(0,*) ' TOO MANY IMAGE POTENTIAL PARAMETERS!!!'
           GO TO 666
        ENDIF
      READ(9,*) (PIMPAR(I),I=1,NIMPAR)
      if(NIMPAR.lt.3) then
        PIMPAR(3) = 0
      endif
C SET UP WRITE POTENTIAL LABEL FOR POT-TYPES IPOT, IIMPOT
      CALL POTSET
      IF(START) WRITE(10,4001) POTENT,IPOT,NPAR,(POTPAR(I),I=1,NPAR)
4001  FORMAT('POTENTIAL IS ',A70/
     &   ' POT TYPE = ',I3, ' NUMBER OF POTENTIAL PARAMETERS IS ',I3/
     &   ' PARAMETERS ARE '/(10X,D12.5) )
      IF(START) WRITE(10,4002) IIMPOT,NIMPAR,(PIMPAR(I),I=1,NIMPAR)
4002  FORMAT(' IMAGE POT TYPE = ',I3,' NUMBER OF IMAGE PARAMETERS IS '
     &       ,I3/' PARAMETERS ARE ',/(10X,D12.5))
      IF(IIMPOT.EQ.2) THEN
         READ(9,*) NBZ,TOL
         READ(9,*) (ZMAX(I),I=0,NBZ)
         READ(9,*) (NZ(I),I=1,NBZ)
         READ(9,*) NBG,GTOL
         READ(9,*) (GMAX(I),I=0,NBG)
         READ(9,*) (NG(I),I=1,NBG)
         IF(START) THEN
            WRITE(10,4003) TOL, GTOL, ZMAX(0)
4003        FORMAT('Z SUM TOLERANCE =',D12.5,'G SUM TOLERANCE =',D12.5/
     &             'ZMESH STARTS AT',D12.5)
            DO 4005 I=1,NBZ
               WRITE(10,4004) NZ(I),ZMAX(I)
4004           FORMAT('    ',I3,' INTERVALS TO ',D12.5)
4005        CONTINUE
            WRITE(10,4006) GMAX(0)
4006        FORMAT('GMESH STARTS AT',D12.5)
            DO 4007 I=1,NBG
               WRITE(10,4004) NG(I),GMAX(I)
4007        CONTINUE
         ENDIF
      ENDIF
C
C READ THERMAL PARAMETERS
      READ(9,*) TEMP,SEED,NITER
        WRITE(10,2323) TEMP,SEED,NITER
2323    FORMAT(1X,'TEMP. = ',F8.4,' SEED= ',F8.4,' NITER = ',I4)
        IF(SEED.GT.1.0D0 .OR. SEED.LT.0.0D0) THEN
           WRITE(0,*) 'INVALID SEED.  MUST HAVE 0<SEED<1.'
           GO TO 666
        ENDIF
        s0=seed
C
      READ(9,*) IMAGE
        WRITE(10,8334) IMAGE
8334    FORMAT(1X,'IMAGE  = ',L1)
C READ STUCK ENERGY AND BURIED DISTANCE
      READ(9,*) SENRGY,BDIST
        WRITE(10,7466) SENRGY
7466       FORMAT(1X,'STUCK ENERGY IS ',f10.5)
        WRITE(10,7467) BDIST
7467    FORMAT(1X,'BURIED DISTANCE IS ',F10.5)
C get crystal structure
      CAll Crystl
      RETURN
666   CONTINUE
      WRITE(0,6010)
6010  FORMAT(' ???? ILLEGAL DATA ENTRY. ????')
      STOP
C
      END
C
C***********************************************************************
C
      SUBROUTINE OUTPUT(MM)
C
      INCLUDE "params.txt"
      INTEGER*4 NN
      REAL*8 AREA(NARRAY),XTRAJ(NARRAY),YTRAJ(NARRAY),ztraj(narray)
      REAL*8 ENRGY(NARRAY),THETA(NARRAY),PHI(NARRAY),time(narray),delmin
      INTEGER TRJADD(4,NARRAY)
      INTEGER LEVEL(NARRAY),nsteps(narray)
      COMMON/DETECT/AREA
      Common/Points/Xtraj,Ytraj,Level
      Common/Trajs/Enrgy,Theta,Phi,Trjadd
      COMMON/STATS/DELMIN,TIME,NCALLS,NSTEPS
      common/ztraj/ztraj
C
      NN=MM-1
C
C
C WRITE OUTPUT
      WRITE(13) NN
      DO 100 I=1,NN
C        Write to the undata file
         WRITE(13) Xtraj(I),Ytraj(I),Level(I)
         write(13) ztraj(i)
         WRITE(13) Enrgy(I),Theta(I),Phi(I),Area(I)
C        Write to the plaintext file
         WRITE(66,*)Xtraj(I),Ytraj(I),ztraj(i),"   ",
     &   Enrgy(I),Theta(I),Phi(I),Level(I),Area(I),"   ",
     &   time(I),nsteps(I)
100   CONTINUE
C
      RETURN
      END
C***********************************************************************
      SUBROUTINE CRYSTL
C
C GET CRYSTAL PARAMETERS TO PUT IN COMMON/XTAL/.
C
C THE DATA FILE FORMAT IS:
C
C     AX AY         LATTICE CONSTANTS  (REAL).
C     NBASIS        NO. OF ATOMS IN THE BASIS  (INTEGER).
C     X Y Z TYPE    POSITION (REAL) AND TYPE (INTEGER) OF BASIS ATOMS.
C     ...           THE REST OF THE NBASIS POSITIONS AND TYPES.
C     NTYPES        NUMBER OF TYPES OF BASIS ATOMS (INTEGER).
C     MASS          MASS IN AMU OF THE BASIS ATOMS (REAL).
C     ...           THE REST OF THE NTYPES MASSES.
C
C
      IMPLICIT REAL*8 (A-H,O-Z)
      INCLUDE "params.txt"
      INTEGER TYPBAS(NBASISMAX),NBASIS,TYPEAT(NPARTMAX)
      integer zion
      integer zsite
      character*2 ATSYM(NTYPEMAX)
      character*2 SYMION
      INTEGER NTYPES
      INTEGER CHARGE(NTYPEMAX)
      integer ZPARTICLES(NTYPEMAX+1)
      REAL*8 MASS(NTYPEMAX),MION
      REAL*8 MASS1(NTYPEMAX),MION1
      logical corr
C
      COMMON/XTAL/AX,AY,XBASIS(3,NBASISMAX),TYPBAS,NBASIS
      COMMON/TYPES/NTYPES
      Common/ZPARS/ZBLPAR(NTYPEMAX+1),ZPARTICLES
      common/symbols/SYMION,ATSYM
      COMMON/MASS/MASS,MION,TYPEAT
      COMMON/MINV/MASS1,MION1
      COMMON/K/SPRING(NTYPEMAX,3)
      COMMON/CHARGE/CHARGE
      common/corr/atomk,rneigh,corr

      zion = 1
      zsite = 1
c     Stuffs the ion Z in the first entry of the ZBL array
      if(SYMION.eq.'Li') then
         zion = 3
      else if(SYMION.eq.'Na') then
         zion = 11
      else if(SYMION.eq.'K') then
         zion = 19
      else if(SYMION.eq.'Rb') then
         zion = 37
      else if(SYMION.eq.'Cs') then
         zion = 55
      endif
      ZPARTICLES(1) = zion
      ZBLPAR(1) = zion
c
      write(10,*) '---------Crystal data-------------'
      READ(9,*) AX,AY
      write(10,1000) ax,ay
 1000 format(1x,' x and y lattice constants: ',f8.3,4x,f8.3)
      READ(9,*) NBASIS
      write(10,1001) nbasis
 1001 format(1x,'number of basis atoms = ',i4)
      do 333 j=1,nbasis
         READ(9,*) (XBASIS(I,J),I=1,3),TYPBAS(J)
333   continue
      do 20 l=1,nbasis
         write(10,1002) (xbasis(i,l),i=1,3),typbas(l)
 1002    format(1x,3(2x,d16.8),4x,i4)
 20   continue
      READ(9,*) NTYPES
      write(10,1003) ntypes
 1003 format(1x,'number of atom types = ',i4)
      DO 10 I=1,NTYPES
         READ(9,*) MASS(I),CHARGE(I), ATSYM(I)

c     Initialize the ZBL potential for this site.
         a0 = 0.5292
         if(ATSYM(I).eq.'Cu') then
            zsite = 29
         else if(ATSYM(I).eq.'Ag') then
            zsite = 47
         else if(ATSYM(I).eq.'Au') then
            zsite = 79
         else if(ATSYM(I).eq.'Pt') then
            zsite = 78
         endif
         ZPARTICLES(I + 1) = zsite
         ZBLPAR(I + 1) = 0.8853 * a0 / ((zion**0.23) + (zsite ** 0.23))
         write(*,*) zion, SYMION, zsite, ATSYM(I)

         write(10,1004) mass(i),charge(i)
 1004    format(1x,'mass = ',d12.6,'amu',' charge = ',i4)
         MASS1(I)=1./MASS(I)
         READ(9,*) SPRING(I,1),SPRING(I,2),SPRING(I,3)
         write(10,1005) (spring(i,l),l=1,3)
 1005    format(1x,'thermal spring constants = ',3(2x,f12.6))
10    CONTINUE
      read(9,*) corr,atomk,rneigh
      write(10,1007) corr
 1007 format(1x,'Corr = ',l1)
      if(atomk.eq.0.0d0 .and. corr) then
         write(10,1010)
      else if (atomk.ne.0.0d0 .and. corr) then
         write(10,1011) atomk
         write(10,1014) dsqrt(rneigh)
      else
         write(10,1012)
      endif
 1011 format(1x,'corr motion:nearest neighbor spring is',(f10.5))
 1014 format(1x,'nearest neighbor distance is ',f12.8)
 1012 format(1x,'no spring forces')
 1010 format(1x,'site-binding: spring constant = thermal spring')
      RETURN
      END
