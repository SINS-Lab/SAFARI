      PROGRAM ION
C
C
C THIS PROGRAM COMPUTES THE ENERGY AND ANGLE OF AN ION
C SCATTERED CLASSICALLY FROM A CRYSTAL SURFACE.
C ION USES THE SAME TRAJECTORY ROUTINES AS SAFARI.
C SEE SAFARI FOR MORE DETAILS.
C
C
C ALL ENERGIES ARE IN EV.
C ALL DISTANCES ARE IN ANGSTROMS.
C ALL TIMES ARE IN ANGSTROMS*SQRT(AMU/EV).
C ALL MASSES ARE IN AMU.
C KEEP THIS IN MIND WHEN WRITING DVDR, ETC.
C
C
      IMPLICIT REAL*8 (A-H,O-Z)
      PARAMETER (NTABLE=30000)
C
C ION:
C
      REAL*8 E0,THETA0,PHI0
C           INPUT PARAMETERS.
C           INCIDENT ENERGY AND ANGLE.
      REAL*8 MION
C           INPUT PARAMETER.
C           MASS OF INCIDENT ION.
      REAL*8 MION1
C               INVERSE MASS.
      REAL*8 X,Y
C           TARGET POSITION OF ION. INITIAL HEIGHT IS INFINITE.
C           X AND Y VARY OVER A UNIT CELL.
      REAL*8 E,THETA,PHI
C           ENERGY AND ANGLE OF SCATTERED ION.
C
C
C CRYSTAL:    ALL THIS IS IN COMMON/XTAL/ AND COMMON/MASS/.
C
      REAL*8 AX,AY
C           DIMENSIONS OF THE SURFACE UNIT CELL IN ANGSTROMS.
      INTEGER NBASIS
C           NUMBER OF BASIS ATOMS IN THE UNIT CELL.
      REAL*8 XBASIS(3,70)
C           BASIS VECTORS OF ATOMS IN THE CELL IN ANGSTROMS.
      INTEGER TYPBAS(70)
C           TYPES OF ATOMS IN CELL.
C           EACH VARIETY OF ATOM IS ASSIGNED AN INTEGER TYPE, WHICH
C           IS THEN REFERRED TO BY ANY ROUTINE WHICH REQUIRES THE
C           MASS OR POTENTIAL OF THE ATOM.
      REAL*8 MASS(10)
C           MASSES OF SURFACE ATOMS OF VARIOUS TYPES.
      REAL*8 MASS1(10)
C
C INTEGRATION OF TRAJECTORIES:
C
      REAL*8 DELT0
C           INITIAL TIME STEP IN SECONDS. INPUT PARAMETER.
      REAL*8 DEMAX,DEMIN
C           MAXIMUM AND MINIMUM TOLERANCES FOR RELATIVE ENERGY
C           NONCONSERVATION. IF ENERGY IS NOT BEING CONSERVED,
C           THEN THE TIME STEP IS ALTERED. SEE SCAT FOR DETAILS.
      REAL*8 ABSERR
C           MAXIMUM TOLERANCE FOR ABSOLUTE ENERGY NONCONSERVATION.
      REAL*8 DELMIN
C           SMALLEST TIME STEP NEEDED TO KEEP ENERGY VARIATION
C           WITHIN BOUNDS.
      INTEGER NCALLS,NSTEPS
C           STATISTICS ON INTEGRATION. SEE SCAT.
      REAL*8 TIME
C           TIME OF FLIGHT FOR ION.
      INTEGER NPART
C           INPUT PARAMETER. NPART ATOMS ON THE SURFACE INTERACT WITH
C           THE ION AT ONCE. LIMIT IS TEN.
      REAL*8 Z1
C           TRAJECTORY INTEGRATION STARTS AT Z=Z1. FOR Z>Z1 IT IS ASSUME
C           THAT ONLY THE BULK POTENTIAL VIM(Z) IS SIGNIFICANT, SO THE
C           MOMENTUM AT Z1 CAN BE FOUND ANALYTICALLY IN TERMS OF THE
C           MOMENTUM AND POTENTIAL AT Z0.
      LOGICAL STUCK,BURIED
C               STUCK IS TRUE IF THE ION DOES NOT RISE BACK TO HEIGHT Z1
C               DURING INTEGRATION.
C               BURIED IS TRUE IF THE ION GOES DOWN TO -Z1.
C
      LOGICAL RECOIL, IMAGE
C
      REAL*8 TDVDR2(NTABLE,3),TVR2(NTABLE,3),TINDR2(NTABLE)
      LOGICAL START
      REAL*8 TDIMDZ(NTABLE)
      EXTERNAL VR2,DVDR2,DVIMDZ,DINDR2
C           POTENTIAL ENERGY FUNCTIONS AND TABLES THEREOF.
C               TVR2 AND TDVDR2 ARE TABULATED AS FUNCTIONS OF R**2.
C               V2 IS THE ION-ATOM POTENTIAL AS A FUNCTION OF R**2.
C               DVDR2 IS THE DERIVATIVE OF  V2 WRT R**2.
      INTEGER NTAB
C           NUMBER OF ENTRIES IN TABLES.
      REAL*8 RRMIN,RRSTEP,ZMIN,ZSTEP
C           MINIMUM TABULATED R**2 AND Z, AND DIFFERENCES BETWEEN SUCCES
C           VALUES. V,DVDR ARE FUNCTIONS OF R; VIM,DVIMDZ ARE FUNCTIONS
C           OF Z
      REAL*8 POTPAR(30), PIMPAR(10)
C        POTENTIAL PARAMETERS.
C
      INTEGER TYPEAT(100)
C           USED IN SCAT AND HAMEQ, NEEDED IN COMMON/MASS/.
C
C FOR PLOTTING TRAJECTORIES. PLOT HAS OUTPUT UNIT, 0 = DON'T PLOT
      LOGICAL PLOTAT
      INTEGER PLOT
C
C CUTOFF ENERGIES AND DISTANCES
      REAL*8 BDIST,SENRGY
C
      EXTERNAL DGET,LGET, IGET
      LOGICAL LGET
      CHARACTER*1 CGET
      CHARACTER*60 POTENT
      INTEGER*4 KALL
C
C
      character*32 fname
      CHARACTER*1 COMAND
C Variables for Convex timing
      REAL TARRAY(2),TIMER
C Variable for IBM RSC 6000 timing
      INTEGER ITIMER
COMMON!!
      COMMON/XTAL/AX,AY,XBASIS,TYPBAS,NBASIS
      COMMON/TYPES/NTYPES
      COMMON/MASS/MASS,MION,TYPEAT
      COMMON/MINV/MASS1,MION1
      COMMON/NRG/DEMAX,DEMIN,ABSERR,DELLOW,DELT0
      COMMON/FLAGS/STUCK,BURIED
      COMMON/STATS/DELMIN,NCALLS,NSTEPS,TIME
      COMMON/TABLES/TDVDR2,TDIMDZ,TINDR2,RRMIN,RRSTEP,ZMIN,ZSTEP,NTAB
      COMMON/TVR2/TVR2
      COMMON/BEAM/E0,THETA0,PHI0
      COMMON/OTHER/Z1,MAXDIV,MINDIV,FAX,FAY,START,NPART
      COMMON/POTPAR/POTPAR,PIMPAR,IPOT,IIMPOT
      COMMON/SWITCH/PLOT,PLOTAT,RECOIL
      COMMON/POTENT/POTENT
      COMMON/KALL/KALL
      COMMON/TEMP/TEMP
      COMMON/RANDOM/SEED,NITER
      COMMON/IMAGE/IMAGE
      COMMON/CUTOFF/SENRGY,BDIST
      COMMON/FAST/FAST
      common/depth/depth
      COMMON/UTILTY/DZERO, XNULL(4), PI
      
C     DATA PI/3.1415 92653 58979 32384 62643D0/
      PI=2.0d0*dasin(1.0d0)
      DZERO = 1.0D-10
      XNULL(1) = 0.0D0
      XNULL(2) = 0.0D0
      XNULL(3) = 0.0D0
      XNULL(4) = 0.0D0
C
C  open input file, unit=9
      OPEN(UNIT=9,STATUS='OLD',FILE='safari.input',ERR=10)
        READ(9,'(A)') FNAME
        CLOSE(9)
        GOTO 11
10    CALL SGET('Enter filename for input data^', FNAME)
11    CONTINUE
      ind=index(fname,' ')-1
      open(unit=9,file=fname(1:ind)//'.input',status='old')
      open(unit=10,file=fname(1:ind)//'.ionparam')

C READ DEFAULTS FROM INPUT FILE
      START = .TRUE.
      CALL INPUTS
      CLOSE(9)
      WRITE(0,1000) POTENT
1000  FORMAT(/' POTENTIAL IS ',A60/)
C
C START WITH POTENTIALS NOT TABULATED...
      NTABRM=NTAB
      NTAB=0
      IF(IMAGE .AND. IIMPOT.EQ.2) CALL IMINIT(1,0)
      TEMP=0.D0
      SEED=0.3434D0
      PLOT=0
      PLOTAT=.FALSE.
C  DEFAULT IMPACT PARAMETERS
      X=0
      Y=0
C
C  **** HERE BEGINS THE LOOP FOR COMMANDS ****
1     COMAND = CGET('command:^')

      IF(COMAND.EQ.'t' .OR. COMAND.EQ.'T') THEN
C   TABULATE THE POTENTIALS
         NTAB=NTABRM
         DO 20 NV=1,NTYPES
            WRITE(0,*) ' TABULATING POTENTIAL ',NV
            CALL TABLE(VR2,RRMIN,RRSTEP,NTAB,TVR2(1,NV),NV)
            CALL TABLE(DVDR2,RRMIN,RRSTEP,NTAB,TDVDR2(1,NV),NV)
20       CONTINUE
         IF(IMAGE) THEN
            IF(IIMPOT.EQ.1) THEN
               CALL TABLE(DVIMDZ,ZMIN,ZSTEP,NTAB,TDIMDZ,0)
            ELSE IF(IIMPOT.EQ.2) THEN
               CALL TABLE(DINDR2,RRMIN,RRSTEP,NTAB,TINDR2,0)
            ENDIF
         ENDIF

      ELSE IF(COMAND.EQ.'b' .OR. COMAND.EQ.'B') THEN
C   GET NEW BEAM PARAMETERS
         WRITE (0,*) 'Old parameters in brackets...'
         WRITE (0,'(''<'',F8.3,''>'')') E0
         E0=DGET('Ion energy in eV^')
         WRITE (0,'(''<'',F6.3,''>'')') THETA0*180.0D0/PI
         THETA0=DGET('Theta in degrees^')*PI/180.0D0
         WRITE (0,'(''<'',F7.3,''>'')') PHI0*180.0D0/PI
         PHI0=DGET('Phi in degrees^')*PI/180.0D0
         WRITE (0,'(''<'',F9.5,''>'')') MION
         MION=DGET('Ion mass in amu^')
         MION1=1.0D0/MION
C    RECOMPUTE STARTING POINT OF TRAJECTORY
         X1=X- Z1*DTAN(THETA0)*DCOS(PHI0)
         Y1=Y- Z1*DTAN(THETA0)*DSIN(PHI0)
C
C     TEMP=DGET('TEMPERATURE, IF APPLICABLE?^')
C     CALL TSETUP(TEMP)
C     SEED=DGET('SEED, IF APPLICABLE?^')
C
      ELSE IF(COMAND.EQ.'i' .OR. COMAND.EQ.'I') THEN
C GET INTEGRATION PARAMETERS.
         IF(LGET('Alter tolerances?^')) THEN
            WRITE (0,*) 'Old tolerances in brackets...'
            WRITE (0,'(''<'',E11.4,''>'')') DELT0
            DELT0=DGET('Max time step^')
            WRITE (0,'(''<'',E11.4,''>'')') DELLOW
            DELLOW=DGET('Min time step^')
            WRITE (0,'(''<'',E11.4,''>'')') DEMAX
            DEMAX=DGET('Exponent^')
C           WRITE (0,'(''<'',E12.5,''>'')') DEMIN
C           DEMIN=DGET('Min rel error^')
            WRITE (0,'(''<'',E12.5,''>'')') ABSERR
            ABSERR=DGET('Max abs error^')
         ENDIF
         WRITE (0,*) 'Old characteristics in brackets...'
         WRITE (0,'(''<'',I4,''>'')') NPART
         NPART=IGET('# surface atoms^')
         IF(NPART.GT.100) NPART=100
C        WRITE (0,'(''<'',L,''>'')') RECOIL
C        RECOIL=LGET('Allow surface recoil?^')
         WRITE (0,'(''<'',F7.3,''>'')') Z1
         Z1=DGET('Interact. height^')
         WRITE (0,'(''<'',F8.3,''>'')') SENRGY
         SENRGY=DGET('Stuck energy^')
         WRITE (0,'(''<'',F7.4,''>'')') BDIST
         BDIST=DGET('Max. penetration dist.^')
C    RECOMPUTE STARTING POINT OF TRAJECTORY
         X1=X- Z1*DTAN(THETA0)*DCOS(PHI0)
         Y1=Y- Z1*DTAN(THETA0)*DSIN(PHI0)

      ELSE IF(COMAND.EQ.'o' .OR. COMAND.EQ.'O') THEN
C  CHANGE OUTPUT FILE AND WHAT GOES IN IT
         CALL SGET('Filename (" "=stdout) or "none"^', FNAME)
         if(INDEX(fname,' ').EQ.1) THEN
            PLOT = 6
         ELSE IF(FNAME(1:4).EQ.'none') THEN
            PLOT = 0
         ELSE
            PLOT = 11
            open(unit=PLOT,file=fname)
            WRITE (0,*) 'File will be closed after one Run.'
         ENDIF
         PLOTAT=LGET('Plot surface atoms?^')

      ELSE IF(COMAND.EQ.'p' .OR. COMAND.EQ.'P') THEN
C   CHANGE POINT OF IMPACT
         WRITE(0,1100)
1100     FORMAT(' Negative numbers are fractions of unit cell.')
         X=DGET('X^')
         IF(X.LT.0.) X=-X*AX
         Y=DGET('Y^')
         IF(Y.LT.0.) Y=-Y*AY
C Back up from impact point to starting point
         X1=X- Z1*DTAN(THETA0)*DCOS(PHI0)
         Y1=Y- Z1*DTAN(THETA0)*DSIN(PHI0)

      ELSE IF(COMAND.EQ.'r' .OR. COMAND.EQ.'R') THEN
         GOTO 100
      ELSE IF(COMAND.EQ.'n' .OR. COMAND.EQ.'N') THEN
         GOTO 10
      ELSE IF(COMAND.EQ.'q' .OR. COMAND.EQ.'Q') THEN
         IF(PLOT.NE.0 .AND. PLOT.NE.6) CLOSE(PLOT)
         STOP
      ELSE
         WRITE(0,*) 'The possible commands, of which only the '//
     &        'first letter is significant, are:'
         WRITE(0,*) '  Tabulate potentials, Beam params, '//
     &        'Integration params, Output,'
         WRITE(0,*) '  Point of impact, Run, New file, and Quit.'
      ENDIF
      GOTO 1
C
C  HERE ENDS THE LOOP FOR FINDING COMMANDS
C  WHAT FOLLOWS IS THE CODE FOR THE RUN COMMAND, SEPARATED OUT
C  FOR CLARITY.
C
100   CONTINUE
      KALL=0
C COMPUTE MOMENTUM AT INFINITY.
      P0=DSQRT(2.0D0*MION*E0)
      PZ0=-P0*DCOS(THETA0)
      PTRANS=P0*DSIN(THETA0)
      PX0=PTRANS*DCOS(PHI0)
      PY0=PTRANS*DSIN(PHI0)
C COMPUTE MOMENTUM AT HEIGHT Z1.
      IF(IMAGE) THEN
         PZ1=-DSQRT(PZ0*PZ0-2.0D0*MION*VIM(0.0,0.0,Z1))
      ELSE
         PZ1=PZ0
      ENDIF
C
C IF on the CONVEX, USE
      timer=dtime(tarray)
C ELSE IF on the RSC6000, USE
C      itimer=mclock()
C END COMMENT IF
      CALL SCAT(X1,Y1,Z1,PX0,PY0,PZ1,Z2,PX2,PY2,PZ2,NPART,1)
C IF on the CONVEX, USE
      timer=dtime(tarray)
      write(0,*)'cpu time = ',tarray(1),' secs'
      write(0,*)'sytem paging time = ',tarray(2),' secs'
C ELSE IF on the RSC6000, USE
C      itimer=mclock()
C      timer = itimer/100.0
C      write(0,*)'cpu time = ', timer, ' secs'
C END COMMENT IF
C
      WRITE(0,1200) X,Y
1200  FORMAT(//' Impact parameters   X= ',
     &                    E11.5,5X,'Y= ',E11.5,' Angstroms'/)
C
C COMPUTE MOMENTUM OF FINAL ATOM AT INFINITY.
C PX2,ETC. IS THE MOMENTUM AT HEIGHT Z1.
      IF(IMAGE) THEN
         PP=PZ2*PZ2+2.0D0*MION*VIM(0.0,0.0,Z2)
      ELSE
         PP=PZ2*PZ2
      ENDIF
      IF(STUCK.OR.BURIED.OR. PP.LT.0.) THEN
C           ION DOES NOT HAVE ENOUGH ENERGY TO ESCAPE THE IMAGE FORCE.
         WRITE(0,1300) Z2,PX2,PY2,PZ2
1300     FORMAT(' NO ESCAPE. AT Z= ',E11.5/' PX= ',E11.5,3X,
     1                 'PY= ',E11.5,3X,'PZ= ',E11.5)
         IF(STUCK) WRITE(0,1301)
1301     FORMAT(' Stuck.')
         IF(BURIED) WRITE(0,1302)
1302     FORMAT(' Buried.')
      ELSE
         PSQR=PP+PX2*PX2+PY2*PY2
         PZ=DSQRT(PP)
         E=PSQR*MION1*.50D0
         THETA=DACOS(PZ/DSQRT(PSQR))
         PHI=DATAN2(PY2,PX2)
         WRITE(0,1400) E0,E,THETA0*180.0D0/PI,THETA*180.0D0/PI,
     1                  PHI0*180.0D0/PI,PHI*180.0D0/PI
1400     FORMAT(9X,'Initial',10X,'Final'/' Energy  ',E14.7,4X,E14.7,
     1             '  eV'/' Theta   ',F11.7,4X,F11.7,'  degrees'/
     2                    ' Phi     ',F11.6,4X,F11.6,'  degrees')
      ENDIF
      WRITE(0,1500) DELMIN,NSTEPS,TIME
1500  FORMAT(/' Smallest time step was ',E11.5/
     1        1X,I6,' steps taken for total time of ',E11.5,
     2        ' Whatevers.'//)
C     WRITE(0,1600) KALL
1600  FORMAT(1X,'NO. OF CALLS TO HAMEQ:',I4)
C
C WRITE TO FILE
C     WRITE(11,1700) XIP,YIP,E,THETA*180.0D0/PI,PHI*180D0/PI
1700  FORMAT(1X,F8.4,1X,F8.4,3(1X,F14.7))
C
      WRITE(0,1800) DEPTH
1800  FORMAT(1X,'DEEPEST PENETRATION WAS ',E11.5)
      IF(PLOT.NE.0 .AND. PLOT.NE.6) CLOSE(PLOT)
C
C  RETURN TO THE COMMAND LOOP
      GOTO 1
C
      END