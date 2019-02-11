      INTEGER FUNCTION INSECT(SN1,SM1,SN2,SM2,X1,X2)
C
C FIND INTERSECTIONS OF SEGMENTS (SN1,SM1) AND (SN2,SM2).
C RETURN THE NUMBER OF INTERSECTIONS INSECT AND THE VECTORS TO
C THE INTESECTIONS X1 AND X2.  If the circles SN1 and SN2 are the
C same, this returns -1.  Maybe someday someone will fix it so that
C in that case it returns 0 when the segments don't overlap and -1
C when they do with the endpoints of the overlap in X1 and X2.
C
      IMPLICIT REAL*8 (A-H,O-Z)
      REAL*8 SN1(4),SM1(4),SN2(4),SM2(4)
      REAL*8 X1(3),X2(3)
      REAL*8 XPLUS(3),XMINUS(3)
      LOGICAL ZIP
      COMMON/UTILTY/DZERO, XNULL(4), PI
C
C INITIALIZE.
C     WRITE(6,*) ' INSECT: STARTING'
      CALL EQUATE(X1,XNULL)
      CALL EQUATE(X2,XNULL)
      INSECT=0
C
C CHECK FOR NULL SEGMENTS.
      IF(ZIP(SM1).OR.ZIP(SM2)) THEN
C        WRITE(' INSECT: NULL SEGMENTS')
         RETURN
      ENDIF
C
C FIND THE INTERSECTIONS OF THE CIRCLES.
      III=ICSECT(SN1,SN2,XPLUS,XMINUS)
      IF(III.EQ.0) RETURN
      IF(III.EQ.-1) THEN
         INSECT = -1
         RETURN
      ENDIF
C
C DO THE INTERSECTIONS LIE ON THE SEGMENTS?
C       CHECK XPLUS.
      IF(DOT(XPLUS,SM1).GE.SM1(4).AND.DOT(XPLUS,SM2).GE.SM2(4)) THEN
         INSECT=1
         CALL EQUATE(X1,XPLUS)
      ENDIF
C       CHECK XMINUS.
      IF(DOT(XMINUS,SM1).GE.SM1(4).AND.DOT(XMINUS,SM2).GE.SM2(4)) THEN
         INSECT=INSECT+1
         IF(INSECT.EQ.2) THEN
            CALL EQUATE(X2,XMINUS)
         ELSE
            CALL EQUATE(X1,XMINUS)
         ENDIF
      ENDIF
C     WRITE(6,*) ' INSECT: NORMAL RETURN'
      RETURN
      END
C
C***********************************************************************
C
      INTEGER FUNCTION ICSECT(CIRC1,CIRC2,X1,X2)
C
C FIND THE INTERSECTIONS BETWEEN TWO CIRCLES CIRC1 AND CIRC2.
C RETURN THE INTERSECTIONS IN X1 AND X2 AND THE NUMBER OF INTERSECTIONS
C IN ICSECT.  If the circles are identical, returns -1.
C
      IMPLICIT REAL*8 (A-H,O-Z)
      REAL*8 CIRC1(4),CIRC2(4)
      REAL*8 X1(3),X2(3)
      REAL*8 DELTAX(3)
      REAL*8 WORK(3),WORK1(3),WORK2(3)
      LOGICAL ZIP
      COMMON/UTILTY/DZERO, XNULL(4), PI
C
C INITIALIZE.
C       WRITE(6,*) ' ICSECT: STARTING'
      ICSECT=0
      CALL EQUATE(X1,XNULL)
      CALL EQUATE(X2,XNULL)
C
C CHECK FOR NULL CIRCLES.
      IF(ZIP(CIRC1).OR.ZIP(CIRC2)) THEN
C        WRITE(6,*) ' ICSECT: NULL CIRCLES'
         RETURN
      ENDIF
C
C COMPUTE DOT PRODUCTS OF AXIS VECTORS.
      DOTN=DOT(CIRC1,CIRC2)
      IF(DOTN.EQ.1.0D0) THEN
         IF(CIRC1(4).EQ.CIRC2(4)) THEN
            ICSECT = -1
C           WRITE(0,*) ' ICSECT: IDENTICAL CIRCLES'
C        ELSE
C           WRITE(0,*) ' ICSECT: COENCENTRIC CIRCLES'
         ENDIF
      ENDIF
C
C DO THE COMPLETE CIRCLES INTERSECT?
C DELTA is (D/2)**2 *sin(TH)**2 where D is the straight distance between the
C points and TH is the angle between the circle axes.
      DELTA=1.0D0-DOTN*DOTN-CIRC1(4)*CIRC1(4)-CIRC2(4)*CIRC2(4)
      DELTA=DELTA+2.0D0*DOTN*CIRC1(4)*CIRC2(4)
      IF(DELTA.LT.0.0D0) THEN
C        WRITE(6,*) ' ICSECT: NO CIRCLE INTERSECTIONS'
         RETURN
      ENDIF
C
C COMPUTE THE CIRCLE INTERSECTIONS.
C 2*DELTAX/sin(TH)**2 is the vector between the two intersection points.
      CALL CROSS(CIRC1,CIRC2,WORK)
      CALL MSCALR(WORK,DSQRT(DELTA),DELTAX)
C  WORK/sin(TH)**2 is the vector average of the intersection points.
      A1=CIRC1(4)-DOTN*CIRC2(4)
      A2=CIRC2(4)-DOTN*CIRC1(4)
      CALL MSCALR(CIRC1,A1,WORK1)
      CALL MSCALR(CIRC2,A2,WORK2)
      CALL VADD(WORK1,WORK2,WORK)
C Except for the sin(TH)**2 scale factor,
C  the the intersections are now WORK +/- DELTAX.
      CALL VADD(WORK,DELTAX,X1)
      CALL VSUB(WORK,DELTAX,X2)
      CALL NORM(X1)
      CALL NORM(X2)
C       WRITE(6,*) ' ICSECT: NORMAL RETURN'
      ICSECT=2
      RETURN
      END
C
C
C**************************************************************
C
C
      LOGICAL FUNCTION INTRNG(POINT,XIN,CIRC1,SEG1,CIRC2,SEG2,
     1          CIRC3,SEG3)
C
C IS THE POINT POINT INSIDE THE TRIANGLE (CIRCI,SEGI) I=1,3 ?
C XIN IS A GIVEN POINT INSIDE THE TRIANGLE.
C
      IMPLICIT REAL*8 (A-H,O-Z)
      REAL*8 POINT(3),XIN(3)
      REAL*8 CIRC1(4),SEG1(4),CIRC2(4),SEG2(4),CIRC3(4),SEG3(4)
      REAL*8 GCIRC(4),GSEG(4)
      REAL*8 WORK1(3),WORK2(3)
      LOGICAL ZIP
      COMMON/UTILTY/DZERO, XNULL(4), PI
C
C CHECK FOR A NULL TRIANGLE.
      IF(ZIP(CIRC1).OR.ZIP(CIRC2).OR.ZIP(CIRC3)) THEN
         INTRNG=.FALSE.
         RETURN
      ENDIF
C
C CONSTRUCT THE SEGMENT JOINING POINT AND XIN.
      CALL GSGMNT(POINT,XIN,.TRUE.,GCIRC,GSEG)
      IF(GSEG(4).eq.1.0D0) THEN
C        POINT AND XIN COINCIDE.
         INTRNG=.TRUE.
         RETURN
      ENDIF
C
C HOW MANY TIMES DOES THE SEGMENT G INTERSECT THE SIDES OF THE TRIANGLE?
      IN=INSECT(GCIRC,GSEG,CIRC1,SEG1,WORK1,WORK2)
      IN=IN+INSECT(GCIRC,GSEG,CIRC2,SEG2,WORK1,WORK2)
      IN=IN+INSECT(GCIRC,GSEG,CIRC3,SEG3,WORK1,WORK2)
C
C IF THE SEGMENT INTERSECTS AN EVEN NUMBER OF TIMES, THEN POINT IS INSID
      INTRNG=MOD(IN,2).EQ.0
      RETURN
      END
C
C***********************************************************************
C
C
      LOGICAL FUNCTION INQUAD(POINT,XIN,CIRC1,SEG1,CIRC2,SEG2,
     1          CIRC3,SEG3,CIRC4,SEG4)
C
C IS THE POINT POINT INSIDE THE QUADRANGLE (CIRCI,SEGI) I=1,4 ?
C XIN IS A GIVEN POINT INSIDE THE QUADRANGLE.
C
      IMPLICIT REAL*8 (A-H,O-Z)
      REAL*8 POINT(3),XIN(3)
      REAL*8 CIRC1(4),SEG1(4),CIRC2(4),SEG2(4),CIRC3(4),SEG3(4)
      REAL*8 CIRC4(4),SEG4(4)
      REAL*8 GCIRC(4),GSEG(4)
      REAL*8 WORK1(3),WORK2(3)
      LOGICAL ZIP
C      WRITE(6,*) ' INQUAD: STARTING'
C
C CHECK FOR A NULL QUADRILATERAL.
      IF(ZIP(CIRC1).AND.ZIP(CIRC2).AND.ZIP(CIRC3)
     1          .AND.ZIP(CIRC4)) THEN
         INQUAD=.FALSE.
         RETURN
      ENDIF
C
C CONSTRUCT THE SEGMENT JOINING POINT AND XIN.
      CALL GSGMNT(POINT,XIN,.TRUE.,GCIRC,GSEG)
C
      IF(GSEG(4).EQ.1.0D0) THEN
C         POINT AND XIN COINCIDE.
         INQUAD=.TRUE.
         RETURN
      ENDIF
C
C HOW MANY TIMES DOES THE SEGMENT G INTERSECT THE SIDES OF THE QUADRANGL
      IN=INSECT(GCIRC,GSEG,CIRC1,SEG1,WORK1,WORK2)
      IN=IN+INSECT(GCIRC,GSEG,CIRC2,SEG2,WORK1,WORK2)
      IN=IN+INSECT(GCIRC,GSEG,CIRC3,SEG3,WORK1,WORK2)
      IN=IN+INSECT(GCIRC,GSEG,CIRC4,SEG4,WORK1,WORK2)
C
C IF THE SEGMENT INTERSECTS AN EVEN NUMBER OF TIMES, THEN POINT IS INSID
      INQUAD=MOD(IN,2).EQ.0
C      WRITE(6,*) ' INQUAD: RETURNING'
      RETURN
      END
C
C************************************************************
C
      SUBROUTINE SEGMNT(CIRC,END1,END2,SHORT,SEG)
C
C TAKE A CIRCLE CIRC AND TWO POINTS ON IT, END1 AND END2,
C AND FIND THE VECTOR AND ANGLE SEG TO DESCRIBE a circle surounding
C THE SEGMENT JOINING THE POINTS.
C SHORT IS A LOGICAL VARIABLE INDICATING WHETHER THE LONGER OR THE
C SHORTER SEGMENT IS TO BE CONSTRUCTED.  This version is careful to
C avoid the small numbers which can appear for near-semicircular segments.
C However, small number trouble could still occur for very small circles;
C hopefully that won't ever happen...
C
      IMPLICIT REAL*8 (A-H,O-Z)
      REAL*8 CIRC(4),SEG(4),END1(3),END2(3)
      LOGICAL SHORT
      REAL*8 SUM(3), WORK(3) ,PEND1(3), PEND2(3)
      LOGICAL VEQ
      COMMON/UTILTY/DZERO,XNULL(4), PI
C
C Check that the endpoints and the axis vector are not zero.
C If they're OK, we assume that they are normalized.
      IF(DOT(CIRC,CIRC).EQ.0.0D0 .OR. DOT(END1,END1).EQ.0.0D0 .OR.
     1      DOT(END2,END2).EQ.0.0D0) THEN
         DO 9 I=1,4
            SEG(I)=0.0D0
9        CONTINUE
         RETURN
      ENDIF
C Check that the endpoints lie on the circle.
      ERR1=DABS(DOT(END1,CIRC)-CIRC(4))
      ERR2=DABS(DOT(END2,CIRC)-CIRC(4))
      IF(ERR1.GT.DZERO .OR. ERR2.GT.DZERO) THEN
         WRITE(0,*) ' SEGMNT ERROR ENDPOINTS NOT ON CIRCLE ****'
         DO 10 I=1,4
            SEG(I)=0.0D0
10       CONTINUE
         RETURN
      ENDIF
C     If the endpoints are identical, then the whole circle or
C      nothing is to be used.
      IF( VEQ(END1, END2) ) THEN
         IF(SHORT) THEN
            CALL EQUATE(SEG, END1)
            SEG(4) = 1.0D0
         ELSE
C           Center is opposite ends, cos(2a) = 2(cos(a)**2) -1
            CALL MSCALR(CIRC, 2.0D0*CIRC(4), WORK)
            CALL VSUB(WORK, END1, SEG)
            SEG(4) = -1.0D0
         ENDIF
         RETURN
      ENDIF
C
C GET TO WORK.
C 1) Project ENDs onto circle; WORK is center of planar circle
      CALL MSCALR(CIRC, CIRC(4), WORK)
      CALL VSUB(END1, WORK, PEND1)
      CALL VSUB(END2, WORK, PEND2)
      SINSQ = DOT(PEND1, PEND1)
C 2) Add the projections together to get direction of segment center
      CALL VADD(PEND1, PEND2, SUM)
      SUMSQ = DOT(SUM, SUM)
      IF( SUMSQ/SINSQ .LT. 0.5D0 ) THEN
C Segment is close to semicircular.  To avoid small numbers, use
C   cross products to get the center
         CALL CROSS(END1,CIRC,PEND1)
         XSWTCH = DOT(PEND1, PEND2)
         CALL CROSS(CIRC,END2,PEND2)
         CALL VADD(PEND1, PEND2, SUM)
         SUMSQ = DOT(SUM, SUM)
         IF(XSWTCH .LT. 0.0D0) CALL MSCALR(SUM, -1.0D0, SUM)
      ENDIF
      IF(SHORT) THEN
         CALL MSCALR(SUM, DSQRT(SINSQ/SUMSQ), SUM)
      ELSE
         CALL MSCALR(SUM, -DSQRT(SINSQ/SUMSQ), SUM)
      ENDIF
C 3) Finally, get segment center relative to sphere center.  Normalization
C   appears from tests comparing with the old version to be unnecessary.
      CALL VADD(WORK, SUM, SEG)
      SEG(4) = DOT(END1, SEG)
      RETURN
      END
C
C***********************************************************************
C
      SUBROUTINE GSGMNT(END1,END2,SHORT,CIRC,SEG)
C
C CONSTRUCT THE GEODESIC SEGMENT (CIRC,SEG) JOINING END1 AND END2.
C This no longer calls SEGMNT since geodesics allow shortcuts.
C
      IMPLICIT REAL*8 (A-H,O-Z)
      REAL*8 CIRC(4),SEG(4),END1(3),END2(3)
      LOGICAL SHORT
      COMMON/UTILTY/DZERO, XNULL(4), PI
C
C Check that the endpoints are not zero.
C If they're OK, we assume that they are normalized.
      IF(DOT(END1,END1).EQ.0.0D0 .OR. DOT(END2,END2).EQ.0.0D0) THEN
         DO 10 I=1,4
            CIRC(I)=0.0D0
            SEG(I)=0.0D0
 10      CONTINUE
         RETURN
      ENDIF
      CIRC(4)=0.0D0
      CALL CROSS(END1,END2,CIRC)
      CALL NORM(CIRC)
      IF(DABS(DOT(END1, CIRC)) .GT. DZERO .OR.
     &   DABS(DOT(END2, CIRC)) .GT. DZERO) THEN
C CROSS failed; presumably END1 and END2 are parallel
         IF(END1(1).GT.DZERO) THEN
            CIRC(1) = END1(2)
            CIRC(2) = -END1(1)
            CIRC(3) = 0.0D0
         ELSE
            CIRC(1) = END1(3)
            CIRC(2) = -END1(2)
            CIRC(3) = 0.0D0
         ENDIF
         CALL NORM(CIRC)
         IF(DOT(END1, END2).GT.0.0D0) THEN
C           END1 and END2 are parallel
            IF(SHORT) THEN
               CALL EQUATE(SEG, END1)
               SEG(4) = 1.0D0
            ELSE
C              Center is opposite ends, cos(2a) = 2(cos(a)**2) -1
               CALL MSCALR(END1, -1.0D0, SEG)
               SEG(4) = -1.0D0
            ENDIF
         ELSE
C           END1 and END2 are anti-parallel
            CALL CROSS(END1, CIRC, SEG)
            SEG(4) = 0.0D0
         ENDIF
      ELSE
C CROSS successful, just get segment
         CALL VADD(END1, END2, SEG)
         CALL NORM(SEG)
         IF(.NOT. SHORT) THEN
            CALL MSCALR(SEG, -1.0D0, SEG)
         ENDIF
         SEG(4) = DOT(END1, SEG)
      ENDIF
      RETURN
      END
