C THIS FILE CONTAINS AN SUBROUTINE WRITTEN BY Jim McLean ON 6/8/90 FOR
C INITIALIZATION OF THE CORRUGATED IMAGE POTENTIAL CALCULATION SUBPROGRAMS
C FROM Eugene Zaremba.  BESIDES CALLING HIS SETUP ROUTINES, IT FIGURES THE
C RELATIONSHIP BETWEEN THE SAFARI AND CORRUGATED COORDINATE AXES.
      SUBROUTINE IMINIT(IDERIV,ISCALE)
C
      IMPLICIT REAL*8 (A-H,O-Z)
      INCLUDE "params.txt"
      LOGICAL GOTZIP,ZIP,NONZIP
      REAL*8 XTEMP(3),XSAVE(3)
      INTEGER TYPBAS(NBASISMAX)
      COMMON/XTAL/AX,AY,XBASIS(3,NBASISMAX),TYPBAS,NBASIS
      COMMON/CORPAR/TOL,GTOL,A,B,D,VIMCOS,VIMSIN
      COMMON/POTPAR/POTPAR(30),PIMPAR(10),IPOT,IIMPOT
      COMMON/UTILTY/DZERO, XNULL(4), PI
      DATA ESQR/14.398D0/
      SQRT12 = DSQRT(0.5D0)
C
C        FIRST, FIGURE OUT THE ROTATION BETWEEN SAFARI'S COORDINATE SYSTEM
C        (SEE ELSEWHERE) AND THE COORDS USED IN THESE SUBPROGRAMS, TO WIT,
C        X AND Y ALIGNED WITH THE SIMPLE RECTANGULAR LATTICE OF THE SURFACE
C        WHICH IS THE ONLY KIND OF SURFACE SUPPORTED.  (LAYERS MUST ALSO
C        ALTERNATE IN A BCC SORT OF WAY, BUT THAT DOESN'T COME IN HERE)
      NSURF=0
      GOTZIP=.FALSE.
      D=1.0D20
      DO 100 I=1,NBASIS
         IF(XBASIS(3,I).EQ.0.0D0) THEN
C               THIS ONE'S ON THE SURFACE
            NSURF=NSURF+1
            IF(ZIP(XBASIS(1,I))) THEN
               GOTZIP=.TRUE.
            ELSE
               CALL EQUATE(XTEMP,XBASIS(1,I))
            ENDIF
         ELSE IF(DABS(XBASIS(3,I)).LT.D) THEN
            D=DABS(XBASIS(3,I))
         ENDIF
100   CONTINUE
      IF(.NOT.GOTZIP) THEN
         WRITE(0,*) ' Please use crystal basis with atom at origin.'
         STOP
      ELSE IF(NSURF.EQ.1) THEN
         A=AX
         B=AY
         VIMCOS=1.0D0
         VIMSIN=0.0D0
      ELSE IF(NSURF.EQ.2) THEN
         IF(XTEMP(1).EQ.0.0D0) THEN
            A=AX
            B=AY/2.0D0
            VIMCOS=1.0D0
            VIMSIN=0.0D0
         ELSE IF(XTEMP(2).EQ.0.0D0) THEN
            A=AX/2.0D0
            B=AY
            VIMCOS=1.0D0
            VIMSIN=0.0D0
         ELSE
            A=AX*SQRT12
            B=AY*SQRT12
            VIMCOS=SQRT12
            VIMSIN=SQRT12
         ENDIF
      ELSE
C         UNLIKELY, BUT MESSY...
         AA=1.0D20
         DO 200 I=1,NBASIS
            IF(XBASIS(3,I).EQ.0.0D0 .AND. NONZIP(XBASIS(1,I))) THEN
               DO 150 JX=0,-1,-1
                  DO 150 JY=0,-1,-1
                     CALL EQUATE(XTEMP,XBASIS(1,I))
                     XTEMP(1) = XTEMP(1) +JX*AX
                     XTEMP(2) = XTEMP(2) +JY*AY
                     A=DOT(XTEMP,XTEMP)
                     IF(A.LT.AA) THEN
                        AA=A
                        CALL EQUATE(XSAVE,XTEMP)
                     ENDIF
150            CONTINUE
            ENDIF
200      CONTINUE
C           FOUND THE SHORTEST DISTANCE...
         A=DSQRT(AA)
         CALL NORM(XSAVE)
         VIMCOS=XSAVE(1)
         VIMSIN=XSAVE(2)
C           NOW HAVE TO FIND OTHER LATTICE CONSTANT...
         BB=1.0D20
         DO 300 I=1,NBASIS
            IF(XBASIS(3,I).EQ.0.0D0 .AND. NONZIP(XBASIS(1,I))) THEN
               DO 250 JX=0,-1,-1
                  DO 250 JY=0,-1,-1
                     CALL EQUATE(XTEMP,XBASIS(1,I))
                     XTEMP(1) = XTEMP(1) +JX*AX
                     XTEMP(2) = XTEMP(2) +JY*AY
                     B=DOT(XTEMP,XTEMP)
                     IF(DABS(DOT(XSAVE,XTEMP)).LT.1.0D-10
     &                                     .AND. B.LT.BB) THEN
                        BB=B
                     ENDIF
250            CONTINUE
            ENDIF
300      CONTINUE
         B=DSQRT(BB)
      ENDIF
C        PUT THE PREFACTOR IN PIMPAR(2)
      PIMPAR(2)=A*B*D *ESQR/4.0D0/PI

C        NOW WE CAN GENERATE DATA TABLES AND SPLINE COEFFICIENTS
      CALL IMDATA(IDERIV,ISCALE)
      CALL VGZSPL(IDERIV)
C
      RETURN
      END

      subroutine imdata(ideriv,iscale)

***********************************************************************
*       imdata.f                                                      *
*---------------------------------------------------------------------*
*                                                                     *
*       decription:   This subroutine generates the table of values   *
*                     of the Fourier coefficients of the image        *
*                     potential which is used for the spline          *
*                     interpolation. If ideriv = 0 only the potential *
*                     Fourier components are calculated; if ideriv = 1*
*                     the Fourier coefficients of both the potential  *
*                     and its spatial derivative are calculated.      *
*                     If iscale = 0 the data is not scaled before     *
*                     spline fitting; if iscale = 1 the data is       *
*                     exponentially scaled before spline fitting,     *
*                     which doesn't work for z<0.                     *
*                                                                     *
*---------------------------------------------------------------------*
*                                                                     *
*       date of creation:      31/05/90 by A.Chizmeshya and E.Zaremba *
*       date of last revision: 08/06/90 by J.McLean                   *
*           to remove file creation and COMMON/CONST, to change       *
*           COMMON/PARMS to SAFARI COMMON/CORPAR and to change        *
*           variable ALF to PIMPAR(1) from COMMON/POTPAR.             *
***********************************************************************

      implicit real*8 (a-h,o-z)
      parameter (ndim = 200,ngaus = 24)
      common/imdat/zp(ndim,ndim,3),zpp(ndim,ndim,3),vgz(ndim,ndim),
     -             vgzp(ndim,ndim),gg(ndim),zz(ndim),sigma,sigmap,
     -             ngt,nzt
      common/zmesh/zmax(0:10),nz(10),nbz
      common/gmesh/gmax(0:10),ng(10),nbg
C      common/parms/tol,gtol,alf,a,b,d
      COMMON/CORPAR/TOL,GTOL,A,B,D,VIMCOS,VIMSIN
      common/gauss/xx(ngaus),ww(ngaus)
      COMMON/POTPAR/POTPAR(30),PIMPAR(10),IPOT,IIMPOT

      aa = 0.d0
      bb = PIMPAR(1)
      call gaus(ngaus,xx,ww,aa,bb)

      nzt = 1
C      write(6,*) 'Creating corrugated image potential data...'
      do izb = 1,nbz
          dz = (zmax(izb)-zmax(izb-1))/nz(izb)
          izmin = 0
          if (izb .ne. 1) izmin = 1
          do iz = izmin,nz(izb)
              z = zmax(izb-1) + iz*dz
C              write(6,200)z
c200   format(1x,'z = ',f6.2)
              zz(nzt) = z

              ngt = 1
              do igb = 1,nbg
                  dg = (gmax(igb)-gmax(igb-1))/ng(igb)
                  igmin = 0
                  if (igb .ne. 1) igmin = 1
                  do ig = igmin,ng(igb)
                      g = gmax(igb-1) + ig*dg
                      gg(ngt) = g
                      exgz = dexp(iscale*g*z)
                      vgz(ngt,nzt) = exgz*genvgz(0,g,z)
                      if (ideriv.eq.1) then
                          vgzp(ngt,nzt) = exgz*genvgz(1,g,z)
                      endif
                      ngt = ngt + 1
                  enddo
              enddo
              nzt = nzt + 1
          enddo
      enddo

      ngt = ngt-1
      nzt = nzt-1

C      write(iunit,90)ngt
C      write(iunit,100)(gg(k),k=1,ngt)
C      write(iunit,90)nzt
C      write(iunit,100)(zz(k),k=1,nzt)
C      do ig = 1,ngt
C          write(iunit,100)(vgz(ig,k),k=1,nzt)
C      enddo
C      if (ideriv.eq.1) then
C          do ig = 1,ngt
C              write(iunit,100)(vgzp(ig,k),k=1,nzt)
C          enddo
C      endif
C 90   format(1x,i5)
C100   format(1x,8d16.8)
C
C      rewind(iunit)
C      close (iunit)
      return
      end

***********************************************************************
      subroutine gaus(n,r,w,ra,rb)
      implicit real*8 (a-h,o-z)
*
*     allowed n-values;  3-10,12,16,20,24
*
      dimension r(n),w(n),xr(64),xr1(33),xr2(31),xw(64),xw1(33),
     + xw2(31),nst(12)
      equivalence(xr1(1),xr(1)),(xr2(1),xr(34)),(xw1(1),xw(1)),
     + (xw2(1),xw(34))

      data nst/1,3,5,8,11,15,19,24,29,35,43,53/

*     Nodes...

      data xr1/
     +     0.000000000000000d0,0.774596669241483d0,0.339981043584856d0,
     +     0.861136311594053d0,0.000000000000000d0,0.538469310105683d0,
     +     0.906179845938664d0,0.238619186083197d0,0.661209386466265d0,
     +     0.932469514203152d0,0.000000000000000d0,0.405845151377397d0,
     +     0.741531185599394d0,0.949107912342759d0,0.183434642495650d0,
     +     0.525532409916329d0,0.796666477413627d0,0.960289856497536d0,
     +     0.000000000000000d0,0.324253423403809d0,0.613371432700590d0,
     +     0.836031107326636d0,0.968160239507626d0,0.148874338981631d0,
     +     0.433395394129247d0,0.679409568299024d0,0.865063366688985d0,
     +     0.973906528517172d0,0.125233408511469d0,0.367831498998180d0,
     +     0.587317954286617d0,0.769902674194305d0,0.904117256370475d0/
      data xr2/
     +     0.981560634246719d0,0.095012509837637d0,0.281603550779258d0,
     +     0.458016777657227d0,0.617876244402643d0,0.755404408355003d0,
     +     0.865631202387831d0,0.944575023073232d0,0.989400934991649d0,
     +     0.076526521133497d0,0.227785851141645d0,0.373706088715419d0,
     +     0.510867001950827d0,0.636053680726515d0,0.746331906460150d0,
     +     0.839116971822218d0,0.912234428251325d0,0.963971927277913d0,
     +     0.993128599185094d0,0.064056892862605d0,0.191118867473616d0,
     +     0.315042679696163d0,0.433793507626045d0,0.545421471388839d0,
     +     0.648093651936975d0,0.740124191578554d0,0.820001985973902d0,
     +     0.886415527004401d0,0.938274552002732d0,0.974728555971309d0,
     +     0.995187219997021d0/

*     Weights...

      data xw1/
     +     0.888888888888889d0,0.555555555555556d0,0.652145154862546d0,
     +     0.347854845137454d0,0.568888888888889d0,0.478628670499366d0,
     +     0.236926885056189d0,0.467913934572691d0,0.360761573048139d0,
     +     0.171324492379170d0,0.417959183673469d0,0.381830050505119d0,
     +     0.279705391489277d0,0.129484966168870d0,0.362683783378362d0,
     +     0.313706645877887d0,0.222381034453374d0,0.101228536290376d0,
     +     0.330239355001260d0,0.312347077040003d0,0.260610696402935d0,
     +     0.180648160694857d0,0.081274388361574d0,0.295524224714753d0,
     +     0.269266719309996d0,0.219086362515982d0,0.149451349150581d0,
     +     0.066671344308688d0,0.249147045813403d0,0.233492536538355d0,
     +     0.203167426723066d0,0.160078328543346d0,0.106939325995318d0/
      data xw2/
     +     0.047175336386512d0,0.189450610455068d0,0.182603415044923d0,
     +     0.169156519395002d0,0.149595988816576d0,0.124628971255533d0,
     +     0.095158511682492d0,0.062253523938647d0,0.027152459411754d0,
     +     0.152753387130725d0,0.149172986472603d0,0.142096109318382d0,
     +     0.131688638449176d0,0.118194531961518d0,0.101930119817240d0,
     +     0.083276741576704d0,0.062672048334109d0,0.040601429800386d0,
     +     0.017614007139152d0,0.127938195346752d0,0.125837456346828d0,
     +     0.121670472927803d0,0.115505668053725d0,0.107444270115965d0,
     +     0.097618652104113d0,0.086190161531953d0,0.073346481411080d0,
     +     0.059298584915436d0,0.044277438817419d0,0.028531388628933d0,
     +     0.012341229799987d0/

      if (n.gt.24.or.n.lt.3) go to 60
      ba=0.5d0*(rb-ra)
      bp=0.5d0*(rb+ra)
      nby2=(n+1)/2
      if(n-10) 20,20,10
  10  nby4=n/4
      if((4*nby4).ne.n) go to 60
      ns=nst(6+nby4)-1
      go to 30
  20  ns=nst(n-2)-1
  30  nss=n-nby2

      do k=1,nby2
          r(nss+k)=xr(ns+k)
          r(nby2+1-k)=-xr(ns+k)
          w(nss+k)=xw(k+ns)*ba
          w(nby2+1-k)=xw(k+ns)*ba
      enddo

      do k=1,n
          r(k)=ba*r(k)+bp
      enddo

      return

  60  write(0,70) n

      stop

  70  format(//'  error in gaus',i10//)
      end


      DOUBLE PRECISION function genvgz(ideriv,g,z)

***********************************************************************
*       genvgz.f                                                      *
*---------------------------------------------------------------------*
*                                                                     *
*       decription:   This function calculates the Fourier            *
*                     coefficient  of the image potential and its     *
*                     derivative at a distance z from the surface     *
*                     plane by summing the contributions from a set   *
*                     of equivalent planes with spacing 2*d.          *
*                                                                     *
*           ideriv:   0 or 1 on input. If ideriv = 0, only the        *
*                     potential is calculated by calling the function *
*                     ud; otherwise the function udp is called to     *
*                     calculate the derivative of the potential.      *
*                                                                     *
*                g:   Surface reciprocal lattice vector.              *
*                                                                     *
*                z:   Distance from surface plane.                    *
*                                                                     *
*---------------------------------------------------------------------*
*                                                                     *
*       date of creation:      31/05/90 by A.Chizmeshya and E.Zaremba *
*       date of revision:      04/06/90 by E. Zaremba                 *
*       date of last revision: 08/06/90 by J.McLean                   *
*               to remove unused COMMON/CONST and replace             *
*               COMMON/PARMS with SAFARI COMMON/CORPAR                *
***********************************************************************

      implicit real*8 (a-h,o-z)
      parameter (n = 24)

C      common/parms/tol,gtol,alf,a,b,d
      COMMON/CORPAR/TOL,GTOL,A,B,D,VIMCOS,VIMSIN
      common/gauss/xx(n),ww(n)

      sum  = 0.d0
      term = tol
      m    = 0
      if (ideriv.eq.0) then
          do while (term.ge.tol)
              zm = dabs(z + 2.d0*m*d)
              call ud(g,zm,v0)
              sum = sum + v0
              term = dabs(v0/sum)
              m = m + 1
          enddo
      else
          do while (term.ge.tol)
              zm = z + 2.d0*m*d
              signzm = dsign(1.d0,zm)
              zm = dabs(zm)
              call udp(g,zm,v0)
              sum = sum + signzm*v0
              term = dabs(v0/sum)
              m = m + 1
          enddo
      endif
      genvgz = sum
      return
      end


      subroutine ud(g,z,vout)

***********************************************************************
*       ud.f                                                          *
*---------------------------------------------------------------------*
*                                                                     *
*       decription:   ud calculates the Fourier coefficient at the    *
*                     distance z from the surface plane for a planar  *
*                     sum of -C*f3/r**4 pair potentials. f3 is a      *
*                     damping function of the Tang-Toennies form.     *
*                                                                     *
*---------------------------------------------------------------------*
*                                                                     *
*       date of creation:      31/05/90 by A.Chizmeshya and E.Zaremba *
*       date of last revision: 08/06/90 by J.McLean                   *
*               to replace COMMON/PARMS with SAFARI COMMON/CORPAR and *
*               replace variable ALF with PIMPAR(1) from COMMON/POTPAR*
***********************************************************************

      implicit real*8 (a-h,o-z)
      parameter (n = 24)
C      common/parms/tol,gtol,alf,a,b,d
      COMMON/CORPAR/TOL,GTOL,A,B,D,VIMCOS,VIMSIN
      common/gauss/xx(n),ww(n)
      COMMON/POTPAR/POTPAR(30),PIMPAR(10),IPOT,IIMPOT

      f1(y) = dsqrt(g*g+y*y)
      f2(y) = dexp(-z*y)/y
      sum   = 0.d0
      do k = 1,n
          ds  = f1(xx(k))
          x2  = xx(k)*xx(k)*f2(ds)
          sum = sum + ww(k)*x2
      enddo
      ds    = f1(PIMPAR(1))
      u1    = PIMPAR(1)**3*f2(ds)/3.d0
      vout  = sum - u1
      vout  = -d*vout/4.d0
      return
      end


      subroutine udp(g,z,vout)

***********************************************************************
*       udp.f                                                         *
*---------------------------------------------------------------------*
*                                                                     *
*       decription:   This subroutine calculates the derivative with  *
*                     respect to z of the Fourier coefficient ud(g,z).*
*                                                                     *
*---------------------------------------------------------------------*
*                                                                     *
*       date of creation:      31/05/90 by A.Chizmeshya and E.Zaremba *
*       date of last revision: 08/06/90 by J.McLean                   *
*               to replace COMMON/PARMS with SAFARI COMMON/CORPAR and *
*               replace variable ALF with PIMPAR(1) from COMMON/POTPAR*
***********************************************************************
      implicit real*8 (a-h,o-z)
      parameter (n = 24)

C      common/parms/tol,gtol,alf,a,b,d
      COMMON/CORPAR/TOL,GTOL,A,B,D,VIMCOS,VIMSIN
      common/gauss/xx(n),ww(n)
      COMMON/POTPAR/POTPAR(30),PIMPAR(10),IPOT,IIMPOT

      f1(y) = dsqrt(g*g+y*y)
      f2(y) = dexp(-z*y)
      sum   = 0.d0
      do k = 1,n
          ds  = f1(xx(k))
          x2  = xx(k)*xx(k)*f2(ds)
          sum = sum + ww(k)*x2
      enddo
      ds    = f1(PIMPAR(1))
      u1    = PIMPAR(1)**3*f2(ds)/3.d0
      vout  = sum - u1
      vout  = d*vout/4.d0
      return
      end

      subroutine vgzspl(ideriv)

***********************************************************************
*       vgzspl.f                                                      *
*---------------------------------------------------------------------*
*                                                                     *
*       decription:   This subroutine generates spline coefficients   *
*                     for the image potential and its derivative with *
*                     respect to z, using as input the arrays vgz and *
*                     vgzp, respectively. The spline coefficients are *
*                     stored in zp and sigma and are used by the      *
*                     function routine surf2.                         *
*                                                                     *
*---------------------------------------------------------------------*
*                                                                     *
*       date of creation:      31/05/90 by A.Chizmeshya and E.Zaremba *
*       date of last revision: 08/06/90 by J.McLean                   *
*               to remove unused parameter iunit                      *
***********************************************************************

      implicit real*8 (a-h,o-z)
      parameter (ndim = 200)
      dimension temp(3*ndim),opt(1)
      common/imdat/zp(ndim,ndim,3),zpp(ndim,ndim,3),vgz(ndim,ndim),
     -             vgzp(ndim,ndim),gg(ndim),zz(ndim),sigma,sigmap,
     -             ngt,nzt

C     write(6,200)
*
*     generate the spline coefficients...
*
      opt(1) = 0.d0
      call surf (ngt,nzt,gg,zz,vgz,ndim,opt,zp,temp,sigma,ierr1)
      if (ierr1.gt.0) then
         write(0,400) ierr1
         stop
      endif
      if (ideriv .eq. 1) then
         opt(1) = 0.d0
         call surf (ngt,nzt,gg,zz,vgzp,ndim,opt,zpp,temp,sigmap,ierr2)
         if (ierr2.gt.0) then
            write(0,401) ierr2
            stop
         endif
      endif
C     write(6,300)
*
*     format list...
*
c200   format(/,3x,' vgzspl: computing spline coefficients...',//)
c300   format(/,3x,' vgzspl: procedure complete.             ',//)
400   format(/,3x,' vgzspl: abnormal termination with ierr1 = ',i4,/,
     -            ' vgzspl: see description in surf.f        ',/,
     -            ' Execution halted.                        ',//)
401   format(/,3x,' vgzspl: abnormal termination with ierr2 = ',i4,/,
     -            ' vgzspl: see description in surf.f        ',/,
     -            ' Execution halted.                        ',//)

      return
      end
