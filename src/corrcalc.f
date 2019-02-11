
      DOUBLE PRECISION FUNCTION vimcor(iscale,xA,yA,zA)

***********************************************************************
*       vimcor.f                                                      *
*---------------------------------------------------------------------*
*                                                                     *
*       decription:   This subroutine calculates the image potential  *
*                     at an arbitrary point outside the T=0 surface   *
*                     for a sum over damped 1/r**4 pair potentials.   *
*                     The routine makes use of the spline fit of the  *
*                     Fourier coefficients.                           *
*           iscale:   If iscale = 0 the unscaled Fourier coefficients *
*                     are spline fit; if iscale = 1 the exponentially *
*                     scaled Fourier coefficients are spline fit,     *
*                     which does not work for z<0.                    *
*                                                                     *
*---------------------------------------------------------------------*
*                                                                     *
*       date of creation:      31/05/90 by A.Chizmeshya and E.Zaremba *
*       date of last revision: 08/06/90 by J.McLean                   *
*               to make it a function, change name to vimcor, replace *
*               COMMON/CONST, scale to SAFARI units and orientation,  *
*               and replace COMMON/PARMS with SAFARI COMMON/CORPAR.   *
***********************************************************************

      implicit real*8 (a-b,d-h,o-z)
      implicit complex*16 (c)
      parameter (ndim = 200)
      common/imdat/zp(ndim,ndim,3),zpp(ndim,ndim,3),vgz(ndim,ndim),
     -             vgzp(ndim,ndim),gg(ndim),zz(ndim),sigma,sigmap,
     -             ngt,nzt
C      common/parms/tol,gtol,alf,a,b,d
      COMMON/CORPAR/TOL,GTOL,A,B,D,VIMCOS,VIMSIN
      COMMON/UTILTY/DZERO, XNULL(4), PI
      HRTREE=27.212d0
      BOHR=0.529177d0

      X=(VIMCOS*XA +VIMSIN*YA) /BOHR
      Y=(VIMCOS*YA -VIMSIN*XA) /BOHR
      Z=ZA/BOHR
      
      gx = 2.d0*pi/a
      gy = 2.d0*pi/b
      cgx =  dcmplx(0.d0,gx)
      cgy =  dcmplx(0.d0,gy)
      cegx = cdexp(cgx*x)
      cegy = cdexp(cgy*y)
      gmax = gg(ngt)

      vgz10 = surf2(0.d0,z,ngt,nzt,gg,zz,vgz,ndim,zp,sigma)
      vgz20 = surf2(0.d0,z+d,ngt,nzt,gg,zz,vgz,ndim,zp,sigma)
      vim = vgz10+vgz20

      nx = 0
      gnx = 0.d0
      ratio = gtol
      cegnx = (1.d0,0.d0)
      signx = 1.d0
      csumx = (0.d0,0.d0)
      do while ((gnx .lt. gmax) .and. (ratio .ge. gtol))
          nx = nx+1 
          gnx = nx*gx 
          cegnx = cegx*cegnx
          signx = -signx
          vgx1 = surf2(gnx,z,ngt,nzt,gg,zz,vgz,ndim,zp,sigma)
          vgx2 = surf2(gnx,z+d,ngt,nzt,gg,zz,vgz,ndim,zp,sigma)
          if (iscale .eq. 1) then
              exgz  = dexp(-gnx*z)
              exgzd = dexp(-gnx*(z+d))
              vgx1 = exgz*vgx1
              vgx2 = exgzd*vgx2
          endif
          termx = vgx1 + vgx2
          csumx = csumx + cegnx*(vgx1 + signx*vgx2)
          sumx = 2.d0*dreal(csumx) 
          ratio = dabs(termx/sumx)
      enddo
      vim = vim + sumx

      ny = 0
      gny = 0.d0
      ratio = gtol
      cegny = (1.d0,0.d0)
      signy = 1.d0
      csumy = (0.d0,0.d0)
      do while ((gny .lt. gmax) .and. (ratio .ge. gtol))
          ny = ny+1 
          gny = ny*gy 
          cegny = cegy*cegny
          signy = -signy
          vgy1 = surf2(gny,z,ngt,nzt,gg,zz,vgz,ndim,zp,sigma)
          vgy2 = surf2(gny,z+d,ngt,nzt,gg,zz,vgz,ndim,zp,sigma)
          if (iscale .eq. 1) then
              exgz  = dexp(-gny*z)
              exgzd = dexp(-gny*(z+d))
              vgy1 = exgz*vgy1
              vgy2 = exgzd*vgy2
          endif
          termy = vgy1 + vgy2
          csumy = csumy + cegny*(vgy1 + signy*vgy2)
          sumy = 2.d0*dreal(csumy) 
          ratio = dabs(termy/sumy)
      enddo
      vim = vim + sumy

      nx = 0
      gnx = 0.d0
      ratio = gtol
      cegnx = (1.d0,0.d0)
      signx = 1.d0
      do while ((gnx .lt. gmax) .and. (ny .gt. 1))
          nx = nx+1 
          gnx = nx*gx 
          cegnx = cegx*cegnx
          signx = -signx
          ny = 0
          gny = 0.d0
          ratio = gtol
          cegny = (1.d0,0.d0)
          signy = 1.d0
          do while ((gny .lt. gmax) .and. (ratio .ge. gtol))
              ny = ny+1 
              gny = ny*gy 
              cegny = cegy*cegny
              signy = -signy
              g = dsqrt(gnx*gnx+gny*gny)
              vg1 = surf2(g,z,ngt,nzt,gg,zz,vgz,ndim,zp,sigma)
              vg2 = surf2(g,z+d,ngt,nzt,gg,zz,vgz,ndim,zp,sigma)
              if (iscale .eq. 1) then
                  exgz  = dexp(-g*z)
                  exgzd = dexp(-g*(z+d))
                  vg1 = exgz*vg1
                  vg2 = exgzd*vg2
              endif
              term = vg1 + vg2
              c1 = cegnx*(cegny+1.d0/cegny)*vg1
              c2 = signx*cegnx*(signy*cegny+signy/cegny)*vg2
              vim =  vim + 2.d0*dreal(c1+c2)
              ratio = dabs(term/vim)
          enddo
      enddo
      VIMCOR=HRTREE*VIM
      return
      end


      subroutine fimcor(iscale,xA,yA,zA,fx,fy,fz)

***********************************************************************
*       fimcor.f                                                      *
*---------------------------------------------------------------------*
*                                                                     *
*       decription:   This subroutine calculates the force components *
*                     of the image potential at an arbitrary point    *
*                     (x,y,z) outside the T=0 surface.                *
*                                                                     *
*           iscale:   If iscale = 0 the unscaled Fourier coefficients *
*                     are spline fit; if iscale = 1 the exponentially *
*                     scaled Fourier coefficients are spline fit,     *
*                     which does not work for z<0.                    *
*                                                                     *
*                     The force is calculated in atomic units         *
*                     (Hartree/Bohr).                                 *
*                                                                     *
*---------------------------------------------------------------------*
*                                                                     *
*       date of creation:      31/05/90 by A.Chizmeshya and E.Zaremba *
*       date of last revision: 08/06/90 by J.McLean                   *
*               to change name to fimcor, replace COMMON/CONST, scale *
*               to SAFARI units and orientation, and replace          *
*               COMMON/PARMS with SAFARI COMMON/CORPAR.               *
***********************************************************************

      implicit real*8 (a-b,d-h,o-z)
      implicit complex*16 (c)
      parameter (ndim = 200)
      common/imdat/zp(ndim,ndim,3),zpp(ndim,ndim,3),vgz(ndim,ndim),
     -             vgzp(ndim,ndim),gg(ndim),zz(ndim),sigma,sigmap,
     -             ngt,nzt
C      common/parms/tol,gtol,alf,a,b,d
      COMMON/CORPAR/TOL,GTOL,A,B,D,VIMCOS,VIMSIN
      COMMON/UTILTY/DZERO, XNULL(4), PI
      HRTREE=27.212d0
      BOHR=0.529177d0

      X=(VIMCOS*XA +VIMSIN*YA) /BOHR
      Y=(VIMCOS*YA -VIMSIN*XA) /BOHR
      Z=ZA/BOHR
      gx = 2.d0*pi/a
      gy = 2.d0*pi/b
      cgx =  dcmplx(0.d0,gx)
      cgy =  dcmplx(0.d0,gy)
      cegx = cdexp(cgx*x)
      cegy = cdexp(cgy*y)
      gmax = gg(ngt)

      vgp10 = surf2(0.d0,z,ngt,nzt,gg,zz,vgzp,ndim,zpp,sigmap)
      vgp20 = surf2(0.d0,z+d,ngt,nzt,gg,zz,vgzp,ndim,zpp,sigmap)
      fz = -vgp10-vgp20

      nx = 0
      gnx = 0.d0
      ratio = gtol
      cegnx = (1.d0,0.d0)
      signx = 1.d0
      csumx = (0.d0,0.d0)
      csumz = (0.d0,0.d0)
      sum = 0.d0
      do while ((gnx .lt. gmax) .and. (ratio .ge. gtol))
          nx = nx+1 
          gnx = nx*gx 
          cegnx = cegx*cegnx
          signx = -signx
          vgx1 = surf2(gnx,z,ngt,nzt,gg,zz,vgz,ndim,zp,sigma)
          vgx2 = surf2(gnx,z+d,ngt,nzt,gg,zz,vgz,ndim,zp,sigma)
          vgz1 = surf2(gnx,z,ngt,nzt,gg,zz,vgzp,ndim,zpp,sigmap)
          vgz2 = surf2(gnx,z+d,ngt,nzt,gg,zz,vgzp,ndim,zpp,sigmap)
          if (iscale .eq. 1) then
              exgz  = dexp(-gnx*z)
              exgzd = dexp(-gnx*(z+d))
              vgx1 = exgz*vgx1
              vgx2 = exgzd*vgx2
              vgz1 = exgz*vgz1
              vgz2 = exgzd*vgz2
          endif
          term = (vgz1 + vgz2)
          sum = sum + term
          csumx = csumx - nx*cgx*cegnx*(vgx1 + signx*vgx2)
          csumz = csumz - cegnx*(vgz1 + signx*vgz2)
          sumx = 2.d0*dreal(csumx) 
          sumz = 2.d0*dreal(csumz) 
          ratio = dabs(term/sum)
      enddo
      fx = sumx
      fz =  fz + sumz

      ny = 0
      gny = 0.d0
      ratio = gtol
      cegny = (1.d0,0.d0)
      signy = 1.d0
      csumy = (0.d0,0.d0)
      csumz = (0.d0,0.d0)
      sum = 0.d0
      do while ((gny .lt. gmax) .and. (ratio .ge. gtol))
          ny = ny+1 
          gny = ny*gy 
          cegny = cegy*cegny
          signy = -signy
          vgy1 = surf2(gny,z,ngt,nzt,gg,zz,vgz,ndim,zp,sigma)
          vgy2 = surf2(gny,z+d,ngt,nzt,gg,zz,vgz,ndim,zp,sigma)
          vgz1 = surf2(gny,z,ngt,nzt,gg,zz,vgzp,ndim,zpp,sigmap)
          vgz2 = surf2(gny,z+d,ngt,nzt,gg,zz,vgzp,ndim,zpp,sigmap)
          if (iscale .eq. 1) then
              exgz  = dexp(-gny*z)
              exgzd = dexp(-gny*(z+d))
              vgy1 = exgz*vgy1
              vgy2 = exgzd*vgy2
              vgz1 = exgz*vgz1
              vgz2 = exgzd*vgz2
          endif
          term = vgz1 + vgz2
          sum = sum + term
          csumy = csumy - ny*cgy*cegny*(vgy1 + signy*vgy2)
          csumz = csumz - cegny*(vgz1 + signy*vgz2)
          sumy = 2.d0*dreal(csumy) 
          sumz = 2.d0*dreal(csumz) 
          ratio = dabs(term/sum)
      enddo
      fy = sumy
      fz =  fz + sumz

      nx = 0
      gnx = 0.d0
      ratio = gtol
      cegnx = (1.d0,0.d0)
      signx = 1.d0
      do while ((gnx .lt. gmax) .and. (ny .gt. 1))
          nx = nx+1 
          gnx = nx*gx 
          cegnx = cegx*cegnx
          signx = -signx
          ny = 0
          gny = 0.d0
          ratio = gtol
          cegny = (1.d0,0.d0)
          signy = 1.d0
          do while ((gny .lt. gmax) .and. (ratio .ge. gtol))
              ny = ny+1 
              gny = ny*gy 
              cegny = cegy*cegny
              signy = -signy
              g = dsqrt(gnx*gnx+gny*gny)
              vg1 = surf2(g,z,ngt,nzt,gg,zz,vgz,ndim,zp,sigma)
              vg2 = surf2(g,z+d,ngt,nzt,gg,zz,vgz,ndim,zp,sigma)
              vp1 = surf2(g,z,ngt,nzt,gg,zz,vgzp,ndim,zpp,sigmap)
              vp2 = surf2(g,z+d,ngt,nzt,gg,zz,vgzp,ndim,zpp,sigmap)
              if (iscale .eq. 1) then
                  exgz  = dexp(-g*z)
                  exgzd = dexp(-g*(z+d))
                  vg1 = exgz*vg1
                  vg2 = exgzd*vg2
                  vp1 = exgz*vp1
                  vp2 = exgzd*vp2
              endif
              term = vp1 + vp2
              cfacx1 = cegnx*(cegny+1.d0/cegny)
              cfacx2 = signx*cegnx*(signy*cegny+signy/cegny)
              cfacy1 = cegnx*(cegny-1.d0/cegny)
              cfacy2 = signx*cegnx*(signy*cegny-signy/cegny)
              cx1 = -nx*cgx*cfacx1*vg1
              cx2 = -nx*cgx*cfacx2*vg2
              cy1 = -ny*cgy*cfacy1*vg1
              cy2 = -ny*cgy*cfacy2*vg2
              cz1 = -cfacx1*vp1
              cz2 = -cfacx2*vp2
              fx  =  fx + 2.d0*dreal(cx1+cx2)
              fy  =  fy + 2.d0*dreal(cy1+cy2)
              fz  =  fz + 2.d0*dreal(cz1+cz2)
              ratio = dabs(term/fz)
          enddo
      enddo
      FT=(VIMCOS*FX -VIMSIN*FY) *HRTREE/BOHR
      FY=(VIMCOS*FY +VIMSIN*FX) *HRTREE/BOHR
      FX=FT
      FZ=FZ*HRTREE/BOHR

      return
      end
