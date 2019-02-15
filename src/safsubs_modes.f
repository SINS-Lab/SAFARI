      subroutine montecarlo(offx,offy, px0, py0, pz1, npart)

*      Run as a montecarlo simulation, where numcha trajectories
*      are simulated in sets of 1000

      implicit real*8 (a-h,o-z)
      include "params.txt"

      common/xtal/ax,ay
      common/detect/area(narray)
      common/trajs/enrgy(narray),theta(narray),phi(narray)
      common/other/z1,maxdiv,mindiv,fax,fay
      common/chain/xstart,ystart,xstep,ystep,numcha
      common/points/xtraj(narray),ytraj(narray),level(narray)
      common/moment/px(narray),py(narray),pz(narray)
      common/random/seed

*     Split into sets of 1000 particles
      iter=numcha/1000
      do 808 kk=1,iter
*         Run each set of 1000
          do 707 l=1,1000
              seed=randsf(seed)
              xtraj(l) = xstart+fax*seed*ax
              seed=randsf(seed)
              ytraj(l) = ystart+fay*seed*ay
*             Back up from impact point to start
              x = xtraj(l) - offx
              y = ytraj(l) - offy
*             Call SCATTR for this particle
              call scattr(x,y,z1,px0,py0,pz1,enrgy(l),
     &             theta(l),phi(l),px(l),py(l),pz(l),npart,l)
              level(l)=1
              area(l)=1./numcha
707       continue
              nber=1000
              call output(nber+1)
 808  continue
      end

      subroutine chainscat(offx,offy, px0, py0, pz1, npart)

*      Run as a chain simulation, where numcha trajectories
*      are simulated in a row

      implicit real*8 (a-h,o-z)
      include "params.txt"

      common/xtal/ax,ay
      common/detect/area(narray)
      common/trajs/enrgy(narray),theta(narray),phi(narray)
      common/other/z1,maxdiv,mindiv,fax,fay
      common/chain/xstart,ystart,xstep,ystep,numcha
      common/points/xtraj(narray),ytraj(narray),level(narray)
      common/moment/px(narray),py(narray),pz(narray)
      common/random/seed

C ASSUME CHAIN WANTED.
      do 321 l=1,numcha
C TAKE CARE OF THERMAL
          seed=randsf(seed)
C THIS WON'T MAKE A DIFFERENCE IF NITER EQ 1
          xtraj(l) = xstart+(l-1)*xstep
          ytraj(l) = ystart+(l-1)*ystep
          x = xtraj(l) -offx
          y = ytraj(l) -offy
          call scattr(x,y,z1,px0,py0,pz1,enrgy(l),theta(l),phi(l),
     &                px(l),py(l),pz(l),npart,l)
          level(l)=1
          area(l)=1./numcha
321   continue
      call output(numcha+1)
      end
