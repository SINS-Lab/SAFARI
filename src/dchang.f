      Subroutine dchang
C
c read input ffom keyboard for new detector parameters
      Implicit Real*8(a-h,o-z)
      Real*8 dparam(10),emin,emax,esize,asize
      integer ndtect,ntype
      EXTERNAL IGET, RGET
      common/dparam/dparam,ndtect
      common/resolv/emin,emax,esize,asize
c initialize
      do 100 i=1,10
         dparam(i)=0.0d0
 100  continue
      ndtect = IGET('detector type (1-4)^')
C 07June2007 Changing RGETs to DGETs
      emin = DGET('min. energy^')
      emax = DGET('max. energy^')
      esize = DGET('energy resolution^')
      asize = DGET('angle resolution^')
      if(ndtect.eq.1) then
         dparam(1) = DGET('spot center theta^')
         dparam(2) = DGET('spot center phi^')
         dparam(3) = DGET('spot radius (degrees)^')
C PRINT OUT THESE PARAMS TO CHECK THEM
C         write(0,*) 'Emin,Emax',emin,emax
C         write(0,*) 'Esize,Asize',esize,asize
C         write(0,*) 'DPARAMS',DPARAM(1),DPARAM(2),DPARAM(3)
         return
      else if(ndtect.eq.2) then
C 15June2007 Changing RGETs to DGETs
         dparam(1) = DGET('min. theta^')
         dparam(2) = DGET('max. theta^')
         dparam(3) = DGET('azimuth^')
         dparam(4) = DGET('full width^')
         return
      else if(ndtect.eq.3) then
C 15June2007 Changing RGETs to DGETs
         dparam(1) = DGET('min. theta^')
         dparam(2) = DGET('max. theta^')
         dparam(3) = DGET('min. phi^')
         dparam(4) = DGET('max. phi^')
         return
      else if(ndtect.eq.4) then
C 15June2007 Changing RGETs to DGETs
         dparam(1) = DGET('max. latitude^')
         dparam(2) = DGET('min. latitude^')
         dparam(3) = DGET('min. phi^')
         dparam(4) = DGET('max. phi^')
         return
      else
         Write(0,*) 'invalid detector number'
         stop
      endif
      return
      end
