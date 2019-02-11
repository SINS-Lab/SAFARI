      Program Analysis
c run detector routines to process unformatted data files fro
c Safari
c
c 21dec94 - Craig A. Keller - modified: glean option added, allows the
c       elimination of stuck and buried trajectories from undata file
      implicit real*8(a-h,o-z)
      character*25 dname,ddname
      character*1 instr
      character*8 ud,ud1
      EXTERNAL CGET, LGET
      LOGICAL LGET
      CHARACTER*1 CGET
      common/dname/ind,dname
      COMMON/UTILTY/DZERO, XNULL(4), PI
C
      PI=2.0d0*dasin(1.0d0)
      DZERO = 1.0D-10
      XNULL(1) = 0.0D0
      XNULL(2) = 0.0D0
      XNULL(3) = 0.0D0
      XNULL(4) = 0.0D0
      ud='.undata'
      ud1='.undata1'
1     Write(6,*) 'Enter filename for analysis'
      read(5,8000) dname
8000  FORMAT(A25)
      ind=index(dname,' ')-1
      ddname=dname(1:ind)//ud
C Try to open file
      open(unit=7,status='old',form='unformatted',file=ddname,
     &      err=666)
C Figure out what to do
2      INSTR = CGET('Command ^')
       if(instr.eq.'s') then
          rewind(7)
          call spct
          go to 2
       else if(instr.eq.'i') then
          rewind(7)
          call impact
          go to 2
       else if(instr.eq.'d') then
          rewind(7)
          call disp
          go to 2
       else if(instr.eq.'g') then
          rewind(7)
          call glean
          go to 2
       else if(instr.eq.'n') then
          close(7)
          go to 1
       else if(instr.eq.'q') then
          close(7)
          stop
       else
          write(6,*) 'acceptable commands are:'
          write(6,*) 'spectrum,impact,display,quit,new,glean'
          go to 2
       endif
 666   continue
       write(6,*) 'no luck finding file ',dname
       if(LGET('want to try again?^')) go to 1
       end
