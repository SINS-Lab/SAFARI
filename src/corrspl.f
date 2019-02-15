      subroutine surf (m,n,x,y,z,iz,opt,zp,temp,sigma,ierr)
***********************************************************************
      implicit real*8 (a-h,o-z)
      integer m,n,iz,ierr,opt(*)
      real*8 x(m),y(n),z(iz,n),zp(m,n,*),temp(*),sigma
c
c                      from the spline under tension package
c                       coded by a. k. cline and r. j. renka
c                            department of computer sciences
c                              university of texas at austin
c
c                               modified by alfred h. morris
c                               naval surface weapons center
c                                          dahlgren virginia
c
c this subroutine determines the parameters necessary to
c compute an interpolatory surface passing through a rect-
c angular grid of functional values. the surface determined
c can be represented as the tensor product of splines under
c tension. the x- and y-partial derivatives around the
c boundary and the x-y-partial derivatives at the four
c corners may be specified or omitted. for actual mapping
c of points onto the surface it is necessary to call the
c function surf2.
c
c on input--
c
c   m is the number of grid lines in the x-direction, i. e.
c   lines parallel to the y-axis (m .ge. 2).
c
c   n is the number of grid lines in the y-direction, i. e.
c   lines parallel to the x-axis (n .ge. 2).
c
c   x is an array of the m x-coordinates of the grid lines
c   in the x-direction. these should be strictly increasing.
c
c   y is an array of the n y-coordinates of the grid lines
c   in the y-direction. these should be strictly increasing.
c
c   z is an array of the m * n functional values at the grid
c   points, i. e. z(i,j) contains the functional value at
c   (x(i),y(j)) for i = 1,...,m and j = 1,...,n.
c
c   iz is the row dimension of the matrix z used in the
c   calling program (iz .ge. m).
c
c   opt is an option vector. if no boundary conditions are
c   to be imposed on the surface then let opt be of length 1
c   and set opt(1)=0. otherwise, see the description of surf
c   in the nswc library manual.
c
c   zp is an array of at least 3*m*n locations.
c
c   temp is an array of at least n+n+m locations which is
c   used for scratch storage.
c
c   sigma contains the tension factor. this value indicates
c   the curviness desired. if abs(sigma) is nearly zero
c   (e. g. .001) the resulting surface is approximately the
c   tensor product of cubic splines. if abs(sigma) is large
c   (e. g. 50.) the resulting surface is approximately
c   bi-linear. if sigma equals zero tensor products of
c   cubic splines result. a standard value for sigma is
c   approximately 1. in absolute value.
c
c on output--
c
c   zp contains the values of the xx-, yy-, and xxyy-partial
c   derivatives of the surface at the given nodes.
c
c   ierr contains an error flag,
c        = 0 for normal return,
c        = 1 if n is less than 2 or m is less than 2,
c        = 2 if the x-values or y-values are not strictly
c            increasing,
c        = 3 the option vector has an error.
c
c this subroutine references package modules ceez, terms,
c and snhcsh.
c
***********************************************************************
c
      integer ind(8),loc(8),num(8)
      data num(5)/1/, num(6)/1/, num(7)/1/, num(8)/1/
c
      mm1 = m-1
      mp1 = m+1
      nm1 = n-1
      np1 = n+1
      npm = n+m
      ierr = 0
      if (n .le. 1 .or. m .le. 1) go to 46
      if (y(n) .le. y(1)) go to 47
c
c process the option vector
c
      num(1) = n
      num(2) = n
      num(3) = m
      num(4) = m
      ind(1) = 0
      ind(2) = 0
      ind(3) = 0
      ind(4) = 0
      ind(5) = 0
      ind(6) = 0
      ind(7) = 0
      ind(8) = 0
c
      l = 1
  100 key = opt(l)
      if (key) 48,110,101
  101 if (key .gt. 8) go to 48
      ind(key) = 1
      l = l+1
      loc(key) = l
      l = l + num(key)
      go to 100
c
c denormalize tension factor in y-direction
c
  110 sigmay = dabs(sigma)*float(n-1)/(y(n)-y(1))
c
c obtain y-partial derivatives along y = y(1)
c
      if (ind(3) .eq. 0) go to 2
      l = loc(3)
      do 1 i = 1,m
        zp(i,1,1) = opt(l)
    1   l = l+1
      go to 5
    2 dely1 = y(2)-y(1)
      dely2 = dely1+dely1
      if (n .gt. 2) dely2 = y(3)-y(1)
      if (dely1 .le. 0. .or. dely2 .le. dely1) go to 47
      call ceez (dely1,dely2,sigmay,c1,c2,c3,n)
      do 3 i = 1,m
    3   zp(i,1,1) = c1*z(i,1)+c2*z(i,2)
      if (n .eq. 2) go to 5
      do 4 i = 1,m
    4   zp(i,1,1) = zp(i,1,1)+c3*z(i,3)
c
c obtain y-partial derivatives along y = y(n)
c
    5 if (ind(4) .eq. 0) go to 7
      l = loc(4)
      do 6 i = 1,m
        npi = n+i
        temp(npi) = opt(l)
    6   l = l+1
      go to 10
    7 delyn = y(n)-y(nm1)
      delynm = delyn+delyn
      if (n .gt. 2) delynm = y(n)-y(n-2)
      if (delyn .le. 0. .or. delynm .le. delyn) go to 47
      call ceez (-delyn,-delynm,sigmay,c1,c2,c3,n)
      do 8 i = 1,m
        npi = n+i
    8   temp(npi) = c1*z(i,n)+c2*z(i,nm1)
      if (n .eq. 2) go to 10
      do 9 i = 1,m
        npi = n+i
    9   temp(npi) = temp(npi)+c3*z(i,n-2)
   10 if (x(m) .le. x(1)) go to 47
c
c denormalize tension factor in x-direction
c
      sigmax = dabs(sigma)*float(m-1)/(x(m)-x(1))
c
c obtain x-partial derivatives along x = x(1)
c
      if (ind(1) .eq. 0) go to 12
      l = loc(1)
      do 11 j = 1,n
        zp(1,j,2) = opt(l)
   11   l = l+1
      if (ind(5)+ind(7) .eq. 2) go to 15
   12 delx1 = x(2)-x(1)
      delx2 = delx1+delx1
      if (m .gt. 2) delx2 = x(3)-x(1)
      if (delx1 .le. 0. .or. delx2 .le. delx1) go to 47
      call ceez (delx1,delx2,sigmax,c1,c2,c3,m)
      if (ind(1) .eq. 1) go to 15
      do 13 j = 1,n
   13   zp(1,j,2) = c1*z(1,j)+c2*z(2,j)
      if (m .eq. 2) go to 15
      do 14 j = 1,n
   14   zp(1,j,2) = zp(1,j,2)+c3*z(3,j)
c
c obtain x-y-partial derivative at (x(1),y(1))
c
   15 if (ind(5) .eq. 0) go to 16
      l = loc(5)
      zp(1,1,3) = opt(l)
      go to 17
   16 zp(1,1,3) = c1*zp(1,1,1)+c2*zp(2,1,1)
      if (m .gt. 2) zp(1,1,3) = zp(1,1,3)+c3*zp(3,1,1)
c
c obtain x-y-partial derivative at (x(1),y(n))
c
   17 if (ind(7) .eq. 0) go to 18
      l = loc(7)
      zxy1ns = opt(l)
      go to 19
   18 zxy1ns = c1*temp(n+1)+c2*temp(n+2)
      if (m .gt. 2) zxy1ns = zxy1ns+c3*temp(n+3)
c
c obtain x-partial derivative along x = x(m)
c
   19 if (ind(2) .eq. 0) go to 21
      l = loc(2)
      do 20 j = 1,n
        npmpj = npm+j
        temp(npmpj) = opt(l)
   20   l = l+1
   21 if (ind(6)+ind(8) .eq. 2) go to 24
      delxm = x(m)-x(mm1)
      delxmm = delxm+delxm
      if (m .gt. 2) delxmm = x(m)-x(m-2)
      if (delxm .le. 0. .or. delxmm .le. delxm) go to 47
      call ceez (-delxm,-delxmm,sigmax,c1,c2,c3,m)
      if (ind(2) .eq. 1) go to 24
      do 22 j = 1,n
        npmpj = npm+j
   22   temp(npmpj) = c1*z(m,j)+c2*z(mm1,j)
      if (m .eq. 2) go to 24
      do 23 j = 1,n
        npmpj = npm+j
   23   temp(npmpj) = temp(npmpj)+c3*z(m-2,j)
c
c obtain x-y-partial derivative at (x(m),y(1))
c
   24 if (ind(6) .eq. 0) go to 25
      l = loc(6)
      zp(m,1,3) = opt(l)
      go to 26
   25 zp(m,1,3) = c1*zp(m,1,1)+c2*zp(mm1,1,1)
      if (m .gt. 2) zp(m,1,3) = zp(m,1,3)+c3*zp(m-2,1,1)
c
c obtain x-y-partial derivative at (x(m),y(n))
c
   26 if (ind(8) .eq. 0) go to 27
      l = loc(8)
      zxymns = opt(l)
      go to 28
   27 zxymns = c1*temp(npm)+c2*temp(npm-1)
      if (m .gt. 2) zxymns = zxymns+c3*temp(npm-2)
c
c set up right hand sides and tridiagonal system for y-grid
c perform forward elimination
c
   28 del1 = y(2)-y(1)
      if (del1 .le. 0.) go to 47
      deli = 1./del1
      do 29 i = 1,m
   29   zp(i,2,1) = deli*(z(i,2)-z(i,1))
      zp(1,2,3) = deli*(zp(1,2,2)-zp(1,1,2))
      zp(m,2,3) = deli*(temp(npm+2)-temp(npm+1))
      call terms (diag1,sdiag1,sigmay,del1)
      diagi = 1./diag1
      do 30 i = 1,m
   30   zp(i,1,1) = diagi*(zp(i,2,1)-zp(i,1,1))
      zp(1,1,3) = diagi*(zp(1,2,3)-zp(1,1,3))
      zp(m,1,3) = diagi*(zp(m,2,3)-zp(m,1,3))
      temp(1) = diagi*sdiag1
      if (n .eq. 2) go to 34
      do 33 j = 2,nm1
        jm1 = j-1
        jp1 = j+1
        npmpj = npm+j
        del2 = y(jp1)-y(j)
        if (del2 .le. 0.) go to 47
        deli = 1./del2
        do 31 i = 1,m
   31     zp(i,jp1,1) = deli*(z(i,jp1)-z(i,j))
        zp(1,jp1,3) = deli*(zp(1,jp1,2)-zp(1,j,2))
        zp(m,jp1,3) = deli*(temp(npmpj+1)-temp(npmpj))
        call terms (diag2,sdiag2,sigmay,del2)
        diagin = 1./(diag1+diag2-sdiag1*temp(jm1))
        do 32 i = 1,m
   32     zp(i,j,1) = diagin*(zp(i,jp1,1)-zp(i,j,1)-
     *                        sdiag1*zp(i,jm1,1))
        zp(1,j,3) = diagin*(zp(1,jp1,3)-zp(1,j,3)-
     *                      sdiag1*zp(1,jm1,3))
        zp(m,j,3) = diagin*(zp(m,jp1,3)-zp(m,j,3)-
     *                      sdiag1*zp(m,jm1,3))
        temp(j) = diagin*sdiag2
        diag1 = diag2
   33   sdiag1 = sdiag2
   34 diagin = 1./(diag1-sdiag1*temp(nm1))
      do 35 i = 1,m
        npi = n+i
   35   zp(i,n,1) = diagin*(temp(npi)-zp(i,n,1)-
     *                      sdiag1*zp(i,nm1,1))
      zp(1,n,3) = diagin*(zxy1ns-zp(1,n,3)-
     *                    sdiag1*zp(1,nm1,3))
      temp(n) = diagin*(zxymns-zp(m,n,3)-
     *                  sdiag1*zp(m,nm1,3))
c
c perform back substitution
c
      do 37 j = 2,n
        jbak = np1-j
        jbakp1 = jbak+1
        t = temp(jbak)
        do 36 i = 1,m
   36     zp(i,jbak,1) = zp(i,jbak,1)-t*zp(i,jbakp1,1)
        zp(1,jbak,3) = zp(1,jbak,3)-t*zp(1,jbakp1,3)
   37   temp(jbak) = zp(m,jbak,3)-t*temp(jbakp1)
c
c set up right hand sides and tridiagonal system for x-grid
c perform forward elimination
c
      del1 = x(2)-x(1)
      if (del1 .le. 0.) go to 47
      deli = 1./del1
      do 38 j = 1,n
        zp(2,j,2) = deli*(z(2,j)-z(1,j))
   38   zp(2,j,3) = deli*(zp(2,j,1)-zp(1,j,1))
      call terms (diag1,sdiag1,sigmax,del1)
      diagi = 1./diag1
      do 39 j = 1,n
        zp(1,j,2) = diagi*(zp(2,j,2)-zp(1,j,2))
   39   zp(1,j,3) = diagi*(zp(2,j,3)-zp(1,j,3))
      temp(n+1) = diagi*sdiag1
      if (m  .eq. 2) go to 43
      do 42 i = 2,mm1
        im1 = i-1
        ip1 = i+1
        npi = n+i
        del2 = x(ip1)-x(i)
        if (del2 .le. 0.) go to 47
        deli = 1./del2
        do 40 j = 1,n
          zp(ip1,j,2) = deli*(z(ip1,j)-z(i,j))
   40     zp(ip1,j,3) = deli*(zp(ip1,j,1)-zp(i,j,1))
        call terms (diag2,sdiag2,sigmax,del2)
        diagin = 1./(diag1+diag2-sdiag1*temp(npi-1))
        do 41 j = 1,n
          zp(i,j,2) = diagin*(zp(ip1,j,2)-zp(i,j,2)-
     *                        sdiag1*zp(im1,j,2))
   41     zp(i,j,3) = diagin*(zp(ip1,j,3)-zp(i,j,3)-
     *                        sdiag1*zp(im1,j,3))
        temp(npi) = diagin*sdiag2
        diag1 = diag2
   42   sdiag1 = sdiag2
   43 diagin = 1./(diag1-sdiag1*temp(npm-1))
      do 44 j = 1,n
        npmpj = npm+j
        zp(m,j,2) = diagin*(temp(npmpj)-zp(m,j,2)-
     *                      sdiag1*zp(mm1,j,2))
   44   zp(m,j,3) = diagin*(temp(j)-zp(m,j,3)-
     *                      sdiag1*zp(mm1,j,3))
c
c perform back substitution
c
      do 45 i = 2,m
        ibak = mp1-i
        ibakp1 = ibak+1
        npibak = n+ibak
        t = temp(npibak)
        do 45 j = 1,n
          zp(ibak,j,2) = zp(ibak,j,2)-t*zp(ibakp1,j,2)
   45     zp(ibak,j,3) = zp(ibak,j,3)-t*zp(ibakp1,j,3)
      return
c
c too few points
c
   46 ierr = 1
      return
c
c points not strictly increasing
c
   47 ierr = 2
      return
c
c the option vector has an error
c
   48 ierr = 3
      return
      end


      subroutine ceez (del1,del2,sigma,c1,c2,c3,n)
c
      implicit real*8 (a-h,o-z)
      real*8 del1,del2,sigma,c1,c2,c3
c
c                      from the spline under tension package
c                       coded by a. k. cline and r. j. renka
c                            department of computer sciences
c                              university of texas at austin
c
c this subroutine determines the coefficients c1, c2, and c3
c used to determine endpoint slopes. specifically, if
c function values y1, y2, and y3 are given at points x1, x2,
c and x3, respectively, the quantity c1*y1 + c2*y2 + c3*y3
c is the value of the derivative at x1 of a spline under
c tension (with tension factor sigma) passing through the
c three points and having third derivative equal to zero at
c x1. optionally, only two values, c1 and c2 are determined.
c
c on input--
c
c   del1 is x2-x1 (.gt. 0.).
c
c   del2 is x3-x1 (.gt. 0.). if n .eq. 2, this parameter is
c   ignored.
c
c   sigma is the tension factor.
c
c and
c
c   n is a switch indicating the number of coefficients to
c   be returned. if n .eq. 2 only two coefficients are
c   returned. otherwise all three are returned.
c
c on output--
c
c   c1, c2, and c3 contain the coefficients.
c
c none of the input parameters are altered.
c
c this subroutine references package module snhcsh.
c
c-----------------------------------------------------------
c
      if (n .eq. 2) go to 2
      if (sigma .ne. 0.) go to 1
      del = del2-del1
c
c tension .eq. 0.
c
      c1 = -(del1+del2)/(del1*del2)
      c2 = del2/(del1*del)
      c3 = -del1/(del2*del)
      return
c
c tension .ne. 0.
c
    1 call snhcsh (dummy,coshm1,sigma*del1,1)
      call snhcsh (dummy,coshm2,sigma*del2,1)
      delp = sigma*(del2+del1)/2.
      delm = sigma*(del2-del1)/2.
      call snhcsh (sinhmp,dummy,delp,-1)
      call snhcsh (sinhmm,dummy,delm,-1)
      denom = coshm1*(del2-del1)-2.*del1*(sinhmp+delp)*
     *        (sinhmm+delm)
      c1 = 2.*(sinhmp+delp)*(sinhmm+delm)/denom
      c2 = -coshm2/denom
      c3 = coshm1/denom
      return
c
c two coefficients
c
    2 c1 = -1./del1
      c2 = -c1
      return
      end


      subroutine terms (diag,sdiag,sigma,del)
c
      implicit real*8 (a-h,o-z)
      real*8 diag,sdiag,sigma,del
c
c                      from the spline under tension package
c                       coded by a. k. cline and r. j. renka
c                            department of computer sciences
c                              university of texas at austin
c
c this subroutine computes the diagonal and superdiagonal
c terms of the tridiagonal linear system associated with
c spline under tension interpolation.
c
c on input--
c
c   sigma contains the tension factor.
c
c and
c
c   del contains the step size.
c
c on output--
c
c               (sigma*del*cosh(sigma*del) - sinh(sigma*del)
c   diag = del*--------------------------------------------.
c                     (sigma*del)**2 * sinh(sigma*del)
c
c                   sinh(sigma*del) - sigma*del
c   sdiag = del*----------------------------------.
c                (sigma*del)**2 * sinh(sigma*del)
c
c and
c
c   sigma and del are unaltered.
c
c this subroutine references package module snhcsh.
c
c-----------------------------------------------------------
c
      if (sigma .ne. 0.) go to 1
      diag = del/3.
      sdiag = del/6.
      return
    1 sigdel = sigma*del
      call snhcsh (sinhm,coshm,sigdel,0)
      denom = del/((sinhm+sigdel)*sigdel*sigdel)
      diag = denom*(sigdel*coshm-sinhm)
      sdiag = denom*sinhm
      return
      end


      DOUBLE PRECISION function surf2 (xx,yy,m,n,x,y,z,iz,zp,sigma)
c
      implicit real*8 (a-h,o-z)
      integer m,n,iz
      real*8 xx,yy,x(m),y(n),z(iz,n),zp(m,n,*),sigma
c
c                      from the spline under tension package
c                       coded by a. k. cline and r. j. renka
c                            department of computer sciences
c                              university of texas at austin
c
c this function interpolates a surface at a given coordinate
c pair using a bi-spline under tension. the subroutine surf1
c should be called earlier to determine certain necessary
c parameters.
c
c on input--
c
c   xx and yy contain the x- and y-coordinates of the point
c   to be mapped onto the interpolating surface.
c
c   m and n contain the number of grid lines in the x- and
c   y-directions, respectively, of the rectangular grid
c   which specified the surface.
c
c   x and y are arrays containing the x- and y-grid values,
c   respectively, each in increasing order.
c
c   z is a matrix containing the m * n functional values
c   corresponding to the grid values (i. e. z(i,j) is the
c   surface value at the point (x(i),y(j)) for i = 1,...,m
c   and j = 1,...,n).
c
c   iz contains the row dimension of the array z as declared
c   in the calling program.
c
c   zp is an array of 3*m*n locations stored with the
c   various surface derivative information determined by
c   surf1.
c
c and
c
c   sigma contains the tension factor (its sign is ignored).
c
c the parameters m, n, x, y, z, iz, zp, and sigma should be
c input unaltered from the output of surf1.
c
c on output--
c
c   surf2 contains the interpolated surface value.
c
c none of the input parameters are altered.
c
c this function references package modules intrvl and
c snhcsh.
c
c-----------------------------------------------------------
c
c inline one dimensional cubic spline interpolation
c
      hermz (f1,f2,fp1,fp2) = (f2*del1+f1*del2)/dels-del1*
     *                        del2*(fp2*(del1+dels)+
     *                              fp1*(del2+dels))/
     *                        (6.*dels)
c
c inline one dimensional spline under tension interpolation
c
      hermnz (f1,f2,fp1,fp2,sigmap) = (f2*del1+f1*del2)/dels
     *          +(fp2*(sinhm1*del2-del1*(2.*(coshp1+1.)*
     *                           sinhp2+sigmap*coshp1*del2))
     *           +fp1*(sinhm2*del1-del2*(2.*(coshp2+1.)*
     *                           sinhp1+sigmap*coshp2*del1))
     *          )/(sigmap*sigmap*dels*(sinhms+sigmap*dels))
c
c denormalize tension factor in x and y direction
c
      sigmax = dabs(sigma)*float(m-1)/(x(m)-x(1))
      sigmay = dabs(sigma)*float(n-1)/(y(n)-y(1))
c
c determine y interval
c
      jm1 = intrvl (yy,y,n)
      j = jm1+1
c
c determine x interval
c
      im1 = intrvl (xx,x,m)
      i = im1+1
      del1 = yy-y(jm1)
      del2 = y(j)-yy
      dels = y(j)-y(jm1)
      if (sigmay .ne. 0.) go to 1
c
c perform four interpolations in y-direction
c
      zim1 = hermz(z(i-1,j-1),z(i-1,j),zp(i-1,j-1,1),
     *                                  zp(i-1,j,1))
      zi = hermz(z(i,j-1),z(i,j),zp(i,j-1,1),zp(i,j,1))
      zxxim1 = hermz(zp(i-1,j-1,2),zp(i-1,j,2),
     *                zp(i-1,j-1,3),zp(i-1,j,3))
      zxxi = hermz(zp(i,j-1,2),zp(i,j,2),
     *              zp(i,j-1,3),zp(i,j,3))
      go to 2
    1 delp1 = (del1+dels)/2.
      delp2 = (del2+dels)/2.
      call snhcsh (sinhm1,dummy,sigmay*del1,-1)
      call snhcsh (sinhm2,dummy,sigmay*del2,-1)
      call snhcsh (sinhms,dummy,sigmay*dels,-1)
      call snhcsh (sinhp1,dummy,sigmay*del1/2.,-1)
      call snhcsh (sinhp2,dummy,sigmay*del2/2.,-1)
      call snhcsh (dummy,coshp1,sigmay*delp1,1)
      call snhcsh (dummy,coshp2,sigmay*delp2,1)
      zim1 = hermnz(z(i-1,j-1),z(i-1,j),zp(i-1,j-1,1),
     *               zp(i-1,j,1),sigmay)
      zi = hermnz(z(i,j-1),z(i,j),zp(i,j-1,1),zp(i,j,1),
     *             sigmay)
      zxxim1 = hermnz(zp(i-1,j-1,2),zp(i-1,j,2),
     *                 zp(i-1,j-1,3),zp(i-1,j,3),sigmay)
      zxxi = hermnz(zp(i,j-1,2),zp(i,j,2),
     *               zp(i,j-1,3),zp(i,j,3),sigmay)
c
c perform final interpolation in x-direction
c
    2 del1 = xx-x(im1)
      del2 = x(i)-xx
      dels = x(i)-x(im1)
      if (sigmax .ne. 0.) go to 3
      surf2 = hermz(zim1,zi,zxxim1,zxxi)
      return
    3 delp1 = (del1+dels)/2.
      delp2 = (del2+dels)/2.
      call snhcsh (sinhm1,dummy,sigmax*del1,-1)
      call snhcsh (sinhm2,dummy,sigmax*del2,-1)
      call snhcsh (sinhms,dummy,sigmax*dels,-1)
      call snhcsh (sinhp1,dummy,sigmax*del1/2.,-1)
      call snhcsh (sinhp2,dummy,sigmax*del2/2.,-1)
      call snhcsh (dummy,coshp1,sigmax*delp1,1)
      call snhcsh (dummy,coshp2,sigmax*delp2,1)
      surf2 = hermnz(zim1,zi,zxxim1,zxxi,sigmax)
      return
      end


      subroutine snhcsh (sinhm,coshm,x,isw)
c
      implicit real*8 (a-h,o-z)
      integer isw
      real*8 sinhm,coshm,x,cut(5)
c
c                      from the spline under tension package
c                       coded by a. k. cline and r. j. renka
c                            department of computer sciences
c                              university of texas at austin
c                          modified by a.h. morris (nswc/dl)
c
c this subroutine returns approximations to
c       sinhm(x) = sinh(x)-x
c       coshm(x) = cosh(x)-1
c and
c       coshmm(x) = cosh(x)-1-x*x/2
c
c on input--
c
c   x contains the value of the independent variable.
c
c   isw indicates the function desired
c           = -1 if only sinhm is desired,
c           =  0 if both sinhm and coshm are desired,
c           =  1 if only coshm is desired,
c           =  2 if only coshmm is desired,
c           =  3 if both sinhm and coshmm are desired.
c
c on output--
c
c   sinhm contains the value of sinhm(x) if isw .le. 0 or
c   isw .eq. 3 (sinhm is unaltered if isw .eq.1 or isw .eq.
c   2).
c
c   coshm contains the value of coshm(x) if isw .eq. 0 or
c   isw .eq. 1 and contains the value of coshmm(x) if isw
c   .ge. 2 (coshm is unaltered if isw .eq. -1).
c
c and
c
c   x and isw are unaltered.
c
c-----------------------------------------------------------
c
      data sp5/.255251817302048d-09/,
     *     sp4/.723809046696880d-07/,
     *     sp3/.109233297700241d-04/,
     *     sp2/.954811583154274d-03/,
     *     sp1/.452867078563929d-01/,
     *     sq1/-.471329214363072d-02/
      data cp5/.116744361560051d-08/,
     *     cp4/.280407224259429d-06/,
     *     cp3/.344417983443219d-04/,
     *     cp2/.232293648552398d-02/,
     *     cp1/.778752378267155d-01/,
     *     cq1/-.545809550662099d-02/
      data zp3/5.59297116264720d-07/,
     *     zp2/1.77943488030894d-04/,
     *     zp1/1.69800461894792d-02/,
     *     zq4/1.33412535492375d-09/,
     *     zq3/-5.80858944138663d-07/,
     *     zq2/1.27814964403863d-04/,
     *     zq1/-1.63532871439181d-02/
      data cut(1)/1.65/, cut(2)/1.2/, cut(3)/1.2/, cut(4)/2.7/,
     *     cut(5)/1.65/
c
      xx = x
      ax = dabs(xx)
      xs = xx*xx
      if (ax .ge. cut(isw+2)) expx = dexp(ax)
c
c sinhm approximation
c
      if (isw .eq. 1 .or. isw .eq. 2) go to 2
      if (ax .ge. 1.65) go to 1
      sinhm = ((((((sp5*xs+sp4)*xs+sp3)*xs+sp2)*xs+sp1)*xs+1.)
     *        *xs*xx)/((sq1*xs+1.)*6.)
      go to 2
    1 sinhm = -(((ax+ax)+1./expx)-expx)/2.
      if (xx .lt. 0.) sinhm = -sinhm
c
c coshm approximation
c
    2 if (isw .ne. 0 .and. isw .ne. 1) go to 4
      if (ax .ge. 1.2) go to 3
      coshm = ((((((cp5*xs+cp4)*xs+cp3)*xs+cp2)*xs+cp1)*xs+1.)
     *        *xs)/((cq1*xs+1.)*2.)
      go to 4
    3 coshm = ((1./expx-2.)+expx)/2.
c
c coshmm approximation
c
    4 if (isw .le. 1) return
      if (ax .ge. 2.70) go to 5
      coshm = ((((zp3*xs+zp2)*xs+zp1)*xs+1.)*xs*xs)/(((((zq4
     *        *xs+zq3)*xs+zq2)*xs+zq1)*xs+1.)*24.)
      return
    5 coshm = (((1./expx-2.)-xs)+expx)/2.
      return
      end


      function intrvl (t,x,n)
      implicit real*8 (a-h,o-z)
      integer n
      real*8 t,x(n)
c-----------------------------------------------------------
c on input--
c
c   t is a real number.
c
c   x is a vector of strictly increasing values.
c
c   n is the length of x (n .ge. 2).
c
c on output--
c
c   intrvl returns an integer i such that
c
c          i = 1       if             t .lt. x(2)
c          i = n-1     if x(n-1) .le. t
c          otherwise       x(i)  .le. t .lt. x(i+1)
c
c none of the input parameters are altered.
c-----------------------------------------------------------
c
      nm1 = n-1
      if (t.lt.x(2)) go to 50
      if (t.ge.x(nm1)) go to 60
      il = 2
      ir = nm1
c
c bisection search
c
   10 i = (il+ir)/2
      if (i.eq.il) go to 40
      if (t-x(i)) 20,40,30
   20 ir = i
      go to 10
   30 il = i
      go to 10
   40 intrvl = i
      return
c
c left end
c
   50 intrvl = 1
      return
c
c right end
c
   60 intrvl = nm1
      return
      end
