subroutine inithiv(odeparms)
      external odeparms
      double precision parms(6)
      common /myparms/parms
      call odeparms(6, parms)
      return
end
      
subroutine derivshiv (neq, t, y, ydot, yout, ip)
      double precision t, y, ydot, yout
      double precision bet,rho,delt,c,lam,N
      common /myparms/ bet,rho,delt,c,lam,N
      integer neq, ip(*)
      dimension y(3), ydot(3), yout(1)
      if(ip(1) < 1) call rexit("nout should be at least 1")
      ydot(1) = lam -rho*y(1) - bet*y(1)*y(3)
      ydot(2) = bet*y(1)*y(3) -delt*y(2)
      ydot(3) = N*delt*y(2) - c*y(3) - bet*y(1)*y(3)
      yout(1) = log(y(3))
      return
end
