PRO schechter,logmass,A,theta,dtheta,NOLOG=nolog
; Function returning a slightly altered Schechter function; the
; difference is that it assumes the value is going to be used with a
; d(log M) instead of dM function.  This results in an extra factor of
; ln(10)*(M/M*) in the calculation.

  al10 = ALOG(10.d0)
  massratio = 10.d0^(logmass-A[1])
  theta = DOUBLE(A[0] * massratio^(A[2]+1.0) * al10 / EXP(massratio))
  IF KEYWORD_SET(nolog) THEN theta = DOUBLE(A[0] * massratio^(A[2]) / EXP(massratio))

  dtheta = DBLARR(N_ELEMENTS(theta),3)
; Calculate the partial derivatives.  Multiply each of these by the
; correspoding variable's uncertainty to get the actual uncertainty.
; dtheta/dtheta0:
  dtheta[*,0] = theta / A[0]
; dtheta/drefmass:
  dtheta[*,1] = theta * (massratio - (A[2]+1.d0))/A[1]
  IF KEYWORD_SET(nolog) THEN dtheta[*,1] = theta * (A[2]/massratio - 1.d0)
; dtheta/dalpha:
  dtheta[*,2] = theta * al10 * (logmass - A[1])

  RETURN

END
