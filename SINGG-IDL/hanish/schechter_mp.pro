FUNCTION schechter_mp,logmass,A
; Function returning a slightly altered Schechter function; the
; difference is that it assumes the value is going to be used with a
; d(log M) instead of dM function.  This results in an extra factor of
; ln(10)*(M/M*) in the calculation.
; This function is for use with the MPFITFUN routine.  Value returned
; is logarithmic.

  al10 = ALOG(10.d0)
  massratio = 10.d0^(logmass-A[1])
  theta = A[0] * massratio^(A[2]+1.0) * al10 / EXP(massratio)
  logtheta = ALOG10(DOUBLE(theta))
  RETURN,logtheta

  dlogtheta = DBLARR(N_ELEMENTS(theta),3)
; Calculate the partial derivatives.  Multiply each of these by the
; correspoding variable's uncertainty to get the actual uncertainty.
; dtheta/dtheta0:
  dlogtheta[*,0] = 1.d0 / (al10*A[0])
; dtheta/drefmass:
  dlogtheta[*,1] = (massratio - (A[2]+1.d0)) / (al10*A[1])
; dtheta/dalpha:
  dlogtheta[*,2] = (logmass - A[1])

  RETURN,logtheta

END
