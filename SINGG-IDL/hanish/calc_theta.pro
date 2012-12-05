FUNCTION calc_theta,logmass,theta0,refmass,alpha,sigma
; Given a mass, and using the HIMF given, return the
; density for that mass.
; INPUTS
;  logmass       Galaxy HI mass, in solar units (not logarithmic)
;  theta0[2,N]   HI Mass Function parameters.  Normally these will
;  refmass[2,N]    be two scalars(value and sigma), but if we're using
;  alpha[2,N]      Monte Carlo logic they'll be 2D arrays.
; OUTPUTS
;  (function)[N] Theta, in units of d(log MHI)
;  sigma[2]      Uncertainty in theta (+,-)

  theta = 0.d0

  sz = SIZE(theta0)
  IF sz[0] EQ 1 THEN BEGIN
; We're not using Monte Carlo logic, so do this the easy way.
    Schechter,logmass,[theta0[0],refmass[0],alpha[0]],theta,dtheta

;; double-check the math on this sigth.
    sigth = SQRT((dtheta[0]*theta0[1])^2 + (dtheta[1]*refmass[1])^2 + (dtheta[2]*alpha[1])^2)
    sigma = [sigth,sigth]

  ENDIF ELSE BEGIN
    num_monte = sz[2]
    bigth = DBLARR(num_monte)
;    bigdth = DBLARR(num_monte,3) ; Not used

    FOR ii = LONG(0),LONG(num_monte)-1 DO BEGIN
      Schechter,logmass,[theta0[0,ii],refmass[0,ii],alpha[0,ii]],th,dth
      bigth[ii] = th
    ENDFOR

    theta=bigth
;    theta = MEAN(bigth)
    sigma = sig_pm(bigth)

  ENDELSE

  RETURN,theta

END
