FUNCTION create_zbin, minstep, fmult, rmax, zmax
  ;
  ; create radial bins in scaled radius z.  The step size in radius,
  ; rr is initially linear and set to minstep.  At large rr it becomes
  ; multiplicative with successive bins large by a factor fmult.
  ;
  ; G. Meurer, 5/2010 ICRAR/UWA
  ;
  rbreak      = minstep/(fmult - 1.0)    ; at rr > rbreak we need to go to multiplicative stepping
  nlin        = ceil(rbreak/minstep)     ; number of linearly stepped bins
  rbreak1     = minstep*float(nlin)      ; actual radius of 1st break (may be > rbreak)
  dlog        = alog(fmult)             ; logarithmic step size
  nlog1       = fix(alog(rmax/rbreak1)/dlog) > 0 ; number of bins in first log stepping sect
  ntot1       = nlin+nlog1                       ; total bins to rmax
  rmax2       = zmax*rmax                        ; rr at zmax
  nlog2       = fix(alog(rmax2/rmax)/dlog) > 0   ; number of bins in 2nd log stepping sect
  ntot2       = ntot1+nlog2                      ; total number of bins
  rr          = minstep*(1.0+findgen(nlin))       ; linear section
  IF nlog1 GT 0 THEN rr = [rr, rr[nlin-1]*exp((1.0+findgen(nlog1))*dlog)]  ; 1st log sect rbreak1:rmax
  IF nlog2 GT 0 THEN rr = [rr, exp(alog(rmax) + (1.0+findgen(nlog2))*dlog)] ; 2nd log sect rmax:zmax*rmax
  rr[ntot1-1] = rmax
  rr[ntot2-1] = rmax2
  zz          = rr/rmax
  return, zz
END 
