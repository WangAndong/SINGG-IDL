PRO binomial_confinterval, nn, rr, clev, cint, ntest=ntest
   ;
   ; For RR successes in NN trials, determines the confidence interval
   ; CINT for the true incidence rate to some confidence level CLEV. 
   ;
   ; For example for 10 successes in 30 trials and a confidence level 
   ; CLEV = 0.90 (90%) this will (should) return CINT = [0.21, 0.49].  
   ; this means that there is a 90% chance that the true incidence rate 
   ; for success is between 0.21 and 0.49; there is a 5% chance
   ; that the incidence rate is < 0.21 and 5% chance that it is > 0.49.
   ;
   ; nn    -> Number of trials performed
   ; rr    -> Number of successes found  (must be =>0 and <= nn)
   ; clev  -> confidence for determining the error interval.
   ;          This must be a number from 0 - 1
   ; cint  <- interval in true incidence rate corresponding 
   ;          to confidence interval.  
   ; ntest -> This is the number of equally spaced points over
   ;          the true incidence rate 0 - 1 in which to calculate 
   ;          the binomial probability.  The default value is 
   ;          ntest = 101
   ;
   ; note CLEV = 0.683 would result in CINT corresponding to mean
   ; -/+ 1 sigma errors for a normal probability function.
   ;
   ; G. Meurer 05/2007
   nt    = 101
   ;
   ; set nt to optional parameter ntest of passed
   IF keyword_set(ntest) THEN nt = ntest
   ;
   ; set incidence rates to check
   norm  = 1.0/double(nt-1)
   ftest = norm*dindgen(nt)
   ;
   ; calculate binomial probability of measuring
   ; rr successes in nn trials at each of the
   ; true incidence rates ftest
   pbin  = make_array(nt, /double, value=0.0d0)
   FOR ii = 0, nt-1 DO pbin[ii] = binomial(rr, nn, ftest[ii]) - binomial(rr+1, nn, ftest[ii])
   ;
   ; integrate pbin from 0 to ftest, then normalize so that the
   ; total integral = 1 at ftest = 1 
   integral    = 0.0d0*pbin
   integral[0] = 0.5d0*(pbin[0]+pbin[1])
   FOR ii = 1, nt-1 DO integral[ii] = integral[ii-1] + 0.5d0*(pbin[ii-1]+pbin[ii])
   integral    = integral/integral[nt-1]
   ;
   ; calculate prob values corresponding to confidence
   ; interval
   temp        = 0.5d0*(1.0d0 - double(clev))
   pvals       = [temp, 1.0d0-temp]
   gg          = where(integral GE 0.5*pvals[0] AND integral LE (1.0 - 0.5*pvals[0]), ngg)
   linterp, integral[gg], ftest[gg], pvals, cint
END 
