PRO seterrmod2, hdr, sky=sky, dark=dark, verbose=verbose, tnorm=tnorm, covar=covar
   COMMON errmod, varpix
   IF covar GT 1.0 THEN cv = covar ELSE cv = 1.0
   ; Use parameters in grism image header to estimate mean 
   ; variance per pixel sky level and dark rate can be set 
   ; on command line.
   f    = make_array(4,value=0.0)
   f[0] = sxpar(hdr, 'ATODGNA', count=na)
   f[1] = sxpar(hdr, 'ATODGNB', count=nb)
   f[2] = sxpar(hdr, 'ATODGNC', count=nc)
   f[3] = sxpar(hdr, 'ATODGND', count=nd)
   nt   = na + nb + nc + nd
   ;
   IF nt EQ 4 THEN gain = total(f) / float(nt) ELSE gain = 1.0
   f    = make_array(4,value=0.0)
   f[0] = sxpar(hdr, 'READNSEA', count=na)
   f[1] = sxpar(hdr, 'READNSEB', count=nb)
   f[2] = sxpar(hdr, 'READNSEC', count=nc)
   f[3] = sxpar(hdr, 'READNSED', count=nd)
   nt   = na + nb + nc + nd
   IF nt EQ 4 THEN rn = total(f) / float(nt) ELSE rn = 5.0
   ;
   exptime = sxpar(hdr, 'EXPTIME', count=na)
   nreads  = sxpar(hdr, 'NCOMBINE', count=nb)
   IF keyword_set(dark) THEN drate = dark ELSE drate = 0.0
   IF keyword_set(sky) THEN skylev = sky ELSE skylev = sxpar(hdr, 'ALIGNSKY',count=nc)
   ;
   varrn    = nreads*rn*rn 
   vardark  = drate*exptime 
   varsky   = skylev*gain
   varpix   = (varsky + varrn + vardark)/(gain * cv)
   IF keyword_set(tnorm) THEN varpix = varpix/exptime
   ;
   IF keyword_set(verbose) THEN BEGIN
      IF keyword_set(covar) THEN print,'covar   = ',cv    
      print,'Nreads  = ',nreads
      print,'drate   = ',drate
      print,'gain    = ',gain
      print,'rn      = ',rn
      print,'exptime = ', exptime
      print,'skylev  = ', skylev
      print,'varrn   = ', varrn
      print,'vardark = ', vardark
      print,'varsky  = ', varsky
      print,'varpix  = ', varpix
   ENDIF 
END 

