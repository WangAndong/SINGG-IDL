PRO zmultiline, lambda, elambda, lambda0, linid, lstat, zmulti, ezmulti
   ;
   ; Determine redshifts from multiple lines using the ratios 
   ; of observed wavelengths.
   ;
   ; lambda  -> Array of input wavelengths.  Must have at least
   ;            two elements.
   ; elambda -> uncertainty in lambda
   ; lambda0 <- rest wavelength of lines
   ; linid   <- line identication string
   ; lstat   <- identification status for each line
   ;            0 : line identification unique
   ;            1 : ambiguous: multiple identifications 
   ;                best match returned
   ;            2 : other input lines have the same identification.
   ;            4 : line unidentified
   ; zmulti  <- redshift: average of all lines with lstat=0, or
   ;            all lines with lstat=1, if there are no cases of
   ;            lstat=0 or 2.  Returned as zmulti=-1.0 if no 
   ;            identifications found.
   ; ezmulti <- Error in zmulti or 0.0
   ;
   ; G. Meurer 12/2004
   ;
   ; initialize outputs, make sure there are enough inputs
   nl       = n_elements(lambda)
   lambda0  = make_array(nl, value=-1.0)
   linid    = make_array(nl, /string, value='NONE')
   lstat    = make_array(nl, /int, value=4)
   zmulti   = -1.0
   ezmulti  = -1.0
   IF nl LE 1 THEN BEGIN 
      print,'Not enough lines to run zmultiline.  Need at least 2'
      return
   ENDIF 
   ;
   ; calculate observed ratios and errors
   ll       = sort(lambda)
   lam      = lambda[ll]
   elam     = elambda[ll]
   nrat     = 0
   FOR jj = 1, nl-1 DO nrat = nrat + jj
   rat      = make_array(nrat, /float)
   erat     = make_array(nrat, /float)
   kbl      = make_array(nrat, /int)
   krd      = make_array(nrat, /int)
   kk       = 0
   FOR jj = 0, nl-2 DO BEGIN
      FOR ii = jj+1, nl-1 DO BEGIN 
         rat[kk]  = lam[ii]/lam[jj]
         erat[kk] = abs(rat[kk])*sqrt((elam[jj]/lam[jj])^2 + (elam[ii]/lam[ii])^2)
         kbl[kk]  = ll[jj]
         krd[kk]  = ll[ii]
         ; print, rat[kk], erat[kk], kbl[kk], krd[kk]
         kk       = kk + 1
      ENDFOR 
   ENDFOR 
   ;
   ; Define lines to consider
   lam0     = [1216., 3728.66, 4342.90, 4862.69, 4996.54, 6564.61]
   id0      = ['Lya', '[OII]', 'H-gamma', 'H-beta', '[OIII]', 'H-alpha']
   nl0      = n_elements(lam0)
   qq0      = make_array(nl0, /int, value=-1)   ; point to position in input lambda arr
   ;
   ; calculate intrinsic ratios
   nrat0    = 0
   FOR jj = 1, nl0-1 DO nrat0 = nrat0 + jj
   ;
   blptr    = make_array(nrat0)
   rdptr    = make_array(nrat0)
   rat0     = make_array(nrat0)
   kk       = 0
   FOR jj = 0, nl0-2 DO BEGIN 
      FOR ii = jj+1, nl0-1 DO BEGIN 
         rat0[kk]  = lam0[ii]/lam0[jj]
         blptr[kk] = jj
         rdptr[kk] = ii
         kk        = kk + 1
      ENDFOR 
   ENDFOR 
   ;
   ; sort, reassign
   ;kk       = sort(rat)
   ;rat      = rat[kk]
   ;blptr    = blptr[kk]
   ;rdptr    = rdptr[kk]
   ;
   ; Find matches
   FOR ii = 0, nrat-1 DO BEGIN 
      drat0 = abs(rat0 - rat[ii])
      kk    = where(drat0 LE erat[ii], nkk)
      kk    = sort(drat0)
      IF nkk EQ 0 THEN BEGIN  
         print, 'No match for ratio, error, lines: ', rat[ii], erat[ii], lambda[kbl[ii]], lambda[krd[ii]]
      ENDIF ELSE BEGIN 
         ppbl   = kbl[ii]
         pprd   = krd[ii]
         testbl = lstat[ppbl]
         testrd = lstat[pprd]
         IF testbl EQ 4 THEN BEGIN 
            linid[ppbl]     = id0[blptr[kk[0]]]
            lambda0[ppbl]   = lam0[blptr[kk[0]]]
            lstat[ppbl]     = 0
         ENDIF ELSE BEGIN 
            IF testbl EQ 1 AND linid[ppbl] EQ id0[blptr[kk[0]]] THEN lstat[ppbl]   = 0
         ENDELSE 
         IF nkk GT 1 AND (lstat[ppbl] EQ 0 OR lstat[ppbl] EQ 2) THEN lstat[ppbl] = lstat[ppbl] + 1
         IF testrd EQ 4 THEN BEGIN 
            linid[pprd]     = id0[rdptr[kk[0]]]
            lambda0[pprd]   = lam0[rdptr[kk[0]]]
            lstat[pprd]     = 0
         ENDIF ELSE BEGIN 
            IF testrd EQ 1 AND linid[pprd] EQ id0[rdptr[kk[0]]] THEN lstat[pprd]   = 0
         ENDELSE 
         IF nkk GT 1 AND (lstat[pprd] EQ 0 OR lstat[pprd] EQ 2) THEN lstat[pprd] = lstat[pprd] + 1
         IF qq0[blptr[kk[0]]] EQ -1 THEN qq0[blptr[kk[0]]] = ppbl $
         ELSE IF qq0[blptr[kk[0]]] NE ppbl AND (lstat[ppbl] EQ 0 OR lstat[ppbl] EQ 1) THEN lstat[ppbl] = lstat[ppbl] + 2
         IF qq0[rdptr[kk[0]]] EQ -1 THEN qq0[rdptr[kk[0]]] = pprd $
         ELSE IF qq0[rdptr[kk[0]]] NE pprd AND (lstat[pprd] EQ 0 OR lstat[pprd] EQ 1) THEN lstat[pprd] = lstat[pprd] + 2
      ENDELSE 
   ENDFOR 
   ;
   ; Make sure all linids values with lstat=2,3 have consistent 
   ; lstats for lines with same id.
   FOR ii = 0, nl0-1 DO BEGIN 
      kk = where(linid EQ id0[ii], nkk)
      IF nkk GT 1 THEN BEGIN 
         jj = where(lstat[kk] EQ 0 OR lstat[kk] EQ 1, njj)
         IF njj GT 0 THEN lstat[kk[jj]] = lstat[kk[jj]] + 2 
      ENDIF 
   ENDFOR 
   ;
   ; assign redshift.
   ; - If there are lines with lstat = 0, use only those
   ; - if not and there are lines with lstat = 1,2,3 use those
   kk = where(lstat EQ 0, nkk)
   IF nkk EQ 0 THEN kk = where(lstat LE 3, nkk)
   IF nkk GT 0 THEN BEGIN 
      zz      = (lambda[kk]/lambda0[kk] - 1.0)
      varzz   = (lambda[kk]/lambda0[kk])^2*(elambda[kk]/lambda[kk])^2
      IF nkk GT 1 THEN poop = moment(zz) ELSE poop = [zz[0], 0.0]
      zmulti  = poop[0]
      ezmulti = sqrt(max([poop[1], total(varzz/float(nkk))]))
   ENDIF 
END 
   ; lstat   <- identification status for each line
   ;            0 : line identification unique
   ;            1 : ambiguous: multiple identifications 
   ;                best match returned
   ;            2 : other input lines have the same identification.
   ;            4 : line unidentified
