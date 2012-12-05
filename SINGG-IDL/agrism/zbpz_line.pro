PRO zbpz_line, lamline, elam, zbpz, zbpzmin, zbpzmax, oddsbpz, threshodds, $
               zline, oddsguess, peakid, lam0, lineid, nmatch, status
   ;
   ; Estimate best guess redshift of line, using its wavelength and 
   ; redshift estimates from BPZ.
   ;
   ; lamline    -> wavelength of line
   ; elam       -> uncertainty in wavelength
   ; zbpz       -> position of redshift peaks from bpz
   ; zbpzmin    -> min redshift of range of each peak
   ; zbpzmax    -> max redshift of range of each peak
   ; oddsbpz    -> odds of peaks from bpz
   ; threshodds -> odds threshold for considering peaks
   ; zline      <- best guess redshift of line
   ; peakid     <- position of peak in zbpz array
   ; lam0       <- rest wavelength of line
   ; lineid     <- line identification
   ; nmatch     <- number of lines within any valid peaks
   ; status     <- status of identification
   ;              -1: invalid bpz results
   ;               0: best match corresponds to peak with
   ;                  highest odds.
   ;               1: best match corresponds to lower odds 
   ;                  peak.
   ;               2: closest redshift match corresponds to
   ;                  highest odds peak but redshift not in
   ;                  the allowed redshift range of this peak;
   ;                  selected redhsift within range of lower
   ;                  odds peak.
   ;               3: no lines in any redshift range
   ;
   ; 08/27/04 - [OIII] wavelength taken to be weighted average of 4959 
   ;            and 5006.  [OII] wavelength taken to be at 
   ;            n_e = 100 / cm^3.  wavelengths converted to vacuum.
   ;
   ; G. Meurer 08/2004
   ;wl0        = [6562.817, 5006.85, 4861.33, 3727., 1216.]
   ;nam0       = ['H-alpha', '[OIII]', 'H-beta', '[OII]', 'Ly-alpha']
   wl0        = [6564.61, 4996.54, 3728.66, 1216.]
   nam0       = ['H-alpha', '[OIII]', '[OII]', 'Ly-alpha']
   nl         = n_elements(wl0)
   ;
   ; find bpz results to consider
   k          = where(oddsbpz GE threshodds AND zbpz GE 0.0,nk)
   IF nk GT 0 THEN BEGIN 
      ;
      ; there are valid bpz results to consider...
      ;
      ; make arrays to store things
      dzmin      = make_array(nk, /float, value=1000.0)
      idmin      = make_array(nk, /int, value=-1)
      nline      = make_array(nk, /int, value=0)
      ;
      ; calculate possible redshifts of line +/- uncertainty
      z          = (lamline/wl0 - 1.0)
      ez         = elam/wl0
      ;
      ; loop through all valid bpz results & store:
      ;  - idmin = array position in wl0,nam0 of line closest to zbpz
      ;  - dzmin = redshift difference of line closest to zbpz
      ;  - nline = number of lines within allowed range
      FOR i = 0, nk-1 DO BEGIN 
         j        = k[i]
         dz       = z - zbpz[j] 
         adz      = abs(dz)
         ll       = sort(adz)
         idmin[j] = ll[0]
         dzmin[j] = dz[ll[0]]
         good     = where(zbpzmin[j] LE z+ez AND zbpzmax[j] GE z-ez,ngood)
         nline[j] = ngood
      ENDFOR 
      jj         = sort(abs(dzmin))
      mm         = sort(abs(dzmin) + 1000.0*float(nline LE 0.0))
      nmatch     = fix(total(nline))
      lam0       = wl0[idmin[mm[0]]]
      lineid     = nam0[idmin[mm[0]]]
      zline      = lamline/lam0 - 1.0
      peakid     = k[mm[0]]
      oddsguess  = oddsbpz[peakid]
      ;
      ; set status 
      IF nmatch EQ 0 THEN BEGIN 
         status = 3
      ENDIF ELSE BEGIN 
         hpeak   = reverse(sort(oddsbpz))
         IF peakid EQ hpeak[0] THEN BEGIN 
            status = 0
         ENDIF ELSE BEGIN 
            IF jj[0] EQ mm[0] THEN BEGIN 
               status = 1
            ENDIF ELSE BEGIN 
               IF jj[0] EQ hpeak[0] THEN status = 2 ELSE status = 1
            ENDELSE 
         ENDELSE 
      ENDELSE 
   ENDIF ELSE BEGIN 
      ;
      ; no valid bpz results found return defaults
      zline      = -1.0
      oddsguess  = -1.0
      peakid     = -1
      lam0       = -1.0
      lineid     = 'INVALID'
      status     = -1
      nmatch     =  0
   ENDELSE 
   ; print, zline, oddsguess, peakid, lam0, lineid, nmatch, status
   ;
END 
