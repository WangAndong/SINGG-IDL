PRO singg_comp_filtdb, fnamarr, frat, datfile, pivwv, bandw, fwhm, avgwv, equvw, rectw
   
   ; Eventually this routine will compile all data on the 
   ; filters used by SINGG.
   ;
   ; fnamarr   <- array of filter names and aliases
   ; filt_tfil <- name of file containing transmission curve.
   ; frat      <- f ratio of transmission curve.
   ; pivwv     <- Pivot wavelength [AA] of filter from synphot.
   ; bandw     <- Synphot RMS bandwidth [AA].
   ; fwhm      <- Band FWHM [AA] from synphot.
   ; avgwv     <- Average wavelength [AA] from synphot.
   ; equvw     <- band equivalent width [AA] from synphot.
   ; rectw     <- band equivalent width [AA] from synphot.
   ; 
   ; G. Meurer 2/2003
   ;
   ; Read in filter names
   filt_rddbfnames,'filter',fnamarr
   ;
   ; Initialize bandpass data arrays
   szfn    = size(fnamarr)
   nfilts  = szfn[1]
   frat    = make_array(nfilts, /float, value=-1.0)
   datfile = make_array(nfilts, /string, value='')
   pivwv   = frat
   bandw   = frat
   fwhm    = frat
   avgwv   = frat
   equvw   = frat
   rectw   = frat
   ;
   ; Read in filter bandpass data
   dbopen,'filter',0
   dbext,-1,'NAME,FRAT,FILENAME,PIVWV,BANDW,FW50,AVGWV,EQUVW,RECTW', $
             filtstr, frat_, datfile_, pivwv_, bandw_, fwhm_, $
             avgwv_, equvw_, rectw_
   dbclose

   ; correlate with official names & fill bandpass data arrays
   fnam            = singg_filtnam(fnamarr, filtstr, pos)
   k               = WHERE(pos GE 0, ngood)
   frat[pos[k]]    = frat_[k]
   datfile[pos[k]] = !singgdir+'Filters/'+datfile_[k]
   pivwv[pos[k]]   = pivwv_[k]
   bandw[pos[k]]   = bandw_[k]
   fwhm[pos[k]]    = fwhm_[k]
   avgwv[pos[k]]   = avgwv_[k]
   equvw[pos[k]]   = equvw_[k]
   rectw[pos[k]]   = rectw_[k]
   
   ; Note non matches:
   ibad = WHERE(pos LT 0, nbad)
   IF nbad GT 0 THEN BEGIN 
      PRINT, '**** Warning the following filters with Bandpass data are not in SINGG filter list:'
      FOR ii = 0, nbad-1 DO BEGIN
         PRINT, filtstr[ibad[ii]]
      ENDFOR 
   ENDIF 
   ibad = WHERE(frat LE -1.0, nbad)
   IF nbad GT 0 THEN BEGIN 
      PRINT, '**** Warning the following SINGG filters do not have Bandpass data:'
      FOR ii = 0, nbad-1 DO BEGIN
         PRINT, fnamarr[ibad[ii],1]
      ENDFOR 
   ENDIF

   RETURN
END 
