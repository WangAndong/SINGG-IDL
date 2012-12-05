PRO pfplt_rdhdr, file, pixsize, filename, funits, fscale, fluxcal, $
                 proftype, numgals, galindex, xcenter, ycenter, $
                 axerat, posang, posangwc, skylev, skysigpx, skysigbx, $
                 fluxrad, flux, flsigsky, flsigcnt, hlradius, $
                 hlsigsky, hlcrcflux, lstart, lend, isoflag, netflag
   ;
   ; Read in the header of a *.profile file produced by Dan 
   ; Hanish's code.
   ;
   ; file      -> input .profile file name
   ; pixsize   <> pixel dimension in arcsec
   ;              if no pixsize entry is found then the pixsize passed
   ;              to the routine is adopted.  If pixsize LE 0.0 is
   ;              passed then pisize=1.0 arcsec is adopted.
   ; filename  <- Image file name
   ; funits    <- flux units; string giving the flux units
   ; fscale    <- flux scale; apply this multiplicative scale factor to 
   ;              fluxes in this file (c/s) to get fluxes in the units
   ;              of FUNITS
   ; fluxcal   <- 1b if data is flux calibrated, otherwise 0b
   ; proftype  <- profile type, usually "brightness" or "isophote" 
   ;              indicating how the center of the profile is determined
   ;              (either brightness peak, or center of outer isophotes).
   ; numgals   <- number of galaxy profiles in the file.  
   ;
   ; The following quantities are returned as arrays, one element for 
   ; each profile.
   ;
   ; galindex  <- index number of profile.
   ; xcenter   <- X(center) of profile [pixels]
   ; ycenter   <- Y(center) of profile [pixels]
   ; axerat    <- Axial ratio of profile (a/b)
   ; posang    <- Position angle, counterclockwise from Up [deg]
   ; posangwc  <- Position angle, from N towards E [deg]
   ; skylev    <- Sky level *+
   ; skysigpx  <- Sky RMS (pixel-to-pixel) *+
   ; skysigbx  <- Sky RMS (box-to-box) *+
   ; fluxrad   <- Full flux radius +
   ; flux      <- Full flux *
   ; flsigsky  <- Flux uncertainty due to sky *
   ; flsigcnt  <- Flux uncertainty due to continuum *
   ; hlradius  <- Half-light radius +
   ; hlsigsky  <- Half-light uncertainty due to sky +
   ; hlcrcflux <- Half-light circular flux per pixel *+
   ; lstart    <- line start in profile file for this profile
   ; lend      <- line end in profile file for this profile
   ; isoflag   <- 0B if this is a brightness peak centered profile
   ;              1B if this is an isophote centetered profile
   ; netflag   <- 0b if FUNITS has 'Angstrom' in it or is empty
   ;               signifying an R, Cont, or NB image.
   ;              1b if it does not signifying a net Halpha image.
   ;
   ; items marked * are in flux units [erg/cm^2/sec] if fscale NE 1.0.  
   ;   otherwise they are in [DN/s].
   ; items marked + are in arcsec if PIXSIZE NE 1.0
   ; items marked in *+ are in absolute surface brightness units 
   ;   [erg/cm^2/s/arcsec^2] if both fscale and pixscale are set (NE 1.0).
   ;
   ; G. Meurer 10/2004
   ;
   fmt      = '(a120)'
   readfmt, file, fmt, line
   jj       = where(strpos(line, '#') EQ 0 AND strpos(line, ' = ') GT 0, nhdr)
   hdr      = line[jj]
   ;
   ; make arrays to store things
   keywd    = make_array(nhdr, /string, value=' ')
   value    = make_array(nhdr, /string, value=' ')
   FOR ii = 0, nhdr-1 DO BEGIN 
      str       = hdr[ii]
      dum       = gettok(str, ' ')
      keywd[ii] = gettok(str, ' ')
      dum       = gettok(str, ' ')
      value[ii] = gettok(str, ' ')
   ENDFOR 
   ;
   ; set default pixsize if need be
   IF pixsize LE 0.0 THEN pixsize=1.0
   ;
   ; Now put things in the right place
   filename =  pfplt_kwdread('FILENAME', keywd, value, '', usetype='STRING')
   pixsize  =  pfplt_kwdread('PIXSIZE', keywd, value, pixsize)
   pixarea  =  pixsize*pixsize
   funits   =  pfplt_kwdread('FUNITS', keywd, value, '', usetype='STRING')
   netflag  =  (strpos(strupcase(funits), 'ANGSTROM') LT 0 AND strlen(strtrim(funits,2)) GE 1)
   fluxcal  =  pfplt_kwdread('FLUXCAL', keywd, value, 0b, usetype='BYTE')
   fscale   =  pfplt_kwdread('FSCALE', keywd, value, 1.0)
   proftype =  pfplt_kwdread('PROFTYPE', keywd, value, 'UNKNOWN', usetype='STRING')
   isoflag  =  (strpos(strupcase(proftype), 'ISOPHOTE') GE 0)
   numgals  =  pfplt_kwdread('NUMGALS', keywd, value, 1, usetype='INT')
   ;
   galindex =  pfplt_kwdread('GALINDEX', keywd, value, 0, usetype='INT')
   xcenter  =  pfplt_kwdread('XCENTER', keywd, value, -1.0)
   ycenter  =  pfplt_kwdread('YCENTER', keywd, value, -1.0)
   axerat   =  pfplt_kwdread('AXERAT', keywd, value, -1.0)
   posang   =  pfplt_kwdread('POSANG', keywd, value, -1.0)
   posangwc =  pfplt_kwdread('POSANGWC', keywd, value, -1.0)
   skylev   =  pfplt_kwdread('SKYLEV', keywd, value, 0.0)*fscale/pixarea
   skysigpx =  pfplt_kwdread('SKYSIGPX', keywd, value, 0.0)*fscale/pixarea
   skysigbx =  pfplt_kwdread('SKYSIGBX', keywd, value, 0.0)*fscale/pixarea
   fluxrad  =  pfplt_kwdread('FLUXRAD', keywd, value, 0.0)*pixsize
   flux     =  pfplt_kwdread('FLUX', keywd, value, 0.0)*fscale
   flsigsky =  pfplt_kwdread('FLSIGSKY', keywd, value, 0.0)*fscale
   flsigcnt =  pfplt_kwdread('FLSIGCNT', keywd, value, 0.0)*fscale
   hlradius =  pfplt_kwdread('HLRADIUS', keywd, value, 0.0)*pixsize
   hlsigsky =  pfplt_kwdread('HLSIGSKY', keywd, value, 0.0)*pixsize
   hlcrxflx =  pfplt_kwdread('HLCRCFLX', keywd, value, 0.0)*fscale/pixarea
   ;
   ; find the start and end lines of each non header section
   jj        =  [00, where(strpos(line, '#') EQ -1 AND strlen(strtrim(line,2)) GT 0, njj)]
   kk0       =  indgen(njj)
   kk1       =  (indgen(njj)+1) MOD njj
   ldif      =  jj[kk1] - jj[kk0]
   ll        =  where(ldif GT 1, nll)
   IF nll NE numgals THEN print, 'Number of profile NE number of galaxies : ', nll, numgals
   lend      =  make_array(nll, /int, value=jj[njj-1]+1)
   lstart    =  jj[ll+1]+1
   FOR ii = 1, nll-1 DO lend[ii-1] = jj[ll[ii]]+1
END
