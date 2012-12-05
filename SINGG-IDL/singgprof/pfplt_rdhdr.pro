PRO pfplt_rdhdr, file, pixsize, filename, funits, fscale, fluxcal, $
                 proftype, numgals, galindex, xcenter, ycenter, $
                 axerat, posang, posangwc, skylev, skysigpx, skysigbx, $
                 rads, radf, radc, fluxs, fluxf, fluxt, flsigskys, flsigskyf, flsigskyt, $
                 flsigcnts, flsigcntf, flsigcntt, ref, ret, $
                 resigskyf, resigskyt, resigcntf, resigcntt, sef, set, $
                 lstart, lend, isoflag, netflag, silent=silent
   ;
   ; Read in the header of a *.profile file produced by Dan 
   ; Hanish's code.
   ;
   ; file      -> input .profile file name
   ; pixsize   <> pixel dimension in arcsec
   ;              if no pixsize entry is found then the pixsize passed
   ;              to the routine is adopted.  If pixsize LE 0.0 is
   ;              passed then pixsize=1.0 arcsec is adopted.
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
   ; rads      <- radius of sky ap +
   ; radf      <- full flux radius +
   ; radc      <- radius where S/N > 3 +
   ; fluxs     <- flux within rads *
   ; fluxf     <- flux within radf *
   ; fluxt     <- adopted total flux *
   ; flsigskys <- uncertainty of fluxs due to sky *
   ; flsigskyf <- uncertainty of fluxf due to sky *
   ; flsigskyt <- uncertainty of fluxt due to sky *
   ; flsigcnts <- uncertainty of fluxs due to continuum subtraction *
   ; flsigcntf <- uncertainty of fluxf due to continuum subtraction *
   ; flsigcntf <- uncertainty of fluxt due to continuum subtraction *
   ; ref       <- Half-light radius for flux within radf +
   ; ret       <- Half-light radius for total flux +
   ; resigskyf <- ref uncertainty due to sky +
   ; resigskyt <- ret uncertainty due to sky +
   ; resigcntf <- ref uncertainty due to continuum subtraction +
   ; resigcntt <- ret uncertainty due to continuum subtraction +
   ; sef       <- surface brightness within ref *+
   ; set       <- surface brightness within ret *+
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
   ; G. Meurer 5/2010  revised to use more recent kwds of D. Hanish
   ;
   ; first read in the whole file, then just save the header lines.
   fmt      = '(a120)'
   readfmt, file, fmt, line, silent=silent
   jj       = where(strpos(line, '#') EQ 0 AND strpos(line, ' = ') GT 0, nhdr)
   hdr      = line[jj]
   ;
   ; make arrays to store things
   keywd    = make_array(nhdr, /string, value=' ')
   value    = make_array(nhdr, /string, value=' ')
   ;
   ; keywd is array of all keywords in header lines
   ; value is array of the corresponding columns
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
   filename  =  pfplt_kwdread('FILENAME', keywd, value, '', usetype='STRING')
   pixsize   =  pfplt_kwdread('PIXSIZE', keywd, value, pixsize)
   pixarea   =  pixsize*pixsize
   funits    =  pfplt_kwdread('FLUX_UNITS', keywd, value, '', usetype='STRING')
   netflag   =  (strpos(strupcase(funits), 'ANGSTROM') LT 0 AND strlen(strtrim(funits,2)) GE 1)
   fluxcal   =  pfplt_kwdread('FLUXCAL', keywd, value, 0b, usetype='BYTE')
   fscale    =  pfplt_kwdread('FLUX_SCALE', keywd, value, 1.0)
   proftype  =  pfplt_kwdread('PROFTYPE', keywd, value, 'UNKNOWN', usetype='STRING')
   isoflag   =  (strpos(strupcase(proftype), 'ISOPHOTE') GE 0)
   numgals   =  pfplt_kwdread('NUMGALS', keywd, value, 1, usetype='INT')
   ;
   galindex  =  pfplt_kwdread('GALINDEX', keywd, value, 0, usetype='INT')
   xcenter   =  pfplt_kwdread('XCENTER', keywd, value, -1.0)
   ycenter   =  pfplt_kwdread('YCENTER', keywd, value, -1.0)
   axerat    =  pfplt_kwdread('AXERAT', keywd, value, -1.0)
   posang    =  pfplt_kwdread('PA_IMAGE', keywd, value, -1.0)
   posangwc  =  pfplt_kwdread('PA', keywd, value, -1.0)
   skylev    =  pfplt_kwdread('SKYLEV', keywd, value, 0.0)*fscale/pixarea
   skysigpx  =  pfplt_kwdread('ERR_SKY_PIXEL', keywd, value, 0.0)*fscale/pixarea
   skysigbx  =  pfplt_kwdread('ERR_SKY_BX', keywd, value, 0.0)*fscale/pixarea
   rads      =  pfplt_kwdread('FLUXRAD_S', keywd, value, 0.0)*pixsize
   radf      =  pfplt_kwdread('FLUXRAD_F', keywd, value, 0.0)*pixsize
   radc      =  pfplt_kwdread('FLUXRAD_C', keywd, value, 0.0)*pixsize
   fluxs     =  pfplt_kwdread('FLUX_S', keywd, value, 0.0)*fscale
   fluxf     =  pfplt_kwdread('FLUX_F', keywd, value, 0.0)*fscale
   fluxt     =  pfplt_kwdread('FLUX_T', keywd, value, 0.0)*fscale
   flsigskys =  pfplt_kwdread('ERR_FLUX_S_SKY', keywd, value, 0.0)*fscale
   flsigskyf =  pfplt_kwdread('ERR_FLUX_F_SKY', keywd, value, 0.0)*fscale
   flsigskyt =  pfplt_kwdread('ERR_FLUX_T_SKY', keywd, value, 0.0)*fscale
   flsigcnts =  pfplt_kwdread('ERR_FLUX_S_CONT', keywd, value, 0.0)*fscale
   flsigcntf =  pfplt_kwdread('ERR_FLUX_F_CONT', keywd, value, 0.0)*fscale
   flsigcntt =  pfplt_kwdread('ERR_FLUX_T_CONT', keywd, value, 0.0)*fscale
   ref       =  pfplt_kwdread('RE_F', keywd, value, 0.0)*pixsize
   ret       =  pfplt_kwdread('RE_T', keywd, value, 0.0)*pixsize
   resigskyf =  pfplt_kwdread('ERR_RE_F_SKY', keywd, value, 0.0)*pixsize
   resigskyt =  pfplt_kwdread('ERR_RE_T_SKY', keywd, value, 0.0)*pixsize
   resigcntf =  pfplt_kwdread('ERR_RE_F_CONT', keywd, value, 0.0)*pixsize
   resigcntt =  pfplt_kwdread('ERR_RE_T_CONT', keywd, value, 0.0)*pixsize
   sef       =  pfplt_kwdread('SE_F', keywd, value, 0.0)*fscale/pixarea
   set       =  pfplt_kwdread('SE_T', keywd, value, 0.0)*fscale/pixarea
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
