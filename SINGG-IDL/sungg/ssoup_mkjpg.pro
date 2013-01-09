PRO ssoup_mkjpg, ll, imcube, photfl, photplam, filo, ebv=ebv, $
                 highcut=highcut, maskcmd=maskcmd, omask=omask, smask=smask, $
                 goslow=goslow
   ;
   ; make 3 color images.
   ; 
   ;  ll       -> logical unit of log file (should be open)
   ;  imcube   -> image cube storing the multi-wavelength data
   ;              in individual planes
   ;  photfl   -> the PHOTFLAM or PHOTFLUX (Halpha) for each plane
   ;              of the cube
   ;  photplam -> Pivot wavelength of each of the planes.
   ;  filo     -> Output filenames.  This should be a 4 element arrays
   ;              for element ; R,G,B pairs:
   ;              0 ; HALPHA,NUV,FUV
   ;              1 ; HALPHA,R,FUV
   ;              2 ; HALPHA,R,NUV
   ;              3 ; R,NUV,FUV
   ;  ebv      -> galactic foreground extinction
   ;  highcut  -> if set the maximum levels will be at the "high" level
   ;              otherwise the "low" levels will be used
   ;  maskcmd  -> details what type of masking is to be done
   ;               0 : no mask applied (default)
   ;               1 : bad objects masked out
   ;               2 : show only pixels used for sky calcs
   ;               3 : show only pixels used to measure objects
   ;              -1 : inverse of 1
   ;              -2 : inverse of 2
   ;              -3 : inverse of 3
   ;  omask    -> bad objects mask
   ;  smask    -> sky mask
   ;  goslow   -> if set then stragetically placed calls to 
   ;              keywait.pro are used to slow down the processing
   ;              to a speed a user can monitor.
   ;
   ; G. Meurer (ICRAR/UWA) 06/2010  based on sample.pro by Ji Hoon Kim
   COMMON bands, band, nband, bandnam, bandavail, nbandavail, combo, ncombo 
   minr_h  = -2.0e-19
   minr_l  = -1.0e-19
   maxr_h  =  2.0e-16
   maxr_l  =  2.0e-17
   minh_l  = -7.0e-18
   minh_h  = -1.4e-17
   maxh_l  =  1.4e-16
   maxh_h  =  1.4e-15
   ;beta    = -1
   beta    = 0
   prog    = 'SSOUP_MKJPG: '
   ;
   plog,ll,prog,'------------------------- starting '+prog+'-------------------------------'
   slow    = keyword_set(goslow)
   ;
   ; pointers to bands
   kk = intarr(nbandavail)
   for i=0,nbandavail-1 do begin
       kk[i] = where(bandavail eq band.(i), blah)
       if blah ne 0 and blah ne 1 then begin
           plog,ll,'image cube must have 0 or 1 '+band.(i)+' plane; found = '+numstr(blah)
           plog,ll,prog,'stopping, could not proceed'
           return
       endif
   endfor
   jr      = where(bandavail EQ band.R, njr)
   jh      = where(bandavail EQ band.HALPHA, njh)
   jn      = where(bandavail EQ band.NUV, njn)
   jf      = where(bandavail EQ band.FUV, njf)
   ;
   ; set up combo names
   cname     = string(bandavail[combo], format='(A,",",A,",",A)')
   nfo       = N_elements(filo)
   
   IF nfo NE ncombo THEN BEGIN 
      plog,ll,prog,'Number of output files ('+numstr(nfo)+') does not equal number of combos ('+numstr(ncombo)+')'
      plog,ll,prog,'stopping, could not proceed'
      return
   ENDIF 
   plog,ll,prog,'am setting up '+numstr(ncombo)+' * 3 color image combos as follows (delimited with | ):'
   str      = ''
   FOR ii = 0, ncombo-1 DO str = str+cname[ii]+' | '
   plog,ll,prog,'  | '+str
   ;
   ; check size
   sz      = size(imcube)
   IF sz[0] NE 3 THEN BEGIN 
      plog,ll,prog,'imcube must be a 3D array. Ndim = '+numstr(sz[0])
      plog,ll,prog,'stopping, could not proceed'
      return
   ENDIF
   nx      = sz[1]
   ny      = sz[2]
   nz      = sz[3]
   ;
   ; store maskcmd so I can change it
   IF keyword_set(maskcmd) THEN mcmd = maskcmd ELSE mcmd = 0
   IF keyword_set(maskcmd) THEN BEGIN 
      ;
      ; read in masks otherwise set to 0
      IF keyword_set(omask) THEN BEGIN 
         plog,ll,prog,'Reading object mask: '+omask
         fits_read, omask, omsk, hdom
         szo  = size(omsk)
         IF szo[1] NE nx OR szo[2] NE ny THEN BEGIN 
            plog,ll,prog,'object mask dimensions: '+numstr(szo[1])+' , '+numstr(szo[2])+' differs from required: '+numstr(nx)+' , '+numstr(ny)
            plog,ll,prog,'stopping, can not proceed'
            return
         ENDIF
      ENDIF ELSE BEGIN 
         plog,ll,prog,'using blank object mask (where required) '
         omsk = make_array(nx, ny, /byte, value=0b)
      ENDELSE 
      ;
      IF keyword_set(smask) THEN BEGIN 
         plog,ll,prog,'Reading sky mask: '+smask
         fits_read, smask, smsk, hdsm
         szs  = size(smsk)
         IF szs[1] NE nx OR szs[2] NE ny THEN BEGIN 
            plog,ll,prog,'sky mask dimensions: '+numstr(szs[1])+' , '+numstr(szs[2])+' differs from required: '+numstr(nx)+' , '+numstr(ny)
            plog,ll,prog,'stopping, can not proceed'
            return
         ENDIF
      ENDIF ELSE BEGIN 
         plog,ll,prog,'using blank sky mask (where required) '
         smsk = make_array(nx, ny, /byte, value=0b)
      ENDELSE 
      ;
      ; set masking factor image according to maskcmd
      test = abs(mcmd)
      fmsk = make_array(nx, ny, /float, value=1.0)
      CASE test OF 
         1: BEGIN 
               plog,ll,prog,'making mask to exclude bad objects'
               pp   = where(omsk GE 1b, npp)
               IF npp GT 0 THEN fmsk[pp] = 0.0
            END 
         2: BEGIN
               plog,ll,prog,'making mask to show good sky pixels'
               pp   = where(smsk GE 1b, npp)
               IF npp GT 0 THEN fmsk[pp] = 0.0
            END 
         3: BEGIN
               plog,ll,prog,'making mask to show pixels used in object measurements'
               fmsk = 0.0*fmsk
               pp   = where(smsk GE 1b AND omsk LE 0b, npp)
               IF npp GT 0 THEN fmsk[pp] = 1.0
            END 
         ELSE: mcmd = 0
      ENDCASE 
      ;
      ; invert mask if maskcmd was < 0
      IF mcmd LT 0 THEN BEGIN 
         plog,ll,prog,'inverting the mask'
         fmsk = 1.0 - fmsk
      ENDIF 
   ENDIF 
   ;
   ; get deredden parameters
   dredf   = make_array(nband, /float, value=1.0)
   IF keyword_set(ebv) THEN BEGIN
         ccm_unred, photplam, dredf, ebv[0]
      plog,ll,prog,'will de-redden fluxes using the following band | wl | factor sets'
      FOR ii = 0, nbandavail-1 DO plog,ll,prog,'   '+ljust(bandavail[ii],6)+' | '+numstr(photplam[ii])+' | '+numstr(dredf[ii])
   ENDIF 
   ;
   ; set levels
   IF NOT keyword_set(highcut) THEN BEGIN 
      ;
      ; low cut values selected
      plog,ll,prog,'selecting low cut display levels.'
      maxr = maxr_l
      minr = minr_l
      minh = minh_l
      maxh = maxh_l
  ENDIF ELSE BEGIN
      ;
      ; high cut values selected
      plog,ll,prog,'selecting high cut display levels.'
      maxr = maxr_h
      minr = minr_h
      minh = minh_h
      maxh = maxh_h
   ENDELSE 
   mind     = make_array(nbandavail,/float,value=0.0)
   maxd     = mind
   mind[jr] = minr
   maxd[jr] = maxr
   mind[jh] = minh
   maxd[jh] = maxh
   mind[jn] = mind[jr]*(photplam[jn]/photplam[jr])^beta
   maxd[jn] = maxd[jr]*(photplam[jn]/photplam[jr])^beta
   mind[jf] = mind[jr]*(photplam[jf]/photplam[jr])^beta
   maxd[jf] = maxd[jr]*(photplam[jf]/photplam[jr])^beta
   plog,ll,prog,'will use the following flux calibrated display levels (band   min   max)'
   FOR ii = 0, nbandavail-1 DO plog,ll,'  ',ljust(bandavail[ii],6)+'  '+numstr(mind[ii])+'   '+numstr(maxd[ii])
   ;
   ; empty cube for putting only the planes we want,
   ; and in the order we want
   imcal    = make_array(nx, ny, nbandavail, /float, value=0.0)
   ;
   ; assemble cube of signed-sqrt calibrated fluxes
   FOR ii = 0, nz-1 DO imcal[*,*,ii] = ssqrt(photfl[kk[ii]]*dredf[ii]*imcube[*,*,kk[ii]])  ; calibrate 
   mind  = ssqrt(mind)    ; convert display ranges to signed sqrt
   maxd  = ssqrt(maxd)    ; convert display ranges to signed sqrt
   ;
   ; loop through combinations
   rgbim  = make_array(nx,ny,3,/byte,value=0b)
   FOR ii = 0, ncombo-1 DO BEGIN 
      ;
      ; make 3 color image
      plog,ll,prog,'making 3color combo: '+cname[ii]
      FOR jj = 0,2 DO rgbim[*,*,jj] = bytscl(imcal[*,*,combo[jj,ii]],min=mind[combo[jj,ii]],max=maxd[combo[jj,ii]])
      ;
      ; apply mask if needed
      IF mcmd NE 0 THEN BEGIN 
         plog,ll,prog,'applying the mask'
         FOR jj = 0,2 DO rgbim[*,*,jj] = byte(fmsk*float(rgbim[*,*,jj]))
      ENDIF 
      ;window,0,/pixmap,xsize=nx,ysize=ny
      window,0,xsize=nx,ysize=ny
      ;keywait,'type anything to display next image: '
      ;window,0,xsize=nx,ysize=ny
      tv,rgbim,true=3
      im=tvrd(true=3)
      ;
      ; write jpg
      plog,ll,prog,'writing file = '+filo[ii]
      write_jpeg,filo[ii],im,TRUE=3,quality=100
      IF slow THEN keywait, 'type any key to continue: '
   ENDFOR 
END 
