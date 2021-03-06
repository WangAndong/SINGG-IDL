PRO ssoup_mkjpg, ll, imcube, photfl, photplam, filo, ebv=ebv, $
                 highcut=highcut, maskcmd=maskcmd, omask=omask, smask=smask, $
                 goslow=goslow, epilepsy=epilepsy, saveprof=saveprof
   ;
   ; make 3 color images.
   ; 
   ;  ll        -> logical unit of log file (should be open)
   ;  imcube    -> image cube storing the multi-wavelength data
   ;               in individual planes
   ;  photfl    -> the PHOTFLAM or PHOTFLUX (Halpha) for each plane
   ;               of the cube
   ;  photplam  -> Pivot wavelength of each of the planes.
   ;  filo      -> Output filenames. 
   ;  ebv       -> galactic foreground extinction
   ;  highcut   -> if set the maximum levels will be at the "high" level
   ;              otherwise the "low" levels will be used
   ;  maskcmd   -> details what type of masking is to be done
   ;                0 : no mask applied (default)
   ;                1 : bad objects masked out
   ;                2 : show only pixels used for sky calcs
   ;                3 : show only pixels used to measure objects
   ;               -1 : inverse of 1
   ;               -2 : inverse of 2
   ;               -3 : inverse of 3
   ;  omask     -> bad objects mask
   ;  smask     -> sky mask
   ;  goslow    -> if set then stragetically placed calls to 
   ;               keywait.pro are used to slow down the processing
   ;               to a speed a user can monitor.
   ;  epilepsy  -> display 3 color images on the screen. We accept no
   ;               responsibility for any epileptic seizures that occur.
   ;  saveprof  -> if set, show apertures. Set to where we can find the
   ;               profile saveset.
   ;
   ; G. Meurer (ICRAR/UWA) 06/2010  based on sample.pro by Ji Hoon Kim
   ; S. Andrews (ICRAR/UWA) 01/2013 added WISE, refactoring, epilepsy, apertures
   COMMON bands, band, nband, bandnam, bandavail, nbandavail, combo, ncombo 
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
   cname     = strjoin(bandavail[combo], ",")
   nfo       = N_elements(filo)
   
   allprofiles = !null
   if keyword_set(saveprof) then begin
       temp   = combo
       x1 = where(bandavail eq band.mir_w4, cw4)
       x2 = where(bandavail eq band.mir_w3, cw3)
       x3 = where(bandavail eq band.mir_w1, cw1)
       combo  = [ [ jh, jr, jn ], [x1, x2, x3] ]
       ncombo = 2
       restore,saveprof
   endif
   
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
   dredf   = make_array(nbandavail, /float, value=1.0)
   IF keyword_set(ebv) THEN BEGIN
         ccm_unred, photplam, dredf, ebv[0]
      plog,ll,prog,'will de-redden fluxes using the following band | wl | factor sets'
      FOR ii = 0, nbandavail-1 DO plog,ll,prog,'   '+ljust(bandavail[ii],6)+' | '+numstr(photplam[ii])+' | '+numstr(dredf[ii])
   ENDIF 
   ;
   ; empty cube for putting only the planes we want,
   ; and in the order we want
   imcal    = make_array(nx, ny, nbandavail, /float, value=0.0)
   ;
   ; assemble cube of calibrated fluxes
   FOR ii = 0, nz-1 DO imcal[*,*,ii] = photfl[kk[ii]]*dredf[ii]*imcube[*,*,kk[ii]]  ; calibrate 
   ;
   ; set levels
   mind     = make_array(nbandavail,/float,value=0.0)
   maxd     = mind
   index_w1 = (where(bandavail eq band.mir_W1, count_w1))[0]
   index_w2 = (where(bandavail eq band.mir_W2, count_w2))[0]
   index_w3 = (where(bandavail eq band.mir_W3, count_w3))[0]
   index_w4 = (where(bandavail eq band.mir_W4, count_w4))[0]   
   IF NOT keyword_set(highcut) THEN BEGIN 
      ;
      ; low cut values selected
      plog,ll,prog,'selecting low cut display levels.'
      mind[jr]  = -1.0e-19 
      maxd[jr]  =  2.0e-17
      mind[jh]  = -7.0e-18
      maxd[jh]  =  1.4e-16
      if count_w1 ge 1 then begin 
          mind[index_w1] = percentile(imcal[*,*,index_w1], 5)
          maxd[index_w1] = percentile(imcal[*,*,index_w1], 99.5)
      endif
      if count_w2 ge 1 then begin 
          mind[index_w2] = percentile(imcal[*,*,index_w2], 5)
          maxd[index_w2] = percentile(imcal[*,*,index_w2], 99.5)
      endif
      if count_w3 ge 1 then begin 
          mind[index_w3] = percentile(imcal[*,*,index_w3], 5)
          maxd[index_w3] = percentile(imcal[*,*,index_w3], 99.5)
      endif
      if count_w4 ge 1 then begin 
          mind[index_w4] = percentile(imcal[*,*,index_w4], 5)
          maxd[index_w4] = percentile(imcal[*,*,index_w4], 99.5)
      endif
  ENDIF ELSE BEGIN
      ;
      ; high cut values selected
      plog,ll,prog,'selecting high cut display levels.'
      mind[jr] = -2.0e-19
      maxd[jr] =  2.0e-16
      mind[jh] = -1.4e-17
      maxd[jh] =  1.4e-15
      if count_w1 ge 1 then begin 
          mind[index_w1] = percentile(imcal[*,*,index_w1], 10)
          maxd[index_w1] = percentile(imcal[*,*,index_w1], 99.9)
      endif
      if count_w2 ge 1 then begin 
          mind[index_w2] = percentile(imcal[*,*,index_w1], 10)
          maxd[index_w2] = percentile(imcal[*,*,index_w1], 99.9)
      endif
      if count_w3 ge 1 then begin 
          mind[index_w3] = percentile(imcal[*,*,index_w1], 10)
          maxd[index_w3] = percentile(imcal[*,*,index_w1], 99.9)
      endif
      if count_w4 ge 1 then begin 
          mind[index_w4] = percentile(imcal[*,*,index_w1], 10)
          maxd[index_w4] = percentile(imcal[*,*,index_w1], 99.9)
      endif
   ENDELSE 
   mind[jn] = mind[jr]*(photplam[jn]/photplam[jr])^beta
   maxd[jn] = maxd[jr]*(photplam[jn]/photplam[jr])^beta
   mind[jf] = mind[jr]*(photplam[jf]/photplam[jr])^beta
   maxd[jf] = maxd[jr]*(photplam[jf]/photplam[jr])^beta
   plog,ll,prog,'will use the following flux calibrated display levels (band   min   max)'
   FOR ii = 0, nbandavail-1 DO plog,ll,'  ',ljust(bandavail[ii],6)+'  '+numstr(mind[ii])+'   '+numstr(maxd[ii])
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
      
      ; write jpeg to screen
      thisdevice = !D.name
      if keyword_set(epilepsy) then begin
          ;window,0,/pixmap,xsize=nx,ysize=ny
          window,0,xsize=nx,ysize=ny
          ;keywait,'type anything to display next image: '
          ;window,0,xsize=nx,ysize=ny
      ; or the Z buffer
      endif else begin
          set_plot, 'Z'
          erase
          device, set_resolution=[nx,ny],set_pixel_depth=24, decomposed=1
      endelse
      tv,rgbim,true=3
      ; draw circles
      if keyword_set(saveprof) then begin
          idx = i eq 0 ? jr : index_w1
          for jj=0,n_elements(allprofiles)-1 do begin
              kr = allprofiles[jj].rkron[idx]/1.5
              tvcircle, kr, xcenter[jj], ycenter[jj], color='green', thick=1.5, /device
              tvcircle, 2.5*kr, xcenter[jj], ycenter[jj], color='red', thick=1.5, /device
              tvcircle, allprofiles[jj].rmax[idx], xcenter[jj], ycenter[jj], color='cyan', thick=1.5, /device
              tvcircle, allprofiles[jj].r50[idx], xcenter[jj], ycenter[jj], color='magenta', thick=1.5, /device
          endfor
      endif
      plog,ll,prog,'writing file = '+filo[ii]
      write_jpeg,filo[ii],tvrd(true=3),TRUE=3,quality=100
      if not keyword_set(epilepsy) then set_plot, thisdevice
      IF slow THEN keywait, 'type any key to continue: '
   ENDFOR
   
   if keyword_set(saveprof) then begin
       undefine,allprofiles
       combo = temp
       ncombo = factorial(nbandavail)/(6*factorial(nbandavail-3))
   endif
END 
