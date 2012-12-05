PRO image_dbmatch, image, catname, entry, name, xpix, ypix, sma, axrat, posang
   ;
   ; Open a FITS image.  Match sources in a variety of databases that 
   ; are either in the Field Of View, or whose outer isophote/beam 
   ; should intersect the FOV.  For these objects plot
   ;
   ; 1. SINGG sources, central and outer isophote center pos.
   ;    ellipse at Rmax, sky annulus: from singg_derived.
   ;    also check entries in singg_sample, singg_reject
   ; 2. HIPASS position; circles with typical error circle, 
   ;    beam size.  hipass_bgc, hicat_feb04, hipass_remeasure
   ; 3. HOPCAT positions, R band Kron radius ellipse(?)
   ; 4. IRAS_PSC: positions, beam size
   ; 5. RC3: positions, R_25 elliptical isophote
   ; 6. NEARBYGAL: positions, R_25 elliptical isophotes
   ; 7. PRIN_GAL: 
   ; 8. QUASAR_AGN, QUASARS: 
   ; 9. HST_CATALOG: WFPC2, ACS, STIS, NICMOS pointings
   ; 10. IUE positions
   ;
   ; return catalog name, entry number in catalog, and pixel position
   ; of sources meeting matching criteria.
   ;
   ; image    -> Fits file to open. Must have a valid WCS.
   ;
   ;             For the sources that are matched:
   ; catname  <- catalog name (actually database name).
   ; entry    <- entry number in catalog.
   ; name     <- Name of target/source in database
   ; xpix     <- x position of matched source in pixel coords
   ; ypix     <- y position of source in pixel coords
   ; sma      <- semi-major axis size of source or aperture
   ; axrat    <- axial ratio a/b of source or aperture
   ; posang   <- position angle in degrees of source or aperture
   ;
   ; G. Meurer 02/2005
   ;
   radian  = 180.0/!pi
   catname = make_array(1, /string, value='')
   entry   = make_array(1, /long, value=-1l)
   name    = make_array(1, /string, value='')
   xpix    = make_array(1, /float, value=-1.0)
   ypix    = make_array(1, /float, value=-1.0)
   sma     = make_array(1, /float, value=-1.0)
   axrat   = make_array(1, /float, value=-1.0)
   posang  = make_array(1, /float, value=-1.0)
   nmatch  = 0
   ;
   ; make a unit circle - will need this later
   npoints = 361
   circ    = circle(0.0, 0.0, 1.0, npoints=npoints)
   xcirc   = reform(circ[0,*], npoints)
   ycirc   = reform(circ[1,*], npoints)
   ;
   ; open image,
   ;   get/derive: image dimensions nx, ny
   ;               pixel size (=sqrt area)
   ;
   fits_read, image, img, hdr, /header_only
   print, 'IMAGE_DBMATCH: read header of image : '+image
   nx      = sxpar(hdr, 'NAXIS1')
   ny      = sxpar(hdr, 'NAXIS2')
   print, 'IMAGE_DBMATCH: Dimensions: ', nx, ny
   extast,hdr,astr,noparams
   dx      = 3600.0*sqrt(astr.cd[0,0]*astr.cd[0,0] + astr.cd[0,1]*astr.cd[0,1])
   dy      = 3600.0*sqrt(astr.cd[1,0]*astr.cd[1,0] + astr.cd[1,1]*astr.cd[1,1])
   pixsize = sqrt(dx*dy)
   print, 'IMAGE_DBMATCH: pixel x_scale, y_scale, sqrt(area) = ', dx, dy, pixsize
   ;
   ; 1a. singg_derived
   ;    * find sources
   db      = 'singg_derived'    ; name of database
   mrad    = 15.0               ; has to be this many arcsec of FOV
   xrange  = [-1.0,1.0]*mrad/dx + [0.0, float(nx)-1.0]
   yrange  = [-1.0,1.0]*mrad/dy + [0.0, float(ny)-1.0]
   print, 'IMAGE_DBMATCH: searching database: '+db
   grm_imdbase,hdr,db,list,xpos=xpos,ypos=ypos,xrange=xrange,yrange=yrange,/radeg
   nm      = n_elements(list)
   IF nm EQ 1 AND list[0] EQ -1 THEN nm = 0
   ;
   ;    * get relevant parameters for derived sources
   IF nm GE 1 THEN BEGIN 
      good   = make_array(nm, /byte, value=0b)
      dbext,list,'name,ra,dec,rmax,pa,axerat',source,ra,dec,aa,pa,ab
      parad  = pa/radian
      ;
      ;   + loop through all matches - find pixel outline for source
      ;     & make sure at least some of outline is in the image
      ;     mark these as good.
      FOR ii = 0, nm-1 DO BEGIN 
         rac  = (ra[ii]+aa[ii]*(xcirc*cos(parad[ii])/ab[ii] - ycirc*sin(parad[ii]))/3600.0) MOD 360.0
         decc = dec[ii]+aa[ii]*(xcirc*sin(parad[ii])/ab[ii] + ycirc*cos(parad[ii]))/3600.0
         adxy, hdr, rac, decc, xp, yp
         kk   = where(xp GE 0.0 AND xp LE float(nx-1) AND yp GE 0.0 AND yp LE float(ny-1), nkk)
         IF nkk GE 1 THEN good[ii] = 1b
      ENDFOR 
      kk     = where(good EQ 1b, nkk)
      ;
      ;   + store parameters of good sources
      IF nkk GE 1 THEN BEGIN 
         IF nmatch LE 0 THEN BEGIN 
            catname = make_array(nkk, /string, value=db)
            entry   = list[kk]
            name    = source[kk]
            xpix    = xpos[kk]
            ypix    = ypos[kk]
            sma     = aa[kk]
            axrat   = ab[kk]
            posang  = pa[kk]
            nmatch  = nkk
         ENDIF ELSE BEGIN 
            catname = [catname, make_array(nkk, /string, value=db)]
            entry   = [entry, list[kk]]
            name    = [name, source[kk]]
            xpix    = [xpix, xpos[kk]]
            ypix    = [ypix, ypos[kk]]
            sma     = [sma, aa[kk]]
            axrat   = [axrat, ab[kk]]
            posang  = [posang, pa[kk]]
            nmatch  = nmatch + nkk
         ENDELSE 
      ENDIF 
   ENDIF 
   ;
   ; 1b. singg_sample
   ;    * find sources
   db      = 'singg_sample'    ; name of database
   mrad    = 7.5*60.0          ; has to be this many arcsec of FOV
   xrange  = [-1.0,1.0]*mrad/dx + [0.0, float(nx)-1.0]
   yrange  = [-1.0,1.0]*mrad/dy + [0.0, float(ny)-1.0]
   print, 'IMAGE_DBMATCH: searching database: '+db
   grm_imdbase,hdr,db,list,xpos=xpos,ypos=ypos,xrange=xrange,yrange=yrange,/radeg
   nm      = n_elements(list)
   IF nm EQ 1 AND list[0] EQ -1 THEN nm = 0
   ;
   ;    * get relevant parameters for derived sources
   IF nm GE 1 THEN BEGIN 
      parad  = 0.0
      aa     = mrad
      ab     = 1.0
      good   = make_array(nm, /byte, value=0b)
      dbext,list,'name,ra,dec',source,ra,dec
      ;
      ;   + loop through all matches - find pixel outline for source
      ;     & make sure at least some of outline is in the image
      ;     mark these as good.
      FOR ii = 0, nm-1 DO BEGIN 
         rac   = (ra[ii]+aa*(xcirc*cos(parad)/ab - ycirc*sin(parad))/3600.0) MOD 360.0
         decc  = dec[ii]+aa*(xcirc*sin(parad)/ab + ycirc*cos(parad))/3600.0
         adxy, hdr, rac, decc, xp, yp
         kk   = where(xp GE 0.0 AND xp LE float(nx-1) AND yp GE 0.0 AND yp LE float(ny-1), nkk)
         IF nkk GE 1 THEN good[ii] = 1b
      ENDFOR 
      kk     = where(good EQ 1b, nkk)
      ;
      ;   + store parameters of good sources
      IF nkk GE 1 THEN BEGIN 
         IF nmatch LE 0 THEN BEGIN 
            catname = make_array(nkk, /string, value=db)
            entry   = list[kk]
            name    = source[kk]
            xpix    = xpos[kk]
            ypix    = ypos[kk]
            sma     = make_array(nkk, /float, value=aa)
            axrat   = make_array(nkk, /float, value=ab)
            posang  = make_array(nkk, /float, value=parad*radian)
            nmatch  = nkk
         ENDIF ELSE BEGIN 
            catname = [catname, make_array(nkk, /string, value=db)]
            entry   = [entry, list[kk]]
            name    = [name, source[kk]]
            xpix    = [xpix, xpos[kk]]
            ypix    = [ypix, ypos[kk]]
            sma     = [sma, make_array(nkk, /float, value=aa)]
            axrat   = [axrat, make_array(nkk, /float, value=ab)]
            posang  = [posang, make_array(nkk, /float, value=parad*radian)]
            nmatch  = nmatch + nkk
         ENDELSE 
      ENDIF 
   ENDIF 
   ;
   ; 1c. singg_reject
   ;    * find sources
   db      = 'singg_reject'     ; name of database
   mrad    = 7.5*60.0          ; has to be this many arcsec of FOV
   xrange  = [-1.0,1.0]*mrad/dx + [0.0, float(nx)-1.0]
   yrange  = [-1.0,1.0]*mrad/dy + [0.0, float(ny)-1.0]
   print, 'IMAGE_DBMATCH: searching database: '+db
   grm_imdbase,hdr,db,list,xpos=xpos,ypos=ypos,xrange=xrange,yrange=yrange,/radeg
   nm      = n_elements(list)
   IF nm EQ 1 AND list[0] EQ -1 THEN nm = 0
   ;
   ;    * get relevant parameters for derived sources
   IF nm GE 1 THEN BEGIN 
      parad  = 0.0
      aa     = mrad
      ab     = 1.0
      good   = make_array(nm, /byte, value=0b)
      dbext,list,'name,ra,dec',source,ra,dec
      ;
      ;   + loop through all matches - find pixel outline for source
      ;     & make sure at least some of outline is in the image
      ;     mark these as good.
      FOR ii = 0, nm-1 DO BEGIN 
         rac   = (ra[ii]+aa*(xcirc*cos(parad)/ab - ycirc*sin(parad))/3600.0) MOD 360.0
         decc  = dec[ii]+aa*(xcirc*sin(parad)/ab + ycirc*cos(parad))/3600.0
         adxy, hdr, rac, decc, xp, yp
         kk   = where(xp GE 0.0 AND xp LE float(nx-1) AND yp GE 0.0 AND yp LE float(ny-1), nkk)
         IF nkk GE 1 THEN good[ii] = 1b
      ENDFOR 
      kk     = where(good EQ 1b, nkk)
      ;
      ;   + store parameters of good sources
      IF nkk GE 1 THEN BEGIN 
         IF nmatch LE 0 THEN BEGIN 
            catname = make_array(nkk, /string, value=db)
            entry   = list[kk]
            name    = source[kk]
            xpix    = xpos[kk]
            ypix    = ypos[kk]
            sma     = make_array(nkk, /float, value=aa)
            axrat   = make_array(nkk, /float, value=ab)
            posang  = make_array(nkk, /float, value=parad*radian)
            nmatch  = nkk
         ENDIF ELSE BEGIN 
            catname = [catname, make_array(nkk, /string, value=db)]
            entry   = [entry, list[kk]]
            name    = [name, source[kk]]
            xpix    = [xpix, xpos[kk]]
            ypix    = [ypix, ypos[kk]]
            sma     = [sma, make_array(nkk, /float, value=aa)]
            axrat   = [axrat, make_array(nkk, /float, value=ab)]
            posang  = [posang, make_array(nkk, /float, value=parad*radian)]
            nmatch  = nmatch + nkk
         ENDELSE 
      ENDIF 
   ENDIF 
   ;
   ; 2a. hipass_bgc
   ;    * find sources
   db      = 'hipass_bgc'      ; name of database
   mrad    = 7.5*60.0          ; has to be this many arcsec of FOV
   xrange  = [-1.0,1.0]*mrad/dx + [0.0, float(nx)-1.0]
   yrange  = [-1.0,1.0]*mrad/dy + [0.0, float(ny)-1.0]
   print, 'IMAGE_DBMATCH: searching database: '+db
   grm_imdbase,hdr,db,list,xpos=xpos,ypos=ypos,xrange=xrange,yrange=yrange,/radeg
   nm      = n_elements(list)
   IF nm EQ 1 AND list[0] EQ -1 THEN nm = 0
   ;
   ;    * get relevant parameters for derived sources
   IF nm GE 1 THEN BEGIN 
      parad  = 0.0
      aa     = mrad
      ab     = 1.0
      good   = make_array(nm, /byte, value=0b)
      dbext,list,'name,ra,dec',source,ra,dec
      ;
      ;   + loop through all matches - find pixel outline for source
      ;     & make sure at least some of outline is in the image
      ;     mark these as good.
      FOR ii = 0, nm-1 DO BEGIN 
         rac   = (ra[ii]+aa*(xcirc*cos(parad)/ab - ycirc*sin(parad))/3600.0) MOD 360.0
         decc  = dec[ii]+aa*(xcirc*sin(parad)/ab + ycirc*cos(parad))/3600.0
         adxy, hdr, rac, decc, xp, yp
         kk   = where(xp GE 0.0 AND xp LE float(nx-1) AND yp GE 0.0 AND yp LE float(ny-1), nkk)
         IF nkk GE 1 THEN good[ii] = 1b
      ENDFOR 
      kk     = where(good EQ 1b, nkk)
      ;
      ;   + store parameters of good sources
      IF nkk GE 1 THEN BEGIN 
         IF nmatch LE 0 THEN BEGIN 
            catname = make_array(nkk, /string, value=db)
            entry   = list[kk]
            name    = source[kk]
            xpix    = xpos[kk]
            ypix    = ypos[kk]
            sma     = make_array(nkk, /float, value=aa)
            axrat   = make_array(nkk, /float, value=ab)
            posang  = make_array(nkk, /float, value=parad*radian)
            nmatch  = nkk
         ENDIF ELSE BEGIN 
            catname = [catname, make_array(nkk, /string, value=db)]
            entry   = [entry, list[kk]]
            name    = [name, source[kk]]
            xpix    = [xpix, xpos[kk]]
            ypix    = [ypix, ypos[kk]]
            sma     = [sma, make_array(nkk, /float, value=aa)]
            axrat   = [axrat, make_array(nkk, /float, value=ab)]
            posang  = [posang, make_array(nkk, /float, value=parad*radian)]
            nmatch  = nmatch + nkk
         ENDELSE 
      ENDIF 
   ENDIF 
   ;
   ; 2b. hicat_feb04
   ;    * find sources
   db      = 'hicat_feb04'      ; name of database
   mrad    = 7.5*60.0          ; has to be this many arcsec of FOV
   xrange  = [-1.0,1.0]*mrad/dx + [0.0, float(nx)-1.0]
   yrange  = [-1.0,1.0]*mrad/dy + [0.0, float(ny)-1.0]
   print, 'IMAGE_DBMATCH: searching database: '+db
   grm_imdbase,hdr,db,list,xpos=xpos,ypos=ypos,xrange=xrange,yrange=yrange,/radeg
   nm      = n_elements(list)
   IF nm EQ 1 AND list[0] EQ -1 THEN nm = 0
   ;
   ;    * get relevant parameters for derived sources
   IF nm GE 1 THEN BEGIN 
      parad  = 0.0
      aa     = mrad
      ab     = 1.0
      good   = make_array(nm, /byte, value=0b)
      dbext,list,'hipass_name,ra,dec',source,ra,dec
      ;
      ;   + loop through all matches - find pixel outline for source
      ;     & make sure at least some of outline is in the image
      ;     mark these as good.
      FOR ii = 0, nm-1 DO BEGIN 
         rac   = (ra[ii]+aa*(xcirc*cos(parad)/ab - ycirc*sin(parad))/3600.0) MOD 360.0
         decc  = dec[ii]+aa*(xcirc*sin(parad)/ab + ycirc*cos(parad))/3600.0
         adxy, hdr, rac, decc, xp, yp
         kk   = where(xp GE 0.0 AND xp LE float(nx-1) AND yp GE 0.0 AND yp LE float(ny-1), nkk)
         IF nkk GE 1 THEN good[ii] = 1b
      ENDFOR 
      kk     = where(good EQ 1b, nkk)
      ;
      ;   + store parameters of good sources
      IF nkk GE 1 THEN BEGIN 
         IF nmatch LE 0 THEN BEGIN 
            catname = make_array(nkk, /string, value=db)
            entry   = list[kk]
            name    = source[kk]
            xpix    = xpos[kk]
            ypix    = ypos[kk]
            sma     = make_array(nkk, /float, value=aa)
            axrat   = make_array(nkk, /float, value=ab)
            posang  = make_array(nkk, /float, value=parad*radian)
            nmatch  = nkk
         ENDIF ELSE BEGIN 
            catname = [catname, make_array(nkk, /string, value=db)]
            entry   = [entry, list[kk]]
            name    = [name, source[kk]]
            xpix    = [xpix, xpos[kk]]
            ypix    = [ypix, ypos[kk]]
            sma     = [sma, make_array(nkk, /float, value=aa)]
            axrat   = [axrat, make_array(nkk, /float, value=ab)]
            posang  = [posang, make_array(nkk, /float, value=parad*radian)]
            nmatch  = nmatch + nkk
         ENDELSE 
      ENDIF 
   ENDIF 
   ;
   ; 2c. hipass_remeasure
   ;    * find sources
   db      = 'hipass_remeasure'      ; name of database
   mrad    = 7.5*60.0          ; has to be this many arcsec of FOV
   xrange  = [-1.0,1.0]*mrad/dx + [0.0, float(nx)-1.0]
   yrange  = [-1.0,1.0]*mrad/dy + [0.0, float(ny)-1.0]
   print, 'IMAGE_DBMATCH: searching database: '+db
   grm_imdbase,hdr,db,list,xpos=xpos,ypos=ypos,xrange=xrange,yrange=yrange,/radeg
   nm      = n_elements(list)
   IF nm EQ 1 AND list[0] EQ -1 THEN nm = 0
   ;
   ;    * get relevant parameters for derived sources
   IF nm GE 1 THEN BEGIN 
      parad  = 0.0
      aa     = mrad
      ab     = 1.0
      good   = make_array(nm, /byte, value=0b)
      dbext,list,'name,ra,dec',source,ra,dec
      ;
      ;   + loop through all matches - find pixel outline for source
      ;     & make sure at least some of outline is in the image
      ;     mark these as good.
      FOR ii = 0, nm-1 DO BEGIN 
         rac   = (ra[ii]+aa*(xcirc*cos(parad)/ab - ycirc*sin(parad))/3600.0) MOD 360.0
         decc  = dec[ii]+aa*(xcirc*sin(parad)/ab + ycirc*cos(parad))/3600.0
         adxy, hdr, rac, decc, xp, yp
         kk   = where(xp GE 0.0 AND xp LE float(nx-1) AND yp GE 0.0 AND yp LE float(ny-1), nkk)
         IF nkk GE 1 THEN good[ii] = 1b
      ENDFOR 
      kk     = where(good EQ 1b, nkk)
      ;
      ;   + store parameters of good sources
      IF nkk GE 1 THEN BEGIN 
         IF nmatch LE 0 THEN BEGIN 
            catname = make_array(nkk, /string, value=db)
            entry   = list[kk]
            name    = source[kk]
            xpix    = xpos[kk]
            ypix    = ypos[kk]
            sma     = make_array(nkk, /float, value=aa)
            axrat   = make_array(nkk, /float, value=ab)
            posang  = make_array(nkk, /float, value=parad*radian)
            nmatch  = nkk
         ENDIF ELSE BEGIN 
            catname = [catname, make_array(nkk, /string, value=db)]
            entry   = [entry, list[kk]]
            name    = [name, source[kk]]
            xpix    = [xpix, xpos[kk]]
            ypix    = [ypix, ypos[kk]]
            sma     = [sma, make_array(nkk, /float, value=aa)]
            axrat   = [axrat, make_array(nkk, /float, value=ab)]
            posang  = [posang, make_array(nkk, /float, value=parad*radian)]
            nmatch  = nmatch + nkk
         ENDELSE 
      ENDIF 
   ENDIF 
   ;
   ; 3.  hopcat
   ;    * find sources
   db      = 'hopcat'    ; name of database
   mrad    = 120.0               ; has to be this many arcsec of FOV
   xrange  = [-1.0,1.0]*mrad/dx + [0.0, float(nx)-1.0]
   yrange  = [-1.0,1.0]*mrad/dy + [0.0, float(ny)-1.0]
   print, 'IMAGE_DBMATCH: searching database: '+db
   grm_imdbase,hdr,db,list,xpos=xpos,ypos=ypos,xrange=xrange,yrange=yrange,/radeg
   nm      = n_elements(list)
   IF nm EQ 1 AND list[0] EQ -1 THEN nm = 0
   ;
   ;    * get relevant parameters for derived sources
   IF nm GE 1 THEN BEGIN 
      good   = make_array(nm, /byte, value=0b)
      dbext,list,'name,ra,dec,a_image,theta_image,a_b',source,ra,dec,aa,pa,ab
      aa     = 2.0*aa
      pa     = (pa + 90.0) MOD 360.0
      parad  = pa/radian
      ;
      ;   + loop through all matches - find pixel outline for source
      ;     & make sure at least some of outline is in the image
      ;     mark these as good.
      FOR ii = 0, nm-1 DO BEGIN 
         rac  = (ra[ii]+aa[ii]*(xcirc*cos(parad[ii])/ab[ii] - ycirc*sin(parad[ii]))/3600.0) MOD 360.0
         decc = dec[ii]+aa[ii]*(xcirc*sin(parad[ii])/ab[ii] + ycirc*cos(parad[ii]))/3600.0
         adxy, hdr, rac, decc, xp, yp
         kk   = where(xp GE 0.0 AND xp LE float(nx-1) AND yp GE 0.0 AND yp LE float(ny-1), nkk)
         IF nkk GE 1 THEN good[ii] = 1b
      ENDFOR 
      kk     = where(good EQ 1b, nkk)
      ;
      ;   + store parameters of good sources
      IF nkk GE 1 THEN BEGIN 
         IF nmatch LE 0 THEN BEGIN 
            catname = make_array(nkk, /string, value=db)
            entry   = list[kk]
            name    = source[kk]
            xpix    = xpos[kk]
            ypix    = ypos[kk]
            sma     = aa[kk]
            axrat   = ab[kk]
            posang  = pa[kk]
            nmatch  = nkk
         ENDIF ELSE BEGIN 
            catname = [catname, make_array(nkk, /string, value=db)]
            entry   = [entry, list[kk]]
            name    = [name, source[kk]]
            xpix    = [xpix, xpos[kk]]
            ypix    = [ypix, ypos[kk]]
            sma     = [sma, aa[kk]]
            axrat   = [axrat, ab[kk]]
            posang  = [posang, pa[kk]]
            nmatch  = nmatch + nkk
         ENDELSE 
      ENDIF 
   ENDIF 
   ;
   ; 4.  iras_psc
   ;    * find sources
   db      = 'iras_psc'    ; name of database
   mrad    = 120.0               ; has to be this many arcsec of FOV
   xrange  = [-1.0,1.0]*mrad/dx + [0.0, float(nx)-1.0]
   yrange  = [-1.0,1.0]*mrad/dy + [0.0, float(ny)-1.0]
   print, 'IMAGE_DBMATCH: searching database: '+db
   grm_imdbase,hdr,db,list,xpos=xpos,ypos=ypos,xrange=xrange,yrange=yrange
   nm      = n_elements(list)
   IF nm EQ 1 AND list[0] EQ -1 THEN nm = 0
   ;
   ;    * get relevant parameters for derived sources
   IF nm GE 1 THEN BEGIN 
      good   = make_array(nm, /byte, value=0b)
      dbext,list,'name,ra,dec,major,posang,minor',source,ra1950,dec1950,aa,pa,bb
      ra1950 = 15.0*ra1950
      jprecess, ra1950, dec1950, ra, dec
      ab     = aa/bb
      parad  = pa/radian
      ;
      ;   + loop through all matches - find pixel outline for source
      ;     & make sure at least some of outline is in the image
      ;     mark these as good.
      FOR ii = 0, nm-1 DO BEGIN 
         rac  = (ra[ii]+aa[ii]*(xcirc*cos(parad[ii])/ab[ii] - ycirc*sin(parad[ii]))/3600.0) MOD 360.0
         decc = dec[ii]+aa[ii]*(xcirc*sin(parad[ii])/ab[ii] + ycirc*cos(parad[ii]))/3600.0
         adxy, hdr, rac, decc, xp, yp
         kk   = where(xp GE 0.0 AND xp LE float(nx-1) AND yp GE 0.0 AND yp LE float(ny-1), nkk)
         IF nkk GE 1 THEN good[ii] = 1b
      ENDFOR 
      kk     = where(good EQ 1b, nkk)
      ;
      ;   + store parameters of good sources
      IF nkk GE 1 THEN BEGIN 
         IF nmatch LE 0 THEN BEGIN 
            catname = make_array(nkk, /string, value=db)
            entry   = list[kk]
            name    = source[kk]
            xpix    = xpos[kk]
            ypix    = ypos[kk]
            sma     = aa[kk]
            axrat   = ab[kk]
            posang  = pa[kk]
            nmatch  = nkk
         ENDIF ELSE BEGIN 
            catname = [catname, make_array(nkk, /string, value=db)]
            entry   = [entry, list[kk]]
            name    = [name, source[kk]]
            xpix    = [xpix, xpos[kk]]
            ypix    = [ypix, ypos[kk]]
            sma     = [sma, aa[kk]]
            axrat   = [axrat, ab[kk]]
            posang  = [posang, pa[kk]]
            nmatch  = nmatch + nkk
         ENDELSE 
      ENDIF 
   ENDIF 
   ;
   ; 5.  rc3
   ;    * find sources
   db      = 'rc3'    ; name of database
   mrad    = 120.0               ; has to be this many arcsec from FOV or closer
   xrange  = [-1.0,1.0]*mrad/dx + [0.0, float(nx)-1.0]
   yrange  = [-1.0,1.0]*mrad/dy + [0.0, float(ny)-1.0]
   print, 'IMAGE_DBMATCH: searching database: '+db
   grm_imdbase,hdr,db,list,xpos=xpos,ypos=ypos,xrange=xrange,yrange=yrange
   nm      = n_elements(list)
   IF nm EQ 1 AND list[0] EQ -1 THEN nm = 0
   ;
   ;    * get relevant parameters for derived sources
   IF nm GE 1 THEN BEGIN 
      good   = make_array(nm, /byte, value=0b)
      dbext,list,'name1,ra,dec,d0_25,pa,r25',source,ra1950,dec1950,aa,pa,lrat
      ra1950 = 15.0*ra1950
      jprecess, ra1950, dec1950, ra, dec
      aa     = 10.0*aa
      ab     = 10.0^lrat
      parad  = pa/radian
      ;
      ;   + loop through all matches - find pixel outline for source
      ;     & make sure at least some of outline is in the image
      ;     mark these as good.
      FOR ii = 0, nm-1 DO BEGIN 
         rac  = (ra[ii]+aa[ii]*(xcirc*cos(parad[ii])/ab[ii] - ycirc*sin(parad[ii]))/3600.0) MOD 360.0
         decc = dec[ii]+aa[ii]*(xcirc*sin(parad[ii])/ab[ii] + ycirc*cos(parad[ii]))/3600.0
         adxy, hdr, rac, decc, xp, yp
         kk   = where(xp GE 0.0 AND xp LE float(nx-1) AND yp GE 0.0 AND yp LE float(ny-1), nkk)
         IF nkk GE 1 THEN good[ii] = 1b
      ENDFOR 
      kk     = where(good EQ 1b, nkk)
      ;
      ;   + store parameters of good sources
      IF nkk GE 1 THEN BEGIN 
         IF nmatch LE 0 THEN BEGIN 
            catname = make_array(nkk, /string, value=db)
            entry   = list[kk]
            name    = source[kk]
            xpix    = xpos[kk]
            ypix    = ypos[kk]
            sma     = aa[kk]
            axrat   = ab[kk]
            posang  = pa[kk]
            nmatch  = nkk
         ENDIF ELSE BEGIN 
            catname = [catname, make_array(nkk, /string, value=db)]
            entry   = [entry, list[kk]]
            name    = [name, source[kk]]
            xpix    = [xpix, xpos[kk]]
            ypix    = [ypix, ypos[kk]]
            sma     = [sma, aa[kk]]
            axrat   = [axrat, ab[kk]]
            posang  = [posang, pa[kk]]
            nmatch  = nmatch + nkk
         ENDELSE 
      ENDIF 
   ENDIF 
   ;
   ; 6.  nearby galaxy catalog
   ;    * find sources
   db      = 'nearbygal'    ; name of database
   mrad    = 120.0               ; has to be this many arcsec from FOV or closer
   xrange  = [-1.0,1.0]*mrad/dx + [0.0, float(nx)-1.0]
   yrange  = [-1.0,1.0]*mrad/dy + [0.0, float(ny)-1.0]
   print, 'IMAGE_DBMATCH: searching database: '+db
   grm_imdbase,hdr,db,list,xpos=xpos,ypos=ypos,xrange=xrange,yrange=yrange
   nm      = n_elements(list)
   IF nm EQ 1 AND list[0] EQ -1 THEN nm = 0
   ;
   ;    * get relevant parameters for derived sources
   IF nm GE 1 THEN BEGIN 
      good   = make_array(nm, /byte, value=0b)
      dbext,list,'name,ra,dec,d25bi,d_d',source,ra1950,dec1950,aa,ab
      ra1950 = 15.0*ra1950
      jprecess, ra1950, dec1950, ra, dec
      aa     = 30.0*aa
      a_b    = 1.0/ab
      ab     = 0.0*a_b + 1.0      ; since there is no PA use ab=1 for checking within FOV
      pa     = 0.0*ab
      parad  = 0.0*pa
      ;
      ;   + loop through all matches - find pixel outline for source
      ;     & make sure at least some of outline is in the image
      ;     mark these as good.
      FOR ii = 0, nm-1 DO BEGIN 
         rac  = (ra[ii]+aa[ii]*(xcirc*cos(parad[ii])/ab[ii] - ycirc*sin(parad[ii]))/3600.0) MOD 360.0
         decc = dec[ii]+aa[ii]*(xcirc*sin(parad[ii])/ab[ii] + ycirc*cos(parad[ii]))/3600.0
         adxy, hdr, rac, decc, xp, yp
         kk   = where(xp GE 0.0 AND xp LE float(nx-1) AND yp GE 0.0 AND yp LE float(ny-1), nkk)
         IF nkk GE 1 THEN good[ii] = 1b
      ENDFOR 
      kk     = where(good EQ 1b, nkk)
      ;
      ;   + store parameters of good sources
      IF nkk GE 1 THEN BEGIN 
         IF nmatch LE 0 THEN BEGIN 
            catname = make_array(nkk, /string, value=db)
            entry   = list[kk]
            name    = source[kk]
            xpix    = xpos[kk]
            ypix    = ypos[kk]
            sma     = aa[kk]
            axrat   = ab[kk]
            posang  = pa[kk]
            nmatch  = nkk
         ENDIF ELSE BEGIN 
            catname = [catname, make_array(nkk, /string, value=db)]
            entry   = [entry, list[kk]]
            name    = [name, source[kk]]
            xpix    = [xpix, xpos[kk]]
            ypix    = [ypix, ypos[kk]]
            sma     = [sma, aa[kk]]
            axrat   = [axrat, ab[kk]]
            posang  = [posang, pa[kk]]
            nmatch  = nmatch + nkk
         ENDELSE 
      ENDIF 
   ENDIF 
   ;
   ; 7.  principal galaxy catalog
   ;    * find sources
   db      = 'prin_gal'    ; name of database
   mrad    = 120.0               ; has to be this many arcsec from FOV or closer
   xrange  = [-1.0,1.0]*mrad/dx + [0.0, float(nx)-1.0]
   yrange  = [-1.0,1.0]*mrad/dy + [0.0, float(ny)-1.0]
   print, 'IMAGE_DBMATCH: searching database: '+db
   grm_imdbase,hdr,db,list,xpos=xpos,ypos=ypos,xrange=xrange,yrange=yrange
   nm      = n_elements(list)
   IF nm EQ 1 AND list[0] EQ -1 THEN nm = 0
   ;
   ;    * get relevant parameters for derived sources
   IF nm GE 1 THEN BEGIN 
      good   = make_array(nm, /byte, value=0b)
      dbext,list,'name1,ra,dec,maj_diam,pos_ang,min_diam',source,ra,dec,aa,pa,bb
      ra     = 15.0*ra
      ab     = aa/bb
      aa     = 30.0*aa
      parad  = pa/radian
      ;
      ;   + loop through all matches - find pixel outline for source
      ;     & make sure at least some of outline is in the image
      ;     mark these as good.
      FOR ii = 0, nm-1 DO BEGIN 
         rac  = (ra[ii]+aa[ii]*(xcirc*cos(parad[ii])/ab[ii] - ycirc*sin(parad[ii]))/3600.0) MOD 360.0
         decc = dec[ii]+aa[ii]*(xcirc*sin(parad[ii])/ab[ii] + ycirc*cos(parad[ii]))/3600.0
         adxy, hdr, rac, decc, xp, yp
         kk   = where(xp GE 0.0 AND xp LE float(nx-1) AND yp GE 0.0 AND yp LE float(ny-1), nkk)
         IF nkk GE 1 THEN good[ii] = 1b
      ENDFOR 
      kk     = where(good EQ 1b, nkk)
      ;
      ;   + store parameters of good sources
      IF nkk GE 1 THEN BEGIN 
         IF nmatch LE 0 THEN BEGIN 
            catname = make_array(nkk, /string, value=db)
            entry   = list[kk]
            name    = source[kk]
            xpix    = xpos[kk]
            ypix    = ypos[kk]
            sma     = aa[kk]
            axrat   = ab[kk]
            posang  = pa[kk]
            nmatch  = nkk
         ENDIF ELSE BEGIN 
            catname = [catname, make_array(nkk, /string, value=db)]
            entry   = [entry, list[kk]]
            name    = [name, source[kk]]
            xpix    = [xpix, xpos[kk]]
            ypix    = [ypix, ypos[kk]]
            sma     = [sma, aa[kk]]
            axrat   = [axrat, ab[kk]]
            posang  = [posang, pa[kk]]
            nmatch  = nmatch + nkk
         ENDELSE 
      ENDIF 
   ENDIF 
   ;
   ; 8a.  quasar_agn
   ;    * find sources
   db      = 'quasar_agn'    ; name of database
   mrad    = 60.0               ; has to be this many arcsec from FOV or closer
   xrange  = [-1.0,1.0]*mrad/dx + [0.0, float(nx)-1.0]
   yrange  = [-1.0,1.0]*mrad/dy + [0.0, float(ny)-1.0]
   print, 'IMAGE_DBMATCH: searching database: '+db
   grm_imdbase,hdr,db,list,xpos=xpos,ypos=ypos,xrange=xrange,yrange=yrange
   nm      = n_elements(list)
   IF nm EQ 1 AND list[0] EQ -1 THEN nm = 0
   ;
   ;    * get relevant parameters for derived sources
   IF nm GE 1 THEN BEGIN 
      good   = make_array(nm, /byte, value=0b)
      dbext,list,'object,ra,dec',source,ra,dec
      ra     = 15.0*ra
      aa     = 60.0 + 0.0*ra
      ab     = 1.0 + 0.0*ra
      pa     = 0.0*ra
      parad  = pa/radian
      ;
      ;   + loop through all matches - find pixel outline for source
      ;     & make sure at least some of outline is in the image
      ;     mark these as good.
      FOR ii = 0, nm-1 DO BEGIN 
         rac  = (ra[ii]+aa[ii]*(xcirc*cos(parad[ii])/ab[ii] - ycirc*sin(parad[ii]))/3600.0) MOD 360.0
         decc = dec[ii]+aa[ii]*(xcirc*sin(parad[ii])/ab[ii] + ycirc*cos(parad[ii]))/3600.0
         adxy, hdr, rac, decc, xp, yp
         kk   = where(xp GE 0.0 AND xp LE float(nx-1) AND yp GE 0.0 AND yp LE float(ny-1), nkk)
         IF nkk GE 1 THEN good[ii] = 1b
      ENDFOR 
      kk     = where(good EQ 1b, nkk)
      ;
      ;   + store parameters of good sources
      IF nkk GE 1 THEN BEGIN 
         IF nmatch LE 0 THEN BEGIN 
            catname = make_array(nkk, /string, value=db)
            entry   = list[kk]
            name    = source[kk]
            xpix    = xpos[kk]
            ypix    = ypos[kk]
            sma     = aa[kk]
            axrat   = ab[kk]
            posang  = pa[kk]
            nmatch  = nkk
         ENDIF ELSE BEGIN 
            catname = [catname, make_array(nkk, /string, value=db)]
            entry   = [entry, list[kk]]
            name    = [name, source[kk]]
            xpix    = [xpix, xpos[kk]]
            ypix    = [ypix, ypos[kk]]
            sma     = [sma, aa[kk]]
            axrat   = [axrat, ab[kk]]
            posang  = [posang, pa[kk]]
            nmatch  = nmatch + nkk
         ENDELSE 
      ENDIF 
   ENDIF 
   ;
   ; 8b.  quasars
   ;    * find sources
   db      = 'quasars'    ; name of database
   mrad    = 60.0               ; has to be this many arcsec from FOV or closer
   xrange  = [-1.0,1.0]*mrad/dx + [0.0, float(nx)-1.0]
   yrange  = [-1.0,1.0]*mrad/dy + [0.0, float(ny)-1.0]
   print, 'IMAGE_DBMATCH: searching database: '+db
   grm_imdbase,hdr,db,list,xpos=xpos,ypos=ypos,xrange=xrange,yrange=yrange
   nm      = n_elements(list)
   IF nm EQ 1 AND list[0] EQ -1 THEN nm = 0
   ;
   ;    * get relevant parameters for derived sources
   IF nm GE 1 THEN BEGIN 
      good   = make_array(nm, /byte, value=0b)
      dbext,list,'object,ra,dec',source,ra1950,dec1950
      ra1950 = 15.0*ra1950
      jprecess, ra1950, dec1950, ra, dec
      aa     = 60.0 + 0.0*ra
      ab     = 1.0 + 0.0*ra
      pa     = 0.0*ra
      parad  = pa/radian
      ;
      ;   + loop through all matches - find pixel outline for source
      ;     & make sure at least some of outline is in the image
      ;     mark these as good.
      FOR ii = 0, nm-1 DO BEGIN 
         rac  = (ra[ii]+aa[ii]*(xcirc*cos(parad[ii])/ab[ii] - ycirc*sin(parad[ii]))/3600.0) MOD 360.0
         decc = dec[ii]+aa[ii]*(xcirc*sin(parad[ii])/ab[ii] + ycirc*cos(parad[ii]))/3600.0
         adxy, hdr, rac, decc, xp, yp
         kk   = where(xp GE 0.0 AND xp LE float(nx-1) AND yp GE 0.0 AND yp LE float(ny-1), nkk)
         IF nkk GE 1 THEN good[ii] = 1b
      ENDFOR 
      kk     = where(good EQ 1b, nkk)
      ;
      ;   + store parameters of good sources
      IF nkk GE 1 THEN BEGIN 
         IF nmatch LE 0 THEN BEGIN 
            catname = make_array(nkk, /string, value=db)
            entry   = list[kk]
            name    = source[kk]
            xpix    = xpos[kk]
            ypix    = ypos[kk]
            sma     = aa[kk]
            axrat   = ab[kk]
            posang  = pa[kk]
            nmatch  = nkk
         ENDIF ELSE BEGIN 
            catname = [catname, make_array(nkk, /string, value=db)]
            entry   = [entry, list[kk]]
            name    = [name, source[kk]]
            xpix    = [xpix, xpos[kk]]
            ypix    = [ypix, ypos[kk]]
            sma     = [sma, aa[kk]]
            axrat   = [axrat, ab[kk]]
            posang  = [posang, pa[kk]]
            nmatch  = nmatch + nkk
         ENDELSE 
      ENDIF 
   ENDIF 
   ;
   ; 9a.  hst_catalog
   ;    * find sources
   db      = 'hst_catalog'    ; name of database
   mrad    = 60.0               ; has to be this many arcsec from FOV or closer
   xrange  = [-1.0,1.0]*mrad/dx + [0.0, float(nx)-1.0]
   yrange  = [-1.0,1.0]*mrad/dy + [0.0, float(ny)-1.0]
   print, 'IMAGE_DBMATCH: searching database: '+db
   grm_imdbase,hdr,db,list,xpos=xpos,ypos=ypos,xrange=xrange,yrange=yrange
   nm      = n_elements(list)
   IF nm EQ 1 AND list[0] EQ -1 THEN nm = 0
   ;
   ;    * get relevant parameters for derived sources
   IF nm GE 1 THEN BEGIN 
      good   = make_array(nm, /byte, value=0b)
      dbext,list,'target,ra,dec,v3,config',source,ra,dec,pa,config
      source = strtrim(source,2)+' '+strtrim(config,2)
      ra     = 15.0*ra
      aa     = 45.0 + 0.0*ra
      ab     = 1.0 + 0.0*ra
      parad  = pa/radian
      ;
      ;   + loop through all matches - find pixel outline for source
      ;     & make sure at least some of outline is in the image
      ;     mark these as good.
      FOR ii = 0, nm-1 DO BEGIN 
         rac  = (ra[ii]+aa[ii]*(xcirc*cos(parad[ii])/ab[ii] - ycirc*sin(parad[ii]))/3600.0) MOD 360.0
         decc = dec[ii]+aa[ii]*(xcirc*sin(parad[ii])/ab[ii] + ycirc*cos(parad[ii]))/3600.0
         adxy, hdr, rac, decc, xp, yp
         kk   = where(xp GE 0.0 AND xp LE float(nx-1) AND yp GE 0.0 AND yp LE float(ny-1), nkk)
         IF nkk GE 1 THEN good[ii] = 1b
      ENDFOR 
      kk     = where(good EQ 1b, nkk)
      ;
      ;   + store parameters of good sources
      IF nkk GE 1 THEN BEGIN 
         IF nmatch LE 0 THEN BEGIN 
            catname = make_array(nkk, /string, value=db)
            entry   = list[kk]
            name    = source[kk]
            xpix    = xpos[kk]
            ypix    = ypos[kk]
            sma     = aa[kk]
            axrat   = ab[kk]
            posang  = pa[kk]
            nmatch  = nkk
         ENDIF ELSE BEGIN 
            catname = [catname, make_array(nkk, /string, value=db)]
            entry   = [entry, list[kk]]
            name    = [name, source[kk]]
            xpix    = [xpix, xpos[kk]]
            ypix    = [ypix, ypos[kk]]
            sma     = [sma, aa[kk]]
            axrat   = [axrat, ab[kk]]
            posang  = [posang, pa[kk]]
            nmatch  = nmatch + nkk
         ENDELSE 
      ENDIF 
   ENDIF 
   ; can't seem to open it...
   ;
   ; 9b.  IUE
   ;    * find sources
   db      = 'IUE'    ; name of database
   mrad    = 20.0               ; has to be this many arcsec from FOV or closer
   xrange  = [-1.0,1.0]*mrad/dx + [0.0, float(nx)-1.0]
   yrange  = [-1.0,1.0]*mrad/dy + [0.0, float(ny)-1.0]
   print, 'IMAGE_DBMATCH: searching database: '+db
   grm_imdbase,hdr,db,list,xpos=xpos,ypos=ypos,xrange=xrange,yrange=yrange
   nm      = n_elements(list)
   IF nm EQ 1 AND list[0] EQ -1 THEN nm = 0
   ;
   ;    * get relevant parameters for derived sources
   IF nm GE 1 THEN BEGIN 
      good   = make_array(nm, /byte, value=0b)
      dbext,list,'object,ra,dec,pos_angle',source,ra1950,dec1950,pa
      ra1950 = 15.0*ra1950
      jprecess, ra1950, dec1950, ra, dec
      aa     = 10.0 + 0.0*ra
      ab     = 2.0 + 0.0*ra
      parad  = pa/radian
      ;
      ;   + loop through all matches - find pixel outline for source
      ;     & make sure at least some of outline is in the image
      ;     mark these as good.
      FOR ii = 0, nm-1 DO BEGIN 
         rac  = (ra[ii]+aa[ii]*(xcirc*cos(parad[ii])/ab[ii] - ycirc*sin(parad[ii]))/3600.0) MOD 360.0
         decc = dec[ii]+aa[ii]*(xcirc*sin(parad[ii])/ab[ii] + ycirc*cos(parad[ii]))/3600.0
         adxy, hdr, rac, decc, xp, yp
         kk   = where(xp GE 0.0 AND xp LE float(nx-1) AND yp GE 0.0 AND yp LE float(ny-1), nkk)
         IF nkk GE 1 THEN good[ii] = 1b
      ENDFOR 
      kk     = where(good EQ 1b, nkk)
      ;
      ;   + store parameters of good sources
      IF nkk GE 1 THEN BEGIN 
         IF nmatch LE 0 THEN BEGIN 
            catname = make_array(nkk, /string, value=db)
            entry   = list[kk]
            name    = source[kk]
            xpix    = xpos[kk]
            ypix    = ypos[kk]
            sma     = aa[kk]
            axrat   = ab[kk]
            posang  = pa[kk]
            nmatch  = nkk
         ENDIF ELSE BEGIN 
            catname = [catname, make_array(nkk, /string, value=db)]
            entry   = [entry, list[kk]]
            name    = [name, source[kk]]
            xpix    = [xpix, xpos[kk]]
            ypix    = [ypix, ypos[kk]]
            sma     = [sma, aa[kk]]
            axrat   = [axrat, ab[kk]]
            posang  = [posang, pa[kk]]
            nmatch  = nmatch + nkk
         ENDELSE 
      ENDIF 
   ENDIF 
   ;
   ; fix coordinates to standard (FITS) pixel convention
   xpix    = xpix + 1.0
   ypix    = ypix + 1.0
   ;
   print, 'IMAGE_DBMATCH: summary of results: '
   print, '            database   entry                       name     Xpix     Ypix    SMA   A/B    angle'
   print, '-----------------------------------------------------------------------------------------------'
   catname = strtrim(temporary(catname),2)
   name    = strtrim(temporary(name),2)
   forprint,catname,entry,name,xpix,ypix,sma,axrat,posang,format='(a20,i8,2x,a25,f9.1,f9.1,f8.2,f7.3,f7.1)'
END 
