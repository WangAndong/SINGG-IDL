PRO hafuvs2, hardfile=hardfile
   ;
   ;  Plot Halpha/FUV versus R-band luminosity
   ;
   gdb       = 'sungg_derived'
   ddb       = 'singg_derived'
   
   mrad      = 0.25             ; matchradius in arcmin
   mrsun     = 4.61
   aspect    = 1.1
   nsigma    = 2.5
   snlimit   = 2.0

; constants from V_rot script   
   aratmin  = 1.4
   wtherm   = 5.0   ; km/s
   vconv    = 1.02  ; pc/Myr per km/s
   reshi    = 26.4
   qq0      = 0.12
   h0       = 70.0
   cc       = 2.9978e5
   h0_lh96  = 75.0
   radian   = 180.0/!pi
   const    = 206.265
   rmeth    = 'max'
   vmeth    = 'c'

; constants regarding star formation converstion
   csfr      = 7.9d-42             ; Kennicutt et al 1994, salpeter IMF
   c_se      = 4.0d0*!pi*(0.206265*3.085678d27)^2*csfr
   lc_se     = alog10(c_se)
   lcsfr     = alog10(csfr)

;range of plotting parameters
   lrrange   = [6.0, 12.0]
   lmrange   = [7.0, 11.5]
   lewrangef = [0.0, 2.0]
   lewrangen = [0.0, 2.3]
   lsfarange = [-4., 1]
   minlse    = -19.0
   maxlse    = -14.0
   lserange  = [minlse, maxlse]
   srrange   = [6.0, 9.0]
   lvcrange  = [0.5, 3.0]
 
   aa        = angstsym()
   ytitlef   = '!3 log(F!dH!4a!3!n / f!dFUV!n ['+aa+'])'
   xtitle    = '!3 log(L!dR!n [L!dsun!n])'
   xtitler   = '!3 log(V!drot !n[km/s])'
   charsize  = 1.7
   symsize   = 1.0
   ;

   ; open UV database, get ra, dec, and surface brightness
   ; values for FUV and NUV sources
   dbopen, gdb
   entfuv = dbfind('filter = fuv')
   entnuv = dbfind('filter = nuv')
   dbext, entfuv, 'name,hipname,optid,ra,dec,se,flux_corr,flux_rms', namef,hnamef,optidf,raf,decf,lsef,ffuv,effuv
   dbext, entnuv, 'name,hipname,optid,ra,dec,se,flux_corr,flux_rms', namen,hnamen,optidn,ran,decn,lsen,fnuv,efnuv
   dbclose
   ;
   ; make arrays to store H-alpha matches
   nf      = n_elements(entfuv)
   nn      = n_elements(entnuv)
   enthaf  = make_array(nf, /long, value=-1l)
   enthan  = make_array(nn, /long, value=-1l)
   ;
   ; open H-alpha database, get good entries
   dbopen, ddb
   goodha  = good_derived3(ng, snlimit=snlimit)
   print, 'Number of good entries in singg_derived: ', ng
   ;
   ; loop through FUV entries find H-alpha matches
   FOR ii = 0, nf-1 DO BEGIN 
      ra   = raf[ii]
      dec  = decf[ii]
      list = dbcircled(ra, dec, mrad, dis, goodha)
      nl   = n_elements(list)
      IF nl EQ 1 AND list[0] LT 0 THEN nl = 0
      IF nl GE 1 THEN BEGIN 
         IF nl EQ 1 THEN BEGIN 
            ;
            ; store matching entry
            enthaf[ii] = list[0]
         ENDIF ELSE BEGIN 
            ;
            ; find closest match if there are multiple
            jj = sort(dis)
            enthaf[ii] = list[jj[0]]
         ENDELSE 
      ENDIF 
   ENDFOR 
   kf = where(enthaf GE 0, nkf)
   ;
   ; loop through NUV entries find H-alpha matches
   FOR ii = 0, nn-1 DO BEGIN 
      ra   = ran[ii]
      dec  = decn[ii]
      list = dbcircled(ra, dec, mrad, dis, goodha)
      nl   = n_elements(list)
      IF nl EQ 1 AND list[0] LT 0 THEN nl = 0
      IF nl GE 1 THEN BEGIN 
         IF nl EQ 1 THEN BEGIN 
            ;
            ; store matching entry
            enthan[ii] = list[0]
         ENDIF ELSE BEGIN 
            ;
            ; find closest match if there are multiple
            jj = sort(dis)
            enthan[ii] = list[jj[0]]
         ENDELSE 
      ENDIF 
   ENDFOR 
   kn = where(enthan GE 0, nkn)
   ;
   ; print match results
   print, 'Number of FUV - H-alpha matches: ', nkf
   print, 'Number of NUV - H-alpha matches: ', nkn
   ;
   ; save only UV results that match
   namef   = namef[kf]
   hnamef  = hnamef[kf]
   optidf  = optidf[kf]
   raf     = raf[kf]
   decf    = decf[kf]
   lsef    = lsef[kf]
   enthaf  = enthaf[kf]
   ffuv    = ffuv[kf]
   namen   = namen[kn]
   hnamen  = hnamen[kn]
   optidn  = optidn[kn]
   ran     = ran[kn]
   decn    = decn[kn]
   lsen    = lsen[kn]
   enthan  = enthan[kn]
   fnuv    = fnuv[kn]
   ;
   csfaf     = 4.34e13
   csfan     = 9.78e13
   lcsfaf    = alog10(csfaf)
   lcsfan    = alog10(csfan)
   lsfaf     = lcsfaf + lsef
   lsfan     = lcsfan + lsen
   ;
   ; get R and Halpha luminosities
   dbext, enthaf, 'name,mabs_r0_t,logf_ha0_t,err_logf_ha_t,logse_ha0_t', snamef, mabsrf, lfhaf, elfhaf, lsehaf
   dbext, enthaf, 'r90_ha_t,err_r90_ha_f,axerat,rmax_f,distance',r90haf,er90haf,axeratf,rmaxf,distf
   dbext, enthaf, 'err_mag_r_t,re_r_t,r90_r_t,re_ha_t,entry_sample,mdyn_method', emabsrf, ref, r90f, rehaf, entsampf, method
   dbext, enthan, 'name,mabs_r0_t,logf_ha0_t,err_logf_ha_t,logse_ha0_t', snamen, mabsrn, lfhan, elfhan, lsehan
   dbext, enthan, 'r90_ha_t,err_r90_ha_f,axerat,rmax_f,distance',r90han,er90han,axeratn,rmaxn,distn
   dbext, enthaf, 'err_mag_r_t, re_r_t, r90_r_t, re_ha_t, entry_sample', emabsrn, ren, r90n, rehan, entsampn
   dbclose

   dbopen,'singg_sample'
   dbext,entsampf,'w50,vhel',w50f,vhelf
   dbclose

   llumrf    = -0.4*(mabsrf - mrsun)
   llumrn    = -0.4*(mabsrf - mrsun)
   lsesfrf   = lc_se + lsehaf
   lsesfrn   = lc_se + lsehan
   ;
;   lserf     = 16.433 - 0.4*muerf
;   lsern     = 16.433 - 0.4*muern
;   elserf    = 0.4*emuerf
;   elsern    = 0.4*emuern
   ;
   ; determine F_Ha/f_uv, and errors
   lewf     = lfhaf - alog10(ffuv)
   lewn     = lfhan - alog10(fnuv)
   elffuv   = alog10(1.0+effuv/ffuv)
   elfnuv   = alog10(1.0+efnuv/fnuv)
   elewf    = sqrt(elffuv^2 + elfhaf^2)
   elewn    = sqrt(elfnuv^2 + elfhan^2)
;   edynmf   = 0 * dynmf
;   elmhif   = 0 * lmhif
   

   ; calculate V_rot based on W50 and r_mas and other parameter
   ; pick radius to use
   CASE rmeth OF
      'r90' : ropt = r90f
      '3re' : ropt = 3.0*ref
      'max' : ropt = rmaxf
      ELSE  : ropt = r90f
   ENDCASE
   ;
   ; calculate things
   zz    = vhelf/cc
   scale = distf/206.265
   ropt  = ropt*scale
   lre   = 3.0 + alog10(ref*scale)
   lr90  = 3.0 + alog10(r90f*scale)
   lrmax = 3.0 + alog10(rmaxf*scale)
   qq    = 1.0/axeratf
   kk    = where(qq LT qq0, nkk)
   IF nkk GT 0 THEN qq[kk] = qq0
   cosi   = sqrt((qq^2 - qq0^2)/(1 - qq0^2))
   inc    = acos(cosi)
   w50a   = w50f/(1.0 + zz)              ; relativistic correction
   w50b   = w50a - 0.13*reshi           ; instrumental broadening correction
   eterm  = exp(-1.0*(w50b/wtherm)^2)
   w50c   = sqrt(w50b^2 + wtherm^2*(1.0 - 2.0*eterm) $
                                    - 2.0*w50b*wtherm*(1.0 - eterm))

   ; pick orbital velocity to use
   CASE vmeth OF
      '0'   : vc = w50f/(2.0*sin(inc))
      'a'   : vc = w50a/(2.0*sin(inc))
      'b'   : vc = w50b/(2.0*sin(inc))
      'c'   : vc = w50c/(2.0*sin(inc))
      ELSE  : vc = w50c/(2.0*sin(inc))
   ENDCASE
   lvc       = alog10(vc)
   evc       = 0* lvc
   ;
   ; determine correlation coefficients
   jj1      = where(lewf GE lewrangef[0] AND lewf LE lewrangef[1] AND llumrf GE lrrange[0] AND llumrf LE lrrange[1], njj1)
   jj2      = where(axeratf GE 1.4 AND method EQ 1 AND lewf GE lewrangef[0] AND lewf LE lewrangef[1] AND lvc GE lvcrange[0] AND lvc LE lvcrange[1], njj2)
   rxy1     = rxy(llumrf[jj1], lewf[jj1])
   rxy2     = rxy(lvc[jj2], lewf[jj2])
   print, 'Correlation coef Ha/UV vs. L_R, N : ', rxy1, njj1
   print, 'Correlation coef Ha/UV vs. V_c, N   : ', rxy2, njj2
   ;
   ; fit SF intensity versus L(R)
   print, 'Fitting log(Ha/FUV) intensity versus log (L_R) [OLS(bissector) ] ...'
   iter_sixlin, llumrf[jj1], lewf[jj1], 2, nsigma, aa1, sigaa1, bb1, sigbb1, $
                sigx1, sigy1, nused1, niter=niter1, indused=indused1
   print, ' intercept = ', aa1[2], ' +/- ', sigaa1[2]
   print, ' slope     = ', bb1[2], ' +/- ', sigbb1[2]
   print, ' sigma(x)  = ', sigx1[2]
   print, ' sigma(y)  = ', sigy1[2]
   print, ' nused     = ', nused1
   print, ' nrej      = ', njj1-nused1
   print, ' niter     = ', niter1
   print, 'Fitting log(Ha/FUV) intensity versus log (V_c) [OLS(bissector) ] ...'
   iter_sixlin, lvc[jj2], lewf[jj2], 2, nsigma, aa2, sigaa2, bb2, sigbb2, $
                sigx2, sigy2, nused2, niter=niter2, indused=indused2
   print, ' intercept = ', aa2[2], ' +/- ', sigaa2[2]
   print, ' slope     = ', bb2[2], ' +/- ', sigbb2[2]
   print, ' sigma(x)  = ', sigx2[2]
   print, ' sigma(y)  = ', sigy2[2]
   print, ' nused     = ', nused2
   print, ' nrej      = ', njj2-nused2
   print, ' niter     = ', niter2
   ;stop
   ;
   ; set plot parameters
   IF keyword_set(hardfile) THEN BEGIN 
      xs    = 8.0
      ys    = xs/(2.*aspect)
      yoff  = 8.0
      xoff  = 3.0
      thick = 2
      set_plot,'ps',/copy, /interpolate
      IF strpos(strlowcase(hardfile), '.eps') GT 0 THEN $
         device,/inches,xs=xs,ys=ys,yo=yoff,xo=xoff,/color,/encapsulated ELSE $
         device,/inches,xs=xs,ys=ys,yo=yoff,xo=xoff,/color
      charsize = 0.6*charsize
      symsize  = 0.6*symsize
   ENDIF ELSE BEGIN 
      wxsize   = 1200
      wysize   = fix(float(wxsize/(2.0*aspect)))
      charsize = charsize*wxsize/800.0
      symsize  = symsize*wxsize/800.0
      thick    = 2
      window, 0, xsize=wxsize, ysize=wysize
   ENDELSE 
   setplotcolors
   ;
   ; plot fuv results
   !p.noerase = 0
   !p.multi   = [2, 2, 1]                  ; left panel
   plot, llumrf[jj1], lewf[jj1], xrange=lrrange, yrange=lewrangef, xstyle=1, ystyle=1, psym=sym(1), $
         xtitle=xtitle, ytitle=ytitlef, charsize=charsize, symsize=symsize, $ 
         thick=thick,xthick=thick, ythick=thick, charthick=thick
   xx         = lrrange 
   yy         = aa1[2] + bb1[2]*xx
   oplot, xx, yy, color=!black, thick=thick+1
   yy         = yy - nsigma*sigy1[2]
   oplot, xx, yy, color=!black, linestyle=2, thick=thick
   yy         = yy + 2.0*nsigma*sigy1[2]
   oplot, xx, yy, color=!black, linestyle=2, thick=thick
   oploterror_old, llumrf[jj1], lewf[jj1], emabsrf[jj1], elewf[jj1], /nohat, errthick=errthick, errcolor=!black, psym=sym(1)
   oplot, llumrf, lewf, symsize=symsize, color=!black, psym=sym(1)
   xlab       = lrrange[0] + 0.05*(lrrange[1] - lrrange[0])
   ylab       = lewrangef[0] + 0.9*(lewrangef[1] - lewrangef[0])
   xyouts, xlab, ylab, '!3 a', alignment=0.5, charsize=charsize, charthick=thick
   ;
   !p.multi   = [1, 2, 1] ; right panel
   plot, lvc[jj2], lewf[jj2], xrange=lvcrange, yrange=lewrangef, xstyle=1, ystyle=1, psym=sym(1), $
         xtitle=xtitler, ytitle=ytitlef, charsize=charsize, symsize=symsize, $ 
         thick=thick, xthick=thick, ythick=thick, charthick=thick
   xx         = lvcrange 
   yy         = aa2[2] + bb2[2]*xx
   oplot, xx, yy, color=!black, thick=thick+1
   yy         = yy - nsigma*sigy2[2]
   oplot, xx, yy, color=!black, linestyle=2, thick=thick
   yy         = yy + 2.0*nsigma*sigy2[2]
   oplot, xx, yy, color=!black, linestyle=2, thick=thick
   oploterror_old, lvc[jj2], lewf[jj2], evc[jj2], elewf[jj2], /nohat, errthick=errthick, errcolor=!black, psym=sym(1)
   oplot, lvc[jj2], lewf[jj2], symsize=symsize, color=!black, psym=sym(1)
   xlab       = lvcrange[0] + 0.05*(lvcrange[1] - lvcrange[0])
   ylab       = lewrangef[0] + 0.9*(lewrangef[1] - lewrangef[0])
   xyouts, xlab, ylab, '!3 b',alignment=0.5, charsize=charsize, charthick=thick
   ;
   ; finish
   !p.multi   = 0
   !p.noerase = 0
;   snap_jpg,'hafuvs.jpg'
   IF keyword_set(hardfile) THEN BEGIN 
      psend, hardfile, /noprint, /clobber
   ENDIF 


END 
