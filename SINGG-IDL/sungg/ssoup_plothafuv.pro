PRO ssoup_plothafuv, ll, saveprofile, fjpg, feps, dcorr=dcorr, kline=kline, epilepsy=epilepsy
  ;
  ; plot Halpha/FUV vs Halpha and R surface brightnesses
  ;
  ;  ll          -> logical unit of log file
  ;  saveprofile -> filename of profile saveset
  ;  fjpg        -> name of output file (JPG)
  ;  feps        -> name of output file (EPS)
  ;  dcorr       -> if set then the plot is set for dust corrected
  ;                 values.
  ;  epilepsy    -> if set, displays plots on screen
  ;
  ; G. Meurer (ICRAR/UWA) 6/2010
  ; G. Meurer (ICRAR/UWA) 9/2010 add points from databases
  ; G. Meurer (ICRAR/UWA) 5/2011:
  ;    * plot upper and lower limits, and handle flags
  ;    * reads new file format
  ;
  snlimit   = 2.0
  mflag   =  99.999  ; magnitude flag value
  emflag  =   9.999  ; error mag flag
  lflag   = -99.999  ; log flux flux value
  elflag  =   9.999  ; error log flag
  cuflag  =   8.888  ; colour upper limit flag
  clflag  =   7.777  ; colour lower limit flag
  findpro,'ssoup_plothafuv.pro',DIRLIST=Dirlist,/noprint
  ddir      = Dirlist[0]         ; expect to find data in same directory as this file
  fmtold    = '(a,f,f,f,f,f,f)'
  odb       = 'singg_derived'
  udb1      = 'sungg_derived_sep08'
  udb2      = 'sungg_derived_2010'
  udb3      = 'sungg_derived_zz'
  prog      = 'SSOUP_PLOTHAFUV: '
  plog,ll,prog,'--------------------- starting '+prog+'---------------------------------'
  ;
  ; read files
  plog,ll,prog,'reading in surface brightness profile saveset'
  restore,saveprofile
  COMMON bands, band
  
  ; conversion constants
  plog,ll,prog,'setting constants and plotting parameters'
  csfr      = 7.9d-42             ; Kennicutt et al 1994, salpeter IMF
  sbconv1   = 4.0d0*!pi*(0.206265*3.085678d27)^2      ; (erg/s/kpc^2)/(erg/s/cm^2/arcsec^2)
  sbconv2   = sbconv1*1.0d-7                          ; (W/kpc^2)/(erg/s/cm^2/arcsec^2)
  c_se      = sbconv1*csfr                            ; (Msun/year/kpc^2)/(erg/s/cm^2/arcsec^2)
  lc_se     = alog10(c_se)                            ; log of c_se
  lsbconv2  = alog10(sbconv2)                         ; log of sbconv2
  lcsfr     = alog10(csfr)                            ; log of Kennicut Halpha SFR conversion
  betauv    = -2.0                                    ; assumed spectral slope
  wlk98     = 2150.d0                                 ; wavelength used by Kennicutt 1998
  wlpiv     = 1535.0795d0                             ; FUV pivot lambda
  csfr2     = 2.16d-40*(wlk98/wlpiv)^betauv           ; K98 UV SFR conversion
  lhafuvk   = alog10(csfr2/csfr)                      ; ratio needed for K98 SFRs to be equal
  lhafuvg   = float(alog10(1.03514d41/9.1205057d39))  ; our model 
  lsr0      = 16.433                                  ; used for convert R AB surface brightness to solar/kpc^2
  ;
  ; set some plot parameters
  aspect    = 1.1
  yrange    = [-0.3, 2.3] 
  xrange0   = [-17.9, -11.6] 
  xrange1   = lsbconv2 + xrange0                      ; Halpha surface brightness range
  xrange2   = [4.7, 11.0]                             ; R band surface brightness range
  aa        = angstsym()
  xmargin   = [8.,2.]
  ymargin   = [3.5,2.5]
  charsize  = 1.0
  symsize   = 0.5
  ; set window parameters
  xs    = 8.0
  ys    = 4.0/aspect
  yoff  = 6.0
  xoff  = 0.0
  thick    = 1
  IF keyword_set(dcorr) THEN raw = 0b ELSE raw = 1b
  ;
  ; maximum allowed errors  
  ;eloglim   = alog10(1.0+1.0/snlimit) ; max error in log fluxes or surface brightness
  ;emaglim   = 2.5*eloglim             ; max error in flux and surface brightness magnitude 
  ;ecloglim  = sqrt(2.0)*eloglim       ; max error in log ratios
  ;ecmaglim  = sqrt(2.0)*emaglim       ; max error in colors
  ;
  ; set level of fiducial line
  IF keyword_set(kline) THEN BEGIN 
     fidstr     = ' Kennicutt '
     lhafuv_fid = lhafuvk 
  ENDIF ELSE BEGIN 
     fidstr     = ' Meurer et al. (2009) '
     lhafuv_fid = lhafuvg 
  ENDELSE 
  plog,ll,prog,'will plot'+fidstr+'fiducial log(Ha/FUV) = '+numstr(lhafuv_fid)
  ;
  ; set titles according to dust correction
  IF keyword_set(dcorr) THEN BEGIN 
     title     = hname
     ytitle    = "!3 log(F!dH!4a!3!n/f!dFUV!n ["+aa+"])"
     xtitleh   = "!3 log(!4R!3!dH!4a!3!n [W kpc!u-2!n])"
     xtitler   = "!3 log(!4R!3!dR!n [L!dR,sun!n kpc!u-2!n])"
     oldfn     = ddir+"hafuvsb_table_norm.dat"
     oldfw     = ddir+"hafuvsb_table_weak.dat"
     oldfu     = ddir+"hafuvsb_table_undet.dat"
     plog,ll,prog,"plots annotated for dust corrected"
     namr      = 'R0'
     namh      = 'HALPHA0'
     namhf     = 'log(HALPHA/FUV)0'
  ENDIF ELSE BEGIN 
     title     = hname+" (raw)"
     ytitle    = "!3 log(F'!dH!4a!3!n/f'!dFUV!n ["+aa+"])"
     xtitleh   = "!3 log(!4R'!3!dH!4a!3!n [W kpc!u-2!n])"
     xtitler   = "!3 log(!4R'!3!dR!n [L!dR,sun!n kpc!u-2!n])"
     oldfn     = ddir+"hafuvsb_table_norm_red.dat"
     oldfw     = ddir+"hafuvsb_table_weak_red.dat"
     oldfu     = ddir+"hafuvsb_table_undet_red.dat"
     plog,ll,prog,"plots annotated for raw (not corrected for internal dust)"
     namr      = 'R'
     namh      = 'HALPHA'
     namhf     = 'log(HALPHA/FUV)'
  ENDELSE 
  ;
  ih   = (where(bname eq band.HALPHA, a))[0]
  ir   = (where(bname eq band.R, z))[0]
  inuv = (where(bname eq band.NUV, z))[0]
  ifuv = (where(bname eq band.FUV, b))[0]
  if a eq 0 or b eq 0 then stop,"Cannot plot Ha/FUV when either Ha or FUV is absent!"
  for i=0,n_elements(allprofiles)-1 do begin
      ; reform filenames based on galaxy number
      fjpg_1 = string(i, format='(%"' + fjpg + '")') 
      feps_1 = string(i, format='(%"' + feps + '")') 
    
      sma = *(allprofiles[i].radius)
      if keyword_set(dustcor) then begin
          sr = *(allprofiles[i].mprof_dustcor[ir])
          esr = *(allprofiles[i].err_mprof_dustcor[ir])
          sha = *(allprofiles[i].mprof_dustcor[ih])
          eshat = *(allprofiles[i].err_mprof_dustcor[ih])
          snuv = *(allprofiles[i].mprof_dustcor[inuv])
          esnuv = *(allprofiles[i].err_mprof_dustcor[inuv])
          sfuv = *(allprofiles[i].mprof_dustcor[ifuv])
          esfuv = *(allprofiles[i].err_mprof_dustcor[ifuv])
          scfn = *(allprofiles[i].col_fuv_nuv_dustcor)
          escfn = *(allprofiles[i].err_col_fuv_nuv_dustcor)
          scnr = *(allprofiles[i].col_nuv_r_dustcor)
          escnr = *(allprofiles[i].err_col_nuv_r_dustcor)
          slewr = *(allprofiles[i].log_ha_r_dustcor)
          eslewr = *(allprofiles[i].err_log_ha_r_dustcor)
          slewf = *(allprofiles[i].log_ha_fuv_dustcor)
          eslewf = *(allprofiles[i].err_log_ha_fuv_dustcor)
      endif else begin
          sr = *(allprofiles[i].mprof[ir])
          esr = *(allprofiles[i].err_mprof[ir])
          sha = *(allprofiles[i].mprof[ih])
          eshat = *(allprofiles[i].err_mprof[ih])
          snuv = *(allprofiles[i].mprof[inuv])
          esnuv = *(allprofiles[i].err_mprof[inuv])
          sfuv = *(allprofiles[i].mprof[ifuv])
          esfuv = *(allprofiles[i].err_mprof[ifuv])
          scfn = *(allprofiles[i].col_fuv_nuv)
          escfn = *(allprofiles[i].err_col_fuv_nuv)
          scnr = *(allprofiles[i].col_nuv_r)
          escnr = *(allprofiles[i].err_col_nuv_r)
          slewr = *(allprofiles[i].log_ha_r)
          eslewr = *(allprofiles[i].err_log_ha_r)
          slewf = *(allprofiles[i].log_ha_fuv)
          eslewf = *(allprofiles[i].err_log_ha_fuv)
      endelse
      ;
      ; determine max radii and corresponding pointers
      plog,ll,prog,'determining points to plot'
      ssoup_psp_indices, ll, 'R', sma, sr, esr, mflag, emflag, clflag, emflag, $
                         rmax_r, jmax_r, jr, kr, lr, mr, njr, nkr, nlr, nmr
      ssoup_psp_indices, ll, 'HALPHA', sma, sha, eshat, lflag, elflag, clflag, elflag, $
                         rmax_h, jmax_h, jh, kh, lh, mh, njh, nkh, nlh, nmh
      ssoup_psp_indices, ll, 'log(HALPHA/FUV)', sma, slewf, eslewf, lflag, emflag, clflag, cuflag, $
                         rmax_hf, jmax_hf, jhf, khf, lhf, mhf, njhf, nkhf, nlhf, nmhf
      jhfr      = where(slewf ne lflag and sr ne mflag, njhfr)
      khfr      = where(esr ne emflag and esr ne clflag and eslewf ne emflag and eslewf ne clflag and eslewf ne cuflag, nkhfr)
      ;
      ; convert quantities to those that will be plotted and in 
      ; appropriate units
      lsr       = lsr0 - 0.4*sr  ; R band surface brightness
      elsr      = 0.4*esr        ; its error
      lsha      = lsbconv2+sha   ; Halpha surface brightness
      elsha     = eshat          ; its error
      lewf      = slewf          ; Ha/FUV
      elewf     = eslewf         ; its error
      ;
      ; get quantities from databases
      ssoup_dbhafuvsb, ll, hname, nfound1, lsha1, elsha1, lsr1, elsr1, lhafuv1, elhafuv1, odb=odb, udb=udb1, raw=raw
      ssoup_dbhafuvsb, ll, hname, nfound2, lsha2, elsha2, lsr2, elsr2, lhafuv2, elhafuv2, odb=odb, udb=udb2, raw=raw
      ssoup_dbhafuvsb, ll, hname, nfound3, lsha3, elsha3, lsr3, elsr3, lhafuv3, elhafuv3, odb=odb, udb=udb3, raw=raw
      plog,ll,prog,'number of records found = '+strtrim(string(nfound1),2)+' in optical db = '$
           +strupcase(odb)+' matched to UV db = '+strupcase(udb1)
      plog,ll,prog,'number of records found = '+strtrim(string(nfound2),2)+' in optical db = '$
           +strupcase(odb)+' matched to UV db = '+strupcase(udb2)
      plog,ll,prog,'number of records found = '+strtrim(string(nfound3),2)+' in optical db = '$
           +strupcase(odb)+' matched to UV db = '+strupcase(udb3)
      ;
      ; read in old data for under-plotting
      plog,ll,prog,'reading old results for under-plotting - normal galaxies: '+oldfn
      readcol, oldfn, hnameon, lshaon, lsron, lewfon, format=fmtold
      plog,ll,prog,'reading old results for under-plotting - weak detections: '+oldfw
      readcol, oldfw, hnameow, lshaow, lsrow, lewfow, format=fmtold
      plog,ll,prog,'reading old results for under-plotting - Ha upper limits: '+oldfu
      readcol, oldfu, hnameou, lshaou, lsrou, lewfou, format=fmtold
      ;
      ; match weak and non-detections
      pw       = where(strtrim(hnameow,2) eq hname, npw)
      pu       = where(strtrim(hnameou,2) eq hname, npu)
      plog,ll,prog,'Number of weak H-alpha detection matches: '+strtrim(string(npw),2)
      plog,ll,prog,'Number of (nominal) H-alpha non-detection matches: '+strtrim(string(npu),2)
      ;
      ssoup_plot_init,feps_1,xs,ys,xoff,yoff
      ;
      ; left panel Halpha/FUV versus Halpha surface brightness
      !p.noerase = 0
      !p.multi   = [2, 2, 1] ; left panel
      plog,ll,prog,'plotting Halpha/FUV vs Halpha surf bright.'
      plot, lsha, lewf, xrange=xrange1, yrange=yrange, xstyle=1, ystyle=1, psym=gsym(9), $
            xtitle=xtitleh, ytitle=ytitle, title=title, charsize=charsize, symsize=symsize, $
            xmargin=xmargin, ymargin=ymargin, thick=thick, xthick=thick, ythick=thick, charthick=thick, $
            /nodata
      oplot, lshaon, lewfon, psym=gsym(1), symsize=symsize, color=!gray
      oplot, lshaow, lewfow, psym=gsym(10), symsize=symsize, color=!gray
      oplot, lshaou, lewfou, psym=gsym(10), symsize=0.6*symsize, color=!gray
      ;
      ; plot weak or non detections (in Halpha) matches and highlight if found
      if npw gt 0 then begin 
         oplot, lshaow[pw], lewfow[pw], psym=gsym(10), symsize=symsize, color=!dpink
      ENDIF ELSE BEGIN 
         IF npu GT 0 THEN BEGIN 
            oplot, lshaou[pu], lewfou[pu], psym=gsym(10), symsize=0.6*symsize, color=!dpink
         ENDIF 
      ENDELSE 
      ;
      ; plot database values, weak and non-detections take 
      ; precedence over matches with first UV database 
      IF nfound1 GT 0 AND (npw EQ 0 and npu eq 0) THEN oploterror_old, lsha1, lhafuv1, elsha1, elhafuv1, psym=gsym(1), symsize=symsize, color=!dpink, errcolor=!dpink, /nohat
      IF nfound2 GT 0 THEN oploterror_old, lsha2, lhafuv2, elsha2, elhafuv2, psym=gsym(1), symsize=symsize, color=!red, errcolor=!red, /nohat
      IF nfound3 GT 0 THEN oploterror_old, lsha3, lhafuv3, elsha3, elhafuv3, psym=gsym(1), symsize=symsize, color=!green, errcolor=!green, /nohat
      ;
      ; draw fiducial line
      oplot, xrange1, lhafuv_fid*[1.,1.], linestyle=4, thick=thick+1, color=!blue
      ;
      ; plot profile and error
      if njhf gt 0 then begin 
         oplot, lsha[jhf], lewf[jhf], linestyle=1, thick=thick                 ; plottable points: dotted line
         oplot, lsha[jhf], lewf[jhf], symsize=symsize, psym=gsym(9), color=!black ; linking hollow points
      endif 
      if nkhf gt 0 then begin 
         ;
         ; points good in x & y: 
         ; solid line linking filled symbols with errors
         oplot, lsha[khf], lewf[khf], thick=thick         
         oplot, lsha[khf], lewf[khf], symsize=symsize, psym=gsym(4), color=!black
         oploterror_old, lsha[khf], lewf[khf], elsha[khf], elewf[khf], $
                     /nohat, errthick=thick+1, psym=3, errcolor=!black
      endif
      if nlhf gt 0 then oplot, lsha[lhf], lewf[lhf], symsize=symsize, psym=gsym(19)  ; lower limit in y 
      if nmhf gt 0 then oplot, lsha[mhf], lewf[mhf], symsize=symsize, psym=gsym(18)  ; upper limit in y 
      if nlh  gt 0 then oplot, lsha[lh], lewf[lh], symsize=symsize, psym=gsym(20)    ; lower limit in x
      if nmh  gt 0 then oplot, lsha[mh], lewf[mh], symsize=symsize, psym=gsym(21)    ; upper limit in x
      ;
      ; right panel Halpha/FUV versus R surface brightness
      !p.multi   = [1, 2, 1]
      plog,ll,prog,'plotting Halpha/FUV vs R surf bright.'
      plot, lsr, lewf, xrange=xrange2, yrange=yrange, xstyle=1, ystyle=1, psym=gsym(9), $
            xtitle=xtitler, ytitle=ytitle, title=title, charsize=charsize, symsize=symsize, $
            xmargin=xmargin, ymargin=ymargin, thick=thick, xthick=thick, ythick=thick, charthick=thick, $
            /nodata
      oplot, lsron, lewfon, psym=gsym(1), symsize=symsize, color=!gray
      oplot, lsrow, lewfow, psym=gsym(10), symsize=symsize, color=!gray
      oplot, lsrou, lewfou, psym=gsym(10), symsize=0.6*symsize, color=!gray
      ;
      ; plot weak or non detections (in Halpha) matches and highlight if found
      if npw gt 0 then begin 
         oplot, lsrow[pw], lewfow[pw], psym=gsym(10), symsize=symsize, color=!dpink
      ENDIF ELSE BEGIN 
         IF npu GT 0 THEN BEGIN 
            oplot, lsrou[pu], lewfou[pu], psym=gsym(10), symsize=0.6*symsize, color=!dpink
         ENDIF 
      ENDELSE 
      ;
      ; plot fiducial line
      oplot, xrange2, lhafuv_fid*[1.,1.], linestyle=4, thick=thick+1, color=!blue
      ;
      ; plot database values
      IF nfound1 GT 0 AND (npw EQ 0 and npu eq 0) THEN oploterror_old, lsr1, lhafuv1, elsr1, elhafuv1, psym=gsym(1), symsize=symsize, color=!dpink, errcolor=!dpink, /nohat
      IF nfound2 GT 0 THEN oploterror_old, lsr2, lhafuv2, elsr2, elhafuv2, psym=gsym(1), symsize=symsize, color=!red, errcolor=!red, /nohat
      IF nfound3 GT 0 THEN oploterror_old, lsr3, lhafuv3, elsr3, elhafuv3, psym=gsym(1), symsize=symsize, color=!green, errcolor=!green, /nohat
      ;
      ; plot profile and error
      if njhfr gt 0 then begin 
         oplot, lsr[jhfr], lewf[jhfr], linestyle=1, thick=thick                 ; plottable points: dotted line
         oplot, lsr[jhfr], lewf[jhfr], symsize=symsize, psym=gsym(9), color=!black ; linking hollow points
      endif 
      if nkhf gt 0 then begin 
         ;
         ; points good in x & y: 
         ; solid line linking filled symbols with errors
         oplot, lsr[khfr], lewf[khfr], thick=thick        
         print,lsr[khfr],lewf[khfr] 
         oplot, lsr[khfr], lewf[khfr], symsize=symsize, psym=gsym(4), color=!black
         oploterror_old, lsr[khfr], lewf[khfr], elsr[khfr], elewf[khfr], $
                     /nohat, errthick=thick+1, psym=3, errcolor=!black
      endif
      if nlhf gt 0 then oplot, lsr[lhf], lewf[lhf], symsize=symsize, psym=gsym(19)  ; lower limit in y 
      if nmhf gt 0 then oplot, lsr[mhf], lewf[mhf], symsize=symsize, psym=gsym(18)  ; upper limit in y 
      if nlh  gt 0 then oplot, lsr[lr], lewf[lr], symsize=symsize, psym=gsym(20)    ; lower limit in x
      if nmr  gt 0 then oplot, lsr[mr], lewf[mr], symsize=symsize, psym=gsym(21)    ; upper limit in x
      ;
      ; ------------------------------------------------------------------
      ; finish plot
      plog,ll,prog,'finishing. Will write plotfile: '+feps_1
      !p.multi   = 0
      !p.noerase = 0
      ssoup_plot_finish,fjpg_1,feps_1,1200,epilepsy=epilepsy
  endfor
END
