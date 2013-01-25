PRO ssoup_plotsprofs, ll, sname, fjpg, feps, epilepsy=epilepsy
  ;
  ; plot surface brightness and surface colour profiles
  ;
  ;   ll         -> logical unit number for plot
  ;   sname      -> Source name
  ;   fjpg       -> output plot file name (JPG)
  ;   feps       -> optput plot file name (EPS)
  ;   epilepsy   -> whether we should display images on the screen
  ;
  ; G. Meurer (ICRAR/UWA) 6/2010
  ; G. Meurer (ICRAR/UWA) 5/2011: 
  ;    * improve doc. 
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
  prog      = 'SSOUP_PLOTSPROFS: '
  plog,ll,prog,'--------------------- starting '+prog+'---------------------------------'
  ;
  ; some plot parameters
  aa        = angstsym()
  rtitle    = '!3 semi-major axis [arcsec]'
  hftitle   = '!3 log(F!dH!4a!3!n/f!dFUV!n,f!dR!N ['+aa+'])'
  abtitle   = '!4 l!3 [ABmag arcssec!u-2!n]'
  abctitle  = '!3 surface colour'
  charsize  = 2.5
  symsize   = 1.0
  thick     = 1
  ;
  ; read in profile files
  COMMON bands, band
  plog,ll,prog,'reading in surface brightness profile saveset'
  restore,sname+"_profiles.save"
  for i=0,n_elements(allprofiles)-1 do begin
    ; reform filenames based on galaxy number
    fjpg_1 = string(i, format='(%"' + fjpg + '")') 
    feps_1 = string(i, format='(%"' + feps + '")') 
    
    ; look here
    index_ha = (where(bname eq band.HALPHA, /null))[0]
    index_nuv = (where(bname eq band.NUV, /null))[0]
    index_fuv = (where(bname eq band.FUV, /null))[0]
    index_r = (where(bname eq band.R, /null))[0]
    
    ; rename stuff
    sma = *(allprofiles[i].radius)
    sr = *(allprofiles[i].mprof[index_r])
    esr = *(allprofiles[i].err_mprof[index_r])
    sha = *(allprofiles[i].mprof[index_ha])
    eshat = *(allprofiles[i].err_mprof[index_ha])
    snuv = *(allprofiles[i].mprof[index_nuv])
    esnuv = *(allprofiles[i].err_mprof[index_nuv])
    sfuv = *(allprofiles[i].mprof[index_fuv])
    esfuv = *(allprofiles[i].err_mprof[index_fuv])
    scfn = *(allprofiles[i].col_fuv_nuv)
    escfn = *(allprofiles[i].err_col_fuv_nuv)
    scnr = *(allprofiles[i].col_nuv_r)
    escnr = *(allprofiles[i].err_col_nuv_r)
    slewr = *(allprofiles[i].log_ha_r)
    eslewr = *(allprofiles[i].err_log_ha_r)
    slewf = *(allprofiles[i].log_ha_fuv)
    eslewf = *(allprofiles[i].err_log_ha_fuv)
    sma0 = sma
    sr0 = *(allprofiles[i].mprof_dustcor[index_r])
    esr0 = *(allprofiles[i].err_mprof_dustcor[index_r])
    sha0 = *(allprofiles[i].mprof_dustcor[index_ha])
    eshat0 = *(allprofiles[i].err_mprof_dustcor[index_ha])
    snuv0 = *(allprofiles[i].mprof_dustcor[index_nuv])
    esnuv0 = *(allprofiles[i].err_mprof_dustcor[index_nuv])
    sfuv0 = *(allprofiles[i].mprof_dustcor[index_fuv])
    esfuv0 = *(allprofiles[i].err_mprof_dustcor[index_fuv])
    scfn0 = *(allprofiles[i].col_fuv_nuv_dustcor)
    escfn0 = *(allprofiles[i].err_col_fuv_nuv_dustcor)
    scnr0 = *(allprofiles[i].col_nuv_r_dustcor)
    escnr0 = *(allprofiles[i].err_col_nuv_r_dustcor)
    slewr0 = *(allprofiles[i].log_ha_r_dustcor)
    eslewr0 = *(allprofiles[i].err_log_ha_r_dustcor)
    slewf0 = *(allprofiles[i].log_ha_fuv_dustcor)
    eslewf0 = *(allprofiles[i].err_log_ha_fuv_dustcor)
    
    ; determine max radii and corresponding pointers
    plog,ll,prog,'determining maxima radii, and points to plot'
    ;
    ; indices below are named as follows:
    ; 1 [j,k,l,m] indicates how to plot points
    ;   j: plottable (good or an upper or lower limit)
    ;   k: good
    ;   l: lower limit
    ;   m: upper limit
    ; 2 [r,h,f,n,fn,nr,hr,hf] indicates band or colour
    ; 3 append 0 for dust corrected
    ; 4 prepend n to indicate count of the indices
    ;
    ssoup_psp_indices, ll, 'R', sma, sr, esr, mflag, emflag, clflag, emflag, $
                       rmax_r, jmax_r, jr, kr, lr, mr, njr, nkr, nlr, nmr
    ssoup_psp_indices, ll, 'R0', sma0, sr0, esr0, mflag, emflag, clflag, emflag, $
                       rmax_r0, jmax_r0, jr0, kr0, lr0, mr0, njr0, nkr0, nlr0, nmr0
    ;
    ssoup_psp_indices, ll, 'HALPHA', sma, sha, eshat, lflag, elflag, clflag, elflag, $
                       rmax_h, jmax_h, jh, kh, lh, mh, njh, nkh, nlh, nmh
    ssoup_psp_indices, ll, 'HALPHA0', sma0, sha0, eshat0, lflag, elflag, clflag, elflag, $
                       rmax_h0, jmax_h0, jh0, kh0, lh0, mh0, njh0, nkh0, nlh0, nmh0
    ;
    ssoup_psp_indices, ll, 'NUV', sma, snuv, esnuv, mflag, emflag, clflag, emflag, $
                       rmax_n, jmax_n, jn, kn, ln, mn, njn, nkn, nln, nmn
    ssoup_psp_indices, ll, 'NUV0', sma0, snuv0, esnuv0, mflag, emflag, clflag, emflag, $
                       rmax_n0, jmax_n0, jn0, kn0, ln0, mn0, njn0, nkn0, nln0, nmn0
    ;
    ssoup_psp_indices, ll, 'FUV', sma, sfuv, esfuv, mflag, emflag, clflag, emflag, $
                       rmax_f, jmax_f, jf, kf, lf, mf, njf, nkf, nlf, nmf
    ssoup_psp_indices, ll, 'FUV0', sma0, sfuv0, esfuv0, mflag, emflag, clflag, emflag, $
                       rmax_f0, jmax_f0, jf0, kf0, lf0, mf0, njf0, nkf0, nlf0, nmf0
    ;
    ssoup_psp_indices, ll, '(FUV-NUV)', sma, scfn, escfn, mflag, emflag, clflag, cuflag, $
                       rmax_fn, jmax_fn, jfn, kfn, lfn, mfn, njfn, nkfn, nlfn, nmfn
    ssoup_psp_indices, ll, '(FUV-NUV)0', sma0, scfn0, escfn0, mflag, emflag, clflag, cuflag, $
                       rmax_fn0, jmax_fn0, jfn0, kfn0, lfn0, mfn0, njfn0, nkfn0, nlfn0, nmfn0
    ;
    ssoup_psp_indices, ll, '(NUV-R)', sma, scnr, escnr, mflag, emflag, clflag, cuflag, $
                       rmax_nr, jmax_nr, jnr, knr, lnr, mnr, njnr, nknr, nlnr, nmnr
    ssoup_psp_indices, ll, '(NUV-R)0', sma0, scnr0, escnr0, mflag, emflag, clflag, cuflag, $
                       rmax_nr0, jmax_nr0, jnr0, knr0, lnr0, mnr0, njnr0, nknr0, nlnr0, nmnr0
    ;
    ssoup_psp_indices, ll, 'log(HALPHA/R)', sma, slewr, eslewr, lflag, emflag, clflag, cuflag, $
                       rmax_hr, jmax_hr, jhr, khr, lhr, mhr, njhr, nkhr, nlhr, nmhr
    ssoup_psp_indices, ll, 'log(HALPHA/R)0', sma0, slewr0, eslewr0, lflag, emflag, clflag, cuflag, $
                       rmax_hr0, jmax_hr0, jhr0, khr0, lhr0, mhr0, njhr0, nkhr0, nlhr0, nmhr0
    ;
    ssoup_psp_indices, ll, 'log(HALPHA/FUV)', sma, slewf, eslewf, lflag, emflag, clflag, cuflag, $
                       rmax_hf, jmax_hf, jhf, khf, lhf, mhf, njhf, nkhf, nlhf, nmhf
    ssoup_psp_indices, ll, 'log(HALPHA/FUV)0', sma0, slewf0, eslewf0, lflag, emflag, clflag, cuflag, $
                       rmax_hf0, jmax_hf0, jhf0, khf0, lhf0, mhf0, njhf0, nkhf0, nlhf0, nmhf0
    ;
    rmax      = 1.05*max([rmax_r,rmax_h,rmax_n,rmax_f,rmax_fn,rmax_nr,rmax_hr,rmax_hf,rmax_r0,rmax_h0,rmax_n0,rmax_f0,rmax_fn0,rmax_nr0,rmax_hr0,rmax_hf0])
    plog,ll,prog,'maximum R band radius:      '+numstr(rmax_r)
    plog,ll,prog,'maximum Halpha band radius: '+numstr(rmax_h)
    plog,ll,prog,'maximum NUV band radius:    '+numstr(rmax_n)
    plog,ll,prog,'maximum FUV band radius:    '+numstr(rmax_f)
    plog,ll,prog,'maximum radius in plots:    '+numstr(rmax)
    ;
    ; set plot limits
    rrange    = [0.0,rmax]
    hfrange   = [-1.0,3.2]
    abrange   = [32.0,14.0]
    shrange   = -18.5 + [0.0,-0.4*(abrange[1]-abrange[0])]
    abcrange  = [-1.5,6.0]
    ;
    ; put Halpha on mag scale
    mmm       = abrange[0] + 2.5*shrange[0]
    msha      = mmm - 2.5*sha
    meshat    = 2.5*eshat
    msha0     = mmm - 2.5*sha0
    meshat0   = 2.5*eshat0
    ;
    ; plot set up
    !p.multi = [0, 1, 3]          ; Three panel plot
     xs       = 6.5
     ys       = 1.25*xs
     yoff     = 3.
     xoff     = 1.2
     thick    = 2
     wxsize   = 600
     ansize   = 1.0
    ssoup_plot_init,feps_1,xs,ys,xoff,yoff
    ;
    ; ------------------------------------------------------------------
    ; panel 1
    ; R, FUV, NUV Halpha radial surface brightness profile
    plog,ll,prog,'plotting surface brightness profiles'
    !p.noerase = 1
    plot, sma[0:1], sr[0:1], xrange=rrange, yrange=abrange, xstyle=1, ystyle=1, $
          charsize=charsize, symsize=symsize, thick=thick, xthick=thick, ythick=thick, $
          xtitle=rtitle, ytitle=abtitle, title=sname, charthick=thick, $
          xmargin=[8,8], ymargin=[2,4], /nodata
    ssoup_overlay_prof, sma, sr, sr0, esr0, symsize, thick, !dorange, jr, kr, lr, mr, njr, nkr, $
      nlr, nmr, jr0, kr0, lr0, mr0, njr0, nkr0, nlr0, nmr0
    ssoup_overlay_prof, sma, snuv, snuv0, esnuv0, symsize, thick, !cyan, jn, kn, ln, mn, njn, nkn, $
      nln, nmn, jn0, kn0, ln0, mn0, njn0, nkn0, nln0, nmn0
    ssoup_overlay_prof, sma, sfuv, sfuv0, esfuv0, symsize, thick, !blue, jf, kf, lf, mf, njf, nkf, $
      nlf, nmf, jf0, kf0, lf0, mf0, njf0, nkf0, nlf0, nmf0
    ssoup_overlay_prof, sma, msha, msha0, eshat0, symsize, thick, !dpink, jh, kh, lh, mh, njh, nkh, $
      nlh, nmh, jh0, kh0, lh0, mh0, njh0, nkh0, nlh0, nmh0
    ;
    ; ------------------------------------------------------------------
    ; panel 2
    ; FUV-NUV, NUV-R colour profiles
    plog,ll,prog,'plotting color profiles'
    !p.noerase = 0
    !p.multi = [2,1,3]
    plot, sma[0:1], scnr[0:1], xrange=rrange, yrange=abcrange, xstyle=1, ystyle=1, $
          charsize=charsize, symsize=symsize, thick=thick, xthick=thick, ythick=thick, $
          xtitle=rtitle, ytitle=abctitle, charthick=thick, xmargin=[8,8], ymargin=[3,3], /nodata
    IF njnr GT 0 THEN BEGIN 
       oplot, sma[jnr], scnr[jnr], thick=thick, color=!orange, linestyle=1
       if nknr gt 0 then oplot, sma[knr], scnr[knr], thick=thick, color=!orange, linestyle=0
       if njnr0 gt 0 then oplot, sma0[jnr0], scnr0[jnr0], thick=thick+1, color=!orange, linestyle=1
       if nknr0 gt 0 then begin 
          oplot, sma0[knr0], scnr0[knr0], symsize=symsize, color=!orange, psym=gsym(2)
          oplot, sma0[knr0], scnr0[knr0], thick=thick+1, color=!orange
          oploterror_old, sma0[knr0], scnr0[knr0], 0.0*sma0[knr0], escnr0[knr0], $
                   /nohat, errthick=thick+1, psym=3, errcolor=!orange, color=!orange
       endif 
       if nlnr0 gt 0 then oplot, sma0[lnr0], scnr0[lnr0], color=!orange, psym=gsym(19), symsize=symsize
       if nmnr0 gt 0 then oplot, sma0[mnr0], scnr0[mnr0], color=!orange, psym=gsym(18), symsize=symsize
    ENDIF 
    ;
    IF njfn GT 0 THEN BEGIN 
       oplot, sma[jfn], scfn[jfn], thick=thick, color=!green, linestyle=1
       if nkfn gt 0 then oplot, sma[kfn], scfn[kfn], thick=thick, color=!green, linestyle=0
       if njfn0 gt 0 then oplot, sma0[jfn0], scfn0[jfn0], thick=thick+1, color=!green, linestyle=1
       if nkfn0 gt 0 then begin 
          oplot, sma0[kfn0], scfn0[kfn0], symsize=symsize, color=!green, psym=gsym(2)
          oplot, sma0[kfn0], scfn0[kfn0], thick=thick+1, color=!green
          oploterror_old, sma0[kfn0], scfn0[kfn0], 0.0*sma0[kfn0], escfn0[kfn0], $
                   /nohat, errthick=thick+1, psym=3, errcolor=!green, color=!green
       endif 
       if nlfn0 gt 0 then oplot, sma0[lfn0], scfn0[lfn0], color=!green, psym=gsym(19), symsize=symsize
       if nmfn0 gt 0 then oplot, sma0[mfn0], scfn0[mfn0], color=!green, psym=gsym(18), symsize=symsize
    ENDIF 
    ;
    ; ------------------------------------------------------------------
    ; panel 3
    ; Halpha/FUV Halpha/R profiles
    ;keywait,'type anything for next plot'
    plog,ll,prog,'plotting flux ratio profiles'
    !p.noerase = 0
    !p.multi = [1,1,3]
    plot, sma[0:1], slewf[0:1], xrange=rrange, yrange=hfrange, xstyle=1, ystyle=1, $
          charsize=charsize, symsize=symsize, thick=thick, xthick=thick, ythick=thick, $
          xtitle=rtitle, ytitle=hftitle, charthick=thick, xmargin=[8,8], ymargin=[3,3], /nodata
    IF njhf GT 0 THEN BEGIN 
       oplot, sma[jhf], slewf[jhf], thick=thick, color=!purple, linestyle=1
       if nkhf gt 0 then oplot, sma[khf], slewf[khf], thick=thick, color=!purple, linestyle=0
       if njhf0 gt 0 then oplot, sma0[jhf0], slewf0[jhf0], thick=thick+1, color=!purple, linestyle=1
       if nkhf0 gt 0 then begin 
          oplot, sma0[khf0], slewf0[khf0], symsize=symsize, color=!purple, psym=gsym(2)
          oplot, sma0[khf0], slewf0[khf0], thick=thick+1, color=!purple
          oploterror_old, sma0[khf0], slewf0[khf0], 0.0*sma0[khf0], eslewf0[khf0], $
                   /nohat, errthick=thick+1, psym=3, errcolor=!purple, color=!purple
       endif 
       if nlhf0 gt 0 then oplot, sma0[lhf0], slewf0[lhf0], color=!purple, psym=gsym(19), symsize=symsize
       if nmhf0 gt 0 then oplot, sma0[mhf0], slewf0[mhf0], color=!purple, psym=gsym(18), symsize=symsize
    ENDIF 
    ;
    IF njhr GT 0 THEN BEGIN 
       oplot, sma[jhr], slewr[jhr], thick=thick, color=!red, linestyle=1
       if nkhr gt 0 then oplot, sma[khr], slewr[khr], thick=thick, color=!red, linestyle=0
       if njhr0 gt 0 then oplot, sma0[jhr0], slewr0[jhr0], thick=thick+1, color=!red, linestyle=1
       if nkhr0 gt 0 then begin 
          oplot, sma0[khr0], slewr0[khr0], symsize=symsize, color=!red, psym=gsym(2)
          oplot, sma0[khr0], slewr0[khr0], thick=thick+1, color=!red
          oploterror_old, sma0[khr0], slewr0[khr0], 0.0*sma0[khr0], eslewr0[khr0], $
                   /nohat, errthick=thick+1, psym=3, errcolor=!red, color=!red
       endif 
       if nlhr0 gt 0 then oplot, sma0[lhr0], slewr0[lhr0], color=!red, psym=gsym(19), symsize=symsize
       if nmhr0 gt 0 then oplot, sma0[mhr0], slewr0[mhr0], color=!red, psym=gsym(18), symsize=symsize
    ENDIF 
    ;
    ; ------------------------------------------------------------------
    ; finish plot
    plog,ll,prog,'finishing. Will write plotfile: '+feps_1
    !p.multi   = 0
    !p.noerase = 0
    ssoup_plot_finish,fjpg_1,feps_1,wxsize,epilepsy=epilepsy
  endfor
END 
