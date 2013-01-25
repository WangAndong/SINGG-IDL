pro ssoup_plotsprofs_mir, ll, sname, fjpg, feps, epilepsy=epilepsy
  ;
  ; Plots mid-infrared radial profiles. I don't know what colors would be
  ; interesting here, so this is a separate file.
  ; 
  ;   ll         -> logical unit number for plot
  ;   sname      -> Source name
  ;   fjpg       -> output plot file name (JPG)
  ;   feps       -> optput plot file name (EPS)
  ;   epilepsy   -> whether we should display images on the screen
  ;   
  ; S. Andrews (ICRAR/UWA) 1/2013
  
  COMMON bands, band
  prog      = 'SSOUP_PLOTSPROFS_MIR: '
  plog,ll,prog,'--------------------- starting '+prog+'---------------------------------'
  
  ; flags
  mflag   =  99.999  ; magnitude flag value
  emflag  =   9.999  ; error mag flag
  clflag  =   7.777  ; colour lower limit flag
  
  ; plot parameters
  charsize  = 0.4
  symsize   = 0.2
  thick     = 1
  abtitle   = '!4 l!3 [ABmag arcssec!u-2!n]'
  rtitle    = '!3 semi-major axis [arcsec]' 
  
  ; read in profile files
  plog,ll,prog,'reading in surface brightness profile saveset'
  restore,sname+"_profiles.save"
  for i=0,n_elements(allprofiles)-1 do begin
    ; reform filenames based on galaxy number
    fjpg_1 = string(i, format='(%"' + fjpg + '")') 
    feps_1 = string(i, format='(%"' + feps + '")')

    w1 = (where(bname eq band.mir_W1, /null))[0]
    w2 = (where(bname eq band.mir_W2, /null))[0]
    w3 = (where(bname eq band.mir_W3, /null))[0]
    w4 = (where(bname eq band.mir_W4, /null))[0]
    ; rename a few things
    radius = *(allprofiles[i].radius)
    sw1    = *(allprofiles[i].mprof[w1])
    sw2    = *(allprofiles[i].mprof[w2])
    sw3    = *(allprofiles[i].mprof[w3])
    sw4    = *(allprofiles[i].mprof[w4])
    sw10   = *(allprofiles[i].mprof_dustcor[w1])
    sw20   = *(allprofiles[i].mprof_dustcor[w2])
    sw30   = *(allprofiles[i].mprof_dustcor[w3])
    sw40   = *(allprofiles[i].mprof_dustcor[w4])
        
    ; find out which indices are plottable
    ssoup_psp_indices, ll, band.mir_W1, radius, sw1, $
       *(allprofiles[i].err_mprof[w1]), mflag, emflag, clflag, emflag, rmax_w1, irmax_w1, ij_w1, $
       ik_w1, il_w1, im_w1, nij_w1, nik_w1, nil_w1, nim_w1
    ssoup_psp_indices, ll, band.mir_W2, radius, sw2, $
       *(allprofiles[i].err_mprof[w2]), mflag, emflag, clflag, emflag, rmax_w2, irmax_w2, ij_w2, $
       ik_w2, il_w2, im_w2, nij_w2, nik_w2, nil_w2, nim_w2
    ssoup_psp_indices, ll, band.mir_W3, radius, sw3, $
       *(allprofiles[i].err_mprof[w3]), mflag, emflag, clflag, emflag, rmax_w3, irmax_w3, ij_w3, $
       ik_w3, il_w3, im_w3, nij_w3, nik_w3, nil_w3, nim_w3
    ssoup_psp_indices, ll, band.mir_W4, radius, sw4, $
       *(allprofiles[i].err_mprof[w4]), mflag, emflag, clflag, emflag, rmax_w4, irmax_w4, ij_w4, $
       ik_w4, il_w4, im_w4, nij_w4, nik_w4, nil_w4, nim_w4
    rmax      = 1.05*max([rmax_w1, rmax_w2, rmax_w3, rmax_w4])
    
    ; start plot
    rrange    = [0.0,rmax]
    abrange   = [32.0,14.0]    
    xs       = 2.5
    ys       = 1.25*xs
    yoff     = 0.4
    xoff     = 0.4
    wxsize   = 600
    ansize   = 1.0
    ssoup_plot_init,feps_1,xs,ys,xoff,yoff
    plog,ll,prog,'plotting mir surface brightness profiles'
    !p.noerase = 1
    plot, radius[0:1], sw1[0:1], xrange=rrange, yrange=abrange, xstyle=1, ystyle=1, $
          charsize=charsize, symsize=symsize, thick=thick, xthick=thick, ythick=thick, $
          xtitle=rtitle, ytitle=abtitle, title=sname, charthick=thick, $
          xmargin=[8,8], ymargin=[2,4], /nodata
    ssoup_overlay_prof, radius, sw1, sw1, *(allprofiles[i].err_mprof[w1]), symsize, thick, !dorange, ij_w1, ik_w1, il_w1, im_w1, $
      nij_w1, nik_w1, nil_w1, nim_w1, ij_w1, ik_w1, il_w1, im_w1, nij_w1, nik_w1, nil_w1, nim_w1
    ssoup_overlay_prof, radius, sw2, sw2, *(allprofiles[i].err_mprof[w2]), symsize, thick, !cyan, ij_w2, ik_w2, il_w2, im_w2, $
      nij_w2, nik_w2, nil_w2, nim_w2, ij_w2, ik_w2, il_w2, im_w2, nij_w2, nik_w2, nil_w2, nim_w2
    ssoup_overlay_prof, radius, sw3, sw3, *(allprofiles[i].err_mprof[w3]), symsize, thick, !blue, ij_w3, ik_w3, il_w3, im_w3, $
      nij_w3, nik_w3, nil_w3, nim_w3, ij_w3, ik_w3, il_w3, im_w3, nij_w3, nik_w3, nil_w3, nim_w3
    ssoup_overlay_prof, radius, sw4, sw4, *(allprofiles[i].err_mprof[w4]), symsize, thick, !dpink, ij_w4, ik_w4, il_w4, im_w4, $
      nij_w4, nik_w4, nil_w4, nim_w4, ij_w4, ik_w4, il_w4, im_w4, nij_w4, nik_w4, nil_w4, nim_w4
    ;
    ;
    ; ------------------------------------------------------------------
    ; finish plot
    plog,ll,prog,'finishing. Will write plotfile: '+feps_1
    !p.multi   = 0
    !p.noerase = 0
    ssoup_plot_finish,fjpg_1,feps_1,wxsize,epilepsy=epilepsy 
 endfor
end