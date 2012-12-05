PRO plot_hdfn_linesens, hardfile=hardfile
   ;
   ; plot grism sensitivity curves
   wd          = '/home/meurer/ACS/Grism'
   fsens1      = 'ACS.WFC.1st.sens.4.fits'
   fsens2      = 'ACS.WFC.2nd.sens.4.fits'
   fsensi      = 'acs_wfc1_f775w.fits'
   fsensz      = 'acs_wfc1_f850lp.fits'
   xrange      = [5000.0,11000.0]
   yrange      = [0.0, 8.0e15]
   hplanck     = 6.62608e-27
   c           = 2.9979e18
   ahst        = 45238.93416
   ;
   noise_g     = 28.6
   noise_i     = 14.9
   noise_z     = 18.0
   texp_g      = 6870.0
   texp_i      = 4500.0
   texp_z      = 6800.0
   lam_i       = [6800.0, 8700.0]
   lam_z       = [8200.0, 10800.0]
   xfid        = 2048.0
   yfid        = 1024.0
   xra         = [-30, 160]
   xrc         = [120, 410]
   xtickformat = '(i5)'
   xtitle      = '!3 Wavelength ['+angstsym()+']'
   ytitle1     = '!3 throughput'
   title1      = '!3 system throughput curves'
   title2      = '!3 filter / grism throughput ratio'
   ytitle2     = '!3 broad / grism'
   ;
   ; go to working directory
   oldwd       = ''
   cd, wd, current=oldwd
   ;
   ; set the plot parameters
   wxsize   = 650
   wysize   = fix(0.8*float(wxsize))
   window, 0, xsize=wxsize, ysize=wysize
   !p.multi = [0, 1, 2]
   IF keyword_set(hardfile) THEN BEGIN 
      psp, /eps
      ; psp
   ENDIF 
   setplotcolors
   setbgfg,!white,!black
   erase
   ;
   ; first order (a) wavelength solution, chip1 (c1) & chip2 (c2)
   c1_dldp_a_0 = [4771.61, 0.0370233, -0.000189538, -0.0000103637, -1.39952e-6, 3.20763e-6]
   c1_dldp_a_1 = [36.4556, 0.00133904, -0.00113125, 3.81861e-8, -8.88458e-8, 1.02802e-7]
   c1_dldp_a_2 = [0.0101197, 9.65156e-7, -4.33325e-7, -2.0178e-10, 6.50773e-11, -5.05835e-10]
   c2_dldp_a_0 = [4786.13, 0.0399595, -0.0136488, -0.0000103637, -1.39952e-6, 3.20763e-6]
   c2_dldp_a_1 = [39.2815, 0.00152543, -0.00156261, 3.81861e-8, -8.88458e-8, 1.02802e-7]
   c2_dldp_a_2 = [0.00880236, 8.28624e-7, 1.68916e-6, -2.0178e-10, 6.50773e-11, -5.05835e-10]
   ;
   ; second order (c) wavelength solution, chip1 (c1) & chip2 (c2)
   c1_dldp_c_0 = [2390.73, 0.0261875, 0.000358604, -5.367e-6, -2.02344e-6, 1.96539e-6]
   c1_dldp_c_1 = [19.245, 0.00068357, -0.000583359, 1.18441e-8, -3.83364e-8, 1.96216e-8]
   c1_dldp_c_2 = [0.0]
   c2_dldp_c_0 = [2398.63, 0.0304327, -0.00788816, -5.367e-6, -2.02344e-6, 1.96539e-6]
   c2_dldp_c_1 = [20.5553, 0.000764, -0.000665692, 1.18441e-8, -3.83364e-8, 1.96216e-8]
   c2_dldp_c_2 = [0.0]
   ;
   s1          = mrdfits(fsens1,1,hsens1)
   lams1       = s1.wavelength
   sens1       = s1.sensitivity
   s2          = mrdfits(fsens2,1,hsens2)
   lams2       = s2.wavelength
   sens2       = s2.sensitivity
   si          = mrdfits(fsensi,1,hsensi)
   lamsi       = si.wavelength
   thrui       = si.throughput
   sz          = mrdfits(fsensz,1,hsensz)
   lamsz       = sz.wavelength
   thruz       = sz.throughput
   ;
   ; sensitivity curve (no longer plotted)
   ;plot, lams1, sens1, xstyle=1, ystyle=1, xrange=xrange, yrange=yrange, $
   ; xtitle=xtitle, ytitle='sensitivity', xtickformat=xtickformat
   ;oplot, lams1, sens1, color=!green
   ;oplot, lams2, sens2, color=!blue
   ;
   xa          = xra[0] + findgen(fix(xra[1] - xra[0])+1)
   xc          = xrc[0] + findgen(fix(xrc[1] - xrc[0])+1)
   c1_dldp_a0  = eval_axe_poly(c1_dldp_a_0, xfid, yfid)
   c1_dldp_a1  = eval_axe_poly(c1_dldp_a_1, xfid, yfid)
   c1_dldp_a2  = eval_axe_poly(c1_dldp_a_2, xfid, yfid)
   c2_dldp_a0  = eval_axe_poly(c2_dldp_a_0, xfid, yfid)
   c2_dldp_a1  = eval_axe_poly(c2_dldp_a_1, xfid, yfid)
   c2_dldp_a2  = eval_axe_poly(c2_dldp_a_2, xfid, yfid)
   c1_dldp_c0  = eval_axe_poly(c1_dldp_c_0, xfid, yfid)
   c1_dldp_c1  = eval_axe_poly(c1_dldp_c_1, xfid, yfid)
   c1_dldp_c2  = eval_axe_poly(c1_dldp_c_2, xfid, yfid)
   c2_dldp_c0  = eval_axe_poly(c2_dldp_c_0, xfid, yfid)
   c2_dldp_c1  = eval_axe_poly(c2_dldp_c_1, xfid, yfid)
   c2_dldp_c2  = eval_axe_poly(c2_dldp_c_2, xfid, yfid)
   print, c1_dldp_a0, c1_dldp_a1, c1_dldp_a2
   print, c2_dldp_a0, c2_dldp_a1, c2_dldp_a2
   print, c1_dldp_c0, c1_dldp_c1, c1_dldp_c2
   print, c2_dldp_c0, c2_dldp_c1, c2_dldp_c2
   ;
   c1_disp_a   = c1_dldp_a1 + xa*c1_dldp_a2
   c2_disp_a   = c2_dldp_a1 + xa*c2_dldp_a2
   c1_disp_c   = c1_dldp_c1 + xc*c1_dldp_c2
   c2_disp_c   = c2_dldp_c1 + xc*c2_dldp_c2
   c1_lam_a    = c1_dldp_a0 + xa*c1_disp_a
   c2_lam_a    = c2_dldp_a0 + xa*c2_disp_a
   c1_lam_c    = c1_dldp_c0 + xc*c1_disp_c
   c2_lam_c    = c2_dldp_c0 + xc*c2_disp_c
   ;
   good        = where(c1_lam_a LE max(lams1) AND c1_lam_a GE min(lams1), ng)
   IF ng GT 0 THEN BEGIN 
      c1_disp_a  = c1_disp_a[good]
      c1_lam_a   = c1_lam_a[good]
   ENDIF 
   good        = where(c2_lam_a LE max(lams1) AND c2_lam_a GE min(lams1), ng)
   IF ng GT 0 THEN BEGIN 
      c2_disp_a  = c2_disp_a[good]
      c2_lam_a   = c2_lam_a[good]
   ENDIF 
   good        = where(c1_lam_c LE max(lams2) AND c1_lam_c GE min(lams2), ng)
   IF ng GT 0 THEN BEGIN 
      c1_disp_c  = c1_disp_c[good]
      c1_lam_c   = c1_lam_c[good]
   ENDIF 
   good        = where(c2_lam_c LE max(lams2) AND c2_lam_c GE min(lams2), ng)
   IF ng GT 0 THEN BEGIN 
      c2_disp_c  = c2_disp_c[good]
      c2_lam_c   = c2_lam_c[good]
   ENDIF 
   ;
   c1_thru_a   = spline(lams1, sens1,  c1_lam_a)*hplanck*c/(c1_lam_a*ahst)
   c2_thru_a   = spline(lams1, sens1,  c2_lam_a)*hplanck*c/(c2_lam_a*ahst)
   c1_thru_c   = spline(lams2, sens2,  c1_lam_c)*hplanck*c/(c1_lam_c*ahst)
   c2_thru_c   = spline(lams2, sens2,  c2_lam_c)*hplanck*c/(c2_lam_c*ahst)
   ;
   ;  First plot - throughput curves
   plot, c1_lam_a, c1_thru_a, xrange=xrange, yrange=[0.0, 0.5], xstyle=1, ystyle=1, $
    xtitle=xtitle, ytitle=ytitle1, title=title1, xtickformat=xtickformat, thick=2.0, charsize=1.5
   oplot, c1_lam_a, c1_thru_a, color=!green, thick=2.0
   ;oplot, c1_lam_c, c1_thru_c, color=!blue, thick=2.0
   oplot, lamsi, thrui, color=!red, thick=2.0
   oplot, lamsz, thruz, color=!black, thick=2.0
   ;
   ; look at ratio of direct to grism throughput
   c1_thru_ai  = spline(lamsi, thrui, c1_lam_a)
   c1_thru_az  = spline(lamsz, thruz, c1_lam_a)
   c1_thru_ci  = spline(lamsi, thrui, c1_lam_c)
   c1_thru_cz  = spline(lamsz, thruz, c1_lam_c)
   thrurat_iga = c1_thru_ai / c1_thru_a
   thrurat_zga = c1_thru_az / c1_thru_a
   thrurat_igc = c1_thru_ci / c1_thru_c
   thrurat_zgc = c1_thru_cz / c1_thru_c
   ;
   ; determine throuput ratio at which direct and grism images are 
   ; equally sensitive to pure line emission.
   fidrat_i    = texp_g*noise_i/(texp_i*noise_g)
   fidrat_z    = texp_g*noise_z/(texp_z*noise_g)
   yi          = fidrat_i*[1.0, 1.0]
   yz          = fidrat_z*[1.0, 1.0]
   ;
   ; second plot ratio curves
   plot, c1_lam_a, thrurat_iga, xrange=xrange, yrange=[0.0,1.5], xstyle=1, ystyle=1, $
    xtitle=xtitle, ytitle=ytitle2, title=title2, xtickformat=xtickformat, thick=2.0, charsize=1.5
   oplot, c1_lam_a, thrurat_iga, color=!red, thick=2.0
   oplot, c1_lam_a, thrurat_zga, color=!black, thick=2.0
   oplot, lam_i, yi, color=!red, linestyle=2, thick=2.0
   oplot, lam_z, yz, color=!black, linestyle=2, thick=2.0
   ;
   ; finish
   !p.multi   = 0
   !p.noerase = 0
   IF keyword_set(hardfile) THEN BEGIN 
      psend, hardfile, /noprint, /clobber
   ENDIF 
END 
