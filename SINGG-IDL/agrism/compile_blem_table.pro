PRO compile_blem_table, nospec=nospec
   ;
   ; Compile table on BLEM (grism image) selected emission line galaxies
   ;
   ; nospec -> if set then spectroscopic results are ignored.
   ;
   ; G. Meurer 08/04
   ; G. Meurer 09/04 add in Capak results, include nospec option
   ;
   ; setup
   setplotcolors
   wd         = '/home/meurer/text/ACS/Grism/Figures/'
   intab      = 'hdfn_blem_emsource_merge.dat'
   hdb        = 'hdfn_idt'
   fmti       = '(i,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,i,i,i,f)'
   IF keyword_set(nospec) THEN filo = 'hdfn_blem_emsource_nospec.cat' ELSE filo = 'hdfn_blem_emsource.cat'
   fmto       = '(a19,1x,a12,1x,a12,1x,i2,1x,i4,1x,f6.1,1x,e9.2,1x,f7.1,1x,'+$
                'f6.2,1x,f6.2,1x,f6.2,1x,f6.2,1x,f6.3,1x,f6.3,1x,f6.3,1x,f6.3,1x,f6.3,1x,f6.3,1x,f5.2,1x,'+$
                'f6.3,1x,f6.3,1x,f6.3,1x,f5.2,1x,f6.3,1x,f6.3,1x,f6.3,1x,f5.2,1x,f6.3,1x,a7,1x,f7.2,1x,i2,1x,i2,1x,a15)'
   hdro       = '# IAU_name             RA            Dec     nlin nknt lam     flux        ew   '+$
                'bmag   vmag   imag   zmag   zspec   zcap  zfly  zbpz1 zbpz1a zbpz1b obpz1  zbpz2 zbpz2a zbpz2b obpz2  '+$
                'zbpz3 zbpz3a zbpz3b obpz3 zgrism line-id   lam0  mat st     flags'
   fnorm      = 1.0e-18
   ;
   filtnam0   = ['F435W', 'F606W', 'F775W', 'F850LP']
   filtnam    = ['b', 'v', 'i', 'z']
   filtplam   = [4317.398, 5917.678, 7693.026, 9054.768]
   wl0        = [6562.817, 5006.85, 4861.33, 3727., 1216.]
   nam0       = ['H-alpha', '[OIII]', 'H-beta', '[OII]', 'Ly-alpha']
   threshodds = 0.05
   sourcelist = ['SPEC', 'CAPAK', 'FLY99', 'BPZ']                              ; source of first guess z
   scolor     = [!red, !magenta, !blue, !cyan]
   ssym       = [3, 4, 1, 5]
   ypl        = [2.0, 3.0, 4.0, 5.0]
   yrange     = [0.0, 6.0]
   maxpeak    = 3                                                     ; max #guesses per source
   oddsdef    = 1.0                                                   ; default odds
   ezfly      = 0.1                                                   ; default dz/(1+z) for FLY99
   ezcapak    = 0.08                                                   ; default dz/(1+z) for Capak
   ezspec     = 0.08                                                  ; default dz/(1+z) for spectroscopy
   nsrc       = n_elements(sourcelist)
   ;
   ; plot stuff
   xs = 6.5
   ys = 0.75*xs
   yoff=3.
   xoff=1.2
   wxsize   = 650
   wysize   = fix(0.75*float(wxsize))
   ;
   cd, wd, current=cwd
   ;
   ; read input catalog
   readcol, intab, idgre, xgre, ygre, ae, be, fwhme, thetae, peake, cw50e, cpeake, cshifte, xime, yime, $
                   cene, w50e, fluxe, ide, line, nknt, siglam, format=fmti
   nem        = n_elements(ide)
   fluxe      = fnorm*fluxe
   ;
   ; Make arrays for storing things:
   iau        = make_array(nem,/string,value='J****')
   rastr      = make_array(nem,/string,value='00:00:00.000')
   decstr     = make_array(nem,/string,value='+99:99:99.99')
   linid      = make_array(nem,/string,value='UNKNOWN')
   ew         = make_array(nem,/float,value=-99.99)
   bmag       = make_array(nem,/float,value=-99.99)
   vmag       = make_array(nem,/float,value=-99.99)
   imag       = make_array(nem,/float,value=-99.99)
   zmag       = make_array(nem,/float,value=-99.99)
   zgrism     = make_array(nem,/float,value=-1.00)
   zbpzml     = make_array(nem,/float,value=-1.00)
   zbpz1      = make_array(nem,/float,value=-1.00)
   zbpz1a     = make_array(nem,/float,value=-1.00)
   zbpz1b     = make_array(nem,/float,value=-1.00)
   obpz1      = make_array(nem,/float,value=-1.00)
   zbpz2      = make_array(nem,/float,value=-1.00)
   zbpz2a     = make_array(nem,/float,value=-1.00)
   zbpz2b     = make_array(nem,/float,value=-1.00)
   obpz2      = make_array(nem,/float,value=-1.00)
   zbpz3      = make_array(nem,/float,value=-1.00)
   zbpz3a     = make_array(nem,/float,value=-1.00)
   zbpz3b     = make_array(nem,/float,value=-1.00)
   obpz3      = make_array(nem,/float,value=-1.00)
   zspec      = make_array(nem,/float,value=-1.00)
   zspec_src  = make_array(nem,/byte,value=-1)
   zfly       = make_array(nem,/float,value=-1.00)
   tfly       = make_array(nem,/int,value=-1)
   zcapak     = make_array(nem,/float,value=-1.00)
   zmincap    = make_array(nem,/float,value=-1.00)
   zmaxcap    = make_array(nem,/float,value=-1.00)
   tcapak     = make_array(nem,/int,value=-1)
   oddscapak  = make_array(nem,/float,value=-1.0)
   nmatch     = make_array(nem,/int,value=0)
   status     = make_array(nem,/int,value=-1)
   lam0       = make_array(nem,/float,value=-1.0)
   matchstr   = make_array(nem,/string,value='')
   ;
   ; make arrays for storing guesses
   mg         = n_elements(sourcelist)*maxpeak
   zguess     = make_array(mg, /float, value=-1.0)
   zguessa    = make_array(mg, /float, value=-1.0)
   zguessb    = make_array(mg, /float, value=-1.0)
   oguess     = make_array(mg, /float, value=-1.0)
   source     = make_array(mg, /int, value='')
   ;
   ; match id number to database
   dbopen, hdb
   list       = dbmatch('id',ide)
   ;
   ; extract relevant quantities
   dbext, list, 'RA,DEC,I_MAG,Z_MAG,IAU_G,ID_G,RA_G,DEC_G,Z_MAG_G,I_MAG_G,V_MAG_G,B_MAG_G', $
                 ra,dec,i_mag,z_mag,iau_g,id_g,ra_g,dec_g,z_mag_g,i_mag_g,v_mag_g,b_mag_g
   dbext, list, 'Z_B_1,Z_B_MIN_1,Z_B_MAX_1,T_B_1,ODDS_1',z_b_1,z_b_min_1,z_b_max_1,t_b_1,odds_1
   dbext, list, 'Z_B_2,Z_B_MIN_2,Z_B_MAX_2,T_B_2,ODDS_2',z_b_2,z_b_min_2,z_b_max_2,t_b_2,odds_2
   dbext, list, 'Z_B_3,Z_B_MIN_3,Z_B_MAX_3,T_B_3,ODDS_3,Z_ML,T_ML',z_b_3,z_b_min_3,z_b_max_3,t_b_3,odds_3,z_ml,t_ml
   dbext, list, 'Z_SPEC_H,FLAG_Z_SPEC_H,SOURCE_Z_SPEC_H',z_spec_h,flag_z_spec_h,source_z_spec_h
   dbext, list, 'z_phot_f,t_f',z_phot_f, t_f
   dbext, list, 'z_b_c,z_b_min_c,z_b_max_c,t_b_c,odds_c',z_b_c,z_b_min_c,z_b_max_c,t_b_c,odds_c
   dbclose
   dmagi      = abs(i_mag - i_mag_g)
   dmagz      = abs(z_mag - z_mag_g)
   k1         = where(id_g GE 0 AND (dmagi LE 2.0 OR dmagz LE 2.0), ngoods)
   print, 'Number of sources in GOODS catalog       : ', ngoods
   k2         = where(id_g LT 0 OR (dmagi GT 2.0 AND dmagz GT 2.0), nbad)
   print, 'Number of sources not in GOODS catalog   : ', nbad
   k3         = where(z_spec_h GE 0.0 AND flag_z_spec_h EQ 0, nspec)
   print, 'Number of sources with spec redshift     : ', nspec
   k4         = where(z_ml GE 0.0, nzml)
   k5         = where(z_b_1 GE 0.0, nzb1)
   k6         = where(z_b_2 GE 0.0, nzb2)
   k7         = where(z_b_3 GE 0.0, nzb3)
   print, 'Number of sources with BPZ (ML, 1, 2, 3) : ', nzml, nzb1, nzb2, nzb3
   k8         = where(z_phot_f GE 0.0, nfly)
   print, 'Number of sources with FLY99 photo-z     : ', nfly
   k9         = where(z_b_c GE 0.0, ncapak)
   print, 'Number of sources with Capak photo-z     : ', ncapak
   ;
   ; store GOODS things
   IF ngoods GT 0 THEN BEGIN 
      iau[k1]       = iau_g[k1]
      rastr[k1]     = degsexi(ra_g[k1],prec=3,/ra)
      decstr[k1]    = degsexi(dec_g[k1],prec=2)
      bmag[k1]      = b_mag_g[k1]
      vmag[k1]      = v_mag_g[k1]
      imag[k1]      = i_mag_g[k1]
      zmag[k1]      = z_mag_g[k1]
      zbpzml[k1]    = z_ml[k1]
      zbpz1[k1]     = z_b_1[k1]
      zbpz1a[k1]    = z_b_min_1[k1]
      zbpz1b[k1]    = z_b_max_1[k1]
      obpz1[k1]     = odds_1[k1]
      zbpz2[k1]     = z_b_2[k1]
      zbpz2a[k1]    = z_b_min_2[k1]
      zbpz2b[k1]    = z_b_max_2[k1]
      obpz2[k1]     = odds_2[k1]
      zbpz3[k1]     = z_b_3[k1]
      zbpz3a[k1]    = z_b_min_3[k1]
      zbpz3b[k1]    = z_b_max_3[k1]
      obpz3[k1]     = odds_3[k1]
   ENDIF 
   ;
   ; Store basics for items not in GOODS
   IF nbad GT 0 THEN BEGIN 
      rastr[k2]     = degsexi(ra[k2],prec=3,/ra)
      decstr[k2]    = degsexi(dec[k2],prec=2)
      iau[k2]       = 'j'+strmid(rastr[k2],0,2)+strmid(rastr[k2],3,2)+strmid(rastr[k2],6,5)+$
                      strmid(decstr[k2],0,3)+strmid(decstr[k2],4,2)+strmid(decstr[k2],7,4)
      imag[k2]      = i_mag[k2]
      zmag[k2]      = z_mag[k2]
   ENDIF 
   ;
   ; Store spectroscopic redshift results
   IF nspec GT 0 AND NOT keyword_set(nospec) THEN BEGIN 
      zspec[k3]     = z_spec_h[k3]
      zspec_src[k3] = source_z_spec_h[k3]
      print,flag_z_spec_h[k3]
   ENDIF 
   ;
   ; store FLY99 results
   IF nfly GT 0 THEN BEGIN 
      zfly[k8]      = z_phot_f[k8]
      tfly[k8]      = t_f[k8]
   ENDIF 
   ;
   ; store Capak results
   IF ncapak GT 0 THEN BEGIN 
      zcapak[k9]      = z_b_c[k9]
      zmincap[k9]     = z_b_min_c[k9]
      zmaxcap[k9]     = z_b_max_c[k9]
      tcapak[k9]      = t_b_c[k9]
      oddscapak[k9]   = odds_c[k9]
   ENDIF 
   ;
   ; calculate equivalent width
   dmagst = 5.0*alog10(filtplam/5470.0)
   FOR i = 0, nem-1 DO BEGIN 
      test   = abs(cene[i]/filtplam - 1.0)
      magarr = [bmag[i], vmag[i], imag[i], zmag[i]] 
      k      = where(magarr LT 0.0, nk)
      IF nk GT 0 THEN test[k] = 100.0
      magarr = magarr + dmagst                  ; convert to stmag
      k      = sort(test)
      IF test[k[0]] LT 2.0 THEN BEGIN 
         flam   = 10.0^(-0.4*(magarr[k[0]] + 21.10))
         ew[i]  = fluxe[i]/flam
      ENDIF 
   ENDFOR 
   ;
   ; find best grism redshift and plot
   FOR i = 0, nem-1 DO BEGIN 
      ztest   = (cene[i]/wl0 - 1.0)
      ytest   = 1.0 + 0.0*ztest
      zbpz    = [zbpz1[i], zbpz2[i], zbpz3[i]]
      obpz    = [obpz1[i], obpz2[i], obpz3[i]]
      zbpza   = [zbpz1a[i], zbpz2a[i], zbpz3a[i]]
      zbpzb   = [zbpz1b[i], zbpz2b[i], zbpz3b[i]]
      source  = -1 + 0*source
      ;
      ; fill out first guess redshifts and related quantities
      ng      = 0
      IF zspec[i] GE 0.0 THEN BEGIN 
         dz          = ezspec*(1.0 + zspec[i])
         source[ng]  = 0
         zguess[ng]  = zspec[i]
         zguessa[ng] = max([zspec[i] - dz,0.0])
         zguessb[ng] = zspec[i] + dz
         oguess[ng]  = oddsdef
         ng          = ng + 1
      ENDIF 
      IF zcapak[i] GE 0.0 THEN BEGIN 
         ;
         ; Capak guess
         source[ng]  = 1
         zguess[ng]  = zcapak[i]
;        dz          = ezcapak*(1.0 + zcapak[i])
;        zguessa[ng] = max([zcapak[i] - dz,0.0])
;        zguessb[ng] = zcapak[i] + dz
         zguessa[ng] = max([zmincap[i],0.0])
         zguessb[ng] = max([zmaxcap[i],0.0])
         oguess[ng]  = oddscapak[i]
         ng          = ng + 1
      ENDIF 
      IF zfly[i] GE 0.0 THEN BEGIN 
         ;
         ; fly99 guess
         dz          = ezfly*(1.0 + zfly[i])
         source[ng]  = 2
         zguess[ng]  = zfly[i]
         zguessa[ng] = max([zfly[i] - dz,0.0])
         zguessb[ng] = zfly[i] + dz
         oguess[ng]  = oddsdef
         ng          = ng + 1
      ENDIF 
      k       = where(zbpz GE 0.0 AND obpz GE threshodds, nk)
      IF nk GT 0 THEN BEGIN 
         FOR j = 0, nk-1 DO BEGIN 
            ;
            ; BPZ guesses
            source[ng]  = 3
            zguess[ng]  = zbpz[j[k]]
            zguessa[ng] = zbpza[j[k]]
            zguessb[ng] = zbpzb[j[k]]
            oguess[ng]  = obpz[j[k]]
            ng          = ng + 1
         ENDFOR 
      ENDIF 
      ;
      ; First plot the redshifts to test
      keywait, 'Type something for plot: '
      ;psp, /eps, xsize=xs, ysize=ys
      ;
      ; do the plot twice, once hard and once on screen 
      FOR qq = 0, 1 DO BEGIN 
         IF qq EQ 0 THEN BEGIN 
            set_plot,'ps',/copy, /interpolate
            device,/portrait,/inches,xs=xs,ys=ys,yo=yoff,xo=xoff,/color,/encapsulated
            hardfile = strtrim(iau[i],2)+'_'+strtrim(string(line[i]),2)+'_zguess_blem.ps'
         ENDIF ELSE BEGIN 
            window, 0, xsize=wxsize, ysize=wysize
         ENDELSE 
         setplotcolors 
         plot, ztest, ytest, psym=sym(4), xrange=[0.0, 5.0], yrange=yrange, xstyle=1, ystyle=1, $
          xtitle='Redshift', ytitle=' ', title=strtrim(string(ide[i]),2)+' : '+iau[i]
         oplot, ztest, ytest, psym=sym(4), color=!dgreen
         ;
         ; loop through guesses and plot
         FOR j = 0, ng-1 DO BEGIN 
            x  = [zguess[j]]
            y  = [ypl[source[j]]]
            s  = oguess[j]/0.4
            oplot, x, y, psym=sym(ssym[source[j]]), color=scolor[source[j]], symsize=s
            x  = [zguessa[j], zguessb[j]]
            y  = ypl[source[j]]*[1.0, 1.0]
            IF qq EQ 0 THEN oplot, x, y, linestyle=0 ELSE oplot, x, y, linestyle=0, color=scolor[source[j]]
         ENDFOR 
         IF qq EQ 0 THEN psend, hardfile, /noprint, /clobber
      ENDFOR 
      lamline   = cene[i]
      elam      = 0.5*w50e[i]
      zmultiguess, lamline, elam, zguess, zguessa, zguessb, oguess, threshodds, source, nsrc, $
                   zb, srcb, l0, lid, st, nm, zbsrc, matchsrc, peakidsrc
      ;
      zgrism[i] = zb
      linid[i]  = lid
      lam0[i]   = l0
      nmatch[i] = nm
      status[i] = st
      ;
      ; make a string showing the matches
      FOR j = 0, nsrc-1 DO matchstr[i]=matchstr[i]+string(matchsrc[j],format='(i2)')+' '
      IF srcb GE 0 THEN sid = sourcelist[srcb] ELSE sid = 'INVALID'
;      print, cene[i], zspec[i], zfly[i], zbpz, zgrism[i], linid[i], lam0[i], nmatch[i], status[i], sid, matchsrc, $
;             format='(f9.2,f7.3,f7.3,f7.3,f7.3,f7.3,f9.3,1x,a8,1x,f9.2,i4,i4,2x,a7,2x,i4,i4,i4)'
      print, cene[i], zspec[i], zfly[i], zbpz, zgrism[i], linid[i], lam0[i], nmatch[i], status[i], sid, matchstr[i], $
             format='(f9.2,f7.3,f7.3,f7.3,f7.3,f7.3,f9.3,1x,a8,1x,f9.2,i4,i4,2x,a7,2x,a12)'
   ENDFOR 
   ;
   openw, lu, filo, /get_lun
   printf,lu, hdro
   FOR i = 0, nem-1 DO BEGIN 
      printf, lu, iau[i], rastr[i], decstr[i], line[i], nknt[i], cene[i], fluxe[i], ew[i], $
              bmag[i], vmag[i], imag[i], zmag[i], zspec[i], zcapak[i], zfly[i], zbpz1[i], zbpz1a[i], zbpz1b[i], obpz1[i], $
              zbpz2[i], zbpz2a[i], zbpz2b[i], obpz2[i], zbpz3[i], zbpz3a[i], zbpz3b[i], obpz3[i], $
              zgrism[i], linid[i], lam0[i], nmatch[i], status[i], matchstr[i], $
              format=fmto
   ENDFOR 
   free_lun, lu
   ;
   cd, cwd
END 
