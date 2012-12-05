PRO compile_emaxe_table, fili=fili, filo=filo, nospec=nospec, nov04=nov04, blemaxe=blemaxe
   ;
   ; Compile table on aXe (direct image) selected emission line galaxies
   ;
   ; nospec -> if set then spectroscopic results are ignored.
   ;
   ; G. Meurer 08/04
   ; G. Meurer 09/04 add in Capak results, include nospec option
   ; G. Meurer 11-12/04 add nov04 switch to use new input file format
   ;                    add fili option to specify input file.
   ;                    add filo option for output file.
   ;                    add blemaxe switch for axe run on blem 
   ;                    positions.  This is needed to allow nknt to be
   ;                    read in.
   ; G. Meurer 12/04 break out plots into plot_zcomp.pro
   ;
   ;
   ; setup
   setplotcolors
   IF keyword_set(nov04) THEN BEGIN 
      intab      = 'hdfn_nov04_emsource_ed.cat'
      fmti       = '(i,f,f,f,f,f,f,f,f,f,f,f,f,i,i,i,f,f,f,i,i)'
      IF keyword_set(nospec) THEN fout = 'hdfn_nov04_axe_emsource_nospec.cat' ELSE fout = 'hdfn_nov04_axe_emsource.cat'
   ENDIF ELSE BEGIN 
      intab      = 'hdfn_emsource_ed.cat'
      fmti       = '(i,f,f,f,f,f,f,f,f,f,f,f,f,i,i,i,f,f,f)'
      IF keyword_set(nospec) THEN fout = 'hdfn_axe_emsource_nospec.cat' ELSE fout = 'hdfn_axe_emsource.cat'
   ENDELSE 
      
   IF keyword_set(fili) THEN intab = fili 
   IF keyword_set(filo) THEN fout  = filo

   wd         = '/home/meurer/text/ACS/Grism/Figures/'
   hdb        = 'hdfn_idt'
   fmto       = '(a19,1x,i4,1x,i4,1x,a12,1x,a12,1x,i2,1x,i4,1x,f6.1,1x,e9.2,1x,f7.1,1x,'+$
                'f6.2,1x,f6.2,1x,f6.2,1x,f6.2,1x,f6.3,1x,f6.3,1x,f6.3,1x,f6.3,1x,f6.3,1x,f6.3,1x,f5.2,1x,'+$
                'f6.3,1x,f6.3,1x,f6.3,1x,f5.2,1x,f6.3,1x,f6.3,1x,f6.3,1x,f5.2,1x,f6.3,1x,a7,1x,f7.2,1x,i2,1x,i2,1x,a15)'
   hdro       = '# IAU_name         sexid grid    RA            Dec     nlin nknt lam     flux        ew   '+$
                'bmag   vmag   imag   zmag   zspec   zcap  zfly  zbpz1 zbpz1a zbpz1b obpz1  zbpz2 zbpz2a zbpz2b obpz2  '+$
                'zbpz3 zbpz3a zbpz3b obpz3 zgrism line-id   lam0  mat st     flags'
   ;
   filtnam0   = ['F435W', 'F606W', 'F775W', 'F850LP']
   filtnam    = ['b', 'v', 'i', 'z']
   filtplam   = [4317.398, 5917.678, 7693.026, 9054.768]
   wl0        = [6562.817, 5006.85, 4861.33, 4342.90, 3727., 1216.]
   nam0       = ['H-alpha', '[OIII]', 'H-beta', 'H-gamma', '[OII]', 'Ly-alpha']
   ntest      = n_elements(wl0)
   threshodds = 0.05
   sourcelist = ['SPEC', 'CAPAK', 'FLY99', 'BPZ']                              ; source of first guess z
   maxpeak    = 3                                                     ; max #guesses per source
   oddsdef    = 1.0                                                   ; default odds
   ezfly      = 0.1                                                   ; default dz/(1+z) for FLY99
   ezcapak    = 0.08                                                  ; default dz/(1+z) for Capak
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
   IF keyword_set(nov04) THEN BEGIN 
      readcol, intab, ide, xime, yime, mage, ae, be, thetae, w50e, classe, $
       rae, dege, minsne, maxsne, nord0e, line, quale, cene, $
       w50e, fluxe, conte, ewe, format=fmti
   ENDIF ELSE BEGIN 
      readcol, intab, ide, xime, yime, mage, ae, be, thetae, w50e, classe, $
       rae, dege, minsne, maxsne, nord0e, line, quale, cene, $
       w50e, fluxe, format=fmti
   ENDELSE 
   ;
   ; set default nknt, linid
   nem        = n_elements(ide)
   nknt       = make_array(nem, value=1)
   idlin      = make_array(nem, value=0)
   IF keyword_set(blemaxe) THEN BEGIN 
      readcol, intab, ide, xime, yime, mage, ae, be, thetae, w50e, classe, $
       rae, dege, minsne, maxsne, nord0e, line, quale, cene, $
       w50e, fluxe, conte, ewe, idlin, nknt, format=fmti
   ENDIF 
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
   dbext, list, 'ID', sexid
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
   ; Determine Equivalent widths
   IF keyword_set(nov04) THEN BEGIN 
      ;
      ; With Nov04 mods use spectroscopic EW
      ew = ewe
   ENDIF ELSE BEGIN 
      ;
      ; calculate "photometric" equivalent width
      dmagst = 5.0*alog10(filtplam/5470.0)
      FOR i = 0, nem-1 DO BEGIN 
         test   = abs(cene[i]/filtplam - 1.0)
         magarr = [bmag[i], vmag[i], imag[i], zmag[i]] 
         k      = where(magarr LT 0.0, nk)
         IF nk GT 0 THEN test[k] = 100.0
         magarr = magarr + dmagst ; convert to stmag
         k      = sort(test)
         IF test[k[0]] LT 2.0 THEN BEGIN 
            flam   = 10.0^(-0.4*(magarr[k[0]] + 21.10))
            ew[i]  = fluxe[i]/flam
         ENDIF 
      ENDFOR 
   ENDELSE 
   ;
   ; Find unique objects
   kk     = sort(sexid)
   temp   = sexid[kk]
   kk     = uniq(temp)
   usexid = temp[kk]
   nu     = n_elements(usexid)
   ;
   ; loop through unique sources
   FOR mm = 0, nu-1 DO BEGIN 
      ;
      ; ll = all entries for source usexid[mm]
      ll       = where(sexid EQ usexid[mm], nll)
      IF nll LE 0 THEN stop, 'No elements in ll ???' 
      ll0      = ll[0]
      ;
      ; store redshifts and plot positions for all lines to test
      ztest    = make_array(ntest, nll, /float, value=0.0)
      ytest    = make_array(ntest, nll, /float, value=0.0)
      FOR ii = 0, nll-1 DO BEGIN 
         ztest[*,ii] = (cene[ll[ii]]/wl0 - 1.0)
         ytest[*,ii] = 1.0 + float(ii)*0.15
      ENDFOR 
      ztest    = reform(ztest, nll*ntest, /overwrite)
      ytest    = reform(ytest, nll*ntest, /overwrite)
      ;
      zbpz    = [zbpz1[ll0], zbpz2[ll0], zbpz3[ll0]]
      obpz    = [obpz1[ll0], obpz2[ll0], obpz3[ll0]]
      zbpza   = [zbpz1a[ll0], zbpz2a[ll0], zbpz3a[ll0]]
      zbpzb   = [zbpz1b[ll0], zbpz2b[ll0], zbpz3b[ll0]]
      source  = -1 + 0*source
      ;
      ; fill out first guess redshifts and related quantities
      ng      = 0
      IF zspec[ll0] GE 0.0 THEN BEGIN 
         ;
         ; spectroscopic redshifts
         dz          = ezspec*(1.0 + zspec[ll0])
         source[ng]  = 0
         zguess[ng]  = zspec[ll0]
         zguessa[ng] = max([zspec[ll0] - dz,0.0])
         zguessb[ng] = zspec[ll0] + dz
         oguess[ng]  = oddsdef
         ng          = ng + 1
      ENDIF 
      IF zcapak[ll0] GE 0.0 THEN BEGIN 
         ;
         ; Capak guess
         source[ng]  = 1
         zguess[ng]  = zcapak[ll0]
         dz          = ezcapak*(1.0 + zcapak[ll0])
         zguessa[ng] = max([zmincap[ll0],0.0])
         zguessb[ng] = max([zmaxcap[ll0],0.0])
         oguess[ng]  = oddscapak[ll0]
         ng          = ng + 1
      ENDIF 
      IF zfly[ll0] GE 0.0 THEN BEGIN 
         ;
         ; fly99 guess
         dz          = ezfly*(1.0 + zfly[ll0])
         source[ng]  = 2
         zguess[ng]  = zfly[ll0]
         zguessa[ng] = max([zfly[ll0] - dz,0.0])
         zguessb[ng] = zfly[ll0] + dz
         oguess[ng]  = oddsdef
         ng          = ng + 1
      ENDIF 
      kk       = where(zbpz GE 0.0 AND obpz GE threshodds, nkk)
      IF nkk GT 0 THEN BEGIN 
         FOR jj = 0, nkk-1 DO BEGIN 
            ;
            ; BPZ guesses
            source[ng]  = 3
            zguess[ng]  = zbpz[kk[jj]]
            zguessa[ng] = zbpza[kk[jj]]
            zguessb[ng] = zbpzb[kk[jj]]
            oguess[ng]  = obpz[kk[jj]]
            ng          = ng + 1
         ENDFOR 
      ENDIF 
      IF nll EQ 1 THEN BEGIN 
         lamline   = cene[ll0]
         elam      = 0.5*w50e[ll0]
         zmultiguess, lamline, elam, zguess, zguessa, zguessb, oguess, threshodds, source, nsrc, $
          zb, srcb, l0, lid, st, nm, zbsrc, matchsrc, peakidsrc
         ezb       = elam/l0
         FOR jj = 0, nsrc-1 DO matchstr[ll0]=matchstr[ll0]+string(matchsrc[jj],format='(i2)')+' '
         IF srcb GE 0 THEN sid = sourcelist[srcb] ELSE sid = 'INVALID'
         print, cene[ll0], zspec[ll0], zfly[ll0], zbpz, zgrism[ll0], linid[ll0], lam0[ll0], nmatch[ll0], status[ll0], sid, matchstr[ll0], $
             format='(f9.2,f7.3,f7.3,f7.3,f7.3,f7.3,f9.3,1x,a8,1x,f9.2,i4,i4,2x,a7,2x,a12)'
         ;
      ENDIF ELSE BEGIN 
         lamline   = cene[ll]
         elam      = 0.5*w50e[ll]
         zmultiline, lamline, elam, l0, lid, st, zb, ezb
         nm        = fix(st EQ 0) - 1*fix(st NE 0)
         ;
         ; check the matches
         IF zspec[ll0] GE 0.0 THEN BEGIN
            dz     = ezspec*(1.0 + zspec[ll0])
            IF zb+ezb GE zspec[ll0] - dz AND zb-ezb LE zspec[ll0]+dz THEN matchstr[ll] = ' 1 ' ELSE matchstr[ll] = '-1 '
         ENDIF ELSE matchstr[ll] = ' 0 '
         IF zcapak[ll0] GE 0.0 THEN BEGIN
            dz          = ezcapak*(1.0 + zcapak[ll0])
            IF zb+ezb GE zcapak[ll0] - dz AND zb-ezb LE zcapak[ll0]+dz THEN matchstr[ll] = matchstr[ll]+' 1 ' ELSE matchstr[ll] = matchstr[ll]+'-1 '
         ENDIF ELSE matchstr[ll] = matchstr[ll]+' 0 '
         IF zcapak[ll0] GE 0.0 THEN BEGIN
            dz          = ezfly*(1.0 + zfly[ll0])
            IF zb+ezb GE zfly[ll0] - dz AND zb-ezb LE zfly[ll0]+dz THEN matchstr[ll] = matchstr[ll]+' 1 ' ELSE matchstr[ll] = matchstr[ll]+'-1 '
         ENDIF ELSE matchstr[ll] = matchstr[ll]+' 0 '
         kk       = where(zbpz GE 0.0 AND obpz GE threshodds, nkk)
         bm       = 0b
         IF nkk GT 0 THEN BEGIN 
            FOR jj = 0, nkk-1 DO BEGIN 
               bm = bm + (zb+ezb GE min([zguessa, zguessb]) AND zb-ezb LE max([zguessa, zguessb]))
            ENDFOR 
            IF bm GT 0b THEN matchstr[ll] = matchstr[ll]+' 1 ' ELSE matchstr[ll] = matchstr[ll]+'-1 '
         ENDIF ELSE matchstr[ll] = matchstr[ll]+' 0 '
      ENDELSE 
      zgrism[ll] = zb
      linid[ll]  = lid
      lam0[ll]   = l0
      nmatch[ll] = nm
      status[ll] = st
      ;
      ; plot the redshift comparisons and results twice, once hard and once on screen 
      keywait, 'Type something for plot: '
      hardfile  = strtrim(iau[ll0],2)+'_'+strtrim(string(line[ll0]),2)+'_zguess_emaxe.ps'
      title     = strtrim(string(ide[ll0]),2)+' : '+iau[ll0]
      plot_zcomp, ztest, ytest, ng, zguess, zguessa, zguessb, oguess, source, zb, ezb, title
      plot_zcomp, ztest, ytest, ng, zguess, zguessa, zguessb, oguess, source, zb, ezb, title, hardfile=hardfile
   ENDFOR 
   ;
   openw, lu, fout, /get_lun
   printf,lu, hdro
   FOR ii = 0, nem-1 DO BEGIN 
      printf, lu, iau[ii], ide[ii], idlin[ii], rastr[ii], decstr[ii], line[ii], nknt[ii], cene[ii], fluxe[ii], ew[ii], $
              bmag[ii], vmag[ii], imag[ii], zmag[ii], zspec[ii], zcapak[ii], zfly[ii], zbpz1[ii], zbpz1a[ii], zbpz1b[ii], obpz1[ii], $
              zbpz2[ii], zbpz2a[ii], zbpz2b[ii], obpz2[ii], zbpz3[ii], zbpz3a[ii], zbpz3b[ii], obpz3[ii], $
              zgrism[ii], linid[ii], lam0[ii], nmatch[ii], status[ii], matchstr[ii], $
              format=fmto
   ENDFOR 
   free_lun, lu
   cd, cwd
END 

