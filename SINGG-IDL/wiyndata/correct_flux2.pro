PRO correct_flux2
   fflux     = '../line_fluxes2.dat'
   fout      = '../line_fluxes2a.dat'
   ftime     = '../exptimes.dat'
   fmtflux   = '(a,a,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f)'
   fmttime1  = '(a,a,a,a,i,f)'
   fmttime2  = '(a,a,a,a,a,a,i,f)'
   fmtout    = '(a16,a12,a216)'
   folders  = ['Run0410_n','Run0503_n','Run0603_n']
   ;
   ; read fluxes
   readcol, fflux, filename, hipassid, l1, w1, f1, l2, w2, f2, l3, w3, f3, $
     l4, w4, f4, l5, w5, f5, l6, w6, f6, format=fmtflux
   nfiles    = n_elements(filename)
   lgrid     = make_array(6, nfiles)
   wgrid     = make_array(6, nfiles)
   fgrid     = make_array(6, nfiles)
   FOR ii = 0, 5 DO BEGIN
       scratch   = execute('lgrid(ii,*) = l' + strcompress(string(ii+1), /remove_all))
       scratch   = execute('wgrid(ii,*) = w' + strcompress(string(ii+1), /remove_all))
       scratch   = execute('fgrid(ii,*) = f' + strcompress(string(ii+1), /remove_all))
   ENDFOR
   ;
   ; read exposure times
   readcol, ftime, thipassid, galid1, galid2, tfilename, $
     nexp, texp, format=fmttime1, /silent
   readcol, ftime, temphi, tempga1, tempga2, tempga3, tempga4, $
     tempfile, tempnexp, temptexp, format=fmttime2, /silent
   thipassid = [thipassid, temphi]
   galid     = [galid1+' '+galid2, $
                tempga1+' '+tempga2+' '+tempga3+' '+tempga4]
   tfilename = [tfilename, tempfile]
   nexp      = [nexp, tempnexp]
   texp      = [texp, temptexp]
   ;
   ; match exposure times to files
   order     = make_array(nfiles)
   FOR ii = 0, nfiles-1 DO BEGIN
       fileloc   = where(tfilename EQ filename(ii))
       order(ii) = fileloc(0)
   ENDFOR
   thipassid = thipassid(order)
   galid     = galid(order)
   tfilename = tfilename(order)
   nexp      = nexp(order)
   texp      = texp(order)
   ;
   ; match filenames to folders
   fileno    = strmid(filename, 3)
   runno     = fix(strmid(fileno, 1, 1))
   nightno   = strmid(fileno, 3, 1)
   ;
   ; find dispersions
   fint      = '../'+folders(runno-1)+nightno+'/intspec'+fileno+'.cr.ms.fits'
   disper    = make_array(nfiles)
   FOR ii = 0, nfiles-1 DO BEGIN
       fitsheader     = headfits(fint(ii))
       disploc        = where(strmid(fitsheader, 0, 6) EQ 'CDELT1')
       eqpos          = strpos(fitsheader(disploc), '=')
       disper(ii)     = float(strmid(fitsheader(disploc), eqpos + 1))
   ENDFOR
   ;
   ; correct fluxes
   factor    = make_array(6, value=1.0) # (texp * disper)
   fgrid     = fgrid / factor
   ;
   ; create output
   openw, 1, fout
   printf, 1, '# File made by hand using IRAF splot and intspec files and ' + $
     'edited by correct_flux2.pro on ' + systime()
   printf, 1, '#                           |---------- [NII] 6548 -------------' + $
     '|--------- H alpha 6563 ------------|---------- [NII] 6584 -------------' + $
     '|----------- HeI 6678 --------------|---------- [SII] 6716 -------------' + $
     '|---------- [SII] 6731 -------------|'
   printf, 1, '#       filename   hipass id  wavelength       width        flux' + $
     '  wavelength       width        flux  wavelength       width        flux' + $
     '  wavelength       width        flux  wavelength       width        flux' + $
     '  wavelength       width        flux'
   FOR ii = 0, nfiles-1 DO BEGIN
       outline    = ''
       FOR jj = 0, 5 DO BEGIN
           IF fgrid(jj,ii) NE 0 THEN fmt = '(f12.2,f12.3,f12.3)' $
           ELSE fmt = '(3i12)'
           outline    = outline + string([lgrid(jj,ii), wgrid(jj,ii), fgrid(jj,ii)], $
                                         format=fmt)
       ENDFOR
       printf, 1, filename(ii), hipassid(ii), outline, format=fmtout
   ENDFOR
;   fmtout    = make_array(nfiles, /string, value='(a16,a12')
;   FOR ii = 0, nfiles-1 DO BEGIN
;       FOR jj = 0, 5 DO BEGIN
;           IF fgrid(jj,ii) NE 0 THEN fmtout = fmtout + ',f12.2,2f12.3' $
;           ELSE fmtout = fmtout + ',3i12'
;       ENDFOR
;   ENDFOR
;   fmtout    = fmtout + ')'
;   FOR ii = 0, nfiles-1 DO printf, 1, filename(ii), hipassid(ii), lgrid(0,ii), $
;     wgrid(0,ii), fgrid(0,ii), lgrid(1,ii), wgrid(1,ii), fgrid(1,ii), lgrid(2,ii), $
;     wgrid(2,ii), fgrid(2,ii), lgrid(3,ii), wgrid(3,ii), fgrid(3,ii), lgrid(4,ii), $
;     wgrid(4,ii), fgrid(4,ii), lgrid(5,ii), wgrid(5,ii), fgrid(5,ii), format=fmtout(ii)
   close, 1
END
