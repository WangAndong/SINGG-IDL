PRO correct_flux, restonly=restonly, disponly=disponly
   fflux      = '../line_fluxes.dat'
   fout       = '../line_fluxesa.dat'
   fmtout     = '(a20,a12,3f16)'
   folders    = ['Run0410_n','Run0503_n','Run0603_n']
   ;
   ; read flux table
   readcol, fflux, filename, hipassid, halpha, nii, ratio, format='(a,a,f,f,f)'
   nfiles     = n_elements(filename)
   ;
   ; match filenames to folders
   fileno     = strmid(filename, 7)
   runno      = fix(strmid(fileno, 1, 1))
   nightno    = strmid(fileno, 3, 1)
   ;
   ; create filename strings
   fintspec   = '../'+folders(runno-1)+nightno+'/'+filename+'.cr.ms.fits'
   ;
   ; find each redshift factor
   restfactor = make_array(nfiles)
   disper     = make_array(nfiles)
   FOR ii=0, nfiles-1 DO BEGIN
       fitsheader     = headfits(fintspec(ii))
       disploc        = where(strmid(fitsheader, 0, 6) EQ 'CDELT1')
       eqpos          = strpos(fitsheader(disploc), '=')
       disper(ii)     = float(strmid(fitsheader(disploc), eqpos + 1))
       restfactorloc  = where(strmid(fitsheader, 0, 8) EQ 'ISRESTFA')
       eqpos          = strpos(fitsheader(restfactorloc), '=')
       restfactor(ii) = float(strmid(fitsheader(restfactorloc), eqpos + 1, $
                             strpos(fitsheader(restfactorloc), '/') - $
                             eqpos - 1))
   ENDFOR 
   ;
   ; adjust fluxes
   IF NOT(keyword_set(disponly)) THEN BEGIN
       halpha     = halpha / restfactor
       nii        = nii / restfactor
   ENDIF
   IF NOT(keyword_set(restonly)) THEN BEGIN
       halpha     = halpha / disper
       nii        = nii / disper
   ENDIF 
   ;
   ; make output file
   openw, 1, fout
   printf, 1, '# File made by hand using splot and edited by correct_flux.pro on ' +$
     systime()
   printf, 1, '#           filename   hipass id     Halpha flux  [NII]6583 flux' +$
     '    [NII]/Halpha'
   FOR ii=0, nfiles-1 DO printf, 1, filename(ii), hipassid(ii), halpha(ii), $
     nii(ii), ratio(ii), format=fmtout
   close, 1
END
