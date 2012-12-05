PRO is_fits2ascii
   filelist   = 'exptimes.dat'
   exptimes   = 'exptimes.dat'
   format1    = '(a,a,a,a,i,f)'
   format2    = '(a,a,a,a,a,a,i,f)'
   folders    = ['Run0410_n','Run0503_n','Run0603_n']
   ;
   ; read filenames
   readcol, filelist, hipassid, galid1, galid2, filename, $
     nexp, texp, format=format1, /silent
   readcol, filelist, temphi, tempga1, tempga2, tempga3, tempga4, $
     tempfile, tempnexp, temptexp, format=format2, /silent
   hipassid   = [hipassid, temphi]
   galid      = [galid1+' '+galid2, $
                 tempga1+' '+tempga2+' '+tempga3+' '+tempga4]
   filename   = [filename, tempfile]
   texp       = [texp, temptexp]
   nfiles     = n_elements(filename)
   ;
   ; match filenames to folders
   fileno     = strmid(filename, 3)
   runno      = fix(strmid(fileno, 1, 1))
   nightno    = strmid(fileno, 3, 1)
   ;
   ; create filename and title strings
   fintspec   = '../'+folders(runno-1)+nightno+'/intspec'+fileno+'.cr.ms.fits'
   outfile    = '../'+folders(runno-1)+nightno+'/intspec'+fileno+'.dat'
   ;
   ; read exposure times
   readcol, exptimes, expfilename, nexp, expt, $
     format='(x,x,x,a,i,f)', /silent
   ;
   ; copy each fits file to ascii
   FOR ii = 0, nfiles-1 DO BEGIN
       ;
       ; open fits file
       spectra  = readfits(fintspec(ii), fitsheader)
       ;
       ; read information from header
       delt1loc = where(strmid(fitsheader, 0, 6) EQ 'CDELT1')
       disper   = float(strmid(fitsheader(delt1loc), $
                             strpos(fitsheader(delt1loc), '=')+1))
       disper   = disper(0)
       crvalloc = where(strmid(fitsheader, 0, 6) EQ 'CRVAL1')
       crval    = float(strmid(fitsheader(crvalloc), $
                             strpos(fitsheader(crvalloc), '=')+1))
       crval    = crval(0)
       crpixloc = where(strmid(fitsheader, 0, 6) EQ 'CRPIX1')
       crpix    = float(strmid(fitsheader(crpixloc), $
                             strpos(fitsheader(crpixloc), '=')+1))
       crpix    = crpix(0)
       restfactorloc = where(strmid(fitsheader, 0, 8) EQ 'ISRESTFA')
       eqpos         = strpos(fitsheader(restfactorloc), '=')
       restfactor    = float(strmid(fitsheader(restfactorloc), eqpos + 1, $
                             strpos(fitsheader(restfactorloc), '/') - $
                             eqpos - 1))
       restfactor    = restfactor(0)
       ;
       ; convert spectra to nice units
       spectra  = spectra / (texp(ii) * disper)
       ;
       ; set wavelengths
       lamda1   = ((findgen(n_elements(spectra(*,0))) - (crpix - 1)) * $
                     disper) + crval
       lamda2   = lamda1 / restfactor
       ;
       ; make .dat file
       openw, 1, outfile(ii)
       printf, 1, '# File created by is_fits2ascii.pro on ' + systime()
       printf, 1, '#     Wavelength  Non-sky fibers      Wavelength     All H-alpha' +$
         '      Wavelength   Rest w/length'
       FOR jj=0, n_elements(lamda1)-1 DO printf, 1, lamda1(jj), spectra(jj,0), $
         lamda1(jj), spectra(jj,1), lamda2(jj), spectra(jj,2), format='(6f16)'
       close, 1
   ENDFOR 
END
