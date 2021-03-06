PRO int_spec
   filelist  = 'exptimes.dat'
   fweight   = 'fiber_weights.dat'
   format1   = '(a,a,a,a,i,f)'
   format2   = '(a,a,a,a,a,a,i,f)'
   folders   = ['Run0410_n','Run0503_n','Run0603_n']
   skyfibers = [2, 16, 22, 37, 54, 70, 80]
   notsky    = make_array(82, value=1)
   notsky(skyfibers-1) = 0
   notskylist = where(notsky EQ 1)
   ;
   ; load list of filenames
   readcol, filelist, hipassid, galid1, galid2, filename, $
     nexp, texp, format=format1, /silent
   readcol, filelist, temphi, tempga1, tempga2, tempga3, tempga4, $
     tempfile, tempnexp, temptexp, format=format2, /silent
   hipassid = [hipassid, temphi]
   galid    = [galid1+' '+galid2, $
               tempga1+' '+tempga2+' '+tempga3+' '+tempga4]
   filename = [filename, tempfile]
   nfiles   = n_elements(filename)
   ;
   ; match filenames to folders
   fileno   = strmid(filename, 3)
   runno    = fix(strmid(fileno, 1, 1))
   nightno  = strmid(fileno, 3, 1)
   ;
   ; create filename strings
   fitsfile = '../'+folders(runno-1)+nightno+'/Redundant/'+filename+'.cr.ms.fits'
   combfile = '../'+folders(runno-1)+nightno+'/comb'+fileno+'.dat'
   outfile  = '../'+folders(runno-1)+nightno+'/intspec'+fileno+'.cr.ms.fits'
   ;
   ; read weights
   readcol, fweight, fid, weight, format='(i,i)'
   weight   = weight / 18.0
   ;
   ; loop over files
   FOR ii = 0, nfiles - 1 DO BEGIN
       ;
       ; read velocity data
       readcol, combfile(ii), apid, z1, f1, z2, f2, z3, f3, z4, f4, z5, f5, z6, f6, $
         meanz, meanv, sdevv, format='(i,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f)', /silent
       nr = n_elements(apid)
       ;
       ; pick out apertures with H alpha emission
       halpha   = apid(where(f2 GT 0)) - 1
       ;
       ; open fits file and perform first two sums
       data        = readfits(fitsfile(ii), fitsheader)
       datadim     = size(data, /dimensions)
       weightgrid  = make_array(datadim(0), value=1) # weight
       data        = data * weightgrid
       intspec     = make_array(datadim(0), 3)
       intspec(*,0) = total(data(*, notskylist), 2)
       intspec(*,1) = total(data(*, halpha), 2)
       ;
       ; find the dispersion and zero point
       cdeltloc   = where(strmid(fitsheader,0,6) EQ 'CDELT1')
       disper     = float(strmid(fitsheader(cdeltloc), $
                                 strpos(fitsheader(cdeltloc), '=')+1))
       disper     = disper(0)
       crvalloc   = where(strmid(fitsheader,0,6) EQ 'CRVAL1')
       crval      = float(strmid(fitsheader(crvalloc), $
                                 strpos(fitsheader(crvalloc), '=')+1))
       crval      = crval(0)
       crpixloc   = where(strmid(fitsheader,0,6) EQ 'CRPIX1')
       crpix      = float(strmid(fitsheader(crpixloc), $
                                 strpos(fitsheader(crpixloc), '=')+1))
       crpix      = crpix(0)
       ;
       ; convert between wavelengths and pixels
       lamda      = ((findgen(n_elements(data(*,0))) - (crpix - 1)) * $
                     disper) + crval
       maxflux    = max(f2, maxfluxind)
       lamzero    = lamda / (1.0 + meanz(maxfluxind))
       ;
       ; shift spectra and perform third sum
       FOR jj = 0, nr - 1 DO BEGIN
           xx           = ((((1.0 + meanz(jj)) * lamzero) - crval) / $
                           disper) + (crpix - 1)
           intspec(*,2) = intspec(*,2) + (1.0 + meanz(jj)) * $
                           sint(xx, data(*, apid(jj)-1))
       ENDFOR
       ;
       ; edit header and write output
       naxis2      = where(strpos(fitsheader, 'NAXIS2') EQ 0)
       numpos      = strpos(fitsheader(naxis2), '82')
       fitsheader(naxis2) = strmid(fitsheader(naxis2), 0, numpos) + ' 3' + $
         strmid(fitsheader(naxis2), numpos+2)
       apidpos     = where(strmid(fitsheader, 0, 4) EQ 'APID')
       fitsheader(apidpos)     = '                                          '+$
         '                                      '
       fitsheader(apidpos(0)) = "ISROW1  = 'Sum over non-sky apertures'"
       fitsheader(apidpos(1)) = "ISROW2  = 'Sum over all apertures with H-alpha detection'"
       fitsheader(apidpos(2)) = "ISROW3  = 'Sum over all apertures with any detection, " + $
                     "relative redshift removed'"
       fitsheader(apidpos(3)) = "ISRESTFA= " + string(1.0+meanz(maxfluxind), $
                     format='(f20)') + " / Multiplication factor of row 3 wavelengths"
       writefits, outfile(ii), intspec, fitsheader
   ENDFOR

END
