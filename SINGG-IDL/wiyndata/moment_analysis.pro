PRO moment_analysis, fgaus

   strgapos   = strpos(fgaus, 'gauss')
   ffits      = strmid(fgaus, 0, strgapos) + 'obj' + strmid(fgaus, strgapos+5, 13) $
     + '.co.cr.ms.fits'
   fout       = strmid(fgaus, 0, strgapos) + 'moment' + strmid(fgaus, strgapos+5, 13) $
     + '.dat'
   cvel       = 299792.458
   lamzero    = [6548.1001,6562.8169,6583.6001,6678.15,6716.4702,6730.8501]
   nlines     = n_elements(lamzero)
   fmtg       =  '(i,a,a,x,x,x,f,f,f,f,f,f,f,i,f,f)'
   fmtout     =  '(3i5,4f16)'
   exptimes   = 'exptimes.dat'
   ;
   ; read gaussian fit positions
   readcol, fgaus, row, filstr, filstr2, chisq, rms, c1, c2, c3, $
     c4, c5, linnum, zz, rvel, format=fmtg, /silent
   nr = n_elements(row)
   ;
   ; read exposure times
   readcol, exptimes, filename, nexp, expt, $
     format='(x,x,x,a,i,f)', /silent
   time  = 1800.0
   thisfile = 'obj'+strmid(fgaus,strpos(fgaus,'gauss')+5,13)
   fileline = where(filename EQ thisfile)
   IF fileline NE -1 THEN time = expt(fileline)
   ;
   ; read fits file
   objfits    = readfits(ffits, fitsheader)
   ;
   ; decode apertures from filstr
   id = make_array(nr, /int, value=0)
   FOR ii = 0, nr-1 DO BEGIN 
      k2   = strpos(filstr2[ii],']',/reverse_search)
      istr = strmid(filstr2[ii],0,k2)
      id[ii] = fix(istr)
   ENDFOR 
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
   lamda      = ((findgen(n_elements(objfits(*,0))) - (crpix - 1)) * $
     disper) + crval
   xx         = round((c4 - crval)/disper + crpix) - 1
   ;
   ; set range to examine
   minx       = round((c4 - 10 - crval)/disper + crpix) - 1
   maxx       = round((c4 + 10 - crval)/disper + crpix) - 1
   ;
   ; initialise arrays
   flux       = make_array(nr)
   vel        = make_array(nr)
   width      = make_array(nr)
   back       = make_array(nr)
   ;
   ; loop over emission lines
   FOR ii = 0, nr-1 DO BEGIN
       ;
       ; pick out data to examine
       data        = objfits(minx(ii):maxx(ii), id(ii)-1)
       lamdata     = lamda(minx(ii):maxx(ii))
       npix        = n_elements(data)
       data        = data / disper
       ;
       ; set and remove background level
       back(ii)    = median([data(0:round(npix/4.0 - 1.0)), $
                            data(round(3.0*npix/4.0 - 1.0):*)])
       data        = data - back(ii)
       ;
       ; calculate flux
       flux(ii)    = total(data) * disper / time
       ;
       ; calculate mean v
       normdist    = data / total(data)
       sumlam      = total(lamdata * normdist)
       vel(ii)     = (cvel * sumlam / lamzero(linnum(ii)-1)) - cvel
       ;
       ; calculate width
       varlam      = total(normdist * (lamdata - sumlam)^2)
       width(ii)   = (cvel / lamzero(linnum(ii)-1)) * sqrt(varlam)
       print, sumlam, sqrt(sqrt(varlam^2))
   ENDFOR 
   ;
   ; open output file and write data
   openw, 1, fout
   printf, 1, '# row   ap line      background            flux' + $
     '        velocity           sigma'
   FOR ii = 0, nr-1 DO printf, 1, row(ii), id(ii), linnum(ii), back(ii), $
     flux(ii), vel(ii), width(ii), format=fmtout
   close, 1

END
