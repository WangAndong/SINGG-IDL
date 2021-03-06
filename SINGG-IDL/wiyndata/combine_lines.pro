PRO combine_lines, fgaus
   fmtg       =  '(i,a,a,x,x,x,f,f,f,f,f,f,f,i,f,f)'
   nlines     = 6
   cvel       = 299792.458
   skyfib     = [2, 16, 22, 37, 54, 70, 80]
   ;
   ; create output file
   gausspos   = strpos(fgaus, 'gauss')
   outfile    = strmid(fgaus, 0, gausspos) + 'comb' + $
     strmid(fgaus, gausspos+5, 13) + '.dat'
   ;
   ; read gaussian fit positions
   readcol, fgaus, row, filstr, filstr2, chisq, rms, c1, c2, c3, $
     c4, c5, linnum, zz, rvel, format=fmtg, /silent
   nr = n_elements(row)
   ;
   ; read flux from equivalent widths file
   gausspos = strpos(fgaus, 'gauss')
   feqw     = strmid(fgaus, 0, gausspos) + 'eqw' + $
     strmid(fgaus, gausspos+5)
   readcol, feqw, flux, format='(x,x,x,x,f,x,x,x,x)', /silent
   ;
   ; decode apertures from filstr
   id = make_array(nr, /int, value=0)
   FOR ii = 0, nr-1 DO BEGIN 
      k2   = strpos(filstr2[ii],']',/reverse_search)
      istr = strmid(filstr2[ii],0,k2)
      id[ii] = fix(istr)
   ENDFOR 
   ;
   ; pick out the rows for each aperture
   hap        = histogram(id, reverse_indices=haprev)
   nap        = n_elements(hap)
   ;
   ; initialise arrays
   aperture   = indgen(nap)+1
   zgrid      = make_array(nap, nlines)
   fluxgrid   = make_array(nap, nlines)
   zcomb      = make_array(nap)
   vcomb      = make_array(nap)
   sdev       = make_array(nap)
   ;
   ; average redshift weighted by flux
   FOR ii = 0, nap-1 DO BEGIN
       IF haprev(ii+1) GT haprev(ii) THEN BEGIN
           cols = linnum(haprev(haprev(ii):haprev(ii+1)-1)) - 1
           zgrid(ii, cols) = $
             zz(haprev(haprev(ii):haprev(ii+1)-1))
           fluxgrid(ii, cols) = $
             flux(haprev(haprev(ii):haprev(ii+1)-1))
           zcomb(ii) = total(zgrid(ii,*) * fluxgrid(ii,*)) / $
             total(fluxgrid(ii,*))
           IF n_elements(cols) GT 1 THEN sdev(ii) = $
             cvel*stddev(zgrid(ii,cols))
       ENDIF
   ENDFOR
   vcomb      = zcomb * cvel
   ;
   ; centre vcomb on 0
   nsky       = n_elements(skyfib)
   notsky     = make_array(nr, /integer, value=1)
   FOR ii = 0, nsky-1 DO BEGIN
       issky = where(id EQ skyfib(ii), nskyii)
       IF nskyii GT 0 THEN notsky(issky) = 0
   ENDFOR
   notskylist = where(notsky EQ 1)
   vcomb      = vcomb - 0.5*cvel*(max(zz(notskylist)) + min(zz(notskylist)))
   ;
   ; make output
   havedata   = where(zcomb GT 0, ndata)
   outgrid    = make_array(ndata, 12)
   outgrid(*,[0,2,4,6,8,10]) = zgrid(havedata, *)
   outgrid(*,[1,3,5,7,9,11]) = fluxgrid(havedata, *)
   aperture   = aperture(havedata)
   zcomb      = zcomb(havedata)
   vcomb      = vcomb(havedata)
   sdev       = sdev(havedata)
   fmtgrid    = make_array(ndata, 6, /string, value='2f16,')
   fmtgrid(where(zgrid(havedata, *) EQ 0)) = '2i16,'
   fmtsdev    = make_array(ndata, /string, value='2f16')
   nosdev     = where(sdev EQ 0, nnosdev)
   IF nnosdev GT 0 THEN fmtsdev(nosdev) = '2i16'
   ;
   ; open output file and print header
   openw, 1, outfile
   printf, 1, '#  ap             z 1          flux 1'+$
     '             z 2          flux 2             z 3          flux 3'+$
     '             z 4          flux 4             z 5          flux 5'+$
     '             z 6          flux 6          mean z          mean v'+$
     '       std dev v'
   ;
   ; print data
   FOR jj = 0, ndata-1 DO BEGIN
       fmtline = fmtgrid(jj,*)
       fmt     = '(i5,'+strjoin(fmtline, /single)+'2f16,'+fmtsdev(jj)+')'
       printf, 1, aperture(jj), outgrid(jj,*), zcomb(jj), vcomb(jj), $
         sdev(jj), format=fmt
   ENDFOR
   close, 1
   

END
