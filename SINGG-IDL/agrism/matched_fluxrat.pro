PRO matched_fluxrat, fluxrat, file1=file1, file2=file2, maxmag=maxmag
   IF NOT keyword_set(file1) THEN file1 = 'matched.mtA'
   IF NOT keyword_set(file2) THEN file2 = 'matched.mtB'
   IF NOT keyword_set(maxmag) THEN maxmag = -9.0
   readcol, file1, id1, x1, y1, mag1
   readcol, file2, id2, x2, y2, mag2
   good = where(mag1 LE maxmag AND mag2 LE maxmag, ngood)
   dmag = mag1-mag2
   plot,mag1[good],dmag[good],ytitle='mag1 - mag2',xtitle='mag1', psym=1
   ;
   grm_avsigclip, dmag[good], 3.0, 10, mean, sigma, nuse, nrej, nit
   fluxrat = 10.0^(-0.4*mean)
   print, 'dmag    = ', mean
   print, 'sigma   = ', sigma
   print, 'nuse    = ', nuse
   print, 'nrej    = ', nrej
   print, 'nit     = ', nit
   print, ' '
   print, 'fluxrat = ', fluxrat
END 
