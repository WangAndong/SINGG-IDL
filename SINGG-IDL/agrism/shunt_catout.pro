PRO shunt_catout, filo, id, xim, yim, mag, spec_class, bx_specx, bx_specy, ra, dec, bx_ra, bx_dec, $
                  sky_lev, sky_sig, max_sn
   ;
   ; write output ascii catalog for shunt classified spectra
   ;
   ; filo        -> name of output file
   ; id          -> ID numbers
   ; xim         -> column position in squashed grism image
   ; yim         -> row position in squashed grism image
   ; mag         -> instrumental mag in grism image
   ; spec_class  -> spectral classification
   ; bx_specx    -> extraction box column coords in unbinned grism image
   ;                (4 corners to be connected in CCW order)
   ; bx_specy    -> extraction box column coords in unbinned grism image
   ;                (4 corners to be connected in CCW order)
   ; ra          -> estimated right ascension of source
   ; dec         -> estimated declination of source
   ; bx_ra       -> error box right ascension coordinates
   ;                (4 corners to be connected in CCW order)
   ; bx_dec      -> error box declination coordinates
   ;                (4 corners to be connected in CCW order)
   ; sky_lev     -> sky level
   ; sky_sig     -> dispersion in sky level
   ; max_sn      -> maximum s/n in spectrum
   ;
   ; G. Meurer 09/2005
   ;           01/2006 (add sky_lev, sky_sig, max_sn)
   hdr1 = '#     squashed im coords  instr spec   spectrum extraction box (unbinned coords)      direct position       direct position error box ----------------------------------------------------------->'
   hdr2 = '# id      xsq      ysq     mag  class  x0    y0    x1    y1    x2    y2    x3    y3    ra          dec       ra0        dec0       ra1        dec1       ra2        dec2       ra3        dec3     sky_lev  sky_sig peak(s/n)'
   fmt = '(i5,f9.2,f9.2,f8.2,2x,a1,2x,i6,i6,i6,i6,i6,i6,i6,i6,'+$
          'f11.6,f11.6,f11.6,f11.6,f11.6,f11.6,f11.6,f11.6,f11.6,f11.6,'+$
          'f9.3, f8.3, f9.1)'
   openw, lu, filo, /get_lun
   printf, lu, hdr1, format='(a)'
   printf, lu, hdr2, format='(a)'
   nn = n_elements(id)
   FOR ii = 0, nn-1 DO $
    printf, lu, id[ii], xim[ii], yim[ii], mag[ii], spec_class[ii], $
            bx_specx[ii,0], bx_specy[ii,0], bx_specx[ii,1], bx_specy[ii,1], bx_specx[ii,2], bx_specy[ii,2], bx_specx[ii,3], bx_specy[ii,3], $
            ra[ii], dec[ii], bx_ra[ii,0], bx_dec[ii,0], bx_ra[ii,1], bx_dec[ii,1], bx_ra[ii,2], bx_dec[ii,2], bx_ra[ii,3], bx_dec[ii,3], $
            sky_lev[ii], sky_sig[ii], max_sn[ii], $
            format=fmt
   free_lun,lu
END
