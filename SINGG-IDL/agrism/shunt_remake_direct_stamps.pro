PRO shunt_remake_direct_stamps, fcat, fdirect, pfxo, zrange_direct, dxstamp, dystamp, $
                                xoffset=xoffset, yoffset=yoffset
   ;
   ; kludge to remake all the direct stamps from the shunt catalog file
   ;
   ; G. Meurer 09/2005
   readcol, fcat, id, xsq, ysq, mag, spec_class, ra, dec, ra0, dec0, ra1, dec1, ra2, dec2, ra3, dec3, $
    format='(l,f,f,f,a,x,x,x,x,x,x,x,x,d,d,d,d,d,d)'
   nobj = n_elements(id)
   fits_read, fdirect, im_direct, hdrd
   pfx_dstamp = pfxo+'_shunt_direct_stamp'
   zrange_d = zrange_direct 
   FOR ii = 0, nobj-1 DO BEGIN 
      rad    = ra[ii]
      decd   = dec[ii]
      idd    = id[ii]
      bxra   = [ra0[ii], ra1[ii], ra2[ii], ra3[ii]]
      bxdec  = [dec0[ii], dec1[ii], dec2[ii], dec3[ii]]
      fstamp = pfx_dstamp+'_'+strtrim(string(idd),2)+'.jpg'
      fhard  = pfx_dstamp+'_'+strtrim(string(idd),2)+'.ps'
      shunt_direct_stamp, im_direct, hdrd, dxstamp, dystamp, zrange_d, idd, rad, decd, bxra, bxdec, $
       xoffset=xoffset, yoffset=yoffset
      im     = tvrd(true=3)
      WRITE_JPEG,fstamp,im,TRUE=3,QUALITY=100
      shunt_direct_stamp, im_direct, hdrd, dxstamp, dystamp, zrange_d, idd, rad, decd, bxra, bxdec, $
       xoffset=xoffset, yoffset=yoffset, hardfile=fhard
   ENDFOR 
END 
