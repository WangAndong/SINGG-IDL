PRO rescale_cat, fili, filo, scale 
   ;
   ; simple minded program to rescale RA & dec coords 
   ; in radian offsets by a 
   ; pixel scale.
   ;
   ; fili -> input file.  ID, RA, DEC, mag expected to be in
   ;         columns, 1,2,3,& 5.
   ; filo -> output file, written as id, rapix, decpix, mag
   ;
   ; G. Meurer 7/2004
   ;
   readcol, fili, id, rarad, decrad, dum, mag, format='(l,f,f,i,f)'
   fact   = 3600.0*180.0/(!pi*scale)
   rapix  = -1.0*fact*rarad
   decpix = fact*decrad
   n      = n_elements(id)
   openw, lu, filo, /get_lun
   FOR i = 0,n-1 DO printf, lu, id[i], rapix[i], decpix[i], mag[i]
   free_lun, lu
END 
