PRO test_hdfn_rjbcats
   ; 
   wd = '/home/meurer/ACS/Grism/HDFN_jun04/Apsis/Work/Match'
   ;cat1 = 'hdf1.cat'
   ;cat2 = 'hdf2.cat'
   ;out1 = 'hdf1_overlap.cat'
   ;out2 = 'hdf2_overlap.cat'
   ;xoff = -189.
   ;yoff = -62.
   cat1 = 'hdf1_pix.dat'
   cat2 = 'hdf2_pix.dat'
   out1 = 'hdf1_pix_overlap.dat'
   out2 = 'hdf2_pix_overlap.dat'
   xoff =  0.0
   yoff =  0.0
   ;
   cd, wd, current=cwd
   ;readcol, cat1, id1, ra1, dec1, dum, mag1, format='(l,f,f,i,f)'
   ;readcol, cat2, id2, ra2, dec2, dum, mag2, format='(l,f,f,i,f)'
   readcol, cat1, id1, ra1, dec1, mag1, format='(l,f,f,f)'
   readcol, cat2, id2, ra2, dec2, mag2, format='(l,f,f,f)'
   x1   = ra1 + xoff
   x2   = ra2 + xoff 
   y1   = dec1 + yoff
   y2   = dec2 + yoff
   xr   = [min([x1, x2]),max([x1, x2])]
   yr   = [min([y1, y2]),max([y1, y2])]
   ;xr   = [0.200, 0.218]
   ;yr   = [0.188, 0.196]
   setplotcolors
   plot, x1, y1, xrange=xr, yrange=yr, xstyle=1, ystyle=1, psym=1
   oplot, x1, y1, psym=1, color=!purple
   oplot, x2, y2, psym=1, color=!dgreen
   ;
   ; define polygon for overlap check
   xc = make_array(5,/float)
   yc = make_array(5,/float)
   FOR i = 0, 3 DO BEGIN 
      cursor, g,h, /data, /down
      xc[i] = g
      yc[i] = h
      gg = make_array(1,value=g)
      hh = make_array(1,value=h)
      oplot,gg,hh,psym=sym(1),color=!red
   ENDFOR
   xc[4] = xc[0]
   yc[4] = yc[0]
   oplot,xc,yc,psym=0,color=!red
   cd, cwd
   ;
   ; now find points within polygon in both arrays
   xc = xc[0:3]
   yc = yc[0:3]
   in1 = inside(x1,y1,xc,yc,/index)
   in2 = inside(x2,y2,xc,yc,/index)
   nin1 = n_elements(in1)
   nin2 = n_elements(in2)
   print,nin1,nin2
   ;
   ; save to files
   openw, lu, out1, /get_lun
   FOR i = 0, nin1-1 DO printf,lu,id1[in1[i]],x1[in1[i]],y1[in1[i]],mag1[in1[i]]
   free_lun, lu
   openw, lu, out2, /get_lun
   FOR i = 0, nin2-1 DO printf,lu,id2[in2[i]],x2[in2[i]],y2[in2[i]],mag2[in2[i]]
   free_lun, lu
END 
