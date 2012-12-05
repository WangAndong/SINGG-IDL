PRO testplot
   matchcat = 'c00_direct_matched.cat'
   readcol, matchcat, id, xim, yim, aim, bim, theta, w50, magauto, class, idc, $
    rah, ram, ras, dd, dm, ds, rmag, zspec, typstr, $
    format='(i,f,f,f,f,f,f,f,f,i,a,a,a,a,a,a,f,f,a)'
   plot,class,w50,psym=1,xrange=[-0.05,1.05],yrange=[0,10],xstyle=1,ystyle=1
   fil_bpz   = '/data3/acs27/meurer/grism/hdfn/axe/bpz/spec_c00_1a.bpz'
   small_gal = where(w50 LE 6.0 AND class LE 0.8,count)
   FOR i = 0, count-1 DO print, id[small_gal[i]]
   small_lis = '/data3/acs27/meurer/grism/hdfn/axe/bpz/small_gal.lis'
   openw,lun,small_lis,/get_lun
   FOR i = 0, count-1 DO printf,lun, 'spec_c00_'+trim(string(id[small_gal[i]]),2)+'.dat'
   free_lun,lun
END 

