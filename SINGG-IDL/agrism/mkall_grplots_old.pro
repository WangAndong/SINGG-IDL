PRO mkall_grplots_old, filimg, filspc, filrib, filcat, outdir
   pstmp = outdir + 'stmp_'
   pspec = outdir + 'spec_'
   pribn = outdir + 'ribn_'
   ;
   rd_grdirct_cat, filcat, id, xim, yim, magauto, aim, bim, thetaim, w50, class
   id    = fix(temporary(id))
   posim = [[xim], [yim]] 
   fits_read,filimg,img,hdr
   FOR i = 0, n_elements(id)-1 DO BEGIN 
      print, 'Working on object #',id[i]
      grism_triplot,i,img,id,posim,filspc,filrib,0,1,2,pstmp,pspec,pribn,$
       hspc,hrib,stampim,bintab,rib
   ENDFOR 
END 
