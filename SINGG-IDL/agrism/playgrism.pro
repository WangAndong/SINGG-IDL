filspc = 'j8eb01g800l_drz_sci_0.SPC.fits'
filrib = 'j8eb01g800l_drz_sci_0.STP.fits'
filimg = '../detectionImage.fits'
filcat = 'j8eb01g800l_drz_sci_0.cat'
rd_grdirct_cat, filcat, id, xim, yim, magauto, aim, bim, thetaim, w50, class
fits_read,filimg,img,hdr
posim = [[xim], [yim]] 
grism_triplot,272,img,id,posim,filspc,filrib,0,1,2,'stmp_','spec_','ribn_', hspc, hrib, stampim, bintab, rib
grism_triplot,273,img,id,posim,filspc,filrib,0,1,2,'stmp_','spec_','ribn_', hspc, hrib, stampim, bintab, rib


filspc = 'j8eb01g800l_drz_sci_1.SPC.fits'
filrib = 'j8eb01g800l_drz_sci_1.STP.fits'
filimg = '../detectionImage.fits'
filcat = 'j8eb01g800l_drz_sci_1.cat'
rd_grdirct_cat, filcat, id, xim, yim, magauto, aim, bim, thetaim, w50, class
fits_read,filimg,img,hdr
posim = [[xim], [yim]] 
grism_triplot,272,img,id,posim,filspc,filrib,0,1,2,'stmp_','spec_','ribn_', hspc, hrib, stampim, bintab, rib
grism_triplot,273,img,id,posim,filspc,filrib,0,1,2,'stmp_','spec_','ribn_', hspc, hrib, stampim, bintab, rib

;
filimg  = '../detectionImage.fits'
filspc = 'j8eb01g800l_drz_sci_1.SPC.fits'
filrib = 'j8eb01g800l_drz_sci_1.STP.fits'
filcat = 'j8eb01g800l_drz_sci_1.cat'
outdir = 'plots/'
mkall_grplots_0, filimg, filspc, filrib, filcat, outdir
filspc = 'j8eb01g800l_drz_sci_2.SPC.fits'
filrib = 'j8eb01g800l_drz_sci_2.STP.fits'
filcat = 'j8eb01g800l_drz_sci_2.cat'
mkall_grplots_0, filimg, filspc, filrib, filcat, outdir


;
filhtml = 'junk.html'
title   = 'test'
filcat  = 'j8eb01g800l_drz_sci_1.cat'
rd_grdirct_cat, filcat, id, xim, yim, magauto, aim, bim, thetaim, w50, class
cd,'plots'
posim = [[xim], [yim]] 
pstmp = 'stmp_'
pribn = 'ribn_'
pspec = 'spec_'
;
elm   = [45, 272,273,439,302]
;
grism_page, filhtml, title, elm, id, posim, aim, bim, thetaim, w50, magauto, class,  pstmp, pspec, pribn

