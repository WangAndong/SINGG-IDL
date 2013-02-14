== Standalone usage instructions==
List of standalone procedures: ssoup_plotsprofs, ssoup_plotsprofs_mir, ssoup_plotboxes

To get something running, type:

IDL> soup,"IDL command line"

Examples:

IDL> ssoup,"ssoup_plotsprofs,-1,'J0221-05','test_%d.jpg','test_%d.jpg',/epilepsy"
IDL> ssoup,"ssoup_inputs, 'ssoup.in', -1, inputstr & ssoup_mkhtml,-1, '.', '.', 'HTML', inputstr, 2"

For more information, see:

IDL> ?execute

==How to add more bands==

1) Download the images. MAKE SURE THAT THE FOV IS GREATER THAN THE ORIGINAL OPTICAL IMAGE (about 800 arcsec), otherwise strange things may happen (the first thing that will go wrong is the sky box plots will have a big square taken out of them). The WISE images have a FOV of 1000 arcsec. Give them descriptive filenames (I am looking at you, WISE).
2) Create a mask.
3) Add your new bands to ssoup_initvars
4) Tell make_ssoupin where to find the images and masks.
5) Edit ssoup_align to process the new images.
6) Put your band-specific FITS header parsing/renaming in ssoup_atidyhdr.pro and ssoup_addphotkwds.pro.
7) Create a template header in soup (same directory as ssoup_atidyhdr.pro) called XX_templ_hdr.dat and tell ssoup_atidyhdr.pro where to find it.
8) Dump your photflams into ssoup_addphotkwds.pro, if they aren't already in the FITS header.
9) Choose a low cut and high cut threshold for JPGs in ssoup_mkjpg.pro.
10) Add AB/Vega offsets if necessary to ssoup_cp_calcmags.pro.

==Save set structures==
Please note that we use pointers to store some array of unknown (and not fixed) size at "compile" time.

===Sky model===
skyboxstr = { $
    order      : 0              ,$ ; polynomial order
    boxsize    : 0              ,$ ; size of box in pixels
    type       : 0              ,$ ; type of fit (0=2d polynomial in x,y, 1=polynomial in radius)
    skylev     : 0.0            ,$ ; box to box sky level
    skysig     : 0.0            ,$ ; pixel to pixel sky RMS
    skysigbox  : 0.0            ,$ ; box to box sky RMS
    params     : ptr_new(!null) ,$ ; fit parameters (in order: 1, x, y, x^2, xy, y^2, ...)
    params_err : ptr_new(!null) ,$ ; error in params
    average    : ptr_new(!null) ,$ ; average
    x          : ptr_new(!null) ,$ ; box x position
    y          : ptr_new(!null) ,$ ; box y position
    sigma      : ptr_new(!null) ,$ ; detection in sigma(?)
    fit        : ptr_new(!null) ,$ ; fitted value
    residual   : ptr_new(!null)  $ ; residual from fit
}
skymodeldata = replicate(skyboxstr, nbandavail)
bname = bandavail
save,filename=inputstr.hname+"_skymodel.save",bname,skymodeldata

===Profiles===
profilestr = { $
      radius                      : ptr_new(!null),     $ ; ?
      radius_int                  : ptr_new(!null),     $ ; like radius, but for integrated quantities
      mprof                       : ptrarr(nbandavail), $ ; surface brightness profile, corresponds (like everything below) 1-1 with bname
      err_mprof                   : ptrarr(nbandavail), $ ; total error in mprof
      mprof_int                   : ptrarr(nbandavail), $ ; integrated (enclosed) surface brightness profile
      err_mprof_int               : ptrarr(nbandavail), $ ; total error in mprof_int
      mprof_dustcor               : ptrarr(nbandavail), $ ; surface brightness profile corrected for dust
      err_mprof_dustcor           : ptrarr(nbandavail), $ ; total error in mprof_dustcor
      mprof_dustcor_int           : ptrarr(nbandavail), $ ; integrated surface (enclosed) brightness profile corrected for dust
      err_mprof_dustcor_int       : ptrarr(nbandavail), $ ; total error in mprof_dustcor_int
      col_fuv_nuv                 : ptr_new(!null),     $ ; FUV-NUV color index (uncorrected for dust)
      err_col_fuv_nuv             : ptr_new(!null),     $ ; error in FUV-NUV color index
      col_nuv_r                   : ptr_new(!null),     $ ; NUV-R color index (uncorrected for dust)
      err_col_nuv_r               : ptr_new(!null),     $ ; error in FUV-NUV color index
      log_ha_r                    : ptr_new(!null),     $ ; log(flux in Ha/flux in R)
      err_log_ha_r                : ptr_new(!null),     $ ; error in above
      log_ha_fuv                  : ptr_new(!null),     $ ; log(flux in Ha/flux in FUV)
      err_log_ha_fuv              : ptr_new(!null),     $ ; error in above
      col_fuv_nuv_int             : ptr_new(!null),     $ ; integrated FUV-NUV color index (uncorrected for dust)
      err_col_fuv_nuv_int         : ptr_new(!null),     $ ; error in FUV-NUV color index
      col_nuv_r_int               : ptr_new(!null),     $ ; integrated NUV-R color index (uncorrected for dust)
      err_col_nuv_r_int           : ptr_new(!null),     $ ; error in FUV-NUV color index
      log_ha_r_int                : ptr_new(!null),     $ ; integrated log(flux in Ha/flux in R)
      err_log_ha_r_int            : ptr_new(!null),     $ ; error in above
      log_ha_fuv_int              : ptr_new(!null),     $ ; integrated log(flux in Ha/flux in FUV)
      err_log_ha_fuv_int          : ptr_new(!null),     $ ; error in above
      col_fuv_nuv_dustcor         : ptr_new(!null),     $ ; FUV-NUV color index (corrected for dust)
      err_col_fuv_nuv_dustcor     : ptr_new(!null),     $ ; error in FUV-NUV color index
      col_nuv_r_dustcor           : ptr_new(!null),     $ ; NUV-R color index (corrected for dust)
      err_col_nuv_r_dustcor       : ptr_new(!null),     $ ; error in FUV-NUV color index
      log_ha_r_dustcor            : ptr_new(!null),     $ ; log(flux in Ha/flux in R)
      err_log_ha_r_dustcor        : ptr_new(!null),     $ ; error in above
      log_ha_fuv_dustcor          : ptr_new(!null),     $ ; log(flux in Ha/flux in FUV)
      err_log_ha_fuv_dustcor      : ptr_new(!null),     $ ; error in above
      col_fuv_nuv_dustcor_int     : ptr_new(!null),     $ ; integrated FUV-NUV color index (corrected for dust)
      err_col_fuv_nuv_dustcor_int : ptr_new(!null),     $ ; error in FUV-NUV color index
      col_nuv_r_dustcor_int       : ptr_new(!null),     $ ; integrated NUV-R color index (corrected for dust)
      err_col_nuv_r_dustcor_int   : ptr_new(!null),     $ ; error in FUV-NUV color index
      log_ha_r_dustcor_int        : ptr_new(!null),     $ ; integrated dust corrected log(flux in Ha/flux in R)
      err_log_ha_r_dustcor_int    : ptr_new(!null),     $ ; error in above
      log_ha_fuv_dustcor_int      : ptr_new(!null),     $ ; integrated dust corrected log(flux in Ha/flux in FUV)
      err_log_ha_fuv_dustcor_int  : ptr_new(!null)      $ ; error in above
}
allprofiles = replicate(profilestr, ngal) ; one entry for each galaxy
bname = bandavail
save,filename=hname+"_profiles.save",bname,allprofiles