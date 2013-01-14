How to add more bands:

1) Download the images. MAKE SURE THAT THE FOV IS GREATER THAN THE ORIGINAL OPTICAL IMAGE (about 700 arcsec), otherwise strange things may happen (the first thing that will go wrong is the sky box plots will have a big square taken out of them). Give them descriptive filenames (I am looking at you, WISE).
2) Create a mask.
3) Add your new bands to ssoup_initvars
4) Tell make_ssoupin where to find the images and masks.
5) Edit ssoup_align.
6) Put your band-specific FITS header parsing/renaming in ssoup_atidyhdr.pro.
7) Create a template header in soup (same directory as ssoup_atidyhdr.pro) called XX_templ_hdr.dat and tell ssoup_atidyhdr.pro where to find it.