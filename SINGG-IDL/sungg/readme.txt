How to add more bands:

1) Download the images. MAKE SURE THAT THE FOV IS GREATER THAN THE ORIGINAL OPTICAL IMAGE (about 800 arcsec), otherwise strange things may happen (the first thing that will go wrong is the sky box plots will have a big square taken out of them). The WISE images have a FOV of 1000 arcsec. Give them descriptive filenames (I am looking at you, WISE).
2) Create a mask.
3) Add your new bands to ssoup_initvars
4) Tell make_ssoupin where to find the images and masks.
5) Edit ssoup_align to process the new images.
6) Put your band-specific FITS header parsing/renaming in ssoup_atidyhdr.pro and ssoup_addphotkwds.pro.
7) Create a template header in soup (same directory as ssoup_atidyhdr.pro) called XX_templ_hdr.dat and tell ssoup_atidyhdr.pro where to find it.
8) Dump your photflams into ssoup_addphotkwds.pro, if they aren't already in the FITS header.
9) Choose a low cut and high cut threshold for JPGs in ssoup_mkjpg.pro.