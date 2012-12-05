PRO shunt_html_out, filhtml, fcat, title, id, xsq, ysq, spec_class, mag, ra, dec, $
                    pfx_plots, pfx_dstamp=pfx_dstamp
   ;
   ; Make html output to display shunt products
   ;
   ; filhtml    -> name of file for html output
   ; fcat       -> name of acsii catalog, a link is made to this file
   ; title      -> title for web page
   ; id         -> array of id numbers
   ; xsq,ysq    -> pixel position in squashed grism image
   ; spec_class -> eye spectroscopic classification
   ; mag        -> grism instrumental mag
   ; ra,dec     -> estimated world coords of source (deg)
   ; pfx_plots  -> prefix for jpg and ps iamges of multi-panel shunt
   ;               diagnostic plots
   ; pfx_dstamp -> prefix for direct image stamps.  If not set then 
   ;               no column is made for direct image stamps.
   ;
   ; G. Meurer 09/2005
   openw, unitp, filhtml, /get_lun, /more
   shunt_pagetop, unitp, title
   shunt_class_key, unitp
   shunt_fcat_link, unitp, fcat
   shunt_thead, unitp, title, dodstamp=keyword_set(pfx_dstamp)
   ;
   nn = n_elements(id)
   FOR ii = 0, nn-1 DO shunt_html_row, unitp, id[ii], xsq[ii], ysq[ii], spec_class[ii], mag[ii], ra[ii], dec[ii], $
    pfx_plots, pfx_dstamp=pfx_dstamp
   ;
   shunt_tfoot, unitp
   shunt_fcat_link, unitp, fcat
   shunt_pagebot, unitp
   free_lun, unitp

END 
