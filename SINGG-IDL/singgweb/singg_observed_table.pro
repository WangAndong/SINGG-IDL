PRO singg_observed_table,obsfil,samptab,outpage

   date=systime()

   title='SINGG: observed galaxies'
   subtitle='updated on ' + date

;
; read sample
;
   singg_readsamp,obsfil,objname,cat,rastr,decstr,nednam,peak,sdv,w50,vhel,vshap,obstr,$
    logmhi,l,b,d,ra,dec,obs

   singg_def_filts,vfilt1,vfilt2,filtnam,colorcode,vhel,filtid

   openw, outunit, outpage, /GET_LUN, /MORE
   singg_pagetop, outunit, title, subtitle
   singg_sample_link,outunit,samptab
   singg_thead, outunit, title, subtitle

   FOR i = 0, n_elements(objname)-1 DO BEGIN 
;
; assign best filter, color
;
      filter = 'unknown'
      colrow = '#ffffff'
      IF filtid[i] GE 0 THEN BEGIN 
         filter = filtnam[filtid[i]]
         colrow = colorcode[filtid[i]]
      ENDIF 
;
; table entry
;
      singg_tabrow, outunit, colrow, objname[i], rastr[i], decstr[i], l[i], b[i], $
       vhel[i], w50[i], peak[i], sdv[i], d[i], logmhi[i], filter, obstr[i], $
       cat[i], nednam[i]
;

   ENDFOR 

   singg_tfoot, outunit

   singg_sample_link,outunit,samptab

   singg_pagebot, outunit
   FREE_LUN, outunit

END

