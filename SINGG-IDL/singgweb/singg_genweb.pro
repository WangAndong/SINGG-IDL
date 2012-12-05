PRO singg_genweb, sampfil, obspage, samppl, samptab, title, subtitle, $
                  jpgflag=jpgflag, imgdir=imgdir
   IF keyword_set(jpgflag) THEN usejpg = jpgflag ELSE usejpg = 0b 

;
; read sample
;
   singg_readsamp,sampfil,objname,cat,rastr,decstr,nednam,peak,sdv,w50,vhel,vshap,obstr,$
    logmhi,l,b,d,ra,dec,obs
  
   nminicol = 5
   nminirow = 4
   nminitab = nminicol*nminirow
   ntabs    = fix((n_elements(objname) + nminitab - 1)/nminitab)
   nobjcell = ntabs*nminitab

   singg_def_filts,vfilt1,vfilt2,filtnam,colorcode,vhel,filtid
;
; Open index web page.  
; Write index page table header
;
   indpage='index.html'
   OPENW, indunit, indpage, /GET_LUN, /MORE  

   singg_pagetop, indunit, title, subtitle
   singg_observed_link,indunit,obspage
   singg_histpl_links,indunit,samppl,pfile=samptab
   singg_indhead, indunit, nminicol, title, sampfil

   i = -1
   FOR k = 1, ntabs DO BEGIN 
      tabpage = 'sample'+strn(k,length=3,padchar='0')+'.html'
      openw, tabunit, tabpage, /GET_LUN, /MORE
      subtitle = 'Page '+strn(k)
      singg_pagetop, tabunit, title, subtitle
      singg_tnavbar, tabunit, k, ntabs, sampfil
      singg_observed_link,tabunit,obspage
      singg_histpl_links,tabunit,samppl,pfile=samptab
      singg_thead, tabunit, title, subtitle
;
      singg_indsumcol, indunit, nminirow, k, tabpage
      FOR j = 0, nminitab-1 DO BEGIN
         i = i + 1
         IF (i LE n_elements(objname)-1) THEN BEGIN 
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
            singg_tabrow, tabunit, colrow, objname[i], rastr[i], decstr[i], l[i], b[i], $
             vhel[i], w50[i], peak[i], sdv[i], d[i], logmhi[i], filter, obstr[i], $
             cat[i], nednam[i]
;
; create individual web page
;
            fndpage=objname[i]+'.html'
            OPENW, fndunit, fndpage, /GET_LUN, /MORE  
            singg_objpage, fndunit, colrow, objname[i], rastr[i], decstr[i], l[i], b[i], $
             vhel[i], w50[j], peak[i], sdv[i], d[i], logmhi[i], filter, obstr[i], $
             cat[i], nednam[i], imgfound, jpgflag=usejpg, imgdir=imgdir
            singg_pagebot, fndunit
            FREE_LUN,fndunit
;
; Make entry in "minitable"
;
            singg_indminicell, indunit, j, nminicol, objname[i], colrow, 1 
         ENDIF ELSE BEGIN 
            singg_indminicell, indunit, j, nminicol, '&nbsp;', '#fffff', 0 
         ENDELSE 
      ENDFOR 
;
      singg_tfoot, tabunit
      singg_pagebot, tabunit
      FREE_LUN, tabunit

   ENDFOR 

   singg_tfoot, indunit

   singg_observed_link,indunit,obspage
   singg_histpl_links,indunit,samppl

   singg_pagebot, indunit
   FREE_LUN, indunit

end
