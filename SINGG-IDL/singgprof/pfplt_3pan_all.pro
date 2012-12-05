PRO pfplt_3pan_all, inlist, outlist
   pixsize = 0.43
   ;
   ; Plot 3panel profiles of all files in inlist
   readcol, inlist, filnet, filcnt, title, format='(a,a,a)'
   nf       = n_elements(filnet)
   openw, lu, outlist, /get_lun
   FOR i = 0, nf-1 DO BEGIN 
      filenet = filnet[i]
      ;
      ; read net profile, figure out how many profiles to plot
      pfplt_rdhdr, filenet, pixsize, filenamenet, funitsnet, fscalenet, fluxcalnet, $
       ptypenet, numgalsnet, gindexnet, xcenternet, ycenternet, axeratnet, posangnet, $
       posangwcnet, skylevnet, skysigpxnet, skysigbxnet, fluxradnet, fluxnet, flsigskynet, $
       flsigcntnet, hlradiusnet, hlsigskynet, hlcrcfluxnet, lstartnet, lendnet, isoflagnet, netflag
      ;
      ;
      ng = n_elements(gindexnet)
      FOR j = 0, ng-1 DO BEGIN 
         gind       = gindexnet[j]
         IF ng GT 1 THEN BEGIN 
            title_full = title[i]+':s'+strtrim(string(gind+1),2) 
            hardfile   = title[i]+'_s'+strtrim(string(gind+1),2)+'_3pan.ps'
         ENDIF ELSE BEGIN 
            title_full = title[i]
            hardfile   = title[i]+'_3pan.ps'
         ENDELSE 
         ;
         keywait, 'Type any key for plot of '+title_full
         ;
         cmd      = '~/bin/mb_ps2png '+hardfile
         pfplt_3pan, filnet[i], filcnt[i], gind, title_full, rad, ew, err
         pfplt_3pan, filnet[i], filcnt[i], gind, title_full, rad, ew, err, hardfile=hardfile
         spawn, cmd
         printf, lu, title[i], rad, ew, err
      ENDFOR 
   ENDFOR 
   free_lun, lu
END 
