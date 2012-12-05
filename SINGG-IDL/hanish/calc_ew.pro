FUNCTION calc_ew,cntfile,hafile,netfile,err_ew50,ew50rad, $
                 TOTAL=total,VERBOSE=verbose,PIXSIZE=pixsize
; For a given galaxy, return an array of EWs, one for each source
; Only the R and Rsub images are used for this.

; 0.432 for 1.5m, 0.396 for 0.9m, 2.3184 for Schmidt, 4.5655 for 2.3m
   IF NOT KEYWORD_SET(pixsize) THEN pixsize  = 0.432

   ferrclip = 0.0
   nsig     = 2.0
   netflag  = 0b
   
   read_profile_header,netfile,netstr,LSTART=lstartnet,LEND=lendnet, $
                       NETFLAG=netflag,/silent
   read_profile_header,cntfile,cntstr,LSTART=lstartcnt,LEND=lendcnt, $
                       NETFLAG=cntflag,/silent
   read_profile_header,hafile,hastr,LSTART=lstartha,LEND=lendha, $
                       NETFLAG=haflag,/silent

   max_gals = netstr[0].numgals

   ew50 = FLTARR(max_gals)
   ew50rad = FLTARR(max_gals)
   err_ew50 = FLTARR(max_gals)

   FOR jj = 0,max_gals-1 DO BEGIN
;;     gn = jj
     gc = WHERE(cntstr.galindex EQ netstr[jj].galindex,ngc)
     gh = WHERE(hastr.galindex EQ netstr[jj].galindex,ngh)
     IF ngc NE 1 OR ngh NE 1 THEN BEGIN
       IF KEYWORD_SET(verbose) THEN BEGIN
         PRINT,'WARNING in calc_ew: cannot find unique profile ',ngc,ngh
         PRINT,'Source #',jj+1
         PRINT,'Files: ',cntfile," ",hafile," ",netfile
       ENDIF
       ew50[jj] = -999.0
       ew50rad[jj] = -999.0
       err_ew50[jj] = -999.0
     ENDIF ELSE BEGIN
       lstartn = lstartnet[jj]
       lendn   = lendnet[jj]
       lstartc = lstartcnt[gc[0]]
       lendc   = lendcnt[gc[0]]
       lstarth = lstartha[gh[0]]
       lendh   = lendha[gh[0]]
       fscalecnt = cntstr[gc[0]].flux_scale
       fscaleha = hastr[gh[0]].flux_scale
       fscalenet = netstr[jj].flux_scale

       pfplt_extractprof,netfile,netflag,smanet,fintnet,dfintnet,ngoodnet, $
                         nbadnet,sbnet,esbnet,dfrawnet,efintsknet,efintcnnet, $
                         pixsize=netstr[jj].pixsize,fscale=fscalenet, $
                         ferrclip=ferrclip,lstart=lstartn,lend=lendn

       pfplt_extractprof,cntfile,cntflag,smacnt,fintcnt,dfintcnt,ngoodcnt, $
                         nbadcnt,sbcnt,esbcnt,dfrawcnt,efintskcnt,efintcncnt, $
                         pixsize=cntstr[gc[0]].pixsize,fscale=fscalecnt, $
                         ferrclip=ferrclip,lstart=lstartc,lend=lendc

       pfplt_extractprof,hafile,haflag,smaha,fintha,dfintha,ngoodha, $
                         nbadha,sbha,esbha,dfrawha,efintskha,efintcnha, $
                         pixsize=hastr[gh[0]].pixsize,fscale=fscaleha, $
                         ferrclip=ferrclip,lstart=lstarth,lend=lendh

; clip profiles
       kk = WHERE((sbnet GE nsig*netstr[jj].err_sky_box OR $
                   sbcnt GE nsig*cntstr[gc[0]].err_sky_box) AND $
                   fintcnt GT 0.0 AND fintnet GT 0.0, nkk)
       IF nkk GE 1 THEN BEGIN 
         smanet     = smanet[kk]
         fintnet    = fintnet[kk]
         efintsknet = efintsknet[kk]
         efintcnnet = efintcnnet[kk]
         fscalenet  = fscalenet[kk]

         fintcnt    = fintcnt[kk]
         efintskcnt = efintskcnt[kk]
         efintcncnt = efintcncnt[kk] ; This should be zero.

         fintha = fintha[kk]
         efintskha = efintskha[kk]
         fscaleha   = fscaleha[kk]

; calculate COGs
         IF KEYWORD_SET(total) THEN cognet = fintnet / netstr[jj].flux_t $
                               ELSE cognet = fintnet / netstr[jj].flux_f
         sortarr         = SORT(ABS(cognet - 0.5))

         ew         = fintnet / fintcnt

;; double-check math here.
         err_ew = SQRT(((efintskha/fintcnt)*(fscalenet/fscaleha))^2 + $
                       ((efintskcnt*fintha/fintcnt^2)*(fscalenet/fscaleha))^2 + $
                       (efintcnnet/fintcnt)^2)

         ew50rad[jj]  = smanet[sortarr[0]]
         ew50[jj]     = ew[sortarr[0]]
         err_ew50[jj] = err_ew[sortarr[0]]
       ENDIF ELSE BEGIN
         IF KEYWORD_SET(verbose) THEN BEGIN
           PRINT,'WARNING in calc_ew: no valid curve points '
           PRINT,'Source #',jj+1
           PRINT,'Files: ',cntfile," ",hafile," ",netfile
         ENDIF
         ew50[jj] = -999.0
         ew50rad[jj] = -999.0
         err_ew50[jj] = -999.0
       ENDELSE
     ENDELSE
   ENDFOR

   RETURN,ew50
END
