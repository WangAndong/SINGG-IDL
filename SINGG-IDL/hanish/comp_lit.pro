PRO comp_lit,PS=ps,TOTAL=total,NII=nii

  tflag = KEYWORD_SET(total)
  niiflag = KEYWORD_SET(nii)
  psflag = KEYWORD_SET(ps)

  fluxtable = !singgdir+"/flux_table.txt"

  readcol_new,fluxtable,ra,dec,name1,name2,hname,lit,hugs,hugsn2, $
              singg_f,singg_t,singg_h,singgn2, $
              COMMENT='#',/silent,FORMAT="A,A,A,A,A,F,F,F,F,F,F,F"
  num_table = N_ELEMENTS(name1)
  IF tflag THEN singg = singg_t ELSE singg = singg_f

  singgdb = "singg_derived"
  dbopen,singgdb,0
  dbext,-1,"NAME,NIICORR,FLAG_O,LOGF_HA_F,LOGF_HA_T,LOGF_HA_O,ERR_LOGF_HA_F,ERR_LOGF_HA_T,ERR_LOGF_HA_O",$
            NAME,NIICORR,FLAG_O,LOGF_HA_F,LOGF_HA_T,LOGF_HA_O,ERR_LOGF_HA_F,ERR_LOGF_HA_T,ERR_LOGF_HA_O
  derind = good_derived(exclopt=1)-1
  dbclose,dummy
  err = FLTARR(num_table)

; Instead of using the SINGG values from the file, use from the database.
for ii = 0,num_table-1 DO BEGIN
  ind = WHERE(STRTRIM(name[derind],2) EQ STRTRIM(hname[ii],2),count)
  if count ne 1 then begin
;    print,"no match: ",hname[ii],count,singg_f[ii]
    singg_f[ii] = -999.0
    singg_t[ii] = -999.0
  endif else begin
;   print,singg_f[ii]-ALOG10(1.0+singgn2[ii]),logf_ha_f[derind[ind[0]]], $
;         singg_t[ii]-ALOG10(1.0+singgn2[ii]),logf_ha_t[derind[ind[0]]]
;   print,singg_f[ii],logf_ha_f[derind[ind[0]]]-niicorr[derind[ind[0]]]/2.5, $
;         singg_t[ii],logf_ha_t[derind[ind[0]]]-niicorr[derind[ind[0]]]/2.5
    singg_f[ii] = logf_ha_f[derind[ind[0]]]-niicorr[derind[ind[0]]]/2.5
    singg_t[ii] = logf_ha_t[derind[ind[0]]]-niicorr[derind[ind[0]]]/2.5

    IF tflag THEN BEGIN
      singg[ii] = singg_t[ii]
      err[ii] = err_logf_ha_t[derind[ind[0]]]
    ENDIF ELSE BEGIN
      singg[ii] = singg_f[ii]
      err[ii] = err_logf_ha_f[derind[ind[0]]]
    ENDELSE

    IF STRTRIM(flag_o[derind[ind[0]]]) EQ "T" THEN BEGIN
      singg_f[ii] = logf_ha_o[derind[ind[0]]]-niicorr[derind[ind[0]]]/2.5
      singg_t[ii] = logf_ha_o[derind[ind[0]]]-niicorr[derind[ind[0]]]/2.5
      singg[ii] = logf_ha_o[derind[ind[0]]]-niicorr[derind[ind[0]]]/2.5
      err[ii] = err_logf_ha_o[derind[ind[0]]]
    ENDIF

    singgn2[ii] = 10.0^(niicorr[derind[ind[0]]]/(-2.5)) - 1.0
if strtrim(hname[ii],2) EQ 'J0409-56' THEN err[ii] = err[ii]
;    print,hname[ii],singg_f[ii],singg_t[ii],singgn2[ii],err[ii],flag_o[derind[ind[0]]]
  endelse
endfor

  IF niiflag THEN BEGIN
    index = WHERE(hugsn2 GT 0.0)
    litn2 = MEAN(hugsn2[index])
    PRINT,"Literature NII correction = ",litn2
  ENDIF

  IF NOT psflag THEN BEGIN
; In X windows, we can use color
    set_plot,'X'
    setplotcolors
    setbgfg,!white,!black
    DEVICE, RETAIN=2, DECOMPOSED=0
    WINDOW,XSIZE=600,YSIZE=1000
    !P.MULTI=[0,1,2,0,0]
    charsz = 2.0
    symsz = 1.5
    thick = 1.0

    ptclr = !black
    lineclr = !dblue
    fitclr = !red
  ENDIF ELSE BEGIN
    set_plot,'PS'
    setplotcolors
    xs = 4.5
    ys = 1.7*xs
    xoff = 1.2
    yoff = 3.0
    DEVICE,/portrait,/inches,xs=xs,ys=ys,yo=yoff,xo=xoff,/color, $
           bits_per_pixel=8,/encapsulated
    !P.MULTI=[0,1,2,0,0]

    charsz = 1.0
    symsz = 1.0
    thick = 2.0

; in PS, we need black-and-white
    ptclor = !black
    lineclr = !black
    fitclr = !ddgray ; not used in ps mode, anyway.
  ENDELSE

  ymin = -15.0
  ymax = -9.0
  dmin = 0.75
  dummy = FLTARR(2)

  IF niiflag THEN BEGIN
    index1 = WHERE(lit GT -20.0 AND singg GT -20.0 $
               AND hugsn2 GT 0.0 AND singgn2 GT 0.0, count1)
    index2 = WHERE(hugs GT -20.0 AND singg_h GT -20.0 $
               AND hugsn2 GT 0.0 AND singgn2 GT 0.0,count2)
    s1 = singg[index1] - ALOG10(1.0+singgn2[index1])
    l1 = lit[index1] - ALOG10(1.0+litn2)
    s2 = singg_h[index2] - ALOG10(1.0+singgn2[index2])
    h2 = hugs[index2] - ALOG10(1.0+hugsn2[index2])
  ENDIF ELSE BEGIN
    index1 = WHERE(lit GT -20.0 AND singg GT -20.0,count1)
    index2 = WHERE(hugs GT -20.0 AND singg_h GT -20.0,count2)
    s1 = singg[index1]
    l1 = lit[index1]
    s2 = singg_h[index2]
    h2 = hugs[index2]
  ENDELSE
  val = poly_fit(s1,l1,1)
  val2 = poly_fit(s2,h2,1)
  grm_avsigclip,(l1-s1),1000.0,20,mean1,sig1,nuse,nrej,nit,fixmean=0.0
  grm_avsigclip,(h2-s2),1000.0,20,mean2,sig2,nuse,nrej,nit,fixmean=0.0

  PRINT,"SINGG vs Literature: y=",val[1],"x + ",val[0]
  PRINT,"  (mean,sig,N)=",mean1,sig1,count1
  PRINT,"  (min,max)=",MIN(l1-s1),MAX(l1-s1)
  PRINT,"SINGG vs HUGS: y=",val2[1],"x + ",val2[0]
  PRINT,"  (mean,sig,N)=",mean2,sig2,count2
  PRINT,"  (min,max)=",MIN(h2-s2),MAX(h2-s2)

; Straight plot
  PLOT,dummy,dummy,XRANGE=[ymin,ymax],YRANGE=[ymin,ymax],CHARSIZE=charsz, $
       COLOR=!black,XSTYLE=1,YSTYLE=1, $
       THICK=thick,CHARTHICK=thick,XTHICK=thick,YTHICK=thick, $
       XTITLE="!3log(F!DH!4a!3(SINGG)!N [erg cm!U-2!N s!U-1!N])", $
       YTITLE="!3log(F!DH!4a!3(literature)!N [erg cm!U-2!N s!U-1!N])"
  PLOTS,[ymin,ymax],[ymin,ymax],COLOR=lineclr,THICK=thick
  IF NOT psflag THEN BEGIN
    PLOTS,[ymin,ymax],[(val[1]*ymin + val[0]),(val[1]*ymax + val[0])], $
          COLOR=fitclr,CLIP=[ymin,ymin,ymax,ymax],NOCLIP=0,LINESTYLE=1, $
          THICK=thick

    PLOTS,[ymin,ymax],[(val2[1]*ymin + val2[0]),(val2[1]*ymax + val2[0])], $
          COLOR=fitclr,CLIP=[ymin,ymin,ymax,ymax],NOCLIP=0,LINESTYLE=2, $
          THICK=thick
  ENDIF

  OPLOT,s1,l1,COLOR=ptclr,SYMSIZE=symsz,PSYM=SYM(6), $
       CLIP=[ymin,ymin,ymax,ymax],THICK=thick

  OPLOT,s2,h2,COLOR=ptclr,SYMSIZE=symsz,PSYM=SYM(1), $
       CLIP=[ymin,ymin,ymax,ymax],THICK=thick

  FOR ii = 0,count1-1 DO BEGIN
    PLOTS,s1[ii]+err[index1[ii]]*[-1.0,1.0],[l1[ii],l1[ii]], $
          COLOR=lineclr,THICK=thick,CLIP=[ymin,ymin,ymax,ymax],NOCLIP=0
  ENDFOR
  FOR ii = 0,count2-1 DO BEGIN
    PLOTS,s2[ii]+err[index2[ii]]*[-1.0,1.0],[h2[ii],h2[ii]], $
          COLOR=lineclr,THICK=thick,CLIP=[ymin,ymin,ymax,ymax],NOCLIP=0
  ENDFOR

; Difference plot
  PLOT,dummy,dummy,XRANGE=[ymin,ymax],YRANGE=[-dmin,dmin],CHARSIZE=charsz, $
       COLOR=!black,XSTYLE=1,YSTYLE=1,YTICKINTERVAL=0.25,YMINOR=5, $
       THICK=thick,CHARTHICK=thick,XTHICK=thick,YTHICK=thick, $
       XTITLE="!3log(F!DH!4a!3(SINGG)!N [erg cm!U-2!N s!U-1!N])", $
       YTITLE="!3log(F!DH!4a!3(literature)!N / F!DH!4a!3(SINGG)!N)"

  PLOTS,[ymin,ymax],[0.0,0.0],COLOR=lineclr,THICK=thick

  PLOTS,[ymin,ymax],[mean1,mean1],COLOR=lineclr,LINESTYLE=1,THICK=thick
  PLOTS,[ymin,ymax],[mean1+sig1,mean1+sig1],COLOR=lineclr,LINESTYLE=1, $
        CLIP=[ymin,-dmin,ymax,dmin],NOCLIP=0,THICK=thick
  PLOTS,[ymin,ymax],[mean1-sig1,mean1-sig1],COLOR=lineclr,LINESTYLE=1, $
        CLIP=[ymin,-dmin,ymax,dmin],NOCLIP=0,THICK=thick
  IF NOT psflag THEN PLOTS,[ymin,ymax], $
             [((val[1]-1.0)*ymin+val[0]),((val[1]-1.0)*ymax+val[0])], $
             COLOR=fitclr,LINESTYLE=1,THICK=thick
  OPLOT,s1,(l1-s1),COLOR=ptclr,SYMSIZE=symsz,PSYM=SYM(6), $
        CLIP=[ymin,-dmin,ymax,dmin],THICK=thick

  PLOTS,[ymin,ymax],[mean2,mean2],COLOR=lineclr,LINESTYLE=2,THICK=thick
  PLOTS,[ymin,ymax],[mean2+sig2,mean2+sig2],COLOR=lineclr,LINESTYLE=2, $
        CLIP=[ymin,-dmin,ymax,dmin],NOCLIP=0,THICK=thick
  PLOTS,[ymin,ymax],[mean2-sig2,mean2-sig2],COLOR=lineclr,LINESTYLE=2, $
        CLIP=[ymin,-dmin,ymax,dmin],NOCLIP=0,THICK=thick
  IF NOT psflag THEN PLOTS,[ymin,ymax], $
        [((val2[1]-1.0)*ymin+val2[0]),((val2[1]-1.0)*ymax+val2[0])], $
        COLOR=fitclr,LINESTYLE=2,THICK=thick
  OPLOT,s2,(h2-s2),COLOR=ptclr,SYMSIZE=symsz,PSYM=SYM(1), $
        CLIP=[ymin,-dmin,ymax,dmin],THICK=thick

  FOR ii = 0,count1-1 DO BEGIN
    PLOTS,s1[ii]+err[index1[ii]]*[-1.0,1.0],(l1[ii]-s1[ii])*[1.0,1.0], $
          COLOR=lineclr,THICK=thick,CLIP=[ymin,-dmin,ymax,dmin],NOCLIP=0
    PLOTS,s1[ii]*[1.0,1.0],(l1[ii]-s1[ii])+err[index1[ii]]*[-1.0,1.0], $
          COLOR=lineclr,THICK=thick,CLIP=[ymin,-dmin,ymax,dmin],NOCLIP=0
  ENDFOR
  FOR ii = 0,count2-1 DO BEGIN
    PLOTS,s2[ii]+err[index2[ii]]*[-1.0,1.0],(h2[ii]-s2[ii])*[1.0,1.0], $
          COLOR=lineclr,THICK=thick,CLIP=[ymin,-dmin,ymax,dmin],NOCLIP=0
    PLOTS,s2[ii]*[1.0,1.0],(h2[ii]-s2[ii])+err[index2[ii]]*[-1.0,1.0], $
          COLOR=lineclr,THICK=thick,CLIP=[ymin,-dmin,ymax,dmin],NOCLIP=0
  ENDFOR

  IF psflag THEN BEGIN
    outfile=!outdir+"litplot"
    IF tflag THEN outfile=outfile + "_t"
    IF niiflag THEN outfile = outfile + "_nii"
    outfile=outfile + ".eps"
    PRINT,"Writing to ",outfile
    psend,outfile,/noprint,/clobber
  ENDIF

  RETURN

END
