PRO calc_ew_all

; Calculate EW for full sample.
; Math: for each source, find where fintnet/flux_t = 0.5... which
; should just be the bin of r_e.

  dbopen,"singg_flux",0
  dbext,-1,"object,galindex,filename,imtype,se_f_brt,ew50_f_brt", $
            object,galindex,filename,imtype,se_t_brt,ew50_t_brt
  dbclose

  Sindex = WHERE(STRTRIM(imtype,2) EQ "net",count)

  FOR ii = 0,count-1 DO BEGIN
    Rindex = WHERE(STRTRIM(object,2) EQ STRTRIM(object[Sindex[ii]],2) AND $
                   galindex EQ galindex[Sindex[ii]] AND $
                   STRTRIM(imtype,2) EQ "cont",Rcount)

    IF Rcount NE 1 THEN BEGIN
      PRINT,"ERROR: wrong number of R matches",Rcount,object[Sindex[ii]],galindex[Sindex[ii]]
PRINT,filename[Rindex]
    ENDIF

    IF Rcount EQ 1 THEN BEGIN
      PRINT,object[Sindex[ii]],ew50_t_brt[Sindex[ii]],(se_t_brt[Sindex[ii]]/se_t_brt[Rindex[0]])
    ENDIF ELSE BEGIN
      FOR jj = 0,Rcount-1 DO BEGIN
        IF STRPOS(filename[Sindex[ii]],"_Rsub") GE 0 THEN $
          test = STRPOS(filename[Rindex],"_R") ELSE $
          test = STRPOS(filename[Rindex],"_C")

        index2 = WHERE(test GT 0,count2)
        IF count2 GT 1 THEN BEGIN
          PRINT,"ERROR!!!",count2
          RETURN
        ENDIF
        PRINT,object[Sindex[ii]],ew50_t_brt[Sindex[ii]],(se_t_brt[Sindex[ii]]/se_t_brt[Rindex[index2[0]]])
      ENDFOR
    ENDELSE
 
  ENDFOR

END
