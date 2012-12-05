PRO read_profile_header,file,profstr,DELSKY=delsky,SILENT=silent, $
                  LSTART=lstart,LEND=lend,NETFLAG=netflag,SCALE=scale
; Parses a profile file, storing the entries found inside in a structure.
; INPUT
;   file           The .profile file
; OPTIONAL INPUT
;   /silent        Suppress warning messages
;   /delsky        Amount to shift surface brightnesses due to sky model
; OUTPUT
;   profstr        A structure containing the extracted elements
; OPTIONAL OUTPUTS
;   lstart         Start line for the block-o-data for this object
;   lend           End line of block-o-data
;   netflag        1b for subtracted images, 0b for others

  ;readcol_new,file,keyword,keyval,FORMAT="X,A,X,A",SKIPLINE=1,/SILENT
  readcol,file,keyword,keyval,FORMAT="X,A,X,A",SKIPLINE=1,/SILENT

  proftype = keyval[WHERE(keyword EQ "PROFTYPE")]
  numgals = LONG(keyval[WHERE(keyword EQ "NUMGALS")])
  filename = keyval[WHERE(keyword EQ "FILENAME")]
  netflag = (STRPOS(filename[0],"sub") GT 0)
  delflag = KEYWORD_SET(delsky)

  IF numgals[0] EQ 0 THEN BEGIN
    PRINT,"ERROR in read_profile_header: no galaxies in file",file
    RETURN
  ENDIF

; This next part's a bit crude.  It assumes there's a thrown-out line
; in readcol_new above (SKIPLINE), a blank line that's ignored, and
; then one "# " blank comment line for each source.
  lstart = (WHERE(keyword EQ "all" AND keyval EQ "and"))+INDGEN(numgals[0])+5
  lend = INTARR(numgals[0])
  endcheck = WHERE(keyword EQ "GALINDEX")
  FOR ii = 0,numgals[0]-2 DO BEGIN
    lend[ii] = endcheck[ii+1]+ii+3
  ENDFOR
  lend[numgals[0]-1] = N_ELEMENTS(keyword)+numgals[0]+1

  dbfile = !singgdir+"/singg_flux.dbd"
; Make a modified structure, without the usual _ISO/_BRT duplication
  ;readcol_new,dbfile,nametemp,typetemp,commenttemp, $
  ;            FORMAT="A,A,A",comment='#',SKIPLINE=6,/SILENT,/QUOTES
  readcol,dbfile,nametemp,typetemp,commenttemp, $
              FORMAT="A,A,A",comment='#',SKIPLINE=6,/SILENT
  bpos = STRPOS(nametemp,"_BRT")
  ipos = STRPOS(nametemp,"_ISO")

; A few variables will have _ISO with the corresponing BRT variable
; missing its suffix, so we assume the BRT is the default...
  good = WHERE((STRLEN(commenttemp) GT 0 AND ipos LT 0), num_vars)
  varname = STRARR(num_vars)
  vartype = STRARR(num_vars)

  FOR ii = 0,num_vars-1 DO BEGIN
    IF bpos[good[ii]] LT 0 THEN varname[ii] = nametemp[good[ii]] $
                     ELSE varname[ii] = STRMID(nametemp[good[ii]],0,bpos[good[ii]])
    IF STRLEN(typetemp[good[ii]]) GE 2 THEN varbyte = STRMID(typetemp[good[ii]],2,1) $
                                       ELSE varbyte = 1
    CASE STRMID(typetemp[good[ii]],0,1) OF
      "C": vartype[ii] = "A"
      "B": vartype[ii] = "B"
      "I": IF varbyte GE 4 THEN vartype[ii] = "J" $
                           ELSE vartype[ii] = "I"
      "R": IF varbyte GE 8 THEN vartype[ii] = "D" $
                           ELSE vartype[ii] = "F"
      ELSE: IF NOT KEYWORD_SET(silent) THEN PRINT,"WARNING: invalid variable type: ",STRMID(typetemp[good[ii]],0,1)
    ENDCASE 
  ENDFOR

; Unique structure name
  title = STRTRIM("header_"+proftype[0],2)

  create_struct2,strtemp,title,varname,vartype
  clear_struct,strtemp,vartype
  profstr = REPLICATE(strtemp,numgals[0])

  FOR ii = 0,num_vars-1 DO BEGIN
    index = WHERE(STRTRIM(keyword,2) EQ STRTRIM(varname[ii],2),count)
    IF count GT 0 THEN BEGIN
      val = keyval[index]

      CASE STRMID(vartype[ii],0,1) OF
        "A": dumpval = val
        "B": BEGIN
               type = SIZE(val,/TYPE)
               CASE type OF
                 1: dumpval = val
                 7: dumpval = STRUPCASE(STRMID(val,0,1)) EQ "T"
                 ELSE: dumpval = BYTE(val)
               ENDCASE
             END
        "I": dumpval = FIX(val)
        "J": dumpval = LONG(val)
        "F": dumpval = FLOAT(val)
        "D": dumpval = DOUBLE(val)
        ELSE: BEGIN
               PRINT,"ERROR in read_profile_header: unknown type ",varname[ii]
               RETURN
             END
      ENDCASE

; Patch for photometry override
      IF KEYWORD_SET(scale) AND STRTRIM(varname[ii],2) EQ 'FLUX_SCALE' THEN BEGIN
        IF NOT KEYWORD_SET(silent) THEN PRINT,scale,dumpval,scale/dumpval
        dumpval = scale
      ENDIF

; If it's in the first few variables, it's the top header part that
; applies to ALL sources equally.
      IF count EQ 1 AND ii LT 12 THEN BEGIN
        profstr[*].(ii) = dumpval[0]
      ENDIF ELSE BEGIN
        FOR jj = 0,count-1 DO BEGIN
          kk = WHERE(lstart GE index[jj])
; kk[0] will be the lowest-numbered galaxy whose data section is
; after that line.  Note that it's 0:(N-1), not 1:N.
          profstr[kk[0]].(ii) = dumpval[jj]
 
; Patch units automatically.  If a value is negative, don't adjust it;
; typically, this is the -999 default NaN code.  If you're going to be
; using values that can legitimately be negative, you'll need to
; rewrite this, but hey, that's your problem.  Nyah nyah nyah.
; Note that this should NOT be done for the seven "header" lines.
          IF STRMID(vartype[ii],0,1) NE "A" AND $
             STRMID(vartype[ii],0,1) NE "B" THEN BEGIN
; If it's a character string or a byte it won't have units, period.
            lbracket = STRPOS(commenttemp[good[ii]],"[",/REVERSE_SEARCH)
            IF lbracket GT 0 AND profstr[kk[0]].(ii) GT -998.0 THEN BEGIN
              IF STRPOS(commenttemp[good[ii]],"[erg/cm^2") GT 0 THEN $
                 profstr[kk[0]].(ii) = profstr[kk[0]].(ii) * profstr[kk[0]].flux_scale
              IF STRPOS(commenttemp[good[ii]],"/arcsec^2]") GT 0 THEN $
                 profstr[kk[0]].(ii) = profstr[kk[0]].(ii) / (profstr[kk[0]].pixsize^2)
              IF STRPOS(commenttemp[good[ii]],"[arcsec]") GT 0 AND $
                 STRTRIM(varname[ii],2) NE "PIXSIZE" THEN $
                 profstr[kk[0]].(ii) = profstr[kk[0]].(ii) * profstr[kk[0]].pixsize
            ENDIF
          ENDIF
        ENDFOR
      ENDELSE
    ENDIF ELSE BEGIN
; If it's not a variable used in the profile files, it's up to the
; user to fill it in later.
      IF NOT KEYWORD_SET(silent) THEN PRINT,"WARNING in read_profile_header: missing variable ",varname[ii]
    ENDELSE
  ENDFOR

  FOR jj = 0,numgals[0]-1 DO BEGIN
; Patch for delsky
    IF delflag THEN BEGIN
      del = delsky * profstr[jj].flux_scale / (profstr[jj].pixsize ^ 2.0)
      profstr[jj].skylev = profstr[jj].skylev - del

      area_s = !pi * (profstr[jj].fluxrad_s^2.0) / profstr[jj].axerat
      area_f = !pi * (profstr[jj].fluxrad_f^2.0) / profstr[jj].axerat
      area_c = !pi * (profstr[jj].fluxrad_c^2.0) / profstr[jj].axerat
      area_t = area_f * (profstr[jj].err_flux_t_sky/profstr[jj].err_flux_f_sky)

      profstr[jj].flux_s = profstr[jj].flux_s - del*area_s
      profstr[jj].flux_f = profstr[jj].flux_f - del*area_f
      profstr[jj].flux_t = profstr[jj].flux_t - del*area_t

      profstr[jj].se_f = profstr[jj].se_f - del
      profstr[jj].se_t = profstr[jj].se_t - del

      profstr[jj].delsky = del
    ENDIF ELSE BEGIN
      profstr[jj].delsky = 0.0
    ENDELSE

; Assemble the final few values we need by extracting from the data
; tables themselves.

    ferrclip = 0.0
    pfplt_extractprof,file,netflag,sma,fint,dfint,ngood, $
                      nbad,sb,esb,dfraw,efintsk,efintcn, $
                      pixsize=profstr[jj].pixsize, $
                      fscale=profstr[jj].flux_scale, $
                      ferrclip=ferrclip,lstart=lstart[jj],lend=lend[jj]

    IF delflag THEN BEGIN
      fint = fint - del*!pi*(sma^2.0)/profstr[jj].axerat
      sb = sb - del
    ENDIF

    IF profstr[jj].flux_t GE profstr[jj].flux_f THEN BEGIN
      Fout = [(profstr[jj].flux_t - profstr[jj].flux_f), $
              (profstr[jj].err_flux_t_sky - profstr[jj].err_flux_f_sky), $
              (profstr[jj].err_flux_t_cont - profstr[jj].err_flux_f_cont)]
    ENDIF ELSE BEGIN
      Fout = FLTARR(3)
; Since _t isn't set, overwrite with copies of the _f variables
    ENDELSE

; Halflight/R90 logic for flux_f
    halflight,fint,efintsk,sma,profstr[jj].fluxrad_f, $
              ahalf,aerr_sky,SB=sbsky,/SILENT
    profstr[jj].re_f = ahalf
    profstr[jj].se_f = sbsky[0]
    profstr[jj].err_re_f_sky = aerr_sky
    profstr[jj].err_se_f_sky = sbsky[1]

    halflight,fint,efintsk,sma,profstr[jj].fluxrad_f, $
              a90,a90err_sky,SB=sb90sky,THRESH=0.9,/SILENT
    profstr[jj].r90_f = a90
    profstr[jj].err_r90_f_sky = a90err_sky

    IF netflag THEN BEGIN
      halflight,fint,efintcn,sma,profstr[jj].fluxrad_f, $
                ahalf,aerr_cont,SB=sbcont,/SILENT
      profstr[jj].err_re_f_cont = aerr_cont
      profstr[jj].err_se_f_cont = sbcont[1]

      halflight,fint,efintcn,sma,profstr[jj].fluxrad_f, $
                a90,a90err_cont,SB=sb90cont,THRESH=0.9,/SILENT
      profstr[jj].err_r90_f_cont = a90err_cont
    ENDIF

    IF profstr[jj].flux_t GT profstr[jj].flux_f THEN BEGIN
; Halflight/R90 logic for flux_t
      halflight,fint,efintsk,sma,profstr[jj].fluxrad_f, $
                ahalf,aerr_sky,Fout=[Fout[0],Fout[1]],SB=sbsky,/SILENT
      profstr[jj].re_t = ahalf
      profstr[jj].se_t = sbsky[0]
      profstr[jj].err_re_t_sky = aerr_sky
      profstr[jj].err_se_t_sky = sbsky[1]

      halflight,fint,efintsk,sma,profstr[jj].fluxrad_f, $
                a90,a90err_sky,Fout=[Fout[0],Fout[1]],SB=sb90sky,THRESH=0.9,/SILENT
      profstr[jj].r90_t = a90
      profstr[jj].err_r90_t_sky = a90err_sky

      IF netflag THEN BEGIN
        halflight,fint,efintcn,sma,profstr[jj].fluxrad_f, $
                  ahalf,aerr_cont,Fout=[Fout[0],Fout[2]],SB=sbcont,/SILENT
        profstr[jj].err_re_t_cont = aerr_cont
        profstr[jj].err_se_t_cont = sbcont[1]

        halflight,fint,efintcn,sma,profstr[jj].fluxrad_f, $
                  a90,a90err_cont,Fout=[Fout[0],Fout[2]],SB=sb90cont,THRESH=0.9,/SILENT
        profstr[jj].err_r90_t_cont = a90err_cont
      ENDIF
    ENDIF ELSE BEGIN
; Since we didn't set the _t stuff before, write it all now.
      profstr[jj].flux_t = profstr[jj].flux_f
      profstr[jj].err_flux_t_sky = profstr[jj].err_flux_f_sky
      profstr[jj].err_flux_t_cont = profstr[jj].err_flux_f_cont
      profstr[jj].re_t = profstr[jj].re_f
      profstr[jj].err_re_t_sky = profstr[jj].err_re_f_sky
      profstr[jj].err_re_t_cont = profstr[jj].err_re_f_cont
      profstr[jj].se_t = profstr[jj].se_f
      profstr[jj].err_se_t_sky = profstr[jj].err_se_f_sky
      profstr[jj].err_se_t_cont = profstr[jj].err_se_f_cont
      profstr[jj].r90_t = profstr[jj].r90_f
      profstr[jj].err_r90_t_sky = profstr[jj].err_r90_f_sky
      profstr[jj].err_r90_t_cont = profstr[jj].err_r90_f_cont
    ENDELSE

    IF profstr[jj].flux_o GT 0.0 THEN BEGIN
; Since our FLUX_O logic doesn't include any of the halflight logic,
; the uncertainties are much, much simpler.
      profstr[jj].flag_o = "T"
      npix = FLOAT(ROUND(profstr[jj].flux_o / profstr[jj].se_o))
      profstr[jj].err_se_o_sky = profstr[jj].err_flux_o_sky / (npix*(profstr[jj].pixsize^2))
      profstr[jj].err_se_o_cont = profstr[jj].err_flux_o_cont / (npix*(profstr[jj].pixsize^2))
      IF delflag THEN BEGIN
        profstr[jj].flux_o = profstr[jj].flux_o - npix*delsky*profstr[jj].flux_scale
        profstr[jj].se_o = profstr[jj].se_o - del
      ENDIF
    ENDIF

  ENDFOR

  RETURN

END
