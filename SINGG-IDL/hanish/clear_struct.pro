PRO clear_struct,str,vartype,VARUNIT=varunit
; INPUT
;   str        Data structure
;   vartype    Type index for each variable
; OPTIONAL INPUT
;   varunit    Units of each variable, used only to check for
;                logarithmic entries.

; Eventually, replace vartype with the inherent 
; and N_ELEMENTS(vartype) with a call to N_STRUCT (astro-lib) or n_tags
  varname = TAG_NAMES(str)

  FOR ii = 0,N_ELEMENTS(vartype)-1 DO BEGIN

; If a variable is logarithmic, it should clear to -999, not zero.
; Note that on most logarithmic variables we list the units of what
; the log is of, so parse name as well as units.
    logcheck = (STRPOS(varname[ii],"LOG") GE 0)
    IF KEYWORD_SET(varunit) THEN BEGIN
      logcheck = logcheck OR (STRPOS(varunit[ii],"dex") GE 0) OR $
           (STRPOS(varunit[ii],"mag") GE 0) OR (STRPOS(varunit[ii],"log") GE 0)
    ENDIF
    IF logcheck THEN zero = -999 ELSE zero = 0

    arraysize = N_ELEMENTS(str.(ii))

    IF arraysize EQ 0 THEN BEGIN
      CASE STRMID(vartype[ii],0,1) OF
        "A": str.(ii) = ""
        "B": str.(ii) = 0b
        "I": str.(ii) = zero
        "J": str.(ii) = LONG(zero)
        "F": str.(ii) = FLOAT(zero)
        "D": str.(ii) = DOUBLE(zero)
      ENDCASE
    ENDIF ELSE BEGIN
      FOR jj = 0,arraysize-1 DO BEGIN
        CASE STRMID(vartype[ii],0,1) OF
          "A": str.(ii)[jj] = ""
          "B": str.(ii)[jj] = 0b
          "I": str.(ii)[jj] = zero
          "J": str.(ii)[jj] = LONG(zero)
          "F": str.(ii)[jj] = FLOAT(zero)
          "D": str.(ii)[jj] = DOUBLE(zero)
        ENDCASE
      ENDFOR
    ENDELSE

  ENDFOR

  RETURN

END
