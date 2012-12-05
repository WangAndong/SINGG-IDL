FUNCTION degsexi, deg, prec=prec, ra=ra, delim=delim
   ;
   ; Convert an array in degrees to a sexigessimal string. 
   ;   deg   -> Input array in degrees
   ;   prec  -> Output precision.  Number of decimal places in the 
   ;            seconds
   ;   ra    -> If set then the array is converted to hours 
   ;            (divided by 15.) and then converted to a string with
   ;            no sign (+ or - not used).  Otherwise a declination
   ;            is assumed and a sign is used in front of the string.
   ;   delim -> delimiter between dd & mm and mm & ss. default is ':'
   ;
   ; G. Meurer (JHU) 02/04
   ;
   invalid = '-99:99:99'
   ;
   ; set default delimiter
   IF NOT keyword_set(delim) THEN delim = ':'
   ;
   ; set default precisions
   IF NOT keyword_set(prec) THEN prec = 0
   ;
   nd  = n_elements(deg)
   sn  = make_array(nd, /string, value='+')
   k   = where(deg LT 0.0, nk)
   IF nk GT 0 THEN sn[k] = '-'
   IF keyword_set(ra) THEN BEGIN 
      dd  = abs(deg/15.0d0)
      sn  = make_array(nd, /string, value='')
   ENDIF ELSE BEGIN 
      dd  = abs(deg)
   ENDELSE 
   ss  = 3600.0d0*dd
   mm  = 60.0d0*dd
   dd  = fix(temporary(dd))
   mm  = fix(temporary(mm) - 60.0d0*double(dd))
   ss  = temporary(ss) - 3600.0d0*double(dd) - 60.0d0*double(mm)
   ;
   ; format seconds
   IF prec GE 1 THEN BEGIN 
      fmt = '(f'+strtrim(string(prec+3),2)+'.'+strtrim(string(prec),2)+')'
      sss = string(ss,format=fmt)
   ENDIF ELSE BEGIN 
      sss = string(fix(ss+0.5),format=(i2))
      ; 
      ; for some reason above line writes strings with lots of 
      ; blanks on left.  So have to right justify.
      FOR i = 0, nd-1 DO BEGIN
         nl = strlen(sss[i])
         sss[i] = strmid(sss[i],nl-2,2)
      ENDFOR 
   ENDELSE 
   ;
   ; check and fix cases where ss = 60
   k   = where(strmid(sss, 0, 2) EQ '60', nk)
   IF nk GT 0 THEN BEGIN 
      temp = sss[k]
      strput, temp, '00', 0
      sss[k] = temp
      mm[k] = mm[k]+1
   ENDIF 
   ;
   ; format minutes
   mmm = string(mm, format='(i2)')
   ;
   ; check and fix cases where mm = 60
   k   = where(mm GE 60, nk)
   IF nk GT 0 THEN BEGIN 
      mmm[k] = '00'
      dd[k]  = dd[k]+1
   ENDIF 
   ;
   ; format degrees
   IF max(dd) LE 99 THEN fmt = '(i2)' ELSE fmt = '(i3)'
   ddd = string(dd, format=fmt)
   ; check and fix ra cases where ddd = '24'
   IF keyword_set(ra) THEN BEGIN 
      
   ENDIF 
   ;
   ; add leading zeros where nec
   k   = where(strmid(ddd,0,1) EQ ' ',nk)
   IF nk GT 0 THEN BEGIN 
      temp = ddd[k]
      strput,temp, '0', 0
      ddd[k] = temp
   ENDIF 
   k   = where(strmid(ddd,1,1) EQ ' ',nk)
   IF nk GT 0 THEN BEGIN 
      temp = ddd[k]
      strput,temp, '0', 1
      ddd[k] = temp
   ENDIF 
   k   = where(strmid(mmm,0,1) EQ ' ',nk)
   IF nk GT 0 THEN BEGIN 
      temp = mmm[k]
      strput,temp, '0', 0
      mmm[k] = temp
   ENDIF 
   k   = where(strmid(sss,0,1) EQ ' ',nk)
   IF nk GT 0 THEN BEGIN 
      temp = sss[k]
      strput,temp, '0', 0
      sss[k] = temp
   ENDIF 
   ;
   ; now put it together
   result = sn+ddd+delim+mmm+delim+sss
   return,result
END 
