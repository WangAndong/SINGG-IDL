PRO stats_db, dbname, lu=lu
   ;
   ; This is a tool to debug idl databases
   ; For each item lists:
   ;    item name
   ;         type
   ;         length of description
   ; For numerical types, stats and plots:
   ;    min value
   ;    max value
   ;    median value
   ;    number of finite values
   ;    number of NaN values
   ;    number of -NaN values
   ;    histogram
   ;
   ; G. Meurer 05/2005
   ;
   fmti     = '(a20,i3,i5,i7,i7,i7,i7,i12,i12,i12)'
   fmtf     = '(a20,i3,i5,i7,i7,i7,i7,g12.5,g12.5,g12.5)'
   hdr1     = '# item              typ d_len finite !finite -NaN   +NaN      min         max         median'
   hdr2     = '# -------------------------------------------------------------------------------------------'
   ;
   IF NOT keyword_set(lu) THEN lu = -1
   ;
   dbopen, dbname
   item_nam = db_item_info('name')
   item_typ = db_item_info('idltype')
   item_dsc = db_item_info('description')
   item_dl  = strlen(strtrim(item_dsc,2))
   nit      = n_elements(item_nam)
   ;
   printf, lu, hdr1
   printf, lu, hdr2
   FOR ii = 0, nit-1 DO BEGIN 
      IF item_typ[ii] GE 1 AND item_typ[ii] LE 5 THEN BEGIN 
         dbext, -1, item_nam[ii], val
         nent     = n_elements(val)
         IF item_typ[ii] GE 4 AND item_typ[ii] LE 6 THEN BEGIN 
            fin   = finite(val)
            kk    = where(fin EQ 1b, nfin)
            jj    = where(fin EQ 0b, nnan)
            IF nnan GT 0 THEN BEGIN 
               jj = where(finite(val,/nan,sign=0),nnan1)
               jj = where(finite(val,/nan,sign=1),nnan2)
            ENDIF ELSE BEGIN 
               nnan1 = 0
               nnan2 = 0
            ENDELSE 
         ENDIF ELSE BEGIN 
            fmto  = fmti
            nfin  = nent
            kk    = lindgen(nent)
            nnan1 = 0
            nnan2 = 0
         ENDELSE 
         IF nfin GT 0 THEN BEGIN 
            minv     = min(val[kk])
            maxv     = max(val[kk])
            medv     = median(val[kk],/even)
            fmto     = fmtf
         ENDIF ELSE BEGIN 
            fmto     = fmti
            minv     =  0
            maxv     =  0
            medv     =  0
         ENDELSE 
      ENDIF ELSE BEGIN 
         fmto        = fmti
         nfin        = nent
         nnan1       =  0
         nnan2       =  0
         minv        =  0
         maxv        =  0
         medv        =  0
      ENDELSE 
      printf, lu, ljust(item_nam[ii],20), item_typ[ii], item_dl[ii], nfin, nent - nfin, nnan1, nnan2, minv, maxv, medv, format=fmto
   ENDFOR 
END 
