FUNCTION notuniq, arr, values
  ;
  ; return the elements of an array that are repeats 
  ; (i.e. not unique).  If there are no repeats then
  ; notuniq = -1
  ;
  ; if a second parameter is given it is filled with the
  ; unique repeated values
  ;
  ; G. Meurer 11/2011
  ;
  ; get size
  narr      = n_elements(arr)
  npar      = n_params()
  IF narr GT 0 THEN BEGIN 
     ; 
     ; get sort order
     ii     = sort(arr)
     jj     = where(arr[ii] EQ shift(arr[ii],-1), njj)
     IF njj LT 1 THEN BEGIN 
        nu  = -1
     ENDIF ELSE BEGIN 
        jj  = [jj, (jj+1 MOD narr)]  ; all elements of the non-unique pairs
        kk  = sort(jj)               ; sort these so we can find repeats
        uu  = uniq(jj[kk])           ; get rid of repeats in jj
        nu  = ii[jj[kk[uu]]]         ; turn into input array indices
        kk  = sort(nu)               ; sort array indices
        nu  = nu[kk]                 ; return sorted values
     ENDELSE 
  ENDIF ELSE BEGIN 
     ;
     ; not an array, trivial case
     nu = -1
  ENDELSE 
  ;
  ; get unique repeated values if requested
  IF npar GT 1 AND nu[0] NE -1 THEN BEGIN 
     values = arr[nu]
     ii     = sort(values)
     uu     = uniq(values[ii])
     values = values[ii[uu]]
  ENDIF 
  return, nu
END 
