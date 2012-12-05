FUNCTION findzero,array,binval,THRESH=thresh,NOEXT=noext,LAST=last
; Interpolation algorithm, used to find where an array crosses zero.
; INPUTS
;   array       Y-axis
;   binval      X-axis
; OPTIONAL INPUTS
;   thresh      If set, ignore any X values below this threshold, and
;                 only use data points from to the RIGHT of the max
;                 value.  Note that this requires array to DECREASE.
;   /noext      Don't extrapolate
;   /last       If there are multiple zeroes, return the LAST one.
; OUTPUT
;  (function)   X value where "array" crosses zero.

  num_bins = N_ELEMENTS(array)
  IF N_ELEMENTS(binval) NE num_bins THEN BEGIN
    PRINT,"ERROR in findzero: array size mismatch ",num_bins,N_ELEMENTS(binval)
    RETURN,-999.0
  ENDIF

  IF MAX(array) LT 0.0 THEN BEGIN
    PRINT,"ERROR in findzero: array does not reach zero",MAX(array)
    RETURN,-999.0
  ENDIF

  lowbin = 0
  IF KEYWORD_SET(thresh) THEN BEGIN
    temp = WHERE(binval GE thresh,count)
    IF count GT 0 THEN threshbin = temp[0]

; Find the peak of the function.
    maxval = MAX(array[threshbin:(num_bins-1)],maxbin,MIN=minval,/NAN)
    IF (maxval LT 0.0 OR minval GT 0.0) AND KEYWORD_SET(noext) THEN BEGIN
      PRINT,"ERROR in findzero: array does not reach zero within specified range ",maxval,minval
      RETURN,-999.0
    ENDIF
    lowbin = threshbin+maxbin
  ENDIF

  testarray = DBLARR(num_bins)
  FOR ii = (lowbin+1),num_bins-1 DO BEGIN
    testarray[ii]=DOUBLE(array[ii])*DOUBLE(array[ii-1])
  ENDFOR
  cross = WHERE(testarray LT 0.0,count)

  IF count GT 0 THEN BEGIN
; Interpolate
    IF KEYWORD_SET(last) THEN indzero = cross[count-1] $
                         ELSE indzero = cross[0]
  ENDIF ELSE BEGIN
; Extrapolate!
    IF KEYWORD_SET(noext) THEN BEGIN
      PRINT,"ERROR in findzero: extrapolation turned off, but no zeroes within range."
      RETURN,-999.0
    ENDIF

; First, see which ends angle towards the zero line.
    slope = 0
    IF ABS(array[num_bins-1]) LT ABS(array[num_bins-2]) THEN slope = slope + 1
    IF ABS(array[lowbin]) LT ABS(array[lowbin+1]) THEN slope = slope + 2

    CASE slope OF
      0: BEGIN
; Both endpoints point away from the zero line
           PRINT,"ERROR in findzero: cannot determine which endpoint to extrapolate"
           RETURN,-999.0
         END
      1: BEGIN
; Only the high end slopes towards the zero.
           indzero = (num_bins-1)
         END
      2: BEGIN
; Only the high end slopes towards the zero.
           indzero = (lowbin+1)
         END
      ELSE: BEGIN
; Both ends slope inward.  Check the flags.
           IF KEYWORD_SET(last) THEN indzero = (num_bins-1) $
                                ELSE indzero = (lowbin+1)
         END
    ENDCASE
  ENDELSE

  scale = binval[indzero] - binval[indzero-1]
  factor = array[indzero]/(array[indzero]-array[indzero-1])
  cent = binval[indzero] - factor*scale

  RETURN,cent

END
