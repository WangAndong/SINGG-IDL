FUNCTION calc_outer_mask,maskin,edge

; Given an input mask, determines a convex "filled" mask, padded by
; "edge" pixels.  Use a negative value if you want to make the mask
; smaller than the data area (i.e., to eliminate fringe effects), use
; a positive value if you want to make the mask larger (i.e., if you
; want to make sure you get ALL of the data.)
; Input mask is assumed to be 1b for BAD pixels.

  sz = SIZE(maskin)
  masktemp = BYTARR(sz[1],sz[2])+1b ; Intermediate step
  maskout = BYTARR(sz[1],sz[2])+1b

; First, go row by row.
  FOR ii = 0,sz[2]-1 DO BEGIN
    row = maskin[*,ii]
    ind = WHERE(row LT 0.5,count) ; good pixels.
    IF count GT 0 THEN BEGIN
      minX = (ind[0] - edge) > 0
      maxX = (ind[count-1] + edge) < (sz[1]-1)
      IF maxX GE minX THEN masktemp[minX:maxX,ii] = 0b
    ENDIF
  ENDFOR

; Next, column by column
  FOR ii = 0,sz[1]-1 DO BEGIN
; Instead of maskin, use masktemp here.
    col = masktemp[ii,*]
    ind = WHERE(col LT 0.5,count) ; good pixels.
    IF count GT 0 THEN BEGIN
      minY = (ind[0] - edge) > 0
      maxY = (ind[count-1] + edge) < (sz[2]-1)
      IF maxY GE minY THEN maskout[ii,minY:maxY] = 0b
    ENDIF
  ENDFOR

  RETURN,maskout
END
