PRO aitoff_galactic,temp_big_lon,temp_big_lat,dcolor
; Simple routine to be used in conjunction with AITOFF and AITOFF_GRID,
; to draw lines on the graph corresponding to galactic coordinates

; Note: while WCSSPH2XY operates correctly in its own rights using mode 21,
; this does not match with the AITOFF_GRID routine.  As a result, you MUST use
; the AITOFF routine to calculate X,Y.

; The "big" spacing is the spacing of the lines themselves.  
; These will be cast into floats to be safe.
  big_lon = FLOAT(temp_big_lon)
  big_lat = FLOAT(temp_big_lat)

; The "small" spacing is the spacing of the dots along each line,
; which must be less than the "big" spacing.  We hard-code this to 2.0 for now.
  small_lon = 2.0
  small_lat = 2.0

; Number of lines
  num_lon = LONG(360.0/big_lon) + 1
  num_lat = LONG(180.0/big_lat) - 1

; Number of grid points
  num_grid_lon = LONG(360.0/small_lon) + 1
  num_grid_lat = LONG(180.0/small_lat) - 1

; Draw lines of longitude
  FOR ii = 0,(num_lon - 1) DO BEGIN
    lo = FLTARR(num_grid_lat) + (big_lon * FLOAT(ii))
    la = (small_lat) * (FINDGEN(num_grid_lat) - (num_grid_lat - 1)/2)

    glactc,temp_ra,temp_dec,2000,lo,la,2
    AITOFF,(temp_ra*15.0),temp_dec,temp_x,temp_y

    OPLOT,temp_x,temp_y,PSYM=3,COLOR=dcolor,LINESTYLE=1
  ENDFOR

; Draw lines of latitude
  FOR ii = 0,(num_lat - 1) DO BEGIN
    lo = (small_lon) * FINDGEN(num_grid_lon) - 180.0
    la = FLTARR(num_grid_lon) + (big_lat * FLOAT(ii-(num_lat - 1)/2))

    glactc,temp_ra,temp_dec,2000,lo,la,2
    AITOFF,(temp_ra*15.0),temp_dec,temp_x,temp_y

    OPLOT,temp_x,temp_y,PSYM=3,COLOR=dcolor,LINESTYLE=1
  ENDFOR  

; Next step to add: label the lines

END