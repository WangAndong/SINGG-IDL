FUNCTION sig_pm,array,MEANVAL=meanval
; Given an input array, find the 2-piece standard deviation, giving
; sigma both above and below the central value

  IF N_ELEMENTS(array) LE 1 THEN RETURN,[0.d0,0.d0]

  IF NOT KEYWORD_SET(meanval) THEN meanval = MEAN(array)
  outarr = DBLARR(2)

  highindex = WHERE(array GE meanval,highcount)
  lowindex = WHERE(array LE meanval,lowcount)

  IF highcount EQ 0 OR lowcount EQ 0 and N_ELEMENTS(array) GT 10 THEN BEGIN
    PRINT,"ERROR in sig_pm: algorithm failed ",highcount,lowcount
    PRINT,meanval
    RETURN,[0.d0,0.d0]
  ENDIF

  IF highcount EQ 1 THEN outarr[0] = (array[highindex[0]]-meanval) ELSE $
    outarr[0] = SQRT((1.d0/FLOAT(highcount-1))*TOTAL((array[highindex]-meanval)^2))
  IF lowcount EQ 1 THEN outarr[1] = (meanval-array[lowindex[0]]) ELSE $
    outarr[1] = SQRT((1.d0/FLOAT(lowcount-1))*TOTAL((array[lowindex]-meanval)^2))

  RETURN,outarr

END
