FUNCTION find_end_time,startdate,exptime
; INPUTS
; startdate(N) UTC dates for the start of each exposure
; exptime(N)   Exposure times for each image
; OUTPUT
; (function)   UTC date of the end of the last exposure

  n_files = N_ELEMENTS(startdate)
  endtime = DBLARR(n_files)

  FOR ii = 0,(n_files-1) DO BEGIN
    utcend = str2utc(startdate[ii])
    endtime[ii] = utc2sec(utcend) + DOUBLE(exptime(ii))
  ENDFOR

;  last = WHERE(endtime EQ MAX(endtime))

;  end_date = utc2str(sec2utc(endtime[last]))
  end_date = utc2str(sec2utc(MAX(endtime)))
  end_date = STRMID(end_date,0,STRLEN(end_date)-1)

  RETURN,end_date

END
