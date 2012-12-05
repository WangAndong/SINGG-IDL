FUNCTION vla_query_url, ra, dec, vel
   ;
   ; Write a URL to make a query to the VLA archive to search 
   ; for 21cm line spectra observations.
   ;
   ; G. Meurer 11/2007
   ;
   str1    = 'https://archive.nrao.edu/archive/ArchiveQuery?PASSWD=&QUERYTYPE=OBSERVATION&DATATYPE=ALL&SORT_PARM=Starttime&SORT_ORDER=Asc&PROTOCOL=HTML&SORT_PARM2=Starttime&SORT_ORDER2=Asc&MAX_ROWS=NO+LIMIT&QUERY_ID=9999&QUERY_MODE=Prepare+Download&LOCKMODE=PROJECT&SITE_CODE=AOC&DBHOST=CHEWBACCA&WRITELOG=0&PROJECT_CODE=&SEGMENT=&OBSERVER=&ARCHIVE_VOLUME=&TIMERANGE1=&TIMERANGE2=&SOURCE_ID=&SRC_SEARCH_TYPE=SIMBAD+or+NED+Resolver&CALIB_TYPE=ALL+Srcs&CENTER_RA='
   str2    = '&LONG_RANGE=&FRAME=Equatorial&CENTER_DEC='
   str3    = '&LAT_RANGE=&EQUINOX=J2000&SRAD=3.0&MIN_EXPOSURE=&OBS_BANDS=L&TELESCOPE=VLA&OBS_MODE=LINE&TELESCOPE_CONFIG=ALL&OBS_POLAR=ALL&SUBARRAY=ALL&OBSFREQ1='
   str4    = '&OBSFREQ2='
   str5    = '&SUBMIT=Submit+Query'
   delim   = '%3A'
   precra  = 2
   precdec = 1
   cc      = 2.9979e5       ; speed-o-lite km/s
   cccm    = cc*1.0e5       ; speed-o-lite cm/s
   dvel    = 800.0          ; velocity range to search
   freq0   = 1420.405751e6  ; rest frequency
   lam0    = cccm / freq0   ; wavelength
   ;
   ; convert ra,dec into a string
   rastr   = degsexi(ra,/ra,prec=precra,delim=delim)
   decstr  = degsexi(dec,prec=precdec,delim=delim)
   ;
   ; get start and end frequencies
   lam1    = lam0*(1.0 + (vel+0.5*dvel)/cc)
   lam2    = lam0*(1.0 + (vel-0.5*dvel)/cc)
   frq1    = 1.0e-6*cccm/lam1
   frq2    = 1.0e-6*cccm/lam2
   frqstr1 = strtrim(string(frq1,format='(f8.2)'),2)
   frqstr2 = strtrim(string(frq2,format='(f8.2)'),2)
   ;
   ; assemble string
   url     = str1+rastr+str2+decstr+str3+frqstr1+str4+frqstr2+str5
   return, url
END 
