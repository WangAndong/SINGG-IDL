PRO mkhopcatdb
; 
; read in hopcat and turn it into an IDL database
;
; G. Meurer 01/2005
;
   invalidi = -99999
   invalidf = -9999.9
   invalidd = -9999.9d0
   fili     = 'FinalHOPCATJan04TabSep.txt'
   filo     = 'hopcat'
   filt     = '_hopcat.tmp'
   csp      = '%'                 ; temporary replacement character for space
   skipline = 3
   fmt1     = '(i,a,a,a,f,f,f,f,a,a,a,a,a,i,a,a,a,a,a,a,a,i,i,i)'
   fmt2     = '(x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,f,f,f,a,a,a,a,a,a)'
   ;
   ; define things for structure and database
   title    = 'HOPCAT Optical counterparts to HICAT - Doyle et al. (2005)'
   item     = ['id_hipass', 'name_hipass', 'name', 'morphology', 'ra', 'dec', 'b_mag', 'r_mag', 'i_mag', $
               'a_b', 'a_image', 'b_image', 'theta_image', 'glong', 'glat', 'b_plate', 'r_plate', 'i_plate', $
               'vel_6df', 'vel_ned', 'class_match', 'ra_corr', 'dec_corr', 'exthicat', $
               'dra', 'ddec', 'dsep', 'ra_hipass', 'dec_hipass', 'velmom', 'w50', 'sp', 'sint']
   tout     = ['I', 'C', 'C', 'C', 'R', 'R', 'R', 'R', 'R', $
               'R', 'R', 'R', 'R', 'R', 'R', 'I', 'I', 'I', $
               'I', 'I', 'I', 'R', 'R', 'R', $
               'R', 'R', 'R', 'R', 'R', 'R', 'I', 'R', 'R']
   tlen     = [2,0,0,0,8,8,4,4,4, $
               4,4,4,4,4,4,2,2,2, $
               2,2,2,8,8,4, $
               4,4,4,8,8,4,2,4,4 ]
   dstring  = 'i,a,a,a,d,d,f,f,f,f,f,f,f,f,f,i,i,i,i,i,i,d,d,f,f,f,f,d,d,f,i,f,f'
   descript = ['ID number from HIPASS', $
               'name from HIPASS (without HIPASS prefix)', $
               'Galaxy name from NED', $
               'Morphology', $
               'RA of matched galaxy (J2000) [deg]', $
               'Dec of Matched galaxy (J2000) [deg]', $
               'Blue mag (Kron-like) [mag]', $
               'Red mag [mag]', $
               'I mag [mag]', $
               'Ratio of Semi major axis to semi minor axis', $
               'Profile RMS along major axis [pixel]', $
               'Profile RMS along minor axis [pixel]', $
               'Position angle (CCW/x)  [deg]', $
               'Blue SuperCOSMOS plate number for original image', $
               'Red SuperCOSMOS plate number for original image', $
               'I SuperCOSMOS plate number for original image', $
               'Galactic longitude [deg]', $
               'Galactic latitude [deg]', $
               'Velocity from 6dF Data  [km/s]', $
               'Velocity From NED Data  [km/s]', $
               'Our catogory matching choice ', $
               'corrected Right ascension of barycenter (J2000) [deg]', $
               'corrected Declination of barycenter (J2000) [deg]', $
               'Extincition values [deg]', $
               'Radio-Optical RA Position Separation [Arcmin]', $
               'Radio-Optical Dec Position Separation [Arcmin]', $
               'Radio-Optical Galaxy Arcmin Position Separation [Arcmin]', $
               'Final right ascension (J2000 hexadecimal) [deg]', $
               'Final declination (J2000 hexadecimal) [deg]', $
               'Flux weighted velocity average between [km/s]', $
               'Velocity profile width at 0.50*max [km/s]', $
               'Peak HI flux density of profile [Jy]', $
               'integrated HI flux of source [Jy km/s]']
   indblk   = ['ra','dec','ra_hipass','dec_hipass','glat', 'glong', 'b_mag', 'r_mag', 'class_match']
;
; old stuff for fixed format file that didn't quite work
;
;   fmt1     = '(i4,2x,a19,2x,a12,2x,a11,2x,f10.1,2x,f8.1,2x,f9.4,2x,f9.1,'+$
;              '2x,f9.3,2x,f9.3,2x,f8.1,2x,a10,2x,a9,2x,i5,'+$
;              '2x,f14.7,2x,f14.7,2x,a13,2x,a14,2x,f10.4,2x,f10.4,2x,f11.4,'+$
;              '2x,i8,2x,i8,2x,i8)'
   ; 4, 6, 25, 27, 39, 41, 52, 54, 64, 66, 73, 75, 84, 86, 95, 
   ; 97, 106, 108, 117, 119, 127, 129, 138, 140, 149, 151, 157,
   ; 159, 173, 175, 189, 191, 204, 206, 220, 222, 232, 234, 244, 246, 257,
   ; 259, 267, 269, 277, 279, 287, 
;   fmt2     = '(99x,99x,91x,f11.4,2x,f11.4,2x,f10.4,2x,f10.4,2x,f10.4,2x,f9.4,'+$
;              '2x,f9.4,2x,a14,2x,a23)'
   ; 289, 300, 302, 313, 315, 325, 327, 337, 339, 349, 351, 360, 
   ; 362, 371, 373, 387, 389, 412
   shome    = getenv('SHOME')
   ;
   IF strlen(shome) EQ 0 THEN BEGIN 
      fdir = '/data1/acs7/meurer/SINGG/HIcatalogs/'
   ENDIF ELSE BEGIN 
      fdir = shome + '/HIcatalogs'
   ENDELSE 
   ;
   ; move to directory containing HOPCAT
   print, 'MKHOPCATDB: changing directory to '+fdir
   cd, fdir, current=cwd
   ;
   ; convert tab separated table to space separated
   print, 'MKHOPCATDB: Reformating '+fili+' -> '+filt
   reform_tabsep, fili, filt, charsp=csp
   ;
   ; read in catalog
   fmt1     = '(i,a,a,a,f,i,f,f,a,a,a,a,a,i,a,a,a,a,a,a,a,i,i,i)'
   fmt2     = '(x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,f,f,f,a,a,a,a,a,a)'
   print, 'MKHOPCATDB: Reading file '+filt
   readcol, filt, id, name_hipass, ra_hipass_str, dec_hipass_str, velmom, w50, sp, sint, $
            a_image_str, b_image_str, theta_image_str, vel_6df_str, vel_ned_str, class_match, $
            ra_corr_str, dec_corr_str, rastr, decstr, b_mag_str, r_mag_str, i_mag_str, $
            b_plate, r_plate, i_plate, format=fmt1, skipline=skipline, /silent
   nhop1       = n_elements(id)
   readcol, filt, glat, glong, exthicat, dra_str, ddec_str, dsep_str, $
            a_b_str, name, morphology, format=fmt2, skipline=skipline, /silent
   nhop2       = n_elements(glat)
   IF nhop1 NE nhop2 THEN BEGIN 
      print, 'ERROR **** in MKHOPCATDB: # entries read in first and second pass do not agree!'
      print, 'MKHOPCATDB: first pass with READFMT, number of entries read  : ', nhop1
      print, 'MKHOPCATDB: second pass with READFMT, number of entries read : ', nhop2
      stop
   ENDIF ELSE BEGIN 
      print, 'MKHOPCATDB: number of entries read  : ', nhop1
   ENDELSE 
   nhop        = nhop1
   ;
   print, 'MKHOPCATDB: deleting temporary file: '+filt
   file_delete, filt
   ;
   ; fix the spaces in string arrays, convert some things to numbers
   print, 'MKHOPCATDB: converting arrays... '
   vel_6df     = make_array(nhop, /long, value=invalidi)
   vel_ned     = make_array(nhop, /long, value=invalidi)
   a_image     = make_array(nhop, value=invalidf)
   b_image     = make_array(nhop, value=invalidf)
   theta_image = make_array(nhop, value=invalidf)
   ra_corr     = make_array(nhop, /double, value=invalidd)
   dec_corr    = make_array(nhop, /double, value=invalidd)
   ra          = make_array(nhop, /double, value=invalidd)
   dec         = make_array(nhop, /double, value=invalidd)
   b_mag       = make_array(nhop, value=invalidf)
   r_mag       = make_array(nhop, value=invalidf)
   i_mag       = make_array(nhop, value=invalidf)
   dra         = make_array(nhop, value=invalidf)
   ddec        = make_array(nhop, value=invalidf)
   dsep        = make_array(nhop, value=invalidf)
   a_b         = make_array(nhop, value=invalidf)
   FOR ii = 0, nhop-1 DO BEGIN 
      str                = name_hipass[ii]
      jj                 = strpos(str, 'J')  ; Start HIPASS name with J 
      IF jj GT 0 THEN str = strmid(str,jj)   ; i.e. strip off 'HIPASS'
      name_hipass[ii]    = strtrim(repchr(str, csp, ' '),2)
      str                = ra_hipass_str[ii]
      ra_hipass_str[ii]  = strtrim(repchr(str, csp, ' '),2)
      str                = dec_hipass_str[ii]
      dec_hipass_str[ii] = strtrim(repchr(str, csp, ' '),2)
      str                = rastr[ii]
      rastr[ii]          = strtrim(repchr(str, csp, ' '),2)
      str                = decstr[ii]
      decstr[ii]         = strtrim(repchr(str, csp, ' '),2)
      ;
      ; put spaces back into ned name & morphology, then strip out all
      ; whitespace
      str                = morphology[ii]
      morphology[ii]     = strcompress(repchr(str, csp, ' '),/remove_all)
      str                = name[ii]
      name[ii]           = strcompress(repchr(str, csp, ' '),/remove_all)
      ;
      ; arrays that should primarily be integer or real
      ; but can be strings when they are invalid.
      str                = vel_6df_str[ii]
      status             = valid_num(str, value, /integer)
      IF status EQ 1 THEN vel_6df[ii] = value
      str                = vel_ned_str[ii]
      status             = valid_num(str, value, /integer)
      IF status EQ 1 THEN vel_ned[ii] = value
      str                = a_image_str[ii]
      status             = valid_num(str, value)
      IF status EQ 1 THEN a_image[ii] = value
      str                = b_image_str[ii]
      status             = valid_num(str, value)
      IF status EQ 1 THEN b_image[ii] = value
      str                = theta_image_str[ii]
      status             = valid_num(str, value)
      IF status EQ 1 THEN theta_image[ii] = value
      str                = ra_corr_str[ii]
      status             = valid_num(str, value)
      IF status EQ 1 THEN ra_corr[ii] = value
      str                = dec_corr_str[ii]
      status             = valid_num(str, value)
      IF status EQ 1 THEN dec_corr[ii] = value
      str                = b_mag_str[ii]
      status             = valid_num(str, value)
      IF status EQ 1 THEN b_mag[ii] = value
      str                = r_mag_str[ii]
      status             = valid_num(str, value)
      IF status EQ 1 THEN r_mag[ii] = value
      str                = i_mag_str[ii]
      status             = valid_num(str, value)
      IF status EQ 1 THEN i_mag[ii] = value
      str                = dra_str[ii]
      status             = valid_num(str, value)
      IF status EQ 1 THEN dra[ii] = value
      str                = ddec_str[ii]
      status             = valid_num(str, value)
      IF status EQ 1 THEN ddec[ii] = value
      str                = dsep_str[ii]
      status             = valid_num(str, value)
      IF status EQ 1 THEN dsep[ii] = value
      str                = a_b_str[ii]
      status             = valid_num(str, value)
      IF status EQ 1 THEN a_b[ii] = value
   ENDFOR 
   ;
   ; convert from sexigessimal to degrees
   ra_hipass   = 15.0*sexideg(ra_hipass_str, delim=':')
   dec_hipass  = sexideg(dec_hipass_str, delim=':')
   good        = where(strpos(rastr,'XX') LT 0, ngood)
   IF ngood GT 0 THEN BEGIN 
      temp            = 15.0*sexideg(rastr[good], delim=':')
      ra[good]        = temp
   ENDIF 
   good        = where(strpos(decstr,'XX') LT 0, ngood)
   IF ngood GT 0 THEN BEGIN 
      temp            = sexideg(decstr[good], delim=':')
      dec[good]       = temp
   ENDIF 
   ; --------------------------------------------------------------
    ;
   ; Go to directory for databases
   zdbase   = getenv('ZDBASE')
   print, 'MKHOPCATDB: moving to database directory '+zdbase
   cd, zdbase
   ;
   ; set lengths of character items
   k       = where(tlen EQ 0,nk)
   IF nk GT 0 THEN FOR i = 0,nk-1 DO result = execute('tlen[k[i]] = max(strlen('+item[k[i]]+'))') $
              ELSE stop
   ;
  ;
   ; create structure
   print, 'MKHOPCATDB: creating HOPCAT structure ...'
   create_struct2, hopcat, 'HOPCAT', item, dstring, dimen=nhop
   ;
   ; open dbd file, write top of the file
   fildbd  = filo + '.dbd'
   print, 'MKHOPCATDB: creating dbd file: '+fildbd
   openw, lu, fildbd, /get_lun
   printf, lu, '#title'
   printf, lu, title
   printf, lu, ''
   printf, lu, '#maxentries'
   printf, lu, strtrim(string(nhop),2)
   printf, lu, ''
   printf, lu, '#items'
   ;
   ; Fill hopcat structure, write item lines of dbd file
   print, 'MKHOPCATDB: Filling hopcat structure, writing item lines of dbd file...'
   mc     = max(strlen(item))
   pritem = ljust(strupcase(item), mc)
   ni     = n_elements(item)
   FOR i = 0, ni-1 DO BEGIN 
      cmd = "hopcat."+strtrim(item[i],2)+" = "+strtrim(item[i],2)
      print,cmd
      result = execute(cmd)
      typstr = ljust(tout[i]+'*'+strtrim(string(tlen[i]),2),5)
      printf,lu,pritem[i]+'   '+typstr+'   "'+descript[i]+'"'
   ENDFOR 
   ;
   ; write index blocks of dbd file and close
   printf,lu,'  '
   printf,lu,'#index'
   ni = n_elements(indblk)
   FOR i = 0, ni-1 DO BEGIN 
      printf,lu,ljust(strupcase(strtrim(indblk[i],2)),mc)+'   sort'
   ENDFOR 
   free_lun,lu   
   ;
   ; write database
   !PRIV = 2
   print, 'MKHOPCATDB: writing database...'
   dbcreate, filo, 1, 1, /external
   dbopen, filo, 1
   dbbuildstruct, hopcat
   dbclose,dummy
   ;
   print, 'MKHOPCATDB: returning to directory '+cwd
   cd, cwd
END 
