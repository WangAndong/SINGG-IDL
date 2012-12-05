PRO mkhicatdb, header, file, filo, hicat, $
               indblk=indblk, newvers=newvers, vfld1=vfld1, vfld2=vfld2, $
               flowmod=flowmod, noconvrt=noconvrt, verbose=verbose
   ;
   ; read hicat catalog; return the result as a structure.
   ; 
   ; header  ->  name of header text file.  This provides a description
   ;             of the fields in the hicat catalog.  This file contains
   ;             three or four columns formatted using the variable fmt 
   ;             which depends on the newvers switch.  The columns are:
   ;             kwd : the name of the field (column).
   ;             typ : the data type for the field.  Must be one of the
   ;                  following:
   ;                  A - string
   ;                  I - integer
   ;                  L - long integer
   ;                  F - floating point number
   ;                  D - double precision
   ;                  S - sexigessimal degrees read in as DD:MM:SS.SS
   ;                      format string.  Stored as double precision.
   ;                  H - sexigessimal hours read in as HH:MM:SS.SS
   ;                      format string.  Stored as double precision
   ;                  N - Name string.  HIPASS stripped off before 
   ;                      storing
   ;                  V - velocity in radio convention.  To be
   ;                      converted to optical convention.
   ;                      treated as F if noconvrt is set.
   ;                  W - velocity width in radio convention, or 
   ;                      integrated flux [int(s*dv)] with velocity
   ;                      defined on radio convention scale. To be
   ;                      converted to optical convention.
   ;                      treated as F if noconvrt is set.
   ;             units   - Units of entry - only if newvers is set.
   ;             comment - description of field.
   ; file    ->  hicat catalog file.  Columns described in header.  Note,
   ;             rows without the correct number of columns are ignored.
   ; filo    ->  Body of output file name.
   ; hicat   <-  structure containing the catalog.
   ; indblk  ->  string array of items to put in the index block of 
   ;             the dbd file.  For now these will
   ;             be given an index type of "sort"
   ; newvers ->  if not set or 0, then May 2002 version assumed,
   ;             otherwise Feb, 2004 version assumed.  
   ;             this effects the format of the header parameters file
   ; vfld1   ->  name of field for fiducial velocity.  This
   ;             only matters if newvers keyword is not set.  If not
   ;             set, then the fiducial velocity field is assumed to
   ;             be 'VEL'
   ; vfld2   ->  name of field for backup fiducial velocity.  This
   ;             is the velocity used where values of vfld1 are undefined.
   ; flowmod  -> File containing output from jphflow.  If not set, the
   ;             flow model parameters are not read.
   ; noconvrt -> V W types not converted to optical convention if set.
   ; verbose  -> set if you want to see what is happening.
   ;
   ; G. Meurer 01/2004
   ; G. Meurer 02/2004 add V, W types & vfld and noconvrt keywords
   ; G. Meurer 12/2004 - adjusted to insert E(B-V) and flag value from 
   ;                     Schlegel et al. (1998, ApJ, 500, 525) into
   ;                     database.
   ;
   ; some definitions
   cstr    = '#'
   mdlen   = 62                    ; max string length of description
   fwork   = '_junk.dat'
   ftmp1   = '_temp1.dat'
   febv1   = filo + '_ebv1.log'
   febv2   = filo + '_ebv2.log'
   indefi  = -99
   indefl  = -99l
   indeff  = -9999.99
   indefd  = -9999.99d0
   c       = 2.9978d5              ; speed of light
   h0      = 70.0                  ; hubble constant
   invrad  = 3.141592654/180.      ; radians / degree
   distmin = 1.0                   ; minimum distance [Mpc]
   ;
   ; define the valid data types, and corresponding formats etc...
   tgood   = ['N', 'A', 'I', 'L', 'F', 'D', 'S', 'H', 'V', 'W', 'B']
   fgood   = '(' + ['a', 'a', 'i', 'l', 'f', 'd', 'a', 'a', 'f', 'f', 'b'] + ')'
   dgood   = ['a', 'a', 'i', 'j', 'f', 'd', 'd', 'd', 'f', 'f', 'b']
   tout    = ['C', 'C', 'I', 'I', 'R', 'R', 'R', 'R', 'R', 'R', 'B']
   tlen    = '*' + ['', '',  '2', '4', '4', '8', '8', '8', '4', '4', '1']
   ;
   ; These are the derived fields
   field2   = ['GLONG', 'GLAT', 'EBV', 'MASK_EBV', 'VUSE', 'VLG', 'VCMB', 'VSHAP', 'VTONRY', 'DISTANCE', 'LOGMHI']
   dstring2 =  'f,f,f,b,f,f,f,f,f,f,f'
   dscript2 = ['Galactic longitude [degrees]', $
               'Galactic latitude [degrees]', $
               'Galactic foreground E(B-V) (Schlegel et al) [mag]', $
               'Byte mask from Schlegel et al', $
               'Best heliocentric radial velocity', $
               'Local group corrected velocity', $
               'CMB frame velocity', $
               'Flow model (Virgo, GA, Shapley Conc.) corrected velocity', $
               'Flow model (Tonry et al.) corrected velocity', $
               'Distance [Mpc]', $
               'Logarithmic HI mass [solar units]']
   nf2      = n_elements(field2)
   ;
   cstr2   = '"'+cstr+'"'
   ;
   vb      = keyword_set(verbose)
   fidset  = 0b
   fidset2 = 0b
   IF keyword_set(vfld1) THEN v_fld = strupcase(strtrim(vfld1,2)) $
   ELSE v_fld = 'VEL'
   IF keyword_set(vfld2) THEN v_fld2 = strupcase(strtrim(vfld2,2)) $
   ELSE v_fld2 = 'VEL_MOM'
   ;
   ; set version dependent things, read header parameters
   IF keyword_set(newvers) THEN BEGIN 
      fmt     = 'a13,2x,a1,1x,a7,8x,a65' ; format of header file
      title   = 'HICAT: HIPASS Catalog version Feb 2004'
      nlen    = 9
      readfmt,header,fmt,kwd,typ,units,comment
   ENDIF ELSE BEGIN 
      fmt     = 'a20,1x,a1,3x,a280' ; format of header file
      title   = 'HICAT: HIPASS Catalog version May 2002'
      nlen    = 20
      readfmt,header,fmt,kwd,typ,comment
      units   = make_array(n_elements(kwd),/string,value='UNDEF')
   ENDELSE 
   ;
   ; trim strings
   field   = strupcase(strtrim(kwd,2))
   dscript = strtrim(comment,2)
   nf      = n_elements(field)
   ptr     = make_array(nf,/int)
   ;
   ; use awk to remove lines without at least nf fields
   cmd     = "awk 'NF >= "+strtrim(string(nf),2)+"' " + file + " > "  + ftmp1
   IF vb THEN print, cmd
   spawn,cmd
   fil = ftmp1
   ;
   ; determine array size
   readcol, fil, val, format='(a)', comment=cstr
   hdimen = n_elements(val)
   IF vb THEN print,'rows in structure = ', hdimen
   ;
   ; make fiducial velocity array
   v_fid_ = make_array(hdimen, /float, value=0.0)
   ;
   ; define structure
   dstring = ''
   FOR i = 0, nf - 1 DO BEGIN 
      IF i GT 0 THEN dstring = dstring + ','
      ptr[i] = where(tgood EQ strupcase(typ[i]))
      IF ptr[i] GE 0 THEN BEGIN 
         dstring = dstring + dgood[ptr[i]]
      ENDIF ELSE BEGIN 
         print, '**** Error at field # ', i, ' unknown type : ', typ[i]
         stop
      ENDELSE 
   ENDFOR
   IF vb THEN BEGIN 
      print,'structure definition string = '
      print,dstring
   ENDIF 
   ;
   ; open dbd file, write top of the file
   fildbd  = filo + '.dbd'
   openw, lu, fildbd, /get_lun
   printf, lu, '#title'
   printf, lu, title
   printf, lu, ''
   printf, lu, '#maxentries'
   printf, lu, strtrim(string(hdimen),2)
   printf, lu, ''
   printf, lu, '#items'
   ;
   ; Add derived fields
   field0   = [field, field2]
   dstring0 = dstring + ',' + dstring2
   ;
   ; create structure to populate
   create_struct2, hicat, 'HICAT', field0, dstring0, dimen=hdimen
   ;
   ; loop through fields and populate structure
   ; awk out field from input file
   ; read field into array
   ; write row of dbh file
   FOR i = 0, nf-1 DO BEGIN 
      ;
      ; spawn awk cmd to read column
      cmd = "awk 'substr($1,1,1) != "+cstr2+" {print $"+strtrim(string(i+1),2)+"}' "+fil+" > "+fwork
      IF vb THEN print, cmd
      spawn, cmd
      ;
      ; read field into an array
      readcol, fwork, sarr, format='(a)'
      na = n_elements(sarr)
      CASE tgood[ptr[i]] OF 
         ;
         ; Name - strip off HIPASS
         'N' : arr = strmid(sarr,6,nlen)
         ;
         ; String - do nothing
         'A' : arr = sarr
         ;
         ; Integer - convert valid entries
         'I' : BEGIN 
                  arr = make_array(na, /int, value=indefi)
                  FOR j = 0, na-1 DO BEGIN 
                     good = strnumber(sarr[j], val) AND strtrim(sarr[j],2) NE '-' 
                     IF good THEN arr[j] = fix(val)
                  ENDFOR 
               END
         ;
         ; Long - convert valid entries
         'L' : BEGIN 
                  arr = make_array(na, /long, value=indefl)
                  FOR j = 0, na-1 DO BEGIN 
                     good = strnumber(sarr[j], val) AND strtrim(sarr[j],2) NE '-' 
                     IF good THEN arr[j] = long(val)
                  ENDFOR 
               END
         ;
         ; float - convert valid entries
         'F' : BEGIN 
                  arr = make_array(na, /float, value=indeff)
                  FOR j = 0, na-1 DO BEGIN 
                     good = strnumber(sarr[j], val) AND strtrim(sarr[j],2) NE '-'
                     IF NOT good THEN val = indeff
                     arr[j] = float(val)
                  ENDFOR 
               END
         ;
         ; Double precision - convert valid entries
         'D' : BEGIN 
                  arr = make_array(na, /double, value=indeff)
                  FOR j = 0, na-1 DO BEGIN 
                     good = strnumber(sarr[j], val) AND strtrim(sarr[j],2) NE '-' 
                     IF good THEN arr[j] = val 
                  ENDFOR 
               END
         ;
         ; Sexigessimal - convert to double precision
         'S' : arr = sexideg(sarr, delim=':')
         ;
         ; Sexigessimal hours - convert to double prec. degrees
         'H' : arr = 15.0d0*sexideg(sarr, delim=':')
         ;
         ; radio convention velocity - convert to optical convention
         'V' : BEGIN 
                  arr = make_array(na, /float, value=indeff)
                  FOR j = 0, na-1 DO BEGIN 
                     good = strnumber(sarr[j], val) AND strtrim(sarr[j],2) NE '-' 
                     IF good THEN IF keyword_set(noconvrt) $
                      THEN arr[j] = float(val) $
                      ELSE arr[j] = float(val/(1.0d0-val/c))
                  ENDFOR 
               END
         ;
         ; radio convention velocity width - convert to 
         ; optical convention.  Warn if fiducial velocity not set.
         'W' : BEGIN 
                  IF fidset EQ 0b THEN $
                   print,'**** Warning converting width or flux: '+field[i]+$
                         ' before fiducial velocity read'
                  arr = make_array(na, /float, value=indeff)
                  FOR j = 0, na-1 DO BEGIN 
                     good = strnumber(sarr[j], val) AND strtrim(sarr[j],2) NE '-' 
                     IF good THEN IF keyword_set(noconvrt) $
                     THEN arr[j] = float(val) $
                     ELSE arr[j] = float(val)/(1.0-v_fid_[j]/c)
                  ENDFOR 
               END
      ENDCASE 
      ;
      ; if this is the column corresponding to fiducial velocity, 
      ; then store.
      IF field[i] EQ v_fld THEN BEGIN  
         v_fid_ = arr
         fidset = 1b
      ENDIF 
      ;
      ; if this is the column corresponding to backup fiducial velocity, 
      ; then store.
      IF field[i] EQ v_fld2 THEN BEGIN  
         v_fid2_ = arr
         fidset2 = 1b
      ENDIF 
      ;
      ; put array into structure
      cmd = "hicat."+field[i]+" = arr"
      result = execute(cmd)
      IF vb AND result EQ 1 THEN print, cmd + " SUCEEDED!" 
      IF result NE 1 THEN BEGIN 
         print, "**** Error: " + cmd + " FAILED!"
         stop
      ENDIF  
      ;
      ; write item line of dbd file
      IF tout[ptr[i]] EQ 'C' THEN BEGIN 
         typstr = 'C*'+strtrim(string(max(strlen(arr))),2)
      ENDIF ELSE BEGIN 
         typstr = tout[ptr[i]]+tlen[ptr[i]]
      ENDELSE 
      nk  = min([mdlen, strlen(dscript[i])])
      printf,lu,field[i]+'   '+typstr+'   "'+strmid(dscript[i], 0, nk)+'"'
      ;
      ; stop
      ;keywait, 'press any key to continue...'
   ENDFOR 
   ;
   ; read flowmodel if keyword set. 
   IF keyword_set(flowmod) THEN BEGIN  
      readcol,flowmod,dum1,dum2,dum3,vhin,vlg,vcmb,vvir,vga,vshap,sty,iau,dum4,dum5,vtonry,$
       format='(a12,f,f,f,f,f,f,f,f,f,f,f,f,f)'
   ENDIF ELSE BEGIN 
      vshap  = 0.0*v_fid_ + indeff
      vtonry = vshap
      vcmb   = 0.0
   ENDELSE 
   ;
   ; derive additional fields
   ;
   ; -- galactic coords
   ra        = hicat.ra
   dec       = hicat.dec
   glactc, ra, dec, 2000.0, glong, glat, 1, /degree
   ;
   ; -- get ebv from Schlegel maps
   ebv       = dust_getval(glong, glat, /interp, outfile=febv1)
   mask_ebv  = dust_getval(glong, glat, map='mask', outfile=febv2)
   ;
   ; -- velocity to use
   vuse      = v_fid_
   k         = where(v_fid_ LE indeff, nk)
   IF nk GT 0 THEN vuse[k] = v_fid2_[k]
   k         = where(arr LE indeff, nk)
   IF nk GT 0 THEN print, 'Warning there are still ',nk, ' undefined velocities'
   ;
   ; -- local group corrected velocity
   vlg       = vuse - 79.0*cos(invrad*glong)*cos(invrad*glat) + $
               296.0*sin(invrad*glong)*cos(invrad*glat) - 36.0*sin(invrad*glat)
   IF nk GT 0 THEN vlg[k] = indeff
   ;
   ; -- distance
   distance  = vshap / h0
   k         = where(distance LE distmin, nk)
   IF nk GT 0 THEN distance[k] = distmin
   k         = where(vshap LE indeff, nk)
   IF nk GT 0 THEN distance[k] = indeff
   ;
   ; -- HI mass
   sint      = hicat.sint
   logmhi    = 2.36e5*distance*distance*sint
   k         = where(logmhi LE 0.0 OR distance LT 0.0, nk)
   IF nk GT 0 THEN logmhi[k] = 0.01
   logmhi    = alog10(logmhi)
   IF nk GT 0 THEN logmhi[k] = indeff
   ;
   FOR i = 0, nf2-1 DO BEGIN 
      ;
      ; -- VUSE - where defined, take as v_fid_.  Where not defined 
      ;           take as v_fid2_.
      CASE field2[i] OF 
         'GLONG' :    arr = glong
         'GLAT':      arr = glat
         'EBV' :      arr = ebv
         'MASK_EBV':  arr = byte(mask_ebv)
         'VUSE':      arr = vuse 
         'VLG' :      arr = vlg
         'VCMB' :     arr = vcmb
         'VSHAP' :    arr = vshap
         'VTONRY' :   arr = vtonry
         'DISTANCE' : arr = distance
         'LOGMHI':    arr = logmhi
      ENDCASE 
      ;
      ; put array into structure
      cmd = "hicat."+field2[i]+" = arr"
      result = execute(cmd)
      IF vb AND result EQ 1 THEN print, cmd + " SUCEEDED!" 
      IF result NE 1 THEN BEGIN 
         print, "**** Error: " + cmd + " FAILED!"
         stop
      ENDIF  
      ;
      ; set descriptor string giving type of array and num. of bytes
      kk     = where(dgood EQ strmid(dstring2,i*2,1), nkk)
      IF nkk EQ 1 THEN typstr = tout[kk]+'*'+tlen[kk] ELSE typstr = 'R*4'
      ;
      ; write item line of dbd file
      printf,lu,field2[i]+'   '+typstr+'   "'+dscript2[i]+'"'
   ENDFOR 
   ;
   ; write index blocks of dbd file
   IF keyword_set(indblk) THEN BEGIN 
      printf,lu,'  '
      printf,lu,'#index'
      ni = n_elements(indblk)
      FOR i = 0, ni-1 DO BEGIN 
         printf,lu,strupcase(strtrim(indblk[i],2))+'   sort'
      ENDFOR 
   ENDIF 
   ;
   ; clean up, close dbd file and delete temporary files
   free_lun, lu
   spawn, '/bin/rm '+fwork
   spawn, '/bin/rm '+ftmp1
   ; spawn, '/bin/rm '+ftmp2
   ;
   ; create database
   !PRIV = 2
   dbcreate, filo, 1, 1, /external
   dbopen, filo, 1
   dbbuildstruct, hicat
   dbclose,dummy
END 
