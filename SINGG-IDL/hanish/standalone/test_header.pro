PRO test_header,runname

  headername = "/home/hanish/header_ieee"
  headerfile = headername+".dbd"

  readcol_new,headerfile,varname,vartype,varcomment, $
              FORMAT="A,A,A",comment='#',SKIPLINE=6
  num_vars = N_ELEMENTS(varname)
  vartype2 = STRARR(num_vars)
  vartype3 = STRARR(num_vars)

  dummy = FLTARR(2)
; test for size
  dbopen,headername,0
  dbext,-1,varname[0],temparr
  dbclose,dummy
  num_gals = N_ELEMENTS(temparr)

  FOR ii = 0,num_vars-1 DO BEGIN
    CASE STRMID(vartype[ii],0,1) OF
      "C": vartype2[ii] = "A"
      "I": vartype2[ii] = "I"
      "R": vartype2[ii] = "F"
    ENDCASE
    CASE STRMID(vartype[ii],0,1) OF
      "C": vartype3[ii] = "A("+STRTRIM(STRING(num_gals),2)+")"
      "I": vartype3[ii] = "I("+STRTRIM(STRING(num_gals),2)+")"
      "R": vartype3[ii] = "F("+STRTRIM(STRING(num_gals),2)+")"
    ENDCASE 
  ENDFOR

  create_struct2,header_old,'Header1',varname,vartype3
  create_struct2,header_new,'Header2',varname,vartype2

  dbopen,headername,0
  FOR ii = 0,num_vars-1 DO BEGIN
    dbext,-1,varname[ii],temparr
    header_old.(ii) = temparr[0:num_gals-1]
  ENDFOR
  dbclose,dummy

  spawn,"ls *ss.fits",imlist
  n_files = N_ELEMENTS(imlist)

  FOR jj = 0,n_files-1 DO BEGIN

      PRINT,"Checking header for image ",imlist[jj]
; First, clear the structure and reset the basic variables.
      FOR kk = 0,num_vars-1 DO BEGIN
          CASE STRMID(vartype[kk],0,1) OF
              "C": header_new.(kk) = ""
              "I": header_new.(kk) = 0
              "R": header_new.(kk) = 0.0
          ENDCASE 
      ENDFOR
      header_new.runname = runname
      
; Determine what type of image it is.
      imlen = STRLEN(imlist[jj])
      IF STRMID(imlist[jj],(imlen-11),3) EQ "sub" THEN BEGIN
          header_new.imtype="net"
      ENDIF ELSE BEGIN
          IF STRMID(imlist[jj],(imlen-13),2) EQ "_6" THEN BEGIN
              header_new.imtype="onband"
          ENDIF ELSE BEGIN
              tempstr = STRUPCASE(STRMID(imlist[jj],(imlen-9),1))
              IF tempstr EQ "R" OR tempstr EQ "C" THEN BEGIN
                  header_new.imtype="cont"
              ENDIF ELSE BEGIN
                  PRINT,"ERROR in test_header: invalid image type for image ",imlist[jj]
                  PRINT,"  for object ",object[ii]
                  RETURN
              ENDELSE
          ENDELSE
      ENDELSE
      
      fits_read,imlist[jj],img,hd,/header_only
      FOR kk = 0,num_vars-1 DO BEGIN
; Next, test to see if varname[kk] is in the header
          tempval = SXPAR(hd,varname[kk],count=count)
          IF count GT 0 THEN BEGIN
; We want to make sure that if it's a string, it trims it.
              IF STRMID(vartype[kk],0,1) EQ "C" THEN tempval = STRTRIM(tempval,2)
              
              header_new.(kk) = tempval
          ENDIF
      ENDFOR                    ; loop over variables
      
; Now that the header's been set, let's compare to the values in the
; database

      FOR ll = 0,num_gals-1 DO BEGIN
        IF STRTRIM(header_old.filename[ll],2) EQ STRTRIM(imlist[jj],2) THEN BEGIN
          PRINT,"Matched file name ",imlist[jj]
          FOR kk = 0,num_vars-1 DO BEGIN

            IF STRTRIM(header_new.(kk),2) NE STRTRIM(header_old.(kk)[ll],2) THEN BEGIN
              PRINT,"  ",varname(kk)," ",header_new.(kk)," ",header_old.(kk)[ll]
            ENDIF
 
          ENDFOR                    ; loop over variables

        ENDIF
      ENDFOR

  ENDFOR                        ; loop over objects

END
