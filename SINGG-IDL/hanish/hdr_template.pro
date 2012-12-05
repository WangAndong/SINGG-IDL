PRO hdr_template, hd, im, ftemp, hdo, unit_log=unit_log, silent=silent
   ;
   ; Apply a header template to an input fits header creating an 
   ; output header.
   ;
   ; hd    -> input header.used as source.
   ; im    -> image used to create basic properties of fits header.
   ; ftemp -> header template file
   ; hdo   <- output header
   ; unit_log -> If set, the logical unit to write logfile info to.
   ; silent -> If set, no error messages will be printed
   ;
   ; G.R. Meurer 1/2003: Initial version
   ; D. Hanish 6/2006: Added /SILENT keyword, CLOSE, 
   ;                   FREE_LUN, and RETURN
   ; G.R. Meurer 6/2010: changed call of NUMLINES to file_lines
   ;                     pass /initialize to fxhmake to insure 
   ;                     the new header is blank
   IF KEYWORD_SET(unit_log) THEN lo = unit_log ELSE lo = -1
   ;
   ; open template file
   ;nlines = NUMLINES(ftemp)
   nlines = file_lines(ftemp)   ; grm 6/2010
   openr, lu, ftemp, /get_lun
   ;
   ; create basic header
   fxhmake, hdo, im, /initialize  ; add /initialize grm 6/2010
   ;
   ; process each template entry one by one
   linstr = ' '
   FOR i = 0, nlines-1 DO BEGIN 
      READF,lu,linstr
      str = linstr
      cmd = STRUPCASE(gettok(str, ' '))
      kwd = gettok(str, ' ')
      CASE cmd OF 
         'C' : BEGIN
                  ; create
                  fmt  = STRUPCASE(gettok(str, ' '))
                  sval = STRTRIM(gettok(str, '/'),2)
                  com  = STRTRIM(str,2)
                  CASE sval OF 
                     'F'  : val = float(sval)
                     'I'  : val = fix(sval)
                     ELSE : val = sval
                  ENDCASE    
                  fxaddpar, hdo, kwd, val, com
               END 
         'P' : BEGIN 
                  ; pass
                  ; search in old header
                  val = fxpar(hd, kwd, count=count, comment=com)
                  IF count GE 1 THEN fxaddpar, hdo, kwd, val, com $
                   ELSE IF NOT KEYWORD_SET(silent) THEN $
               PRINTF, lo, '**** HDR_TEMPLATE: keyword : '+kwd+ ' Not found'
                  ; not found write to log
               END
      ENDCASE 
   ENDFOR 

   CLOSE,lu
   FREE_LUN,lu

   RETURN

END 
