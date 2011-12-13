;+
; NAME:
;	COLWRITE
; PURPOSE:
;	Write a free-format ASCII data file with columns of data.
;
; CALLING SEQUENCE:
;	COLWRITE, name, v1, [ v2, v3, v4, v5, ...  v25 , 
;             /DEBUG ,  /SILENT , HEADER = , NUMLINE = ]
;
; INPUTS:
;	NAME - Name of ASCII data file, scalar string. 
;               In VMS, an extension of .DAT is assumed, if not supplied.
;	V1,V2,V3,...V15 - IDL vectors containing columns of data.
;		Up to 25 columns may be written.  The type of the
;		output vectors are the types supplied.
;
; OPTIONAL INPUT KEYWORDS:
;
;	SILENT - If SILENT is set and non-zero then messages will be 
;		suppressed.
;	DEBUG - If this keyword is non-zero, then additional information is
;		 printed as COLWRITE attempts to write the file.
;	HEADER - String array to be placed at top of text file, one
;	          array element per line.
;	NUMLINE - Scalar specifying number of lines in the file to write.  
;		Default is to write length of v1 data.
;
; OUTPUTS:
;
; EXAMPLES:
;	Write a file POSITION.DAT to contain a star name and 6 columns
;	of data giving an RA and Dec in sexigesimal format.
;
;	IDL> COLWRITE,'POSITION',name,hr,min,sec,deg,dmin,dsec  
;
;
; RESTRICTIONS:
;	This procedure is designed for generality and not for speed.
;	If a large ASCII file is to be read repeatedly, it may be worth
;	writing a specialized reader.
;
;	Columns to be read as strings must not contain spaces or commas, 
;	since these are interpreted as column delimiters.    Use READFMT
;	to read such files.
;
;	Numeric values are converted to specified format.  For example,
;	the value 0.13 read with an 'I' format will be converted to 0.
;
; PROCEDURES CALLED
;	ZPARCHECK
;
; REVISION HISTORY:
;
;       Thu May 22 20:00:21 1997, ACS Filter Testing <acsftest@acs3>
;           Created based heavily on READCOL procedure. WJM
;
;-
pro colwrite,filename,v1,v2,v3,v4,v5,v6,v7,v8,v9,v10,v11,v12,v13,v14,v15, $
             v16,v17,v18,v19,v20,v21,v22,v23,v24,v25, $
             DEBUG=debug, SILENT=silent, HEADER = header, $
             NUMLINE=numline

   On_error,2       ;Return to caller

   if N_params() lt 2 then begin
      print,'Syntax - readcol, filename, v1, [ v2, v3,...v25, '
      print,'        /SILENT  ,HEADER =, NUMLINE = , /DEBUG]'
      return
   endif

; Get number of lines in file

   ndata = N_ELEMENTS( v1 )
   if ndata LE 0 then return

   if keyword_set(DEBUG) then $
    message,'V1 contains ' + strtrim(ndata,2) + ' lines',/INF

   if not keyword_set( header ) then headlen = 0 ELSE headlen = n_elements(header)
   nlines = ndata + headlen
   if keyword_set( NUMLINE ) then nlines = numline < nlines

   ncol = N_params() - 1 ;Number of columns of data expected

   if keyword_set(DEBUG) then begin
      message,'writing ' + strtrim(nlines,2) + ' lines',/INF
      message,'writing ' + strtrim(ncol,2) + ' columns',/INF
   endif
   
; Create output arrays

   vs = ''
   for i = 0L, ncol-1 do begin
      vs = vs + " + '     ' + strtrim(v"+strtrim(i+1,2)+"(j),2)"
   endfor

   cmd = 'temp = ' + vs
   openw, lun, filename, /get_lun

   temp = ' '
   if headlen GT 0 then $
    for i = 0, headlen-1 do printf, lun, header(i) ;Write header

   for j = 0L, ndata-1 do begin
      re = execute( cmd )
      printf, lun, temp
   endfor

   free_lun,lun

   IF NOT keyword_set(silent) THEN message, strtrim(ncol,2)+' columns of ' $
    + strtrim(ndata,2) + ' points written to ' + filename,/info

   return
end
