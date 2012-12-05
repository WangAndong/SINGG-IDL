pro dbbuildstruct,struct,ROWNUM=rownum,NOINDEX=noindex, $
                         STATUS=status,SILENT=silent
;+
; NAME:
;	DBBUILDSTRUCT
; PURPOSE:
;	Build a database by updating or appending new values for every item.  
; EXPLANATION:
;	The database must be opened for update (with DBOPEN) before calling 
;	DBBUILDSTRUCT.
;
; CALLING SEQUENCE:
;	DBBUILDSTRUCT, [ strval, /NOINDEX, /SILENT, STATUS =  ]
;
; INPUTS:
;       STRUCT - an input structure to be added line by line to the database.
;
; OPTIONAL INPUT KEYWORDS:
;       ROWNUM - If this keyword is supplied and non-zero then
;                DBBUILDSTRUCT will update an existing row of the
;                database, rather than append the data to the end.
;                This value will range from 1 to N, and it is the
;                responsibility of the calling routine to ensure that
;                the format of the structure matches that of the
;                existing database.
;	NOINDEX - If this keyword is supplied and non-zero then DBBUILDSTRUCT will
;             *not* create an indexed file.    Useful to save time if
;             DBBUILDSTRUCT is to be called several times and the indexed file
;             need only be created on the last call
;
;	SILENT  - If the keyword SILENT is set and non-zero, then DBBUILDSTRUCT
;	      will not print a message when the index files are generated
;
; OPTIONAL OUTPUT KEYWORD:
;	STATUS - Returns a status code denoting whether the operation was
;	      successful (1) or unsuccessful (0).  Useful when DBBUILDSTRUCT is
;	      called from within other applications.
;
; EXAMPLE:
;	Suppose a database named STARS contains the four items NAME,RA,DEC, and
;	FLUX.   Assume that one already has the four vectors containing the
;	values, and that the database definition (.DBD) file already exists.
;
;	IDL> !PRIV=2                  ;Writing to database requires !PRIV=2
;	IDL> dbcreate,'stars',1,1   ;Create database (.DBF) & index (.DBX) file
;	IDL> dbopen,'stars',1         ;Open database for update
;	IDL> dbbuildstruct,name,ra,dec,flux ;Write 4 vectors into the database
;
; NOTES:
;	Do not call DBCREATE before DBBUILDSTRUCT if you want to append entries
;       to an existing database
;
;	DBBUILDSTRUCT checks that each value vector matches the idl type given
;       in the database definition (.DBD) file, and that character strings
;       are the proper length. 
; REVISION HISTORY:
;	Written          W. Landsman           March, 1989
;	Added /NOINDEX keyword           W. Landsman        November, 1992
;	User no longer need supply all items   W. Landsman  December, 1992 
;	Added STATUS keyword, William Thompson, GSFC, 1 April 1994
;	Added /SILENT keyword, William Thompson, GSFC, October 1995
;	Faster build of external databases on big endian machines 
;				  W. Landsman    GSFC, November 1997  
;	Converted to IDL V5.0   W. Landsman 24-Nov-1997
;       Use SIZE(/TNAME) for error mesage display  W.Landsman   July 2001
;       Fix message display error introduced July 2001  W. Landsman  Oct. 2001 
;       Totally overhauled from dbbuild to use structures instead of
;       arrays, allow for update instead of always append.  D. Hanish 2004

;-
;  On_error,2                            ;Return to caller
  if N_params() LT 1 then begin
    print,'Syntax - DBBUILDSTRUCT, struct,' 
    print,'         /NOINDEX, /SILENT, STATUS =  ]'
    return
  endif

  varnames = TAG_NAMES(struct)
  num_vars = N_TAGS(struct)

  IF NOT KEYWORD_SET(rownum) THEN rownum = 0l ; "0" means "append"

  dtype = ['UNDEFINED','BYTE','INTEGER*2','INTEGER*4','REAL*4','REAL*8', $
           'COMPLEX','STRING','STRUCTURE','DCOMPLEX','POINTER','OBJECT', $ 
           'UNSIGNED*2', 'UNSIGNED*4', 'INTEGER*8','UNSIGNED*8']
 
;  Initialize STATUS as unsuccessful (0).  If the routine is successful, this
;  will be updated below.

  status = 0

  nitem = db_info( 'ITEMS' )
  if N_elements( nitem ) EQ 0 then return

  items = indgen(nitem)
  db_item, items, itnum, ivalnum, idltype, sbyte, numvals, nbyte

  FOR ii = 1,num_vars do begin
    s = SIZE(struct.(ii-1))

    IF s[s[0] + 1] NE idltype[ii] THEN BEGIN
      message, 'Item '+STRTRIM(db_item_info('NAME',ii),2) + $
         ' - parameter '+STRTRIM(ii-1,2)+' - has an incorrect data type',/INF
      message, 'Required data type is '+dtype(idltype[ii]), /INF
      message, 'Supplied data type is '+dtype(s[s[0]+1]), /INF
      RETURN
    ENDIF
  ENDFOR

  external = db_info('external',0)
  if external then noconvert = is_ieee_big() else noconvert = 1b

  entry = make_array( DIMEN = db_info('LENGTH'),/BYTE ) ;Empty entry array
  nvalues = long(db_item_info('NVALUES'))       ;# of values per item
  nbyte = nbyte*nvalues                             ;Number of bytes per item
  Nv = N_ELEMENTS(struct.(0))/nvalues[1]                   
  for ii = 0l, Nv - 1 do BEGIN
     
     IF (ii MOD 100) EQ 0 THEN print, "working on entry : ", ii
     i1 = ii*nvalues         
     i2 = i1+nvalues-1
     grm_dbxput,LONG(rownum),entry,idltype[0],sbyte[0],nbyte[0]
     FOR jj = 1,num_vars DO BEGIN
        s = SIZE(struct.(jj-1))
        temp = struct.(jj-1)
        grm_dbxput,temp[i1[jj]:i2[jj]],entry, $
                   idltype[jj],sbyte[jj],nbyte[jj]
     ENDFOR

     dbwrt,entry,noconvert=noconvert ;Write the entry into the database

  endfor

  if not keyword_set( NOINDEX ) then begin

    indexed = db_item_info( 'INDEX' )      ;Need to create an indexed file?
    if total(indexed) GE 1 then begin
      if not keyword_set(silent) THEN $
	   message,'Now creating indexed files',/INF
      dbindex,items
    endif

  endif

  dbclose

; Mark successful completion, and return.

  status = 1
  RETURN
END
