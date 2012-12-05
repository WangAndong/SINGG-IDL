PRO sr2web_objtab, fhtml, objlis, namelis=namelis, title=title, subtitle=subtitle, ncolumns=ncolumns
   ;
   ; Write an html file containing a table of links to individual
   ; HIPASS object SR2 web pages.  This code should be run in the 
   ; base directory for SR2QA web pages.
   ;
   ; fhtml    -> Name of HTML file to be written
   ; objlis   -> (array) List of object names for the table.  A 
   ;             link to the corresponding SR2QA web page will be made 
   ;             for each element.  Hence the names should correspond
   ;             exactly to the SR2 HIPASS names.
   ; namelis  -> (array) If set this will be the "enclosed text" 
   ;             corresponding to each element in objlis.  If not set 
   ;             the enclosed text will be the same as objlis
   ; title    -> Title for the web page.  This is marked up using 
   ;             <title></title> and <h1></h1> by singg_pagetop.
   ; subtitle -> Subtitle for the web page. This is marked up using
   ;             <h2></h2> in singg_pagetop
   ; ncolumns -> The number of columns in the table.  The default is 8.
   ;
   ; G. Meurer 8/2007
   prog       = 'sr2web_objtab'
   prog2      = strupcase(prog)+': '    ; for messages
   title0     = 'SINGG Release 2 Quality Assurance (Subsample)'
   subtitle0  = ' '
   ncol       = 8
   ;
   print, prog2+'starting...'
   ;
   nobj       = n_elements(objlis)
   ;
   ; set variables according to whether optional parameters are sent.
   IF keyword_set(namelis) THEN BEGIN 
      nnam    = n_elements(namelis)
      IF nnam NE nobj THEN BEGIN 
         print, prog2+'ERROR the number of elements in OBJLIS and NAMELIS must be equal.  They are: ', nobj, nnam
         return
      ENDIF
      ;
      print, prog2+'will mark up NAMELIS as enclosed text to OBJLIS links'
   ENDIF ELSE BEGIN 
      namelis = objlis
      print, prog2+'will mark up OBJLIS as enclosed text to OBJLIS links'
   ENDELSE 
   ;
   IF NOT keyword_set(title) THEN title = title0
   IF NOT keyword_set(subtitle) THEN subtitle = subtitle0
   print, prog2+'web page title = "'+title+'"'
   print, prog2+'web page subtitle = "'+subtitle+'"'
   ;
   IF keyword_set(ncolumns) THEN ncol = ncolumns
   print, prog2+'will write '+strtrim(string(ncol),2)+' columns per table row'
   ;
   ; Open HTML file, write top of page
   print,prog2+'opening file : '+fhtml
   openw, lu, fhtml, /get_lun
   singg_pagetop, lu, title, subtitle
   ;
   ; write top of table of links to object web pages
   printf,lu,'<P><TABLE border=1 cellpadding=3>'
   printf,lu,'<TR bgcolor=#707070>'
   printf,lu,'   <TH align="center" colspan='+strtrim(string(ncol,2))+'>SR2 subsample</TH>'
   printf,lu,'</TR>'
   print, prog2+'writing object links'
   ;
   ; loop through target IDs
   FOR ii = 0, nobj-1 DO BEGIN 
      href    = objlis[ii]+'/'+objlis[ii]+'_sr2qa.html'
      ;
      ; start a new table row if need be
      kk      = ii MOD ncol
      IF kk EQ 0 THEN printf,lu,'<TR bgcolor=#ffffff>'
      ;
      ; markup link to object page
      printf,lu,'   <TD align="center"><a href="'+href+'">'+namelis[ii]+'</a></TD>'
      ;
      ; finish up row if need be
      IF kk EQ ncol-1 THEN printf,lu,'</TR>'
   ENDFOR 
   ;
   ; finish off table
   print,prog2+'finishing off HTML file'
   printf,lu,'</P></TABLE>'
   ;
   ; finish off web page
   singg_pagebot, lu, prog=prog+'.pro'
   free_lun, lu
   ;
   print,prog2+'finished.'
END 

