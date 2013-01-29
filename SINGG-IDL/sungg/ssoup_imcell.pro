PRO ssoup_imcell, ll, lu, fjpgi, fjpgo, width=width, uannot=uannot, plot=plot, noresize=noresize
  ;
  ; mark up an html table cell to show
  ; * thumbnail with link to full size image
  ; * RGB mapping unless plot is set
  ;
  ; ll       -> logical unit of log file
  ;             (already opened)
  ; lu       -> logical unit of output html file
  ;             (already opened)
  ; fjpgi    -> name of jpg image for whivh the mark-up
  ;             and thumbnail implies
  ; fjpgo    <- name of created thumbnail jpg
  ; width    -> if set the width of the thumbnail in pixels
  ;             default is 200 pixels
  ; uannot   -> if set the annotation string that will be set 
  ;             underneath the image.  
  ; plot     -> if set the passed jpg is a plot not an image so 
  ;             no RGB mark up is done.
  ; noresize -> if set, skip JPG resizing
  ; 
  ; G. Meurer (UWA/ICRAR) 07/2010: originally written
  ; G. Meurer (UWA/ICRAR) 08/2012: fix bug with quotes and <font color=>
  ;
  prog      = 'SSOUP_IMCELL: '
  COMMON bands, band, nband, bandnam, bandavail, nbandavail, combo
  bandlet   =  strlowcase(strmid(bandavail, 0, 2))
  fc        = '<font color="'+['#FF0000', '#00FF00', '#0000FF']+'">'
  ;
  ; find last suffix delimited by "_"
  pp        = strpos(fjpgi, '_', /reverse_search)+1
  suf       = strmid(fjpgi,pp,6)
  rgbstr    = '<font color="#FF0000">R</font>,<font color="#00FF00">G</font>,<font color="#0000FF">B</font> = '
  blah      = bytarr(nbandavail)
  ; yawn
  for kk = 0,nbandavail-1 do blah[kk] = strmatch(suf, '*'+bandlet[kk]+'*')
  jj = where(blah, njj)
  for ii=0, 2 do begin
     if njj eq 3 then rgbstr = rgbstr+fc[ii]+bandnam[jj[ii]]+'</font>' else rgbstr = rgbstr+fc[ii]+'?</font>'
     if ii ne 2 then rgbstr = rgbstr + ','
  endfor
  ;
  ; set annotation for under the image
  undannot  = ''
  IF keyword_set(uannot) THEN undannot ='<br>'+uannot
  ;
  ; set width to default if not passed
  IF NOT keyword_set(width) THEN width = 200
  ;
  pp        = strpos(fjpgi,'.jpg')
  fjpgo     = strmid(fjpgi,0,pp)+'_sm.jpg'
  if not keyword_set(noresize) then begin
      cmd       = 'convert '+fjpgi+' -resize '+numstr(width)+' '+fjpgo
      plog,ll,prog,'making thumbnail using command: '+cmd
      spawn,cmd
  endif
  IF NOT keyword_set(plot) THEN BEGIN 
     printf,lu,'  <td align="center">'+rgbstr+'</br><a href="'+fjpgi+'"><img src="'+fjpgo+'"></a>'+undannot+'</td>'
  ENDIF ELSE BEGIN 
     printf,lu,'  <td align="center"><a href="'+fjpgi+'"><img src="'+fjpgo+'"></a>'+undannot+'</td>'
  ENDELSE 
END
