PRO ssoup_mkhtml, ll,  srcdir, basedir, outdir, inputstr, $ 
                  uselink=uselink
  ;
  ; Makes a web page showing the output from a ssoup run.  
  ; The output file are written, copied or linked to
  ;
  ; <basedir>/<outdir>
  ;
  ;   ll           -> logical unit to write log file entries to
  ;   srcdir       -> source directory
  ;   basedir      -> base directory for output
  ;   outdir       -> output directory. 
  ;   inputstr     -> an input structure, containing all of the following: 
  ;   hname        -> hipass names
  ;   fjpg_low     -> name of low cut jpg images
  ;   fjpg_high    -> name of high cut jpg images
  ;   fjpg_mlow1   -> name of low cut jpg images masked with maskcmd=1
  ;   fjpg_mhigh1  -> name of high cut jpg images masked with maskcmd=1
  ;   fjpg_mlow2   -> name of low cut jpg images masked with maskcmd=2
  ;   fjpg_mhigh2  -> name of high cut jpg images masked with maskcmd=2
  ;   fjpg_mlow3   -> name of low cut jpg images masked with maskcmd=3
  ;   fjpg_mhigh3  -> name of high cut jpg images masked with maskcmd=3
  ;   fjpg_imlow1  -> name of low cut jpg images masked with maskcmd=-1
  ;   fjpg_imhigh1 -> name of high cut jpg images masked with maskcmd=-1
  ;   fjpg_imlow2  -> name of low cut jpg images masked with maskcmd=-2
  ;   fjpg_imhigh2 -> name of high cut jpg images masked with maskcmd=-2
  ;   fjpg_imlow3  -> name of low cut jpg images masked with maskcmd=-3
  ;   fjpg_imhigh3 -> name of high cut jpg images masked with maskcmd=-3
  ;   fcompare     -> name of db vs. ssoup comparison file 
  ;   scalprof     -> name of ascii output calibrated surface 
  ;                   brightness/color profiles
  ;   fcalprof     -> name of ascii output calibrated enclosed 
  ;                   flux / color profiles
  ;   scalprof0    -> name of ascii output calibrated surface 
  ;                   brightness/color profiles - dust corrected
  ;   fcalprof0    -> name of ascii output calibrated enclosed 
  ;                   flux / color profiles - dust corrected
  ;   profjpg      -> name of output profile plot in jpg format
  ;   profps       -> name of output profile plot in ps format
  ;   hafuvjpg     -> name of output raw Halpha/fuv plot in jpg format
  ;   hafuvps      -> name of output raw Halpha/fuv plot in ps format
  ;   hafuvjpg0    -> name of output dust corr Halpha/fuv plot in jpg format
  ;   hafuvps0     -> name of output dust corr Halpha/fuv plot in ps format
  ;   fbplot_jpg   -> name of background sky box plot files in JPG format
  ;   fbplot_eps   -> name of background sky box plot files in EPS format
  ;   uselink      -> if set, then link files to outdir, otherwise they 
  ;                   are copied.
  ;
  ; G. Meurer ICRAR/UWA  07/2010  (ICRAR/UWA): wrote code
  ; G. Meurer ICRAR/UWA  08/2012  (ICRAR/UWA):
  ;           - now mark up sky background plot files
  ;           - improve documentation
  prog      = 'SSOUP_MKHTML: '
  subtitle  = 'Results from SSOUP'
  callprog  = 'SSOUP'
  widthi    = 200
  widthp    = 300
  widthh    = 400
  widths    = 600
  ;
  COMMON bands, band, nband, bandnam, bandavail, nbandavail, combo, ncombo
  ;
  ; expand base and source directories
  IF basedir EQ '.' THEN bdir = file_expand_path(basedir) ELSE bdir = expand_path(basedir)
  IF srcdir EQ '.' THEN sdir = file_expand_path(srcdir) ELSE sdir = expand_path(srcdir)
  ;
  ; go to base directory
  cd, bdir, current=cwd
  ;
  ; check if target directory is there.  
  ; * if so nuke it
  ; * otherwise or afterwards create clean directory having target name
  ; * then enter directory
  finfo      = file_info(outdir)
  IF finfo.exists THEN BEGIN 
     plog,ll,prog,'Deleting old directory'
     file_delete, outdir, /recursive
  ENDIF 
  plog,ll,prog,'creating directory, name = '+outdir
  file_mkdir, outdir
  ;
  ch         = strmid(bdir,0,1,/reverse_offset)
  IF ch NE '/' THEN bdir = bdir+'/'
  odir = bdir+outdir 
  plog,ll,prog,'going to source directory = '+sdir
  cd, sdir
  ;
  ; copy (or link) files to the output directory
  f2cp       = bdir+[inputstr.fjpg_low, inputstr.fjpg_high, inputstr.fjpg_mlow1, $
               (inputstr.fjpg_mhigh1), inputstr.fjpg_mlow2, inputstr.fjpg_mhigh2, $
               (inputstr.fjpg_mlow3), inputstr.fjpg_mhigh3, inputstr.fjpg_imlow1, $
               (inputstr.fjpg_imhigh1), inputstr.fjpg_imlow2, inputstr.fjpg_imhigh2, $
               (inputstr.fjpg_imlow3), inputstr.fjpg_imhigh3, inputstr.fcompare, inputstr.scalprof, $
               (inputstr.fcalprof), inputstr.scalprof0, inputstr.fcalprof0, inputstr.profjpg, $
               (inputstr.profps), inputstr.hafuvjpg, inputstr.hafuvps, inputstr.hafuvjpg0, $
               (inputstr.hafuvps0), inputstr.fbplotj, inputstr.fbplote]
  nf         = n_elements(f2cp)
  IF NOT keyword_set(uselink) THEN BEGIN 
     FOR jj = 0, nf-1 DO BEGIN 
        plog,ll,prog,'copying '+f2cp[jj]+' to '+odir
        file_copy, f2cp[jj], odir
     ENDFOR 
  ENDIF ELSE BEGIN 
     FOR jj = 0, nf-1 DO BEGIN 
        plog,ll,prog,'linking '+f2cp[jj]+' to '+odir
        file_link, f2cp[jj], odir
     ENDFOR 
  ENDELSE 
  ;
  cd, odir
  ;
  ; open web page, write top of file
  filo       = inputstr.hname+'_ssoup.html'
  plog,ll,prog,'opening file: '+filo
  openw,lu,filo,/get_lun
  plog,ll,prog,'starting markup...'
  singg_pagetop,lu,inputstr.hname,subtitle
  ;
  ; determine URL to SR2QA page and sample page links
  l1tab      = sample_link('')
  link1      = sample_link(inputstr.hname)
  l2tab      = sr2qa_link('')
  link2      = sr2qa_link(inputstr.hname)
  printf,lu, 'SINGG sample page <a href="'+link1+'">link</a> (<a href="'+l1tab+'">full sample table</a>)<br>'
  printf,lu, 'SR2QA page <a href="'+link2+'">link</a>  (<a href="'+l2tab+'">full SR2QA table</a>)<br>'
  printf,lu, '<hr>'
  ;
  ; table of three color thumbnails and links
  plog,ll,prog,'creating 3 colour image table'
  printf,lu,'<h3>Three colour images</h3>'
  printf,lu,'<P><TABLE border=1 cellpadding=3>'
  printf,lu,'<tr>'
  printf,lu,'  <th colspan=2><b>Three colour images, high cut</b></th>'
  printf,lu,'  <th colspan=2><b>Three colour images, low cut</b></th>'
  printf,lu,'</tr>'
  for i=0,ncombo-1,2 do begin
      printf,lu,'<tr>'
      ssoup_imcell,ll,lu,inputstr.fjpg_high[i],fjpgo,width=widthi
      if (i+1) ne ncombo then ssoup_imcell,ll,lu,inputstr.fjpg_high[i+1],fjpgo,width=widthi
      ssoup_imcell,ll,lu,inputstr.fjpg_low[i],fjpgo,width=widthi
      if (i+1) ne ncombo then ssoup_imcell,ll,lu,inputstr.fjpg_low[i+1],fjpgo,width=widthi
      printf,lu,'</tr>'
  endfor
  printf,lu,'</table>'
  printf,lu,'<hr>'
  ;
  ; mark-up profile plots
  plog,ll,prog,'marking up three color image'
  printf,lu,'<h3>Surface brightness profile</h3>'
  pp        = strpos(inputstr.profjpg,'.jpg')
  fjpgo     = strmid(inputstr.profjpg,0,pp)+'_sm.jpg'
  cmd       = 'convert '+inputstr.profjpg+' -resize '+numstr(widthp)+' '+fjpgo
  plog,ll,prog,'making thumbnail using command: '+cmd
  spawn,cmd
  printf,lu,'<a href="'+inputstr.profjpg+'"><img src="'+fjpgo+'"></a><br>'
  printf,lu,'<a href="'+inputstr.profps+'">PS</a> &nbsp; Text: <a href="'+inputstr.scalprof+'">raw</a>, <a href="'+inputstr.scalprof0+'">dust corr.</a>'
  printf,lu,'<hr>'
  ;
  ; mark up Halpha/FUV versus surface brightness plots
  plog,ll,prog,'marking up Halpha/FUV vs surface brightness plots'
  printf,lu,'<h3>Local H&alpha;/FUV versus surface brightness plots</h3>'
  printf,lu,'<P><TABLE border=1 cellpadding=3>'
  printf,lu,'<tr>'
  printf,lu,'  <th><b>H&alpha;/FUV versus &Sigma; , raw</b></th>'
  printf,lu,'  <th><b>H&alpha;/FUV versus &Sigma; , dust corrected</b></th>'
  printf,lu,'</tr>'
  printf,lu,'<tr>'
  ssoup_imcell,ll,lu,inputstr.hafuvjpg,fjpgo,width=widthh,uannot='<a href="'+inputstr.hafuvps+'">PS</a>',/plot
  ssoup_imcell,ll,lu,inputstr.hafuvjpg0,fjpgo,width=widthh,uannot='<a href="'+inputstr.hafuvps0+'">PS</a>',/plot
  printf,lu,'</tr>'
  printf,lu,'</table>'
  printf,lu,'<hr>'
  ;
  ; read-in database comparison table
  plog,ll,prog,'reading database comparison table: '+inputstr.fcompare
  fmtc     = '(a,a,f,f,f,a,f,f,f)'
  readcol, inputstr.fcompare, snam, band, r50d, flxd, eflxd, dum, r50s, flxs, eflxs, format=fmtc
  nc       = n_elements(snam)
  ;
  ; Mark up database comparison table
  plog,ll,prog,'marking up database comparison table '
  printf,lu,'<h3>Database comparison</h3>'
  printf,lu,'<P><TABLE border=1 cellpadding=3>'
  printf,lu,'<tr>'
  printf,lu,'  <th rowspan=2>SINGG<br>name</th><th rowspan=2>band</th><th colspan=3>Database</th><th colspan=3>SSOUP</th>'
  printf,lu,'</tr>'
  printf,lu,'<tr>'
  printf,lu,'  <th>R<sub>50</sub></th><th>mag/log(flx)</th><th>err</th><th>R<sub>50</sub></th><th>mag/log(flx)</th><th>err</th>'
  printf,lu,'</tr>'
  FOR ii = 0, nc-1 DO BEGIN 
     printf,lu,'<tr>'
     printf,lu,'  <td>'+snam[ii]+'</td><td>'+band[ii]+'</td>'+$
            '<td>'+numstr(r50d[ii])+'</td><td>'+numstr(flxd[ii])+'</td><td>'+numstr(eflxd[ii])+'</td>'+$
            '<td>'+numstr(r50s[ii])+'</td><td>'+numstr(flxs[ii])+'</td><td>'+numstr(eflxs[ii])+'</td>'
     printf,lu,'</tr>'
  ENDFOR 
  printf,lu,'</table>'
  printf,lu,'<hr>'
  ;
  ; mark-up sky background files
  plog,ll,prog,'creating sky background table of images.'
  printf,lu,'<h3>Sky background maps</h3>'
  printf,lu,'<P><TABLE border=1 cellpadding=3>'
  printf,lu,'<th>Band</th><th>Plots (data &nbsp; &nbsp; &nbsp; model &nbsp; &nbsp; &nbsp; residuals)</th>'
  FOR ii = 0, nbandavail-1 DO BEGIN 
     printf,lu,'<tr>'
     printf,lu,'<td>'+bandnam[ii]+'</td>'
     ssoup_imcell,ll,lu,inputstr.fbplotj[ii],fjpgo,width=widths,uannot='<a href="'+inputstr.fbplote[ii]+'">EPS</a>',/plot
     printf,lu,'</tr>'
  ENDFOR
  printf,lu,'</TABLE>'
  ;
  ; mark-up masked images
  printf,lu,'<h3>Three colour images with masks applied</h3>'
  plog,ll,prog,'creating 3 colour bad object masked images table, high cut.'
  printf,lu,'<P><TABLE border=1 cellpadding=3>'
  printf,lu,'<tr>'
  printf,lu,'  <th colspan=2><b>Three colour images, bad objects mask, high cut</b></th>'
  printf,lu,'  <th colspan=2><b>Three colour images, bad objects inverted mask, high cut</b></th>'
  printf,lu,'</tr>'
  for i=0,ncombo-1,2 do begin
      printf,lu,'<tr>'
      ssoup_imcell,ll,lu,inputstr.fjpg_mhigh1[i],fjpgo,width=widthi
      if (i+1) ne ncombo then ssoup_imcell,ll,lu,inputstr.fjpg_mhigh1[i+1],fjpgo,width=widthi
      ssoup_imcell,ll,lu,inputstr.fjpg_imhigh1[i],fjpgo,width=widthi
      if (i+1) ne ncombo then ssoup_imcell,ll,lu,inputstr.fjpg_imhigh1[i+1],fjpgo,width=widthi
      printf,lu,'</tr>'
  endfor
  printf,lu,'</table>'
  ;
  plog,ll,prog,'creating 3 colour bad object masked images table, low cut.'
  printf,lu,'<P><TABLE border=1 cellpadding=3>'
  printf,lu,'<tr>'
  printf,lu,'  <th colspan=2><b>Three colour images, bad objects mask, low cut</b></th>'
  printf,lu,'  <th colspan=2><b>Three colour images, bad objects inverted mask, low cut</b></th>'
  printf,lu,'</tr>'
  for i=0,ncombo-1,2 do begin
      printf,lu,'<tr>'
      ssoup_imcell,ll,lu,inputstr.fjpg_mlow1[i],fjpgo,width=widthi
      if (i+1) ne ncombo then ssoup_imcell,ll,lu,inputstr.fjpg_mlow1[i+1],fjpgo,width=widthi
      ssoup_imcell,ll,lu,inputstr.fjpg_imlow1[i],fjpgo,width=widthi
      if (i+1) ne ncombo then ssoup_imcell,ll,lu,inputstr.fjpg_imlow1[i+1],fjpgo,width=widthi
      printf,lu,'</tr>'
  endfor
  printf,lu,'</table>'
  ;
  ;
  plog,ll,prog,'creating 3 colour sky masked images table, high cut.'
  printf,lu,'<P><TABLE border=1 cellpadding=3>'
  printf,lu,'<tr>'
  printf,lu,'  <th colspan=2><b>Three colour images, sky mask, high cut</b></th>'
  printf,lu,'  <th colspan=2><b>Three colour images, sky inverted mask, high cut</b></th>'
  printf,lu,'</tr>'
  printf,lu,'<tr>'
  for i=0,ncombo-1,2 do begin
      printf,lu,'<tr>'
      ssoup_imcell,ll,lu,inputstr.fjpg_mhigh2[i],fjpgo,width=widthi
      if (i+1) ne ncombo then ssoup_imcell,ll,lu,inputstr.fjpg_mhigh2[i+1],fjpgo,width=widthi
      ssoup_imcell,ll,lu,inputstr.fjpg_imhigh2[i],fjpgo,width=widthi
      if (i+1) ne ncombo then ssoup_imcell,ll,lu,inputstr.fjpg_imhigh2[i+1],fjpgo,width=widthi
      printf,lu,'</tr>'
  endfor
  printf,lu,'</table>'
  ;
  plog,ll,prog,'creating 3 colour sky masked images table, low cut.'
  printf,lu,'<P><TABLE border=1 cellpadding=3>'
  printf,lu,'<tr>'
  printf,lu,'  <th colspan=2><b>Three colour images, sky mask, low cut</b></th>'
  printf,lu,'  <th colspan=2><b>Three colour images, sky inverted mask, low cut</b></th>'
  printf,lu,'</tr>'
  for i=0,ncombo-1,2 do begin
      printf,lu,'<tr>'
      ssoup_imcell,ll,lu,inputstr.fjpg_mlow2[i],fjpgo,width=widthi
      if (i+1) ne ncombo then ssoup_imcell,ll,lu,inputstr.fjpg_mlow2[i+1],fjpgo,width=widthi
      ssoup_imcell,ll,lu,inputstr.fjpg_imlow2[i],fjpgo,width=widthi
      if (i+1) ne ncombo then ssoup_imcell,ll,lu,inputstr.fjpg_imlow2[i+1],fjpgo,width=widthi
      printf,lu,'</tr>'
  endfor
  printf,lu,'</table>'
  ;
  plog,ll,prog,'creating 3 colour object only masked images table, high cut.'
  printf,lu,'<P><TABLE border=1 cellpadding=3>'
  printf,lu,'<tr>'
  printf,lu,'  <th colspan=2><b>Three colour images, object only mask, high cut</b></th>'
  printf,lu,'  <th colspan=2><b>Three colour images, object only inverted mask, high cut</b></th>'
  printf,lu,'</tr>'
  for i=0,ncombo-1,2 do begin
      printf,lu,'<tr>'
      ssoup_imcell,ll,lu,inputstr.fjpg_mhigh3[i],fjpgo,width=widthi
      if (i+1) ne ncombo then ssoup_imcell,ll,lu,inputstr.fjpg_mhigh3[i+1],fjpgo,width=widthi
      ssoup_imcell,ll,lu,inputstr.fjpg_imhigh3[i],fjpgo,width=widthi
      if (i+1) ne ncombo then ssoup_imcell,ll,lu,inputstr.fjpg_imhigh3[i+1],fjpgo,width=widthi
      printf,lu,'</tr>'
  endfor
  printf,lu,'</table>'
  ;
  plog,ll,prog,'creating 3 colour object only masked images table, low cut.'
  printf,lu,'<P><TABLE border=1 cellpadding=3>'
  printf,lu,'<tr>'
  printf,lu,'  <th colspan=2><b>Three colour images, object only mask, low cut</b></th>'
  printf,lu,'  <th colspan=2><b>Three colour images, object only inverted mask, low cut</b></th>'
  printf,lu,'</tr>'
  for i=0,ncombo-1,2 do begin
      printf,lu,'<tr>'
      ssoup_imcell,ll,lu,inputstr.fjpg_mlow3[i],fjpgo,width=widthi
      if (i+1) ne ncombo then ssoup_imcell,ll,lu,inputstr.fjpg_mlow3[i+1],fjpgo,width=widthi
      ssoup_imcell,ll,lu,inputstr.fjpg_imlow3[i],fjpgo,width=widthi
      if (i+1) ne ncombo then ssoup_imcell,ll,lu,inputstr.fjpg_imlow3[i+1],fjpgo,width=widthi
      printf,lu,'</tr>'
  endfor
  printf,lu,'</table>'
  ;
  ; mark up comparison table
  ;
  ; make thumbnails of plots
  ;
  ; show plot thumbnails and links
  ;
  ; write bottom of page
  singg_pagebot,lu,prog=callprog
  plog,ll,prog,'closing file: '+filo
  free_lun, lu
  ;
  ; finish
  plog,ll,prog,'returning to starting directory: '+cwd
  cd, cwd
  plog,ll,prog,'finished'
END 
