PRO ssoup_mkhtml, ll,  srcdir, basedir, outdir, inputstr, ngal, $ 
                  uselink=uselink, abridged=abridged
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
  ;   inputstr     -> the input structure made by ssoup_inputs.pro
  ;   ngal         -> the number of galaxies detected
  ;   uselink      -> if set, then link files to outdir, otherwise they 
  ;                   are copied.
  ;   abridged     -> if set, output a shorter summary version to index.html
  ;
  ; G. Meurer ICRAR/UWA  07/2010  (ICRAR/UWA): wrote code
  ; G. Meurer ICRAR/UWA  08/2012  (ICRAR/UWA):
  ;           - now mark up sky background plot files
  ;           - improve documentation
  prog      = 'SSOUP_MKHTML: '
  subtitle  = keyword_set(abridged) ? 'Selected Results from SSOUP' : 'Results from SSOUP'
  callprog  = 'SSOUP'
  widthi    = 200
  widthp    = 300
  widthh    = 400
  widths    = 600
  ;
  COMMON bands, band, nband, bandnam, bandavail, nbandavail, combo, ncombo
  ;
  ; expand base and source directories
  bdir = basedir EQ '.' ? file_expand_path(basedir) : expand_path(basedir)
  sdir = srcdir  EQ '.' ? file_expand_path(srcdir)  : expand_path(srcdir)
  ;
  ; go to base directory
  cd, bdir, current=cwd
  ;
  ; check if target directory is there.  
  ; * if so nuke it
  ; * otherwise or afterwards create clean directory having target name
  ; * then enter directory
  if not keyword_set(abridged) then begin
      finfo      = file_info(outdir)
      IF finfo.exists THEN BEGIN 
         plog,ll,prog,'Deleting old directory'
         file_delete, outdir, /recursive
      ENDIF 
      plog,ll,prog,'creating directory, name = '+outdir
      file_mkdir, outdir
  endif
  ;
  ch         = strmid(bdir,0,1,/reverse_offset)
  IF ch NE '/' THEN bdir = bdir+'/'
  odir = bdir+outdir 
  plog,ll,prog,'going to source directory = '+sdir
  cd, sdir
  ;
  ; copy (or link) files to the output directory
  ; all this stuff only needs to happen once
  f2cp       = [inputstr.fjpg_low, inputstr.fjpg_high, inputstr.fjpg_mlow1, $
               (inputstr.fjpg_mhigh1), inputstr.fjpg_mlow2, inputstr.fjpg_mhigh2, $
               (inputstr.fjpg_mlow3), inputstr.fjpg_mhigh3, inputstr.fjpg_imlow1, $
               (inputstr.fjpg_imhigh1), inputstr.fjpg_imlow2, inputstr.fjpg_imhigh2, $
               (inputstr.fjpg_imlow3), inputstr.fjpg_imhigh3, inputstr.fcompare, inputstr.scalprof, $
               (inputstr.fcalprof), inputstr.scalprof0, inputstr.fcalprof0, inputstr.fbplotj, $
               inputstr.fbplote]
  hafuvjpg  = string(indgen(ngal), format='(%"' + inputstr.hafuvjpg + '")')
  hafuvjpg0 = string(indgen(ngal), format='(%"' + inputstr.hafuvjpg0 + '")')
  hafuvps   = string(indgen(ngal), format='(%"' + inputstr.hafuvps + '")')
  hafuvps0  = string(indgen(ngal), format='(%"' + inputstr.hafuvps0 + '")')
  profjpg   = string(indgen(ngal), format='(%"' + inputstr.profjpg + '")')
  profps    = string(indgen(ngal), format='(%"' + inputstr.profps + '")')
  f2cp = [f2cp, hafuvjpg, hafuvjpg0, hafuvps, hafuvps0, profjpg, profps]
  ; add mid-infrared profiles if we have them
  w1 = where(bandavail eq band.mir_W1, count_w1)
  w2 = where(bandavail eq band.mir_W2, count_w2)
  w3 = where(bandavail eq band.mir_W3, count_w3)
  w4 = where(bandavail eq band.mir_W4, count_w4)
  if count_w1 + count_w2 + count_w3 + count_w4 ge 1 then begin
      mir_profjpg = string(indgen(ngal), format='(%"' + inputstr.mir_profjpg + '")')
      mir_profps  = string(indgen(ngal), format='(%"' + inputstr.mir_profps + '")')
      f2cp = [f2cp, mir_profjpg, mir_profps]
  endif
  f2cp = bdir+f2cp
  if not keyword_set(abridged) then begin
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
  endif
  ;
  cd, odir
  ;
  ; open web page, write top of file
  filo       = keyword_set(abridged) ? 'index.html' : inputstr.hname+'_ssoup.html'
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
  if keyword_set(abridged) then begin
      printf, lu, 'This page presents only selected results and images, see <a href="' + inputstr.hname+'_ssoup.html">here</a> for everything.' 
  endif
  noresize = keyword_set(abridged)
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
      ssoup_imcell,ll,lu,inputstr.fjpg_high[i],fjpgo,width=widthi,noresize=noresize
      if (i+1) ne ncombo then ssoup_imcell,ll,lu,inputstr.fjpg_high[i+1],fjpgo,width=widthi,noresize=noresize
      ssoup_imcell,ll,lu,inputstr.fjpg_low[i],fjpgo,width=widthi,noresize=noresize
      if (i+1) ne ncombo then ssoup_imcell,ll,lu,inputstr.fjpg_low[i+1],fjpgo,width=widthi,noresize=noresize
      printf,lu,'</tr>'
  endfor
  printf,lu,'</table>'
  printf,lu,'<hr>'
  ;
  ; mark-up profile plots
  plog,ll,prog,'marking up three color image'
  printf,lu,'<h3>Surface brightness profile</h3><table border=1 cellpadding=3><tr><th>Bands</th>'
  for i=0,ngal-1 do printf,lu,"<th>Galaxy " + numstr(i) + "</th>
  printf,lu,"<tr><td>Optical/UV</td>
  for i=0,ngal-1 do begin
      pp        = strpos(profjpg[i],'.jpg')
      fjpgo     = strmid(profjpg[i],0,pp)+'_sm.jpg'
      cmd       = 'convert '+profjpg[i]+' -resize '+numstr(widthp)+' '+fjpgo
      plog,ll,prog,'making thumbnail using command: '+cmd
      spawn,cmd
      printf,lu,"<td>
      printf,lu,'<a href="'+profjpg[i]+'"><img src="'+fjpgo+'"></a><br><br>'
      printf,lu,'<a href="'+profps[i]+'">PS</a> &nbsp; Text: <a href="'+inputstr.scalprof+'">raw</a>, <a href="'+inputstr.scalprof0+'">dust corr.</a></td>'
  endfor
  printf,lu,"<tr><td>MIR (test)</td>
  for i=0,ngal-1 do begin
      pp        = strpos(mir_profjpg[i],'.jpg')
      fjpgo     = strmid(mir_profjpg[i],0,pp)+'_sm.jpg'
      cmd       = 'convert '+mir_profjpg[i]+' -resize '+numstr(widthp)+' '+fjpgo
      plog,ll,prog,'making thumbnail using command: '+cmd
      spawn,cmd
      printf,lu,"<td>
      printf,lu,'<a href="'+mir_profjpg[i]+'"><img src="'+fjpgo+'"></a><br><br>'
      printf,lu,'TEST IMAGE <a href="'+mir_profps[i]+'">PS</a> &nbsp; Text: <a href="'+inputstr.scalprof+'">raw</a>, <a href="'+inputstr.scalprof0+'">dust corr.</a></td>'
  endfor
  printf,lu,'</tr></table><hr>'
  ;
  ; mark up Halpha/FUV versus surface brightness plots
  plog,ll,prog,'marking up Halpha/FUV vs surface brightness plots'
  printf,lu,'<h3>Local H&alpha;/FUV versus surface brightness plots</h3>'
  printf,lu,'<P><TABLE border=1 cellpadding=3>'
  printf,lu,'<tr>'
  printf,lu,'  <th><b>Galaxy</b></th>
  printf,lu,'  <th><b>H&alpha;/FUV versus &Sigma; , raw</b></th>'
  printf,lu,'  <th><b>H&alpha;/FUV versus &Sigma; , dust corrected</b></th>'
  printf,lu,'</tr>'
  for i=0,ngal-1 do begin
      printf,lu,'<tr><td>' + numstr(i) + "</td>
      ssoup_imcell,ll,lu,hafuvjpg[i],fjpgo,width=widthh,uannot='<a href="'+hafuvps[i]+'">PS</a>',/plot,noresize=noresize
      ssoup_imcell,ll,lu,hafuvjpg0[i],fjpgo,width=widthh,uannot='<a href="'+hafuvps0[i]+'">PS</a>',/plot,noresize=noresize
      printf,lu,'</tr>'
  endfor
  printf,lu,'</table>'
  printf,lu,'<hr>'
  ;
  ; read-in database comparison table
  plog,ll,prog,'reading database comparison table: '+inputstr.fcompare
  fmtc     = '(a,a,f,f,f,a,f,f,f)'
  readcol, inputstr.fcompare, snam, banda, r50d, flxd, eflxd, dum, r50s, flxs, eflxs, format=fmtc
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
     printf,lu,'  <td>'+snam[ii]+'</td><td>'+banda[ii]+'</td>'+$
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
     ssoup_imcell,ll,lu,inputstr.fbplotj[ii],fjpgo,width=widths,uannot='<a href="'+inputstr.fbplote[ii]+'">EPS</a>',/plot,noresize=noresize
     printf,lu,'</tr>'
  ENDFOR
  printf,lu,'</TABLE>'
  ;
  ; mark-up masked images
  if not keyword_set(abridged) then begin
  printf,lu,'<h3>Three colour images with masks applied</h3>'
  plog,ll,prog,'creating 3 colour bad object masked images table, high cut.'
  printf,lu,'<P><TABLE border=1 cellpadding=3>'
  printf,lu,'<tr>'
  printf,lu,'  <th colspan=2><b>Three colour images, bad objects mask, high cut</b></th>'
  printf,lu,'  <th colspan=2><b>Three colour images, bad objects inverted mask, high cut</b></th>'
  printf,lu,'</tr>'
  for i=0,ncombo-1,2 do begin
      printf,lu,'<tr>'
      ssoup_imcell,ll,lu,inputstr.fjpg_mhigh1[i],fjpgo,width=widthi,noresize=noresize
      if (i+1) ne ncombo then ssoup_imcell,ll,lu,inputstr.fjpg_mhigh1[i+1],fjpgo,width=widthi,noresize=noresize
      ssoup_imcell,ll,lu,inputstr.fjpg_imhigh1[i],fjpgo,width=widthi,noresize=noresize
      if (i+1) ne ncombo then ssoup_imcell,ll,lu,inputstr.fjpg_imhigh1[i+1],fjpgo,width=widthi,noresize=noresize
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
      ssoup_imcell,ll,lu,inputstr.fjpg_mlow1[i],fjpgo,width=widthi,noresize=noresize
      if (i+1) ne ncombo then ssoup_imcell,ll,lu,inputstr.fjpg_mlow1[i+1],fjpgo,width=widthi,noresize=noresize
      ssoup_imcell,ll,lu,inputstr.fjpg_imlow1[i],fjpgo,width=widthi,noresize=noresize
      if (i+1) ne ncombo then ssoup_imcell,ll,lu,inputstr.fjpg_imlow1[i+1],fjpgo,width=widthi,noresize=noresize
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
      ssoup_imcell,ll,lu,inputstr.fjpg_mhigh2[i],fjpgo,width=widthi,noresize=noresize
      if (i+1) ne ncombo then ssoup_imcell,ll,lu,inputstr.fjpg_mhigh2[i+1],fjpgo,width=widthi,noresize=noresize
      ssoup_imcell,ll,lu,inputstr.fjpg_imhigh2[i],fjpgo,width=widthi,noresize=noresize
      if (i+1) ne ncombo then ssoup_imcell,ll,lu,inputstr.fjpg_imhigh2[i+1],fjpgo,width=widthi,noresize=noresize
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
      ssoup_imcell,ll,lu,inputstr.fjpg_mlow2[i],fjpgo,width=widthi,noresize=noresize
      if (i+1) ne ncombo then ssoup_imcell,ll,lu,inputstr.fjpg_mlow2[i+1],fjpgo,width=widthi,noresize=noresize
      ssoup_imcell,ll,lu,inputstr.fjpg_imlow2[i],fjpgo,width=widthi,noresize=noresize
      if (i+1) ne ncombo then ssoup_imcell,ll,lu,inputstr.fjpg_imlow2[i+1],fjpgo,width=widthi,noresize=noresize
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
      ssoup_imcell,ll,lu,inputstr.fjpg_mhigh3[i],fjpgo,width=widthi,noresize=noresize
      if (i+1) ne ncombo then ssoup_imcell,ll,lu,inputstr.fjpg_mhigh3[i+1],fjpgo,width=widthi,noresize=noresize
      ssoup_imcell,ll,lu,inputstr.fjpg_imhigh3[i],fjpgo,width=widthi,noresize=noresize
      if (i+1) ne ncombo then ssoup_imcell,ll,lu,inputstr.fjpg_imhigh3[i+1],fjpgo,width=widthi,noresize=noresize
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
      ssoup_imcell,ll,lu,inputstr.fjpg_mlow3[i],fjpgo,width=widthi,noresize=noresize
      if (i+1) ne ncombo then ssoup_imcell,ll,lu,inputstr.fjpg_mlow3[i+1],fjpgo,width=widthi,noresize=noresize
      ssoup_imcell,ll,lu,inputstr.fjpg_imlow3[i],fjpgo,width=widthi,noresize=noresize
      if (i+1) ne ncombo then ssoup_imcell,ll,lu,inputstr.fjpg_imlow3[i+1],fjpgo,width=widthi,noresize=noresize
      printf,lu,'</tr>'
  endfor
  printf,lu,'</table>'
  endif
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
