PRO make_ssoupin, status, ll=ll, wd=wd, hname=hname, file=file
  ;
  ; make an input file for ssoup
  ;
  ;   ll    -> logical unit for reporting progress.  If not set then 
  ;            ll = -1 (output to terminal)
  ;   wd    -> if set, the name of the directory containing 
  ;            the files to be processed by ssoup.  If not set the
  ;            default is the current directory.
  ;   hname -> name of HIPASS target to be processed.
  ;   file  -> name of the ssoup input file to be created.  If not set
  ;            a file ssoup.in will be created.
  ;  
  ; G. Meurer (ICRAR/UWA) 6/2010
  ; G. Meurer (ICRAR/UWA) 1/2012
  ;    + improved default for Rsub image name, to handle case of
  ;      J0008-59 (NB filter included in im name)
  ;    + improved default for R mask name also for case of 
  ;      J0008-59; now tries either hname+'_mask.fits' and
  ;      hname+'_R_mask.fits'
  ;
  ; some arrays we will need later
  bandavail = ['']
  fili  = ['']
  skyord = [0]
  COMMON bands, band, nband, bandnam, aaaa1, aaaa2, aaaa3, aaaa4 ; sigh
  ssoup_initvars
  ;
  ; set logical unit for log file
  IF NOT keyword_set(ll) THEN ll = -1
  prog      = 'MAKE_SSOUPIN: '
  plog,ll,prog,'------------------------ starting '+prog+'-------------------------'
  ;
  ; go to initial directory if needed
  IF keyword_set(wd) THEN BEGIN 
     plog,ll,prog,'going to working directory : '+wd
     cd,wd,current=cwd
  ENDIF 
  ;
  ; determine HIPASS target name
  IF NOT keyword_set(hname) THEN BEGIN 
     sstr  = '*_ss.fits'
     flist = file_search(sstr)
     jj    = strpos(flist[0],'_')
     IF jj GT 0 THEN BEGIN 
        hname = strmid(flist[0],0,jj)
     Endif ELSE BEGIN 
        plog,ll,prog,'could not guess HIPASS target name using file search string: "'+sstr+'", exiting.'
        status = 0b
        return
     ENDELSE 
     plog,ll,prog,'will use derived HIPASS target name: '+hname
  ENDIF ELSE BEGIN 
     plog,ll,prog,'will use passed HIPASS target name: '+hname
  ENDELSE 
  ;
  ; these search strings are in the same order as band
  ; fixme: this is rather crappy
   sstr = ptrarr(nband)
   sstr[0] = ptr_new(hname + '*_?sub_ss.fits')          ; Ha
   sstr[1] = ptr_new(hname + '_?_ss.fits')              ; R
   sstr[2] = ptr_new('*'+['-nd-int.fits', '_nuv.fits']) ; NUV
   sstr[3] = ptr_new('*'+['-fd-int.fits', '_fuv.fits']) ; FUV
   sstr[4] = ptr_new(hname + '-wise-w1.fits')           ; MIR-W1
   sstr[5] = ptr_new(hname + '-wise-w2.fits')           ; MIR-W2
   sstr[6] = ptr_new(hname + '-wise-w3.fits')           ; MIR-W3
   sstr[7] = ptr_new(hname + '-wise-w4.fits')           ; MIR-W4
   ; masks
   sstrm = ptrarr(nband)
   sstrm[0] = ptr_new(hname+'*_*sub_mask.fits')             ; Ha
   sstrm[1] = ptr_new(hname+['_mask.fits', '_R_mask.fits']) ; R
   sstrm[2] = ptr_new('*_uv_mask.fits')                     ; NUV, FUV
   sstrm[3] = sstrm[2]           
   sstrm[4] = ptr_new(hname+'-wise-dummymask.fits') ; WISE (placeholder)
   sstrm[5] = sstrm[4]
   sstrm[6] = sstrm[4] 
   sstrm[7] = sstrm[4]

   for i=0,nband-1 do begin
       ; find input image
       done = 0b
       j = 0
       nr = n_elements(*sstr[i])
       repeat begin
           fili_band = file_search((*sstr[i])[j], count=count)
           IF count EQ 0 THEN BEGIN 
               plog,ll,prog,'could not find '+band.(i)+' band image using search string: "'+(*sstr[i])[j]+'" ...'
               if band.(i) eq band.R or band.(i) eq band.HALPHA then begin
                   status = 0b
                   return
               endif
               j++
           ENDIF ELSE BEGIN
               fili  = [fili, fili_band[0]]
               bandavail = [bandavail, band.(i)]
               if band.(i) eq band.R or band.(i) eq band.HALPHA then begin
                   skyord = [skyord, 2]
               endif else begin
                   skyord = [skyord, 1]
               endelse
               done = 1b
           endelse
       endrep until done or j eq nr
   endfor
   ; trim arrays
   bandavail = bandavail[1:*]
   fili = fili[1:*]
   skyord = skyord[1:*]
   nbandavail = n_elements(bandavail)
   film = strarr(nbandavail)
   ; look for mask image
   for i=0,nband-1 do begin       
       j = 0
       done = 0b
       nr = n_elements(*sstrm[i])
       repeat begin
           film_band = file_search((*sstrm[i])[j], count=count)
           IF count EQ 0 THEN BEGIN 
               plog,ll,prog,'could not find '+band.(i)+' mask image using search string: "'+(*sstrm[i])[j]+'" ...'
               j++
           ENDIF ELSE BEGIN
               film[i] = film_band[0]
               done = 1b
           endelse
       endrep until done or j eq nr
   endfor
  ;
  ; derive other names
  filo = strarr(nbandavail)
  filp = strarr(nbandavail)
  fbox = strarr(nbandavail)
  fbplot_jpg = strarr(nbandavail)
  fbplot_eps = strarr(nbandavail)
  ; silly little hack
  ih = where(bandavail eq band.HALPHA, nih)
  if nih gt 0 then bandavail[ih[0]] = 'Halpha'
  for i=0,nbandavail-1 do begin
      filo[i] = hname+'_aligned_' + bandavail[i] + '.fits'
      filp[i] = hname+'_aligned_' + bandavail[i] + '.profile'
      fbox[i] = hname+'_aligned_box_' + bandavail[i] + '.dat'
      fbplot_jpg[i] = hname+'_aligned_skyplot_' + bandavail[i] + '.jpg'
      fbplot_eps[i] = hname+'_aligned_skyplot_' + bandavail[i] + '.eps'
  endfor
  if nih gt 0 then bandavail[ih[0]] = band.HALPHA ; unhack
  film_out        = hname+'_aligned_mask.fits'
  film_sout       = hname+'_aligned_skymask.fits'
  fcompare        = hname+'_compare.dat'
  scalprof        = hname+'_aligned_sprof.dat'
  fcalprof        = hname+'_aligned_fprof.dat'
  scalprof0       = hname+'_aligned_sprof0.dat'
  fcalprof0       = hname+'_aligned_fprof0.dat'
  profjpg         = hname+'_aligned_sprof.jpg'
  profps          = hname+'_aligned_sprof.ps'
  hafuvjpg        = hname+'_aligned_hafuv.jpg'
  hafuvps         = hname+'_aligned_hafuv.ps'
  hafuvjpg0       = hname+'_aligned_hafuv0.jpg'
  hafuvps0        = hname+'_aligned_hafuv0.ps'
  
  ; combos
  ncombo = factorial(nbandavail)/(6*factorial(nbandavail-3)) ; number of 3 color combos
  combo = transpose(combigen(nbandavail, 3))
  combostr = strlowcase(strjoin(strmid(bandavail[combo], 0, 2))) ; generates hrn, hrf, etc.
  fjpgl = strarr(ncombo)
  fjpgh = strarr(ncombo)
  fjpgl_msk1 = strarr(ncombo)
  fjpgh_msk1 = strarr(ncombo)
  fjpgl_msk2 = strarr(ncombo)
  fjpgh_msk2 = strarr(ncombo)
  fjpgl_msk3 = strarr(ncombo)
  fjpgh_msk3 = strarr(ncombo)
  fjpgl_imsk1 = strarr(ncombo)
  fjpgh_imsk1 = strarr(ncombo)
  fjpgl_imsk2 = strarr(ncombo)
  fjpgh_imsk2 = strarr(ncombo)
  fjpgl_imsk3 = strarr(ncombo)
  fjpgh_imsk3 = strarr(ncombo)
  for i=0,ncombo-1 do begin
      fjpgl[i]       = hname + '_aligned_'       + combostr[i] + '1.jpg'
      fjpgh[i]       = hname + '_aligned_'       + combostr[i] + '2.jpg'
      fjpgl_msk1[i]  = hname + '_aligned_msk1_'  + combostr[i] + '1.jpg'
      fjpgh_msk1[i]  = hname + '_aligned_msk1_'  + combostr[i] + '2.jpg'
      fjpgl_msk2[i]  = hname + '_aligned_msk2_'  + combostr[i] + '1.jpg'
      fjpgh_msk2[i]  = hname + '_aligned_msk2_'  + combostr[i] + '2.jpg'
      fjpgl_msk3[i]  = hname + '_aligned_msk3_'  + combostr[i] + '1.jpg'
      fjpgh_msk3[i]  = hname + '_aligned_msk3_'  + combostr[i] + '2.jpg'
      fjpgl_imsk1[i] = hname + '_aligned_imsk1_' + combostr[i] + '1.jpg'
      fjpgh_imsk1[i] = hname + '_aligned_imsk1_' + combostr[i] + '2.jpg'
      fjpgl_imsk2[i] = hname + '_aligned_imsk2_' + combostr[i] + '1.jpg'
      fjpgh_imsk2[i] = hname + '_aligned_imsk2_' + combostr[i] + '2.jpg'
      fjpgl_imsk3[i] = hname + '_aligned_imsk3_' + combostr[i] + '1.jpg'
      fjpgh_imsk3[i] = hname + '_aligned_imsk3_' + combostr[i] + '2.jpg'
  endfor
  ;
  ; open output file
  IF NOT keyword_set(file) THEN file = 'ssoup.in'
  plog,ll,prog,'creating input file for SSOUP : '+file
  openw,lu,file,/get_lun
  ; 
  ; write output file, copy to log file
  printf,lu, 'HNAME           = '+hname
  plog,ll,'','HNAME           = '+hname
  for i=0,nbandavail-1 do begin
      printf,lu, 'FILI_'       + bandavail[i] + ' = ' + fili[i]
      plog,ll,'','FILI_'       + bandavail[i] + ' = ' + fili[i]
      printf,lu, 'FILM_'       + bandavail[i] + ' = ' + film[i]
      plog,ll,'','FILM_'       + bandavail[i] + ' = ' + film[i]
      printf,lu, 'FILO_'       + bandavail[i] + ' = ' + filo[i]
      plog,ll,'','FILO_'       + bandavail[i] + ' = ' + filo[i]
      printf,lu, 'SKYORD_'     + bandavail[i] + ' = ' + strtrim(string(skyord[i]),2)
      plog,ll,'','SKYORD_'     + bandavail[i] + ' = ' + strtrim(string(skyord[i]),2)
      printf,lu, 'FILP_'       + bandavail[i] + ' = ' +filp[i]
      plog,ll,'','FILP_'       + bandavail[i] + ' = ' +filp[i]
      printf,lu, 'FBOX_'       + bandavail[i] + ' = ' +fbox[i]
      plog,ll,'','FBOX_'       + bandavail[i] + ' = ' +fbox[i]
      printf,lu, 'FBPLOT_JPG_' + bandavail[i] + ' = ' +fbplot_jpg[i]
      plog,ll,'','FBPLOT_JPG_' + bandavail[i] + ' = ' +fbplot_jpg[i]
      printf,lu, 'FBPLOT_EPS_' + bandavail[i] + ' = ' +fbplot_eps[i]
      plog,ll,'','FBPLOT_EPS_' + bandavail[i] + ' = ' +fbplot_eps[i]
  endfor
  printf,lu, 'FILM_OUT        = '+film_out
  plog,ll,'','FILM_OUT        = '+film_out
  printf,lu, 'FILM_SOUT       = '+film_sout
  plog,ll,'','FILM_SOUT       = '+film_sout
  combostr = strupcase(combostr)
  for i=0,ncombo-1 do begin
      printf,lu, 'FJPGL_'       + combostr[i] + ' = ' + fjpgl[i]
      plog,ll,'','FJPGL_'       + combostr[i] + ' = ' + fjpgl[i]
      printf,lu, 'FJPGH_'       + combostr[i] + ' = ' + fjpgh[i]
      plog,ll,'','FJPGH_'       + combostr[i] + ' = ' + fjpgh[i]
      printf,lu, 'FJPGL_MSK1_'  + combostr[i] + ' = ' + fjpgl_msk1[i]
      plog,ll,'','FJPGL_MSK1_'  + combostr[i] + ' = ' + fjpgl_msk1[i]
      printf,lu, 'FJPGH_MSK1_'  + combostr[i] + ' = ' + fjpgh_msk1[i]
      plog,ll,'','FJPGH_MSK1_'  + combostr[i] + ' = ' + fjpgh_msk1[i]
      printf,lu, 'FJPGL_MSK2_'  + combostr[i] + ' = ' + fjpgl_msk2[i]
      plog,ll,'','FJPGL_MSK2_'  + combostr[i] + ' = ' + fjpgl_msk2[i]
      printf,lu, 'FJPGH_MSK2_'  + combostr[i] + ' = ' + fjpgh_msk2[i]
      plog,ll,'','FJPGH_MSK2_'  + combostr[i] + ' = ' + fjpgh_msk2[i]      
      printf,lu, 'FJPGL_MSK3_'  + combostr[i] + ' = ' + fjpgl_msk3[i]
      plog,ll,'','FJPGL_MSK3_'  + combostr[i] + ' = ' + fjpgl_msk3[i]
      printf,lu, 'FJPGH_MSK3_'  + combostr[i] + ' = ' + fjpgh_msk3[i]
      plog,ll,'','FJPGH_MSK3_'  + combostr[i] + ' = ' + fjpgh_msk3[i]
      printf,lu, 'FJPGL_IMSK1_' + combostr[i] + ' = ' + fjpgl_imsk1[i]
      plog,ll,'','FJPGL_IMSK1_' + combostr[i] + ' = ' + fjpgl_imsk1[i]
      printf,lu, 'FJPGH_IMSK1_' + combostr[i] + ' = ' + fjpgh_imsk1[i]
      plog,ll,'','FJPGH_IMSK1_' + combostr[i] + ' = ' + fjpgh_imsk1[i]
      printf,lu, 'FJPGL_IMSK2_' + combostr[i] + ' = ' + fjpgl_imsk2[i]
      plog,ll,'','FJPGL_IMSK2_' + combostr[i] + ' = ' + fjpgl_imsk2[i]
      printf,lu, 'FJPGH_IMSK2_' + combostr[i] + ' = ' + fjpgh_imsk2[i]
      plog,ll,'','FJPGH_IMSK2_' + combostr[i] + ' = ' + fjpgh_imsk2[i]      
      printf,lu, 'FJPGL_IMSK3_' + combostr[i] + ' = ' + fjpgl_imsk3[i]
      plog,ll,'','FJPGL_IMSK3_' + combostr[i] + ' = ' + fjpgl_imsk3[i]
      printf,lu, 'FJPGH_IMSK3_' + combostr[i] + ' = ' + fjpgh_imsk3[i]
      plog,ll,'','FJPGH_IMSK3_' + combostr[i] + ' = ' + fjpgh_imsk3[i]
  endfor
  printf,lu, 'FCOMPARE        = '+fcompare
  plog,ll,'','FCOMPARE        = '+fcompare
  printf,lu, 'SCALPROF        = '+scalprof
  plog,ll,'','SCALPROF        = '+scalprof
  printf,lu, 'FCALPROF        = '+fcalprof
  plog,ll,'','FCALPROF        = '+fcalprof
  printf,lu, 'SCALPROF0       = '+scalprof0
  plog,ll,'','SCALPROF0       = '+scalprof0
  printf,lu, 'FCALPROF0       = '+fcalprof0
  plog,ll,'','FCALPROF0       = '+fcalprof0
  printf,lu, 'PROFJPG         = '+profjpg
  plog,ll,'','PROFJPG         = '+profjpg
  printf,lu, 'PROFPS          = '+profps
  plog,ll,'','PROFPS          = '+profps
  printf,lu, 'HAFUVJPG        = '+hafuvjpg
  plog,ll,'','HAFUVJPG        = '+hafuvjpg
  printf,lu, 'HAFUVPS         = '+hafuvps
  plog,ll,'','HAFUVPS         = '+hafuvps
  printf,lu, 'HAFUVJPG0       = '+hafuvjpg0
  plog,ll,'','HAFUVJPG0       = '+hafuvjpg0
  printf,lu, 'HAFUVPS0        = '+hafuvps0
  plog,ll,'','HAFUVPS0        = '+hafuvps0
  ;
  ; close output file
  free_lun,lu
  plog,ll,prog,'closed file: '+file
  ;
  ; return to original directory if needed.
  IF keyword_set(wd) THEN BEGIN 
     plog,ll,prog,'returning to starting directory : '+cwd
     cd,cwd
  ENDIF 
  ;
  status = 1b
  plog,ll,prog,'returning with status = '+numstr(fix(status))
  ptr_free,sstr,sstrm
END 
