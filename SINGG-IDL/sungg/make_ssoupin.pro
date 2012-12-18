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
  rulen      = '*'+['-nd-int.fits', '_nuv.fits']
  rulef      = '*'+['-fd-int.fits', '_fuv.fits']
  ; some arrays we will need later
  bands = ['']
  fili  = ['']
  film  = ['']
  skyord = [0]
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
  ; find Halpha image
  sstr     = '*_?sub_ss.fits'
  fili_ha  = file_search(hname+sstr,count=count)
  IF count EQ 0 THEN BEGIN 
     plog,ll,prog,'could not find Halpha image using search string: "'+sstr+'", exiting'
     status = 0b
     return
  ENDIF ELSE BEGIN 
     fili  = [fili,  fili_ha[0]]
     bands = [bands, 'HALPHA']
     skyord = [skyord, 2]
  ENDELSE 
  ;
  ; find R image
  sstr     = hname+'_?_ss.fits'
  fili_r   = file_search(sstr,count=count)
  IF count EQ 0 THEN BEGIN 
     plog,ll,prog,'could not find R band image using search string: "'+sstr+'", exiting'
     status = 0b
     return
  ENDIF ELSE BEGIN 
     fili  = [fili,  fili_r[0]]
     bands = [bands, 'R']
     skyord = [skyord, 2]
  ENDELSE 
  ;
  ; find NUV image
  nr       = n_elements(rulen)
  ii       = 0
  repeat begin 
     sstr     = rulen[ii]
     fili_nuv = file_search(sstr,count=count)
     IF count EQ 0 THEN BEGIN 
        plog,ll,prog,'could not find NUV band image using search string: "'+sstr+'" ...'
     ENDIF ELSE BEGIN 
        fili  = [fili,  fili_nuv[0]]
        bands = [bands, 'NUV']
        skyord = [skyord, 1]
     ENDELSE
     ii    = ii + 1
  endrep until ((ii eq nr) or (count gt 0))
  if count eq 0 then begin
     plog,ll,prog,'could not find NUV band image using any rule, exiting'
     status = 0b
  endif
  ;
  ; find FUV image
  nr       = n_elements(rulef)
  ii       = 0
  repeat begin 
     sstr     = rulef[ii]
     fili_fuv = file_search(sstr,count=count)
     IF count EQ 0 THEN BEGIN 
        plog,ll,prog,'could not find FUV band image using search string: "'+sstr+'" ...'
     ENDIF ELSE BEGIN 
        fili  = [fili,  fili_fuv[0]]
        bands = [bands, 'FUV']
        skyord = [skyord, 1]
     ENDELSE
     ii    = ii + 1
  endrep until ((ii eq nr) or (count gt 0))
  if count eq 0 then begin
     plog,ll,prog,'could not find FUV band image using any rule, exiting'
     status = 0b
  endif 
  ;
  ; find Halpha mask image
  sstr    = hname+'*_*sub_mask.fits'
  film_ha = file_search(sstr,count=count)
  IF count EQ 0 THEN BEGIN 
     plog,ll,prog,'**** warning Halpha mask could not be found using search string: "'+sstr+'"'
     film_ha  = hname+'_Rsub_mask.fits'
     plog,ll,prog,'continuing using default name: '+film_ha
  ENDIF ELSE BEGIN 
     film  = [film, film_ha[0]]
  ENDELSE
  ;
  ; find R mask image
  film_r   = hname+'_mask.fits'
  inf      = file_info(film_r)
  IF NOT inf.exists THEN BEGIN 
     try1  = film_r
     try2  = hname+'_R_mask.fits'
     inf   = file_info(try2)
     IF inf.exists THEN BEGIN 
        film = [film, try2]
     ENDIF ELSE BEGIN 
        plog,ll,prog,'**** warning could not find either guesses for R mask: '+try1+' , '+try2
        plog,ll,prog,'continuing, anyway (but you will want to fix this)...'
        film = [film, try1]
     ENDELSE 
  ENDIF else begin
      film = [film, film_r]
  endelse
  ;
  ; find UV mask image
  sstr     = '*_uv_mask.fits'
  filmuv   = file_search(sstr,count=count)
  IF count eq 0 THEN BEGIN 
     ;
     ; that didn't work try another guess
     plog,ll,prog,'could not find file containing: '+sstr+'  will try another guess.'
     sstr  = '*mask.fuv.fits'
     filmuv = file_search(sstr,count=count)
     if count gt 0 then begin 
        film = [film, filmuv[0], filmuv[0]]
     endif else begin 
        plog,ll,prog,'**** warning could not find a UV mask file using search string : '+sstr+'  continuing, anyway ...'
     endelse 
  ENDIF else begin 
     film = [film, filmuv[0], filmuv[0]]
  endelse 
  ; trim arrays
  bands = bands[1:*]
  fili = fili[1:*]
  film = film[1:*]
  skyord = skyord[1:*]
  nbands = n_elements(bands)
  ;
  ; derive other names
  filo = strarr(nbands)
  filp = strarr(nbands)
  fbox = strarr(nbands)
  fbplot_jpg = strarr(nbands)
  fbplot_eps = strarr(nbands)
  ; silly little hack
  ih = where(bands eq 'HALPHA', nih)
  if nih gt 0 then bands[ih[0]] = 'Halpha'
  for i=0,nbands-1 do begin
      filo[i] = hname+'_aligned_' + bands[i] + '.fits'
      filp[i] = hname+'_aligned_' + bands[i] + '.profile'
      fbox[i] = hname+'_aligned_box_' + bands[i] + '.dat'
      fbplot_jpg[i] = hname+'_aligned_skyplot_' + bands[i] + '.jpg'
      fbplot_eps[i] = hname+'_aligned_skyplot_' + bands[i] + '.eps'
  endfor
  if nih gt 0 then bands[ih[0]] = 'HAPLHA' ; unhack
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
  fjpgl_hnf       = hname+'_aligned_hnf1.jpg'
  fjpgl_hrf       = hname+'_aligned_hrf1.jpg'
  fjpgl_hrn       = hname+'_aligned_hrn1.jpg'
  fjpgl_rnf       = hname+'_aligned_rnf1.jpg'
  fjpgh_hnf       = hname+'_aligned_hnf2.jpg'
  fjpgh_hrf       = hname+'_aligned_hrf2.jpg'
  fjpgh_hrn       = hname+'_aligned_hrn2.jpg'
  fjpgh_rnf       = hname+'_aligned_rnf2.jpg'
  fjpgl_msk1_hnf  = hname+'_aligned_msk1_hnf1.jpg'
  fjpgl_msk1_hrf  = hname+'_aligned_msk1_hrf1.jpg'
  fjpgl_msk1_hrn  = hname+'_aligned_msk1_hrn1.jpg'
  fjpgl_msk1_rnf  = hname+'_aligned_msk1_rnf1.jpg'
  fjpgh_msk1_hnf  = hname+'_aligned_msk1_hnf2.jpg'
  fjpgh_msk1_hrf  = hname+'_aligned_msk1_hrf2.jpg'
  fjpgh_msk1_hrn  = hname+'_aligned_msk1_hrn2.jpg'
  fjpgh_msk1_rnf  = hname+'_aligned_msk1_rnf2.jpg'
  fjpgl_msk2_hnf  = hname+'_aligned_msk2_hnf1.jpg'
  fjpgl_msk2_hrf  = hname+'_aligned_msk2_hrf1.jpg'
  fjpgl_msk2_hrn  = hname+'_aligned_msk2_hrn1.jpg'
  fjpgl_msk2_rnf  = hname+'_aligned_msk2_rnf1.jpg'
  fjpgh_msk2_hnf  = hname+'_aligned_msk2_hnf2.jpg'
  fjpgh_msk2_hrf  = hname+'_aligned_msk2_hrf2.jpg'
  fjpgh_msk2_hrn  = hname+'_aligned_msk2_hrn2.jpg'
  fjpgh_msk2_rnf  = hname+'_aligned_msk2_rnf2.jpg'
  fjpgl_msk3_hnf  = hname+'_aligned_msk3_hnf1.jpg'
  fjpgl_msk3_hrf  = hname+'_aligned_msk3_hrf1.jpg'
  fjpgl_msk3_hrn  = hname+'_aligned_msk3_hrn1.jpg'
  fjpgl_msk3_rnf  = hname+'_aligned_msk3_rnf1.jpg'
  fjpgh_msk3_hnf  = hname+'_aligned_msk3_hnf2.jpg'
  fjpgh_msk3_hrf  = hname+'_aligned_msk3_hrf2.jpg'
  fjpgh_msk3_hrn  = hname+'_aligned_msk3_hrn2.jpg'
  fjpgh_msk3_rnf  = hname+'_aligned_msk3_rnf2.jpg'
  fjpgl_imsk1_hnf = hname+'_aligned_imsk1_hnf1.jpg'
  fjpgl_imsk1_hrf = hname+'_aligned_imsk1_hrf1.jpg'
  fjpgl_imsk1_hrn = hname+'_aligned_imsk1_hrn1.jpg'
  fjpgl_imsk1_rnf = hname+'_aligned_imsk1_rnf1.jpg'
  fjpgh_imsk1_hnf = hname+'_aligned_imsk1_hnf2.jpg'
  fjpgh_imsk1_hrf = hname+'_aligned_imsk1_hrf2.jpg'
  fjpgh_imsk1_hrn = hname+'_aligned_imsk1_hrn2.jpg'
  fjpgh_imsk1_rnf = hname+'_aligned_imsk1_rnf2.jpg'
  fjpgl_imsk2_hnf = hname+'_aligned_imsk2_hnf1.jpg'
  fjpgl_imsk2_hrf = hname+'_aligned_imsk2_hrf1.jpg'
  fjpgl_imsk2_hrn = hname+'_aligned_imsk2_hrn1.jpg'
  fjpgl_imsk2_rnf = hname+'_aligned_imsk2_rnf1.jpg'
  fjpgh_imsk2_hnf = hname+'_aligned_imsk2_hnf2.jpg'
  fjpgh_imsk2_hrf = hname+'_aligned_imsk2_hrf2.jpg'
  fjpgh_imsk2_hrn = hname+'_aligned_imsk2_hrn2.jpg'
  fjpgh_imsk2_rnf = hname+'_aligned_imsk2_rnf2.jpg'
  fjpgl_imsk3_hnf = hname+'_aligned_imsk3_hnf1.jpg'
  fjpgl_imsk3_hrf = hname+'_aligned_imsk3_hrf1.jpg'
  fjpgl_imsk3_hrn = hname+'_aligned_imsk3_hrn1.jpg'
  fjpgl_imsk3_rnf = hname+'_aligned_imsk3_rnf1.jpg'
  fjpgh_imsk3_hnf = hname+'_aligned_imsk3_hnf2.jpg'
  fjpgh_imsk3_hrf = hname+'_aligned_imsk3_hrf2.jpg'
  fjpgh_imsk3_hrn = hname+'_aligned_imsk3_hrn2.jpg'
  fjpgh_imsk3_rnf = hname+'_aligned_imsk3_rnf2.jpg'
  ;
  ; open output file
  IF NOT keyword_set(file) THEN file = 'ssoup.in'
  plog,ll,prog,'creating input file for SSOUP : '+file
  openw,lu,file,/get_lun
  ; 
  ; write output file, copy to log file
  printf,lu, 'HNAME           = '+hname
  plog,ll,'','HNAME           = '+hname
  for i=0,nbands-1 do begin
      printf,lu, 'FILI_'       + bands[i] + ' = ' + fili[i]
      plog,ll,'','FILI_'       + bands[i] + ' = ' + fili[i]
      printf,lu, 'FILM_'       + bands[i] + ' = ' + film[i]
      plog,ll,'','FILM_'       + bands[i] + ' = ' + film[i]
      printf,lu, 'FILO_'       + bands[i] + ' = ' + filo[i]
      plog,ll,'','FILO_'       + bands[i] + ' = ' + filo[i]
      printf,lu, 'SKYORD_'     + bands[i] + ' = ' + strtrim(string(skyord[i]),2)
      plog,ll,'','SKYORD_'     + bands[i] + ' = ' + strtrim(string(skyord[i]),2)
      printf,lu, 'FILP_'       + bands[i] + ' = ' +filp[i]
      plog,ll,'','FILP_'       + bands[i] + ' = ' +filp[i]
      printf,lu, 'FBOX_'       + bands[i] + ' = ' +fbox[i]
      plog,ll,'','FBOX_'       + bands[i] + ' = ' +fbox[i]
      printf,lu, 'FBPLOT_JPG_' + bands[i] + ' = ' +fbplot_jpg[i]
      plog,ll,'','FBPLOT_JPG_' + bands[i] + ' = ' +fbplot_jpg[i]
      printf,lu, 'FBPLOT_EPS_' + bands[i] + ' = ' +fbplot_eps[i]
      plog,ll,'','FBPLOT_EPS_' + bands[i] + ' = ' +fbplot_eps[i]
  endfor
  printf,lu, 'FILM_OUT        = '+film_out
  plog,ll,'','FILM_OUT        = '+film_out
  printf,lu, 'FILM_SOUT       = '+film_sout
  plog,ll,'','FILM_SOUT       = '+film_sout
  printf,lu, 'FJPGL_HNF       = '+fjpgl_hnf
  plog,ll,'','FJPGL_HNF       = '+fjpgl_hnf
  printf,lu, 'FJPGL_HRF       = '+fjpgl_hrf
  plog,ll,'','FJPGL_HRF       = '+fjpgl_hrf
  printf,lu, 'FJPGL_HRN       = '+fjpgl_hrn
  plog,ll,'','FJPGL_HRN       = '+fjpgl_hrn
  printf,lu, 'FJPGL_RNF       = '+fjpgl_rnf
  plog,ll,'','FJPGL_RNF       = '+fjpgl_rnf
  printf,lu, 'FJPGH_HNF       = '+fjpgh_hnf
  plog,ll,'','FJPGH_HNF       = '+fjpgh_hnf
  printf,lu, 'FJPGH_HRF       = '+fjpgh_hrf
  plog,ll,'','FJPGH_HRF       = '+fjpgh_hrf
  printf,lu, 'FJPGH_HRN       = '+fjpgh_hrn
  plog,ll,'','FJPGH_HRN       = '+fjpgh_hrn
  printf,lu, 'FJPGH_RNF       = '+fjpgh_rnf
  plog,ll,'','FJPGH_RNF       = '+fjpgh_rnf
  printf,lu, 'FJPGL_MSK1_HNF  = '+fjpgl_msk1_hnf
  plog,ll,'','FJPGL_MSK1_HNF  = '+fjpgl_msk1_hnf
  printf,lu, 'FJPGL_MSK1_HRF  = '+fjpgl_msk1_hrf
  plog,ll,'','FJPGL_MSK1_HRF  = '+fjpgl_msk1_hrf
  printf,lu, 'FJPGL_MSK1_HRN  = '+fjpgl_msk1_hrn
  plog,ll,'','FJPGL_MSK1_HRN  = '+fjpgl_msk1_hrn
  printf,lu, 'FJPGL_MSK1_RNF  = '+fjpgl_msk1_rnf
  plog,ll,'','FJPGL_MSK1_RNF  = '+fjpgl_msk1_rnf
  printf,lu, 'FJPGH_MSK1_HNF  = '+fjpgh_msk1_hnf
  plog,ll,'','FJPGH_MSK1_HNF  = '+fjpgh_msk1_hnf
  printf,lu, 'FJPGH_MSK1_HRF  = '+fjpgh_msk1_hrf
  plog,ll,'','FJPGH_MSK1_HRF  = '+fjpgh_msk1_hrf
  printf,lu, 'FJPGH_MSK1_HRN  = '+fjpgh_msk1_hrn
  plog,ll,'','FJPGH_MSK1_HRN  = '+fjpgh_msk1_hrn
  printf,lu, 'FJPGH_MSK1_RNF  = '+fjpgh_msk1_rnf
  plog,ll,'','FJPGH_MSK1_RNF  = '+fjpgh_msk1_rnf
  printf,lu, 'FJPGL_MSK2_HNF  = '+fjpgl_msk2_hnf
  plog,ll,'','FJPGL_MSK2_HNF  = '+fjpgl_msk2_hnf
  printf,lu, 'FJPGL_MSK2_HRF  = '+fjpgl_msk2_hrf
  plog,ll,'','FJPGL_MSK2_HRF  = '+fjpgl_msk2_hrf
  printf,lu, 'FJPGL_MSK2_HRN  = '+fjpgl_msk2_hrn
  plog,ll,'','FJPGL_MSK2_HRN  = '+fjpgl_msk2_hrn
  printf,lu, 'FJPGL_MSK2_RNF  = '+fjpgl_msk2_rnf
  plog,ll,'','FJPGL_MSK2_RNF  = '+fjpgl_msk2_rnf
  printf,lu, 'FJPGH_MSK2_HNF  = '+fjpgh_msk2_hnf
  plog,ll,'','FJPGH_MSK2_HNF  = '+fjpgh_msk2_hnf
  printf,lu, 'FJPGH_MSK2_HRF  = '+fjpgh_msk2_hrf
  plog,ll,'','FJPGH_MSK2_HRF  = '+fjpgh_msk2_hrf
  printf,lu, 'FJPGH_MSK2_HRN  = '+fjpgh_msk2_hrn
  plog,ll,'','FJPGH_MSK2_HRN  = '+fjpgh_msk2_hrn
  printf,lu, 'FJPGH_MSK2_RNF  = '+fjpgh_msk2_rnf
  plog,ll,'','FJPGH_MSK2_RNF  = '+fjpgh_msk2_rnf
  printf,lu, 'FJPGL_MSK3_HNF  = '+fjpgl_msk3_hnf
  plog,ll,'','FJPGL_MSK3_HNF  = '+fjpgl_msk3_hnf
  printf,lu, 'FJPGL_MSK3_HRF  = '+fjpgl_msk3_hrf
  plog,ll,'','FJPGL_MSK3_HRF  = '+fjpgl_msk3_hrf
  printf,lu, 'FJPGL_MSK3_HRN  = '+fjpgl_msk3_hrn
  plog,ll,'','FJPGL_MSK3_HRN  = '+fjpgl_msk3_hrn
  printf,lu, 'FJPGL_MSK3_RNF  = '+fjpgl_msk3_rnf
  plog,ll,'','FJPGL_MSK3_RNF  = '+fjpgl_msk3_rnf
  printf,lu, 'FJPGH_MSK3_HNF  = '+fjpgh_msk3_hnf
  plog,ll,'','FJPGH_MSK3_HNF  = '+fjpgh_msk3_hnf
  printf,lu, 'FJPGH_MSK3_HRF  = '+fjpgh_msk3_hrf
  plog,ll,'','FJPGH_MSK3_HRF  = '+fjpgh_msk3_hrf
  printf,lu, 'FJPGH_MSK3_HRN  = '+fjpgh_msk3_hrn
  plog,ll,'','FJPGH_MSK3_HRN  = '+fjpgh_msk3_hrn
  printf,lu, 'FJPGH_MSK3_RNF  = '+fjpgh_msk3_rnf
  plog,ll,'','FJPGH_MSK3_RNF  = '+fjpgh_msk3_rnf
  printf,lu, 'FJPGL_IMSK1_HNF = '+fjpgl_imsk1_hnf
  plog,ll,'','FJPGL_IMSK1_HNF = '+fjpgl_imsk1_hnf
  printf,lu, 'FJPGL_IMSK1_HRF = '+fjpgl_imsk1_hrf
  plog,ll,'','FJPGL_IMSK1_HRF = '+fjpgl_imsk1_hrf
  printf,lu, 'FJPGL_IMSK1_HRN = '+fjpgl_imsk1_hrn
  plog,ll,'','FJPGL_IMSK1_HRN = '+fjpgl_imsk1_hrn
  printf,lu, 'FJPGL_IMSK1_RNF = '+fjpgl_imsk1_rnf
  plog,ll,'','FJPGL_IMSK1_RNF = '+fjpgl_imsk1_rnf
  printf,lu, 'FJPGH_IMSK1_HNF = '+fjpgh_imsk1_hnf
  plog,ll,'','FJPGH_IMSK1_HNF = '+fjpgh_imsk1_hnf
  printf,lu, 'FJPGH_IMSK1_HRF = '+fjpgh_imsk1_hrf
  plog,ll,'','FJPGH_IMSK1_HRF = '+fjpgh_imsk1_hrf
  printf,lu, 'FJPGH_IMSK1_HRN = '+fjpgh_imsk1_hrn
  plog,ll,'','FJPGH_IMSK1_HRN = '+fjpgh_imsk1_hrn
  printf,lu, 'FJPGH_IMSK1_RNF = '+fjpgh_imsk1_rnf
  plog,ll,'','FJPGH_IMSK1_RNF = '+fjpgh_imsk1_rnf
  printf,lu, 'FJPGL_IMSK2_HNF = '+fjpgl_imsk2_hnf
  plog,ll,'','FJPGL_IMSK2_HNF = '+fjpgl_imsk2_hnf
  printf,lu, 'FJPGL_IMSK2_HRF = '+fjpgl_imsk2_hrf
  plog,ll,'','FJPGL_IMSK2_HRF = '+fjpgl_imsk2_hrf
  printf,lu, 'FJPGL_IMSK2_HRN = '+fjpgl_imsk2_hrn
  plog,ll,'','FJPGL_IMSK2_HRN = '+fjpgl_imsk2_hrn
  printf,lu, 'FJPGL_IMSK2_RNF = '+fjpgl_imsk2_rnf
  plog,ll,'','FJPGL_IMSK2_RNF = '+fjpgl_imsk2_rnf
  printf,lu, 'FJPGH_IMSK2_HNF = '+fjpgh_imsk2_hnf
  plog,ll,'','FJPGH_IMSK2_HNF = '+fjpgh_imsk2_hnf
  printf,lu, 'FJPGH_IMSK2_HRF = '+fjpgh_imsk2_hrf
  plog,ll,'','FJPGH_IMSK2_HRF = '+fjpgh_imsk2_hrf
  printf,lu, 'FJPGH_IMSK2_HRN = '+fjpgh_imsk2_hrn
  plog,ll,'','FJPGH_IMSK2_HRN = '+fjpgh_imsk2_hrn
  printf,lu, 'FJPGH_IMSK2_RNF = '+fjpgh_imsk2_rnf
  plog,ll,'','FJPGH_IMSK2_RNF = '+fjpgh_imsk2_rnf
  printf,lu, 'FJPGL_IMSK3_HNF = '+fjpgl_imsk3_hnf
  plog,ll,'','FJPGL_IMSK3_HNF = '+fjpgl_imsk3_hnf
  printf,lu, 'FJPGL_IMSK3_HRF = '+fjpgl_imsk3_hrf
  plog,ll,'','FJPGL_IMSK3_HRF = '+fjpgl_imsk3_hrf
  printf,lu, 'FJPGL_IMSK3_HRN = '+fjpgl_imsk3_hrn
  plog,ll,'','FJPGL_IMSK3_HRN = '+fjpgl_imsk3_hrn
  printf,lu, 'FJPGL_IMSK3_RNF = '+fjpgl_imsk3_rnf
  plog,ll,'','FJPGL_IMSK3_RNF = '+fjpgl_imsk3_rnf
  printf,lu, 'FJPGH_IMSK3_HNF = '+fjpgh_imsk3_hnf
  plog,ll,'','FJPGH_IMSK3_HNF = '+fjpgh_imsk3_hnf
  printf,lu, 'FJPGH_IMSK3_HRF = '+fjpgh_imsk3_hrf
  plog,ll,'','FJPGH_IMSK3_HRF = '+fjpgh_imsk3_hrf
  printf,lu, 'FJPGH_IMSK3_HRN = '+fjpgh_imsk3_hrn
  plog,ll,'','FJPGH_IMSK3_HRN = '+fjpgh_imsk3_hrn
  printf,lu, 'FJPGH_IMSK3_RNF = '+fjpgh_imsk3_rnf
  plog,ll,'','FJPGH_IMSK3_RNF = '+fjpgh_imsk3_rnf
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
END 
