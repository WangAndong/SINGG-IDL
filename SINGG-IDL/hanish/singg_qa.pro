PRO singg_qa,RUNLIST=runlist,SKIP=skip,OUTDIR=outdir,BXW=bxw,PLOTSIG=plotsig
; Make the QA images for all SINGG galaxies at once, and update web
; pages to reflect the new information.
; OPTIONAL INPUTS:
; runlist       As always, a list of runs to be used.
; outdir        Output directory (default is the current one)
; /skip         Don't remake the images if they already exist.
; bxw           Overrides scale of images
;
; 2005++ written by Dan Hanish
; 09/2007 G. Meurer changed calls of singg_pagebot to hanish_pagebot (for now).

  spawn,"pwd",cdir

; Inputs will be in ./(runlist)/Proc3 and Proc4/Jwhatever

; outdir = ~/outputs/QA or ./QA
  IF KEYWORD_SET(outdir) THEN BEGIN
    odir = STRTRIM(outdir,2)
; If it doesn't end in a slash, add one.
    IF STRMID(odir,0,1,/reverse_offset) NE '/' THEN odir = odir+'/'
; If it's a relative path (either './' or nothing), add the pwd.
    IF STRMID(odir,0,2) EQ './' THEN odir = STRTRIM(cdir[0],2)+STRMID(odir,1,STRLEN(odir)-1)
    IF STRMID(odir,0,1) NE '/' THEN odir = STRTRIM(cdir[0],2)+'/'+odir
  ENDIF ELSE BEGIN
; Default is to use the output directory declared in the .idlstartup file
    odir = !outdir+'QA/'
;    odir = STRTRIM(cdir[0],2)+'/'
  ENDELSE

  IF NOT KEYWORD_SET(bxw) THEN bxw = 2
  IF bxw EQ 2 THEN suff = '.jpg' ELSE suff = '_'+STRTRIM(STRING(bxw),2)+'.jpg'
  IF NOT KEYWORD_SET(plotsig) THEN plotsig = [-4.0,20.0]

  OPENW,indunit,odir+'index.html',/GET_LUN,/MORE
  singg_pagetop,indunit,"QA 2/2/07","Attempt #7"
  
  IF NOT KEYWORD_SET(runlist) THEN $
    runlist = ['Run01','Run02','Run03','Run04','Run04s','Run05','Run06','Run07','Run08','Run09','Run10','Run11','Run12','Run13','Run15','Run17']

  FOR ii = 0,N_ELEMENTS(runlist)-1 DO BEGIN
    PRINT,"Performing QA for ",runlist[ii]

    idir = STRTRIM(cdir[0],2)+'/'+runlist[ii]+'/'

    catfile = idir+'Proc3/'+runlist[ii]+'.catalog'
    IF FILE_TEST(catfile) THEN BEGIN
      read_catalog,catfile,run_struct,object,filter,Rfile,Nfile,Sfile, $
                           ellipse,refnum,Rmask2,Nmask2,nsig,/SILENT
      PRINTF,indunit,''
      PRINTF,indunit,runlist[ii]
      PRINTF,indunit,''
      photfile = idir+'Proc4/'+runlist[ii]+'_all.html'
      IF FILE_TEST(photfile) THEN BEGIN
        PRINTF,indunit,'<p><a href="'+photfile+'">PHOTOMETRY</a></p>'
        PRINTF,indunit,''
      ENDIF
    ENDIF

    FOR jj = 0,N_ELEMENTS(object)-1 DO BEGIN
      proc3dir = idir+'Proc3/'+STRTRIM(object[jj],2)+'/'
      proc4dir = idir+'Proc4/'+STRTRIM(object[jj],2)+'/'

      PRINT,"  Object ",object[jj]
      IF FILE_TEST(proc4dir+ellipse[jj]) THEN BEGIN
        read_ellipse_file,proc4dir+ellipse[jj],n_ellipses,refimage,Dx,Dy, $
                          Px,Py,pa,a_i,b_i,z_s,z_f,z_c

        short = STRMID(STRTRIM(Sfile[jj],2),0,STRLEN(Sfile[jj])-8)
        pagename = short+'_'+STRTRIM(runlist[ii])+'.html'

        PRINTF,indunit,'  <p><a href="'+pagename+'">'+short+'</a></p>'

        skyjpg = STRMID(Sfile[jj],0,STRLEN(Sfile[jj])-8)+'_sky.jpg'
        OPENW,pageunit,odir+pagename,/GET_LUN,/MORE
        PRINTF,pageunit,'<p>Sky profile <a href="'+proc4dir+skyjpg+'"><img src = "'+proc4dir+skyjpg+'">Plot</a></p>'
        FOR kk = 1,n_ellipses DO BEGIN
          fluxjpg = STRMID(Sfile[jj],0,STRLEN(Sfile[jj])-8)+'_flux'+STRTRIM(STRING(kk),2)+'.jpg'
          PRINTF,pageunit,'<p>Flux COG #'+STRTRIM(STRING(kk),2)+' <a href="'+ $
                 proc4dir+fluxjpg+'"><img src = "'+proc4dir+fluxjpg+'">Plot</a></p>'
          profjpg = STRMID(Sfile[jj],0,STRLEN(Sfile[jj])-5)+'_prof'+STRTRIM(STRING(kk),2)+'.jpg'
          PRINTF,pageunit,'<p>Profile #'+STRTRIM(STRING(kk),2)+' <a href="'+ $
                 proc4dir+profjpg+'"><img src = "'+proc4dir+profjpg+'">Plot</a></p>'
        ENDFOR

        Oflag = FILE_TEST(proc4dir+short+'_net_O_good.jpg')

        IF NOT FILE_TEST(proc4dir+short+"_3clr"+suff) OR $
           NOT FILE_TEST(proc4dir+short+"_3clr_clean"+suff) OR $
           NOT KEYWORD_SET(skip) THEN BEGIN
          a = a_i * z_f
          b = b_i * z_f

          multflag = (STRPOS(Sfile[jj],'_6') GT 0)
          IF multflag THEN Rbase = STRMID(Rfile[jj],0,STRLEN(Rfile[jj])-5) $
                      ELSE Rbase = STRMID(Rfile[jj],0,STRLEN(Rfile[jj])-7)

          Rmaskfile = Rbase+'_mask.fits'
          Smaskfile = STRMID(Sfile[jj],0,STRLEN(Sfile[jj])-5)+'_mask.fits'
          Imaskfile = Rbase+'_inc_mask.fits'
          Omaskfile = STRMID(Sfile[jj],0,STRLEN(Sfile[jj])-5)+'_override.fits'

          IF FILE_TEST(proc4dir+Rmaskfile) AND FILE_TEST(proc4dir+Smaskfile) AND FILE_TEST(proc4dir+Imaskfile) THEN BEGIN
            Oflag = FILE_TEST(proc4dir+Omaskfile)
            IF Oflag THEN BEGIN
              fits_read,proc4dir+Omaskfile,Omask,junk,/data_only
              Oflag = MAX(Omask)
            ENDIF ELSE BEGIN
              Omask = BYTARR(1,1)
            ENDELSE

            Tfile = Rbase+"_check.fits"

            qa_plot,Rfile[jj],Nfile[jj],Sfile[jj],Tfile,Rmaskfile,Smaskfile,Imaskfile,Omask, $
                    Dx,Dy,pa,a,b,indir=proc3dir,outdir=proc4dir,sky_rad=(a_i*z_s),bxw=bxw,plotsig=plotsig
          ENDIF ELSE BEGIN
            PRINT,'ERROR in singg_qa: missing mask files ',object[jj]
            RETURN
          ENDELSE

        ENDIF

        PRINTF,pageunit,'<p>3-color image <a href="'+proc4dir+short+ $
               '_3clr.jpg"><img src = "'+proc4dir+short+'_3clr.jpg">Plot</a></p>'
        PRINTF,pageunit,'<p>R good-pixel map <a href="'+proc4dir+short+ $
               '_good.jpg"><img src = "'+proc4dir+short+'_good.jpg">Plot</a></p>'
        PRINTF,pageunit,'<p>R bad-pixel map <a href="'+proc4dir+short+ $
               '_bad.jpg"><img src = "'+proc4dir+short+'_bad.jpg">Plot</a></p>'
        PRINTF,pageunit,'<p>Ha good-pixel map <a href="'+proc4dir+short+ $
               '_net_good.jpg"><img src = "'+proc4dir+short+'_net_good.jpg">Plot</a></p>'
        PRINTF,pageunit,'<p>Ha bad-pixel map <a href="'+proc4dir+short+ $
               '_net_bad.jpg"><img src = "'+proc4dir+short+'_net_bad.jpg">Plot</a></p>'
        IF Oflag THEN BEGIN
          PRINTF,pageunit,'<p>Ha override good-pixel map <a href="'+proc4dir+short+ $
                 '_net_O_good.jpg"><img src = "'+proc4dir+short+'_net_O_good.jpg">Plot</a></p>'
          PRINTF,pageunit,'<p>Ha override bad-pixel map <a href="'+proc4dir+short+ $
                 '_net_O_bad.jpg"><img src = "'+proc4dir+short+'_net_O_bad.jpg">Plot</a></p>'
        ENDIF

        singg_tfoot,pageunit
        ;singg_pagebot,pageunit
        hanish_pagebot,pageunit
        CLOSE,pageunit
        FREE_LUN,pageunit
      ENDIF

    ENDFOR

  ENDFOR

;  PRINTF,indunit,'<p>QA Notes <a href="/home/meurer/Text/SINGG/Detect/QA_notes">Here</a></p>'
  PRINTF,indunit,'<p>QA Notes <a href="/data1/acs22/hanish/notes.txt">Here</a></p>'

  singg_tfoot,indunit
  ;singg_pagebot,indunit
  hanish_pagebot,indunit
  CLOSE,indunit
  FREE_LUN,indunit

  RETURN
END
