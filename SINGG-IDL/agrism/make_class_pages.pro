PRO make_class_pages, setup=setup
   ;
   ; Make html pages of interesting sources classified with 
   ; grism_classall.
   ;
   ; setup   -> if set, this specifies a file of idl statements to be 
   ;            executed that will define the variables needed by
   ;            this code
   plotdir   = 'Plots/'                     ; this is directory containing png plots
   pstmp     = 'stmp_'                      ; prefix for stamps
   pspcc     = 'spcc_'                      ; prefix for count rate spec
   pspcf     = 'spcf_'                      ; prefix for flux calibrated spec 
   pribn     = 'ribn_'                      ; prefix for ribbon plot
   pscii     = 'spec_'                      ; prefix for ascii spec 
   pfit      = 'spec_gfit_'                 ; prefix for Gaussian fit plots
   logf      = 'emsource.cat'               ; emission line source catalog
   logm      = 'mstar_sn.cat'               ; stellar spectra catalog
   logi      = 'spurious.cat'               ; spurious source catalog
   filemhtml = 'emsource.html'              ; output emission line source page
   filsthtml = 'mstar_sn.html'              ; output stellar spectra page
   filsphtml = 'spurious.html'              ; output spurious source page
   titleem   = 'Emission line sources from grism_classify'              ; title for filemhtml
   titlest   = 'Stellar spectra from grism_classify'                    ; title for filsthtml
   titlesp   = 'Spurious / interesting sources from grism_classify'     ; title for filsphtml
   commst    = 'Sources marked as M star or SNe with grism_classify.'   ; comment for filsthtml
   commsp    = 'Sources marked as <i>spurious</i> with grism_classify.' ; comment for filsphtml
   ;
   ; execute commands in setup file, if it exists
   IF keyword_set(setup) THEN BEGIN 
      readfmt, setup, '(a120)', line
      nl = n_elements(line)
      FOR i = 0, nl-1 DO BEGIN 
         lin = strtrim(line[i],2)
         print, lin
         IF strmid(lin,0,1) NE ';' THEN result = execute(lin)
      ENDFOR 
   ENDIF 
   ;
   cwd      = ''
   cd, plotdir, current=cwd
   grp_emcat2html, logf, filemhtml, titleem, pstmp, pfit, pspcc, pspcf, pribn, pscii
   grp_logcat2html, logm, filsthtml, titlest, commst, pstmp, pspcc, pspcf, pribn, pscii
   grp_logcat2html, logi, filsphtml, titlesp, commsp, pstmp, pspcc, pspcf, pribn, pscii
   cd, cwd
   ;
END 
