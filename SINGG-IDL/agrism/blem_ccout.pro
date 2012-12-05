PRO blem_ccout, npk, xp, yp, peak, shift, w50, wl, prms, psexid, file=file, $
                selmeth=selmeth, selccid=selccid, qualcc=qualcc
   ; print cross cor output from blind_emfind
   ;
   ; npk     -> number of peaks
   ; xp      -> x position of peak in detection(?) image
   ; yp      -> y position of peak in detection(?) image
   ; peak    -> amplitude of peak (normalised?)
   ; shift   -> shift of peak wrt grism source
   ; w50     -> fwhm of gaussian
   ; wl      -> wavelength corresponding to shift
   ; prms    ->
   ; psexid  -> sextractor id of source
   ; file    -> if set a file with this name is opened and all results 
   ;            are written to it.  Otherwise results go to terminal.
   ; selmeth -> selection method.  If set then selmeth is printed
   ; selccid -> selected peak. If set then it is printed
   ; qualcc  -> cross correlation quality code.  If set then it is 
   ;            printed.
   ;
   ; G. Meurer  12/2002 - originally written
   ;            06/2004 - smal format change, documented.
   ;
   IF keyword_set(file)    THEN openw,lu,file,/get_lun ELSE lu = -1
   IF keyword_set(qualcc)  THEN printf,lu,'# cross_cor_quality_code = ',qualcc
   IF keyword_set(selccid) THEN printf,lu,'# Selected_cc_peak       = ',fix(selccid)
   IF keyword_set(selmeth) THEN printf,lu,'# sel_method             = ',selmeth
   ;
   printf,lu,'# '
   printf,lu,'# ccid    xim      yim      peak      shift    w50    lambda      peak/rms      sexid'
   printf,lu,'-------------------------------------------------------------------------------------'
   FOR i = 0, npk-1 DO printf, lu, i, xp[i], yp[i], peak[i], shift[i], w50[i], wl[i], prms[i], psexid[i], $
    format='(i4, f9.2, f9.2, f12.6, f9.2, f8.2, f9.2, f15.6, 2x, a8)'
   IF lu NE -1 THEN free_lun,lu
END 

