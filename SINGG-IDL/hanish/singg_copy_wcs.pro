PRO singg_copy_wcs,hd_in,hd_out
; Used to copy all the WCS-related information from one header to another, 
; even if it might not be used for anything.
  SXADDPAR,hd_out,"RA",SXPAR(hd_in,'RA'),''
  SXADDPAR,hd_out,"DEC",SXPAR(hd_in,'DEC'),''

; First, the old stuff
  SXADDPAR,hd_out,"WCSDIM",SXPAR(hd_in,'WCSDIM'),''
  SXADDPAR,hd_out,"LTM1_1",SXPAR(hd_in,'LTM1_1'),''
  SXADDPAR,hd_out,"LTM2_2",SXPAR(hd_in,'LTM2_2'),''
  SXADDPAR,hd_out,"WAT0_001",SXPAR(hd_in,'WAT0_001'),''
  SXADDPAR,hd_out,"WAT1_001",SXPAR(hd_in,'WAT1_001'),''
  SXADDPAR,hd_out,"WAT2_001",SXPAR(hd_in,'WAT2_001'),''
  SXADDPAR,hd_out,"LTV1",SXPAR(hd_in,'LTV1'),''
  SXADDPAR,hd_out,"ASEC22",SXPAR(hd_in,'ASEC22'),''

; Now, the new stuff
  SXADDPAR,hd_out,"LTV2",SXPAR(hd_in,'LTV2'),''
  SXADDPAR,hd_out,"LTM2_1",SXPAR(hd_in,'LTM2_1'),''
  SXADDPAR,hd_out,"LTM1_2",SXPAR(hd_in,'LTM1_2'),''

  SXADDPAR,hd_out,"RADECSYS",SXPAR(hd_in,'RADECSYS'),''
  SXADDPAR,hd_out,"EQUINOX",SXPAR(hd_in,'EQUINOX'),''

  SXADDPAR,hd_out,"CTYPE1",SXPAR(hd_in,'CTYPE1'),''
  SXADDPAR,hd_out,"CTYPE2",SXPAR(hd_in,'CTYPE2'),''
  SXADDPAR,hd_out,"CRVAL1",SXPAR(hd_in,'CRVAL1'),''
  SXADDPAR,hd_out,"CRVAL2",SXPAR(hd_in,'CRVAL2'),''
  SXADDPAR,hd_out,"CRPIX1",SXPAR(hd_in,'CRPIX1'),''
  SXADDPAR,hd_out,"CRPIX2",SXPAR(hd_in,'CRPIX2'),''

  SXADDPAR,hd_out,"CD1_1",SXPAR(hd_in,'CD1_1'),''
  SXADDPAR,hd_out,"CD1_2",SXPAR(hd_in,'CD1_2'),''
  SXADDPAR,hd_out,"CD2_1",SXPAR(hd_in,'CD2_1'),''
  SXADDPAR,hd_out,"CD2_2",SXPAR(hd_in,'CD2_2'),''

  RETURN

END
