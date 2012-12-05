PRO write_profile_header,unit,infile,as_pix,Funits,Fscale, $
                              proftype,headline,n_ellipses
; While most of the variables written to the profile files will depend
; on which galaxy in the frame is being measured, several variables
; only depend on the image itself.  Write them first.

  unitflag = (Funits NE "DN/sec")

  PRINTF,unit,headline
  PRINTF,unit,""
  PRINTF,unit,"# FILENAME = "+STRTRIM(infile,2)+" / Image file name"
  PRINTF,unit,"# PIXSIZE = "+STRING(as_pix)+" / Pixel size [arcsec]"
  PRINTF,unit,"# FLUX_UNITS = "+STRTRIM(Funits,2)+" / Flux units"
  PRINTF,unit,"# FLUX_SCALE = "+STRING(Fscale)+" / Flux scale [above units / (DN/sec)]"
  PRINTF,unit,"# FLUXCAL = ",unitflag," / Flux calibration complete?"
  PRINTF,unit,"# PROFTYPE = "+STRTRIM(proftype)+" / Profile type (isophote/brightness/other)"
  PRINTF,unit,"# NUMGALS = "+STRING(n_ellipses)+" / Number of sources in image"

  RETURN

END
