FUNCTION proc4_smask,Rsexfile,Ssexfile,mask,ellfile,ecntrat
; Normally, this is one of the last steps in the PROC4 routine.
; However, we also need to be able to run it separately; first, for
; debugging purposes, and second, for multiple-filter objects (where
; we don't want to remake the Rmask every time).  For that, call the
; MAKE_SMASK driver routine.

  fits_read,Rsexfile,Rimg,Rhd
  fits_read,Ssexfile,Simg,Shd

  goodval = 0b
  badval  = 1b
  sz = SIZE(Rimg)

  getrot,Shd,rot,cdelt 
  cdelt  = abs(cdelt)*3600.
  as_pix = cdelt[0]

  read_ellipse_file,ellfile,n_ellipses,refimage,Dx,Dy,Px,Py,pa, $
                    a_i,b_i,z_s,z_f,z_c

  buffer = SXPAR(Rhd,'BUFFER')
  Dx = Dx + buffer
  Dy = Dy + buffer
  Px = Px + buffer
  Py = Py + buffer

  a = a_i * z_s
  b = b_i * z_s
  theta = (pa-90.0)*!dtor

  edgemask = make_array(sz[1],sz[2],/byte,value=badval)
  edgemask[buffer:(sz[1]-buffer-1),buffer:(sz[2]-buffer-1)] = goodval

  Rseeing = SXPAR(Rhd,'SEEING')
  Sgrow = MAX([Rseeing/as_pix,3.0]) * 3.0

; Calculate skysig for the subtracted image.  This wouldn't be put
; into the header until after sky_calibration is run, but we need this
; script to be able to be called BEFORE sky_calibration.
  sky_box = 35
  AreaZ = 125.0*sky_box^2/(!pi*a[0:n_ellipses-1]*b[0:n_ellipses-1]) > 0.5
  xyad,Shd,Dx[0:n_ellipses-1],Dy[0:n_ellipses-1],racen,deccen
  diamaj = a[0:n_ellipses-1] * as_pix
  diamin = b[0:n_ellipses-1] * as_pix
  dr = (SQRT((diamaj+diamin)^2.0 + $
        4.0*AreaZ*diamaj*diamin)-diamaj-diamin)/2.0

  tempmask = mask_ellipse_ann3(Simg,Shd,racen,deccen, $
                               diamaj/30.0,diamin/30.0, $
                               dr/60.0,pa[0:n_ellipses-1],goodmask=1b,/amflag)

  mysky,Rimg,Rimsky2,Rskysig2,mask=(mask OR edgemask OR tempmask),/silent
  mysky,Simg,Simsky2,Sskysig2,mask=(mask OR edgemask OR tempmask),/silent

  Scheck = FLTARR(sz[1],sz[2])
  badpix = WHERE(mask EQ badval,badcount)
  Scheck[badpix] = (Rimg[badpix]-Rimsky2)*ecntrat
  badnet = WHERE(ABS(Scheck) GT Sskysig2,netcount)
  Smask = make_array(sz[1],sz[2],/byte,value=goodval)
  IF netcount GT 0 THEN BEGIN
    Smask[badnet] = badval
; Now that we've got the Rsub mask, grow it by a few pixels, too.
    PRINT,"Growing subtracted mask by "+STRTRIM(STRING(Sgrow),2)+" pixels
    grow_mask, Smask, Smasko, Sgrow, goodval=badval, badval=goodval
  ENDIF ELSE BEGIN
    PRINT,"ERROR in proc4_smask: no pixels in subtracted-image mask"
    RETURN,Smask
  ENDELSE

  RETURN,Smasko

END
