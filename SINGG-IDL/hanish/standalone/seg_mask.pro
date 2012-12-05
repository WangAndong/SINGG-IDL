PRO seg_mask,segimg,grow,mask, $
             INCLUSION=inclusion,SFILE=Sfile,ELLIPSE=ellipse
; Given a segmentation mask image, extract a binary mask.
; INPUTS
;   segimg[x,y]        Segmentation image
;   grow               Should be set to seeing, in pixels.
; OPTIONAL INPUTS
;   /inclusion         If set, return the net "inclusion mask" instead
;   Sfile              If set, return the _?sub mask image instead.
;   ellipse            Name of ellipse file
; OUTPUT
;   mask[x,y]          Mask image

  sz = SIZE(segimg) 

  Sflag = KEYWORD_SET(Sfile)
  incflag = KEYWORD_SET(inclusion)

  badval = 1b
  goodval = 0b

; Our default mask:
  masktemp = (segimg LT 0)
  grow_mask,masktemp,mask,grow,goodval=badval,badval=goodval

  IF Sflag THEN BEGIN
    IF incflag THEN BEGIN
      PRINT,"ERROR in seg_mask: cannot have both SFILE and /INCLUSION set"
      RETURN
    ENDIF

    fits_read,Sfile,Simg,Shd
    Sskysub = STRTRIM(SXPAR(Shd,'SKYSUB',count=count),2)
    Ssky = 0.0
    IF count GT 0 AND Sskysub EQ 'T' THEN Ssky = SXPAR(Shd,'SKYLEV')
    ecntrat = SXPAR(Shd,'ECNTRAT2')
    Rfile = SXPAR(Shd,'PRED00')
    Nfile = SXPAR(Shd,'PRED01')

    getrot,Shd,rot,cdelt 
    cdelt  = abs(cdelt)*3600.
    as_pix = cdelt[0]

    fits_read,Rfile,Rimg_old,Rhd
    Rexptime = SXPAR(Rhd,'EXPTIME') / SXPAR(Rhd,'NCOMBINE')
    buffer = SXPAR(Rhd,"BUFFER")
    Rskysub = STRTRIM(SXPAR(Rhd,'SKYSUB',count=count),2)
    Rsky = 0.0
    IF count GT 0 AND Rskysub EQ 'T' THEN Rsky = SXPAR(Rhd,'SKYLEV')
    Rimg = (Rimg_old+Rsky) * Rexptime

    fits_read,Nfile,Nimg,Nhd,/header_only
    Nexptime = SXPAR(Nhd,'EXPTIME') / SXPAR(Nhd,'NCOMBINE')

    Simg = (Simg+Ssky) * Nexptime

    ecntrat = SXPAR(Shd,'ECNTRAT2') * (Nexptime/Rexptime)

    edgemask = make_array(sz[1],sz[2],/byte,value=badval)
    edgemask[buffer:(sz[1]-buffer-1),buffer:(sz[2]-buffer-1)] = goodval

    IF NOT KEYWORD_SET(ellipse) THEN BEGIN
      PRINT,"ERROR in seg_mask: ELLIPSE must be set for net mask"
      RETURN
    ENDIF
    read_ellipse_file,ellipse,n_ellipses,refimage,Dx,Dy,Px,Py,pa, $
                      a_i,b_i,z_s,z_f,z_c
    a = a_i * z_s
    b = b_i * z_s

    bxw = 35
    AreaZ = FLTARR(n_ellipses)
    FOR ii = 0,n_ellipses-1 DO BEGIN
      AreaZ[ii] = MAX([125.0*bxw^2/(!pi*a[ii]*b[ii]), 0.5])
    ENDFOR
    xyad,Shd,(Dx+buffer),(Dy+buffer),racen,deccen
    diamaj = a * as_pix
    diamin = b * as_pix
    dr = (SQRT((diamaj+diamin)^2.0 + $
          4.0*AreaZ*diamaj*diamin)-diamaj-diamin)/2.0
    tempmask = mask_ellipse_ann3(Simg,Shd,racen,deccen, $
                                 diamaj/30.0,diamin/30.0, $
                                 dr/60.0,pa,goodmask=1b,/amflag)

    mysky,Simg,Simsky,Simsig,mask=(mask OR edgemask OR tempmask),/silent
    mysky,Rimg,Rimsky,Rimsig,mask=(mask OR edgemask OR tempmask),/silent

    Scheck = FLTARR(sz[1],sz[2])
    badpix = WHERE(mask EQ badval,badcount)

    Scheck[badpix] = (Rimg[badpix]-Rimsky)*ecntrat
    badnet = WHERE(ABS(Scheck) GT Simsig,netcount)
    masktemp = make_array(sz[1],sz[2],/byte,value=goodval)
    IF netcount GT 0 THEN BEGIN
      masktemp[badnet] = badval
      grow_mask,masktemp,mask,(3.0*grow),goodval=badval,badval=goodval
    ENDIF
  ENDIF ELSE BEGIN
    IF incflag THEN BEGIN
; Inclusion mask.
      masktemp = ((segimg MOD 16) GE 8)
      grow_mask,masktemp,mask,(2.0*grow),goodval=badval,badval=goodval
    ENDIF
  ENDELSE

  RETURN
END
