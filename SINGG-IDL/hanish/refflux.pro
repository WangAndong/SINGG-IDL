PRO refflux,inlist,indir,exptime,edge,buffer,skylev,skysig, $
            scale,serr,im_ref,MIN_EXPTIME=min_exptime,ROUND=round
; This routine takes the .fits.stars files and parses them to determine
; a flux, proportional to the brightest image in the set.  This allows us to
; scale all of the sky-subtracted images to the point where flux is relatively
; constant for a given object.  This leads to smoother combines and lower
; uncertainties (by compensating for the scale in the noise cube).

; INPUTS
; inlist(N)       Array of object images, usually "obj???????".
; indir           Directory containing the .stars and .match files (proc3)
; exptime(N)      Array of exposure times of images
; edge            Ignore stars within this distance of the edge of the image
; buffer          If the image has been padded, this is by how much.
; skylev(N)       Sky level for each image
; skysig(N)       Uncertainty in sky levels

; OUTPUTS
; scale(N)        Array of scaling levels.  1.0 corresponds to the clearest
;                 image (not necessarily the BRIGHTEST image)
; serr(N)         Array; uncertainty in scale
; im_ref          Index of the brightest image in the set

; OPTIONAL INPUTS
; min_exptime     Minimum exposure time of input image to be considered for
;                 reference image calculations.
; /round          If scale is close to 1.0, just set it to 1.0.

  n_files = N_ELEMENTS(inlist)

  IF NOT KEYWORD_SET(min_exptime) THEN min_exptime = 0.0

  base = STRTRIM(indir,2)+STRTRIM(inlist,2)+'.buff'
  imfile = base+'.fits'
;  imstarfile = base+'.fits.stars'
  matchfile = base+'.match'

  scale = FLTARR(n_files)
  err = FLTARR(n_files)
  flux = FLTARR(n_files)
  offset = FLTARR(2,n_files)

; Set up an array of offsets:
  FOR ii = 0,(n_files-1) DO BEGIN
    IF FILE_TEST(matchfile[ii]) AND NOT FILE_TEST(matchfile[ii],/ZERO_LENGTH) THEN BEGIN
      readcol_new,matchfile[ii],x1,y1,x2,y2,xfit,yfit,delx,dely,/silent
      offset[0,ii] = MEAN(xfit-x1)
      offset[1,ii] = MEAN(yfit-y1)
    ENDIF ELSE BEGIN
; When the posref image was shifted, there was no .match file, or
; there was an empty one.
      offset[0:1,ii] = [0.0,0.0]
    ENDELSE
  ENDFOR

; First, figure out which one's the reference image.

  fratemax = -999.0
  im_ref = 0

  FOR ii = 0,n_files-1 DO BEGIN
; Entry 0 is a bit redundant, but we need to make sure it doesn't
; bypass the min_exptime logic.

    IF ii GT 0 THEN BEGIN
      del_M = calc_mdiff(imfile[ii],imfile[0], $
                         (edge+buffer),sigma,nmatch, $
                         IMSKY=[skylev[ii],skysig[ii]], $
                         REFSKY=[skylev[0],skysig[0]], $
                         OFFSET=[(offset[0,ii]-offset[0,0]),(offset[1,ii]-offset[1,0])])
;                         IMOFFSET=[offset[0,ii],offset[1,ii]], $
;                         REFOFFSET=[offset[0,0],offset[1,0]])
; This gave us a magnitude difference and uncertainty; convert to a flux 
      flux[ii] = 10.0^(-0.4*del_M)

      IF nmatch LT 5 THEN BEGIN
        PRINT,'ERROR in refflux: insufficient matches ',nmatch
 print,imfile[ii],imfile[0]
 print,(edge+buffer) ;;
 print,skylev[ii],skylev[0] ;;
 print,skysig[ii],skysig[0] ;;
 print,offset[0,ii],offset[1,ii] ;;
 print,offset[0,0],offset[1.0]  ;;
stop
        RETURN
      ENDIF

    ENDIF ELSE BEGIN
; Duh.
      flux[ii] = 1.0
    ENDELSE

    IF flux[ii]/Exptime[ii] GT fratemax AND $
                Exptime[ii] GE min_exptime THEN BEGIN
      fratemax = flux[ii]/Exptime[ii]
      im_ref = ii
    ENDIF
  ENDFOR

  IF fratemax LT 0 THEN BEGIN
    PRINT,'ERROR in refflux: all images excluded because of MIN_EXPTIME. ',min_exptime
    RETURN
  ENDIF

; Then, calculate the scale factors, with correct errors.
; Unfortunately, this pretty much requires re-running calc_mdiff,
; using the fluxref image as the reference star table.
  FOR ii = 0,n_files-1 DO BEGIN
    IF ii NE im_ref THEN BEGIN
      del_M = calc_mdiff(imfile[ii],imfile[im_ref], $
                         (edge+buffer),sigma,nmatch, $
                         IMSKY=[skylev[ii],skysig[ii]], $
                         REFSKY=[skylev[im_ref],skysig[im_ref]], $
                         OFFSET=[(offset[0,ii]-offset[0,im_ref]),(offset[1,ii]-offset[1,im_ref])])
;                         IMOFFSET=[offset[0,ii],offset[1,ii]], $
;                         REFOFFSET=[offset[0,im_ref],offset[1,im_ref]])
; This gave us a magnitude difference and uncertainty; convert to a flux 
      flux[ii] = 10.0^(-0.4*del_M)

; if F(a) = 10.0^(0.4*(a +/- b), dF = F * ln(10) * 0.4 * b.  Trust me.
      err[ii] = flux[ii]*ALOG(10.0)*0.4*sigma/SQRT(nmatch)

    ENDIF ELSE BEGIN
; Duh.
      flux[ii] = 1.0
      err[ii] = 0.0
    ENDELSE
  ENDFOR

  scale = FLTARR(n_files)
  serr = FLTARR(n_files)

;  scale = flux[im_ref]/flux

  FOR ii=0,n_files-1 DO BEGIN
    scale[ii] = flux[im_ref]/flux[ii]
    serr[ii] = scale[ii] * (err[ii]/flux[ii])

    IF KEYWORD_SET(round) AND ABS(scale[ii] - 1.0) LT (err[ii]/flux[ii]) THEN BEGIN
      scale[ii] = 1.0
      serr[ii] = 0.0
    ENDIF
  ENDFOR

; Outputs
  PRINT,"  Flux reference image: ",inlist[im_ref]+'.fits'
  FOR ii = 0,n_files-1 DO BEGIN
    PRINT,"    Scale for image ",inlist[ii],".fits = ",scale[ii]," +/- ",serr[ii]
  ENDFOR

  RETURN
END
