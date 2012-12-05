FUNCTION mask_ellipse_ann3, image, header, ra, dec, dmaj, dmin, dr, pa, $
                            goodmask=goodmask, amflag=amflag, inmask=inmask
;
; compute an elliptical annulus mask. 
; defaults to dontval = 0b
; doval = 1b - dontval (always)
; ra & dec -- decimal degrees
; dmaj - interior major axis diameter for annulus
; dmin - interior minor axis diameter for annulus
; dr - Width of annulus: N.B. both the interior semi- major axis and minor
;      axis are incremented by this amount to get outer aperture.
;      if dr <= 0 then only pixels interior to interior aperture are masked.
; ratio - major to minor axis ratio
; pa - position angle (ASSUMES North UP and EAST left image)
; goodmask -
; amflag - if true then sizes are in arcminutes, defaults to pixels.
; inmask - input mask
;
; Modified version of d25mask, by G.R. Meurer 11/2001
; d25mask written By M. Seibert 5/2001
;
; Latest version: uses arrays for ra, dec, dmaj, dmin, dr, pa.  Makes one mask
; with annuli for multiple objects.
   n_ellipses = N_ELEMENTS(ra)

   dontval = 0b
   doval   = 1b
   IF KEYWORD_SET(goodmask) THEN BEGIN 
      IF (goodmask EQ 1b) THEN BEGIN 
         dontval = 1b
         doval   = 0b
      ENDIF ELSE BEGIN 
         doval   = 1b
         dontval = 0b
      ENDELSE 
   ENDIF 

   getrot,header,rot,cdelt 
   cdelt = abs(cdelt)*3600.
   as_pix = cdelt[0]

   adxy,header,ra,dec,x0,y0 ;,/print

   IF KEYWORD_SET(amflag) THEN factor = 0.5*60.0/as_pix ELSE factor = 0.5

   amaj=factor*dmaj
   amin=factor*dmin
   ratio = amaj/amin

   sz=size(image)
   IF KEYWORD_SET(inmask) THEN BEGIN
      ; print,'inmask is set; pixels to be masked should equal ',doval
      szm = size(inmask)
      IF (szm[1] NE sz[1]) OR (szm[2] NE sz[2]) THEN BEGIN 
         print, 'Error in mask_ellipse_ann : different sizes for image & mask'
         print, ' image: ', sz
         print, ' mask : ', szm
         return,inmask
      ENDIF 
      mask = inmask
   ENDIF ELSE BEGIN
      mask = make_array(sz[1], sz[2], /BYTE, VALUE=dontval)
   ENDELSE 

   amaj2  =  amaj + 2.0*factor*dr
   amin2  =  amin + 2.0*factor*dr
   ratio2 =  amaj2/amin2

;  Now, find pixels inside each annulus.  Pixels inside the innermost rings are
;  ALWAYS masked, pixels outside each outer ring are only masked if no other
;  ring included that pixel.  So, the "hardmask" will correspond to pixels that
;  should always be masked, while "softmask" will be the ones that'll be masked
;  unless they go with another ellipse.

   ima1 = FLTARR(sz[1],sz[2])
   ima2 = FLTARR(sz[1],sz[2])
   softmask = make_array(sz[1], sz[2], /BYTE, VALUE=dontval)
   hardmask = make_array(sz[1], sz[2], /BYTE, VALUE=doval)

   FOR ii = 0,n_ellipses-1 DO BEGIN
; Create two arrays to make masks.  "ima1" is the elliptical distance from the 
; center using the input axial ratios; "ima2" is the elliptical distance from
; the center using the calculated axial ratios from the outer ellipse.
; If dr<0, ima2 won't be used.
     dist_ellipse,ima1,[sz[1],sz[2]],x0[ii],y0[ii],ratio[ii],pa[ii]

     IF dr[ii] GT 0.0 THEN BEGIN
       dist_ellipse,ima2,[sz[1],sz[2]],x0[ii],y0[ii],ratio2[ii],pa[ii]

; dr was nonzero, which means mask anything inside "inpix" or outside "outpix"
; Pixels outside should only be masked if it's outside all ellipses
       outpix = WHERE(ima2 LE amaj2[ii],outcount)
       IF outcount GT 0 THEN softmask[outpix] = doval
; Pixels inside an inner radius should always equal dontval
       inpix = WHERE(ima1 LE amaj[ii],incount)
       IF incount GT 0 THEN hardmask[inpix] = dontval
     ENDIF ELSE BEGIN
; dr was zero or less, which means mask anything inside the ellipse
PRINT,"dr negative ",ii
       dopix = WHERE(ima1 LE amaj[ii],count)
       IF count GT 0 THEN softmask[dopix] = dontval 
     ENDELSE
   ENDFOR

; hardmask is dontval inside any inner ring.
; softmask is dontval outside all outer rings.
; mask is dontval unless you say otherwise.

; Problem: if inmask is doval, you want to mask that pixel.  That means that
; if /goodmask is used, you want the donut AND NOT (mask = doval), but
; if it's not used, you want the donut OR mask=doval
   IF KEYWORD_SET(goodmask) THEN BEGIN
     goodpix = WHERE((hardmask EQ doval AND softmask EQ doval) AND $
                     (mask NE doval),count)
   ENDIF ELSE BEGIN
     goodpix = WHERE((hardmask EQ doval AND softmask EQ doval) OR $
                     (mask EQ doval),count)
   ENDELSE
   IF count GT 0 THEN mask[goodpix] = doval

return,mask
END
