FUNCTION singg_cr,inlist,indir,shfile,maskfile,$
                  rdnoise,gain,wellsatur,sky_mask,imsize,edge,buffer, $
                  Noise,Npix,Exptime,WExptime,start_date, $
                  sky,skysig,Wsky,Wskysig, $
                  Scale,Serr,im_ref,HEADER_SKY=header_sky
; This routine, called by singg_combine, does the actual image assembly.
; We do this in a subroutine to help save dynamic memory, because
; otherwise the program would need to store several large data cubes.

; INPUTS:
; inlist(N)        Prefix of star/match files to be used by refflux for scale
; indir            Directory containing the .stars and .match files
; shfile(N)        Names of object files to be used by cr_weight for combine
; maskfile(N)      Names of mask files to be used
; rdnoise(N)       Pixel read noise (default 3.1 for each)
; gain(N)          Gain of each image (default 1.0 for each)
; wellsatur        Well saturation threshold
; sky_mask(X,Y)    Basic mask for sky
; imsize[2]        2D Size of image
; edge             Ignore stars within this distance of the edge of the image
; buffer           If the image has been padded, this is by how much.

; OPTIONAL INPUT:
; /header_sky      Use the sky level and sigma from the header instead
;                    of calculating it on the fly.

; OUTPUT:
; Image(X,Y)       Combined image (returned value of function)
; Noise(X,Y)       Pixel-by-pixel noise map
; Npix(X,Y)        Number of pixels used at each location
; Exptime(N)       Exposure times per image
; WExptime         Weighted total exposure time
; start_date(N)    Start date and time of the exposure
; sky(N)           Rough-estimate sky levels
; skysig(N)        Rough-estimate sky uncertainty
; Wsky             Weighted average sky level
; Wskysig          Weighted average sky uncertainty
; scale(N)         Array of scaling levels.  1.0 for the brightest image,
;                  increasing as brightness drops
; serr(N)          Array; uncertainty in scale
; im_ref           integer corresponding to the brightest image in the set,
;                  as an index of the imfile array.

  n_files = N_ELEMENTS(shfile)

  Wcube = DBLARR(imsize[0],imsize[1],n_files)
  Wnoise = FLTARR(imsize[0],imsize[1],n_files)

  sky = FLTARR(n_files)
  skysig = FLTARR(n_files)
  start_date = STRARR(n_files)
  Exptime = FLTARR(n_files)

  FOR ii = 0,n_files-1 DO BEGIN
    IF KEYWORD_SET(header_sky) THEN BEGIN
      IF FILE_TEST(shfile[ii]) THEN fits_read,shfile[ii],junk,hd,/header_only $
                               ELSE img = readfits(shfile[ii]+'.gz',hd,/SILENT)
      sky[ii] = SXPAR(hd,'SKYLEV')
      skysig[ii] = SXPAR(hd,'SKYSIG')
    ENDIF ELSE BEGIN
      IF FILE_TEST(shfile[ii]) THEN img = readfits(shfile[ii],hd,/SILENT) $
                               ELSE img = readfits(shfile[ii]+'.gz',hd,/SILENT)
      IF FILE_TEST(maskfile[ii]) THEN maskimg = readfits(maskfile[ii],/SILENT) $
                                 ELSE maskimg = readfits(maskfile[ii]+'.gz',/SILENT)
      temp_sky_mask = (sky_mask OR (maskimg LT 0.5))

      mysky,img,temp_sky,temp_skysig,mask=temp_sky_mask,/silent
      sky[ii]=temp_sky
      skysig[ii]=temp_skysig
    ENDELSE
    Exptime[ii] = SXPAR(hd,'EXPTIME')
    start_date[ii] = SXPAR(hd,'DATE-OBS')
    PRINT,"  For image ",inlist[ii]," the sky level is ",sky[ii]
  ENDFOR

; Unfortunate logic issue: we need sky and skysig before refflux, and we need
; scale AFTER refflux, so we have to split up the loop.
  PRINT,"  Calculating scale factors"
  refflux,inlist,indir,Exptime,edge,buffer,sky,skysig,Scale,Serr,im_ref, $
          min_exptime = (MAX(Exptime)/2.0),/round
  weight = 1.0/Scale
; Correct this array slightly; if the scale is close to 1.0, don't bother.

  FOR ii = 0,N_ELEMENTS(Scale)-1 DO BEGIN
    IF ABS(Scale[ii] - 1.0) LT (Serr[ii]/Scale[ii]) THEN weight[ii] = 1.0
  ENDFOR

  WExptime = MEAN(Exptime)/MEAN(weight)

  Wsky = MEAN(sky)/MEAN(weight)
  Wskysig = MEAN(skysig)/MEAN(weight)
  PRINT,"  Average weighted sky level = ",Wsky," +/- ",Wskysig

  FOR ii = 0,n_files-1 DO BEGIN
; Yes, we're reading the image again.  It's either that or create another
; huge cube solely for an intermediate step.
    IF FILE_TEST(shfile[ii]) THEN img = readfits(shfile[ii],/SILENT) $
                             ELSE img = readfits(shfile[ii]+'.gz',/SILENT)

; Weight each data cube, and correct for sky variations.
    Wcube[*,*,ii] = (img - sky[ii]) / weight[ii]
    Wnoise[*,*,ii] = (SQRT(img*gain[ii] + rdnoise[ii]^2)/gain[ii]) / weight[ii]
; note that bad-column pixels can give NAN since img<-rd^2
  ENDFOR

; Set up the mask cube, masking out saturated pixels, edge effects, and so on
  PRINT,"  Creating mask cube"

  mask = singg_mask_cube(Wcube,sky,skysig,scale,maskfile,wellsatur,sat_mask)
; Remember, mask is 1b for GOOD pixels, 0b for BAD ones.

; Now that the data is prepared, run the actual combine routine.
  PRINT,"  Running cr_reject combine algorithm"
  rd_noise_dn = -1.0  ; Negative means to use the input noise cube
  dark_dn = 0.0       ; We've already corrected for darks, if needed
  tempgain = 1.0      ; We've already corrected for gain, if needed
  mult_noise = 0.05   ; It's an internal constant thing.  Just accept it.

  IF n_files GT 1 THEN BEGIN
    cr_weight,Wcube,rd_noise_dn,dark_dn,tempgain,mult_noise,Image,Noise,Npix, $
              noise_cube=Wnoise,/noskyadjust,weighting=0,dilation=1, $
              weight_array=weight,input_mask=mask,/median_loop,/init_med
  ENDIF ELSE BEGIN
; We only have one image.  This makes things much easier in some ways, since
; all our "cubes" are actually 2D arrays ready to be dumped into the right
; places.
    Image = TEMPORARY(Wcube)
    Npix = LONG(mask)
    Noise = TEMPORARY(Wnoise)
  ENDELSE

; Now, we need to fix the image and put it in the right units.
; First, turn it into a COUNT RATE image by dividing by the weighted 
; exposure time.

  Image = TEMPORARY(Image) / n_files

; Next, fill in saturated stars.  Really, we should use some better
; method to find the max legitimate value, but this works for our test case.
  Image = Image + FLOAT(sat_mask)*MAX(Image) ; was wellsatur

; Then, add in the average sky level.
  Image = TEMPORARY(Image) + Wsky

; Finally, divide by exposure time.
  Image = TEMPORARY(Image) / Exptime[im_ref]

; Note that "bad" pixels that weren't truly saturated (all those zeroes around
; the edge, for instance) will simply be set to the average sky level.
; Routines using these images should be sure to use the npix mask as well.

  RETURN,Image

END
