PRO calc_growth,img,mask,binZ,cenX,cenY,netsky,theta,a,b, $
                 Ngood,Nbad,Flux,Flux_good,SB,SBsig
; INPUT
; img(X,Y)    Input image, sky-subtracted
; mask(X,Y)   Mask images, of pixels to ignore
; binZ        Array of values giving the radius of each bin (divided
;               by the semimajor axis)
; cenX, cenY  X and Y of center of curves
; netsky      Sky correction for this galaxy relative to the already-subtracted
;             sky (0.0 for the first galaxy)
; theta,a,b   Ellipse parameters (position angle, semimajor, semiminor)
; OUTPUT
; Ngood(N,E)  Number of good pixels in bin
; Nbad(N,E)   Number of bad pixels in bin
; zbin(N,E,3) Array of useful data for each bin:
; Flux(N,E)   Total flux per bin, corrected for missing/masked pixels
; Flux_good(N,E) Total flux of good pixels in each bin
; SB(N,E)     Average flux per good pixel (surface brightness)
; SBsig(N,E)  SD of surface brightness

  num_bins = N_ELEMENTS(binZ)
  num_gals = N_ELEMENTS(cenX)
  binsize = (binZ[num_bins-1]-binZ[num_bins-2])
  sz = SIZE(img)
  zmax = FLTARR(num_gals) + binZ[num_bins-1]

  Ngood = LONARR(num_bins,num_gals)
  Nbad = LONARR(num_bins,num_gals)
  Ndump = LONARR(num_bins,num_gals)
  square = DBLARR(num_bins,num_gals)
  Flux = DBLARR(num_bins,num_gals)
  Flux_good = DBLARR(num_bins,num_gals)
  SB = DBLARR(num_bins,num_gals)
  SBsig = DBLARR(num_bins,num_gals)

  Zgrid = FLTARR(num_gals,sz[1],sz[2])
  FOR ii = 0,num_gals-1 DO BEGIN
    dist_ellipse,tempmask,[sz[1],sz[2]],cenX[ii],cenY[ii],(a[ii]/b[ii]),(theta[ii]/!dtor)+90.0
    Zgrid[ii,*,*] = (tempmask / a[ii])

    IF ii GT 0 THEN BEGIN
      ztest = calc_z(cenX[0:ii-1],cenY[0:ii-1],cenX[ii],cenY[ii],theta[ii],a[ii],b[ii])
      zmax[ii] = MIN(ztest)-binsize < zmax[ii]
    ENDIF
  ENDFOR

; New logic:
; 1: find each valid pixel
; 2: if it's uncontested, bin it.
; 3: if it's contested, subtract the mean of the earlier galaxy from
; the image before calculating the latter.

  FOR ii = 0,num_gals-1 DO BEGIN
    ztemp = Zgrid[ii,*,*] ; Need to do it this way.
    loc = WHERE(ztemp LT zmax[ii],count)
    y = FIX(loc/sz[1])
    x = loc - y*sz[1]

    IF count GT 0 THEN BEGIN
      FOR jj = LONG(0),LONG(count-1) DO BEGIN
        valid = WHERE(Zgrid[*,x[jj],y[jj]]-zmax LT 0.0,num_valid)
        IF num_valid GT 0 THEN BEGIN
          IF Zgrid[ii,x[jj],y[jj]] LT binZ[0] THEN binnum = 0 ELSE $
                     junk = MIN(SQRT(binZ-Zgrid[ii,x[jj],y[jj]]),binnum,/NAN)
; If Zgrid[galindex,x[jj],y[jj]] < binZ[0], it uses zero.  If it's
; within the binZ array, it finds the next largest one.
          IF mask[x[jj],y[jj]] THEN BEGIN
            Nbad[binnum,ii] = Nbad[binnum,ii] + LONG(1)
          ENDIF ELSE BEGIN
            IF num_valid EQ 1 THEN BEGIN
              Ngood[binnum,ii] = Ngood[binnum,ii] + LONG(1)
              delzbin = img[x[jj],y[jj]] - netsky[ii]
            ENDIF ELSE BEGIN
              delzbin = 0.0
              valid2 = WHERE(Zgrid[*,x[jj],y[jj]] LT 1.0,num_valid2)

;              IF (num_valid2 EQ 1 AND ii EQ valid2[0]) OR (num_valid2 GT 1 AND ii EQ MAX(valid2)) OR (num_valid2 EQ 0 AND ii EQ valid[num_valid-1]) THEN BEGIN
;                Ngood[binnum,ii] = Ngood[binnum,ii] + LONG(1)
;                delzbin = img[x[jj],y[jj]] - netsky[ii]
;              ENDIF ELSE BEGIN
;                Nbad[binnum,ii] = Nbad[binnum,ii] + LONG(1)
;              ENDELSE

              CASE num_valid2 OF
                0: BEGIN
                  IF ii EQ valid[num_valid-1] THEN BEGIN
                    Ngood[binnum,ii] = Ngood[binnum,ii] + LONG(1)
                    delzbin = img[x[jj],y[jj]] - netsky[ii]
                  ENDIF ELSE BEGIN
                    Nbad[binnum,ii] = Nbad[binnum,ii] + LONG(1)
                  ENDELSE
                   END
                1: BEGIN
; Only one still wants this.
                  IF ii EQ valid2[0] THEN BEGIN
                    Ngood[binnum,ii] = Ngood[binnum,ii] + LONG(1)
                    delzbin = img[x[jj],y[jj]] - netsky[ii]
                  ENDIF ELSE BEGIN
                    Nbad[binnum,ii] = Nbad[binnum,ii] + LONG(1)
                  ENDELSE
                   END
                ELSE: BEGIN
; At least two still want this.  Give it to the
; higher-numbered one, with the assumption that we'll always put the
; little sources at the end.
                  IF ii EQ valid2[num_valid2-1] THEN BEGIN
                    Ngood[binnum,ii] = Ngood[binnum,ii] + LONG(1)
; Find the mean galaxy fluxes in the non-overlapping parts of the
; earlier galaxies.
                    delzbin = img[x[jj],y[jj]] - netsky[ii]
                    FOR kk = 0,num_valid2-1 DO BEGIN
                      IF valid2[kk] NE ii THEN BEGIN
                        junk = MIN(SQRT(binZ-Zgrid[valid2[kk],x[jj],y[jj]]),val,/NAN)
                        delzbin = delzbin - SB[val,valid2[kk]]
                      ENDIF
                    ENDFOR
                  ENDIF ELSE BEGIN
                    delzbin = 0.0
; For now, we're calling any "dumped" pixel "bad".
                    Ndump[binnum,ii] = Ndump[binnum,ii]+1
                    Nbad[binnum,ii] = Nbad[binnum,ii]+1
                  ENDELSE
                   END
              ENDCASE
            ENDELSE
            Flux_good[binnum,ii] = Flux_good[binnum,ii] + delzbin
            square[binnum,ii] = square[binnum,ii] + delzbin^2.0
          ENDELSE
        ENDIF
      ENDFOR
    ENDIF

; Now that we've assembled the bins for this object, find the bin-wide averages.
    FOR jj = 0,(num_bins-1) DO BEGIN
      IF Ngood[jj,ii] GT 0 THEN BEGIN
        SB[jj,ii] = Flux_good[jj,ii]/DOUBLE(Ngood[jj,ii])

        variance = square[jj,ii] - (Flux_good[jj,ii]^2)/FLOAT(Ngood[jj,ii])

; NOTE: like most other things in this code, SBsig is a
; standard deviation, not a SDOM.  Divide by root(N) to get SDOM.
        IF Ngood[jj,ii] GT 1 THEN SBsig[jj,ii] = SQRT(variance/FLOAT(Ngood[jj,ii]-1)) $
                             ELSE SBsig[jj,ii] = SQRT(variance)
      ENDIF ELSE BEGIN
; If Ngood = 0 then we'll just leave SB and SBsig set to zero.  But,
; if Ndump > 0, that means the entire annulus is screwy.
        IF Ndump[jj,ii] GT 0 THEN BEGIN
          PRINT,"ERROR in calc_growth: ellipses were selected incorrectly. ",ii
          PRINT," Rearrange ellipses in file or reduce aperture sizes."
          RETURN
        ENDIF
      ENDELSE
    ENDFOR

; Now that the binned variables are set up, start totalling fluxes
    Area = !pi*a[ii]*b[ii]*binZ[0]^2
    Fbase = SB[0,ii]*Area
    minval = 0.0

    Flux[0,ii] = MAX([Fbase,minval])

; We COULD just pass out Flux_good, but integer numbers of pixels
; make for some sloppy math.  Curve everything to a floating-point area.
    FOR jj = 1,(num_bins-1) DO BEGIN
; Area is in square pixels
      DonutArea = !pi*a[ii]*b[ii]*(binZ[jj]^2-binZ[jj-1]^2)

      Fbase = Fbase + (SB[jj,ii]*DonutArea)

      Flux[jj,ii] = MAX([Fbase,minval])
    ENDFOR

  ENDFOR

  RETURN

END
