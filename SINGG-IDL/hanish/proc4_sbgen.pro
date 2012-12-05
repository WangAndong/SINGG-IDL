PRO proc4_sbgen,sb_box,zcheck,zmax,binsize,buffer,sz, $
                n_ellipses,mask,Rimg,Nimg,Rsky,Nsky, $
                bin_num,bin_Rtot,bin_Ntot
; Called by our main "proc4" setup program to assemble SB profiles.

  num_bins = LONG(MAX(zmax)/binsize)
  bin_z = FINDGEN(num_bins)*binsize
; If bin_z is changed, proc4 and proc4_sbplot will also need fixing.
  IF n_ellipses GT 0 THEN BEGIN
    bin_Rtot = DBLARR(n_ellipses,num_bins)
    bin_Ntot = DBLARR(n_ellipses,num_bins)
    bin_num = LONARR(n_ellipses,num_bins)
  ENDIF ELSE BEGIN
    bin_Rtot = DBLARR(1,num_bins) ; Empty
    bin_Ntot = DBLARR(1,num_bins) ; Empty
    bin_num = LONARR(1,num_bins)
  ENDELSE

  FOR ii = 0,n_ellipses-1 DO BEGIN
    FOR xx = buffer/sb_box,(sz[1]-buffer-1)/sb_box - 1 DO BEGIN
      cenX = (xx*sb_box + (sb_box-1)/2)
      FOR yy = buffer/sb_box,(sz[2]-buffer-1)/sb_box - 1 DO BEGIN
        cenY = (yy*sb_box + (sb_box-1)/2)

        zbin = zcheck[*,cenX,cenY]
        bindex = LONG((zbin[ii]-bin_z[0])/binsize)
        IF zbin[ii] LT zmax[ii] AND NOT mask[cenX,cenY] THEN BEGIN
          delnum = LONG(sb_box^2)
          delR = (Rimg[cenX,cenY]-Rsky)*delnum
          delN = (Nimg[cenX,cenY]-Nsky)*delnum

          valindex = WHERE(zbin-zmax LT 0.0,num_valid)
          IF num_valid GT 1 THEN BEGIN
; In overlap areas, first priority goes to the later number (which is
; assumed to be the smaller galaxy) UNLESS the later galaxy would
; overlap the center of the former.
            valindex2 = WHERE(zbin LT 1.0,num_valid2)
            IF (num_valid2 EQ 0 AND ii EQ valindex[num_valid-1]) OR $
               (num_valid2 EQ 1 AND ii EQ valindex2[0]) OR $
               (num_valid2 GT 1 AND ii EQ MAX(valindex2)) THEN BEGIN
              FOR ll = 0,num_valid-1 DO BEGIN
                IF zbin[valindex[ll]] LT bin_z[0] THEN val = 0 ELSE $
                    junk = MIN(SQRT(zbin[valindex[ll]]-bin_z),val,/NAN)
                IF bin_num[valindex[ll],val] GT 0 AND valindex[ll] NE ii THEN BEGIN
                  delR = delR - (bin_Rtot[valindex[ll],val]/bin_num[valindex[ll],val])*delnum
                  delN = delN - (bin_Ntot[valindex[ll],val]/bin_num[valindex[ll],val])*delnum
                ENDIF
              ENDFOR
            ENDIF ELSE BEGIN
              delnum = LONG(0)
              delR = 0.d0
              delN = 0.d0
            ENDELSE
          ENDIF
          bin_num[ii,bindex] = bin_num[ii,bindex] + delnum
          bin_Rtot[ii,bindex] = bin_Rtot[ii,bindex] + delR
          bin_Ntot[ii,bindex] = bin_Ntot[ii,bindex] + delN
        ENDIF
      ENDFOR
    ENDFOR
  ENDFOR
  
; Make sure after returning that you call proc4_sbplot if you actually
; want it to plot something.
  RETURN
END
