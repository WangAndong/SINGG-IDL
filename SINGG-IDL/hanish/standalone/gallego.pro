PRO gallego

  lumbin = [40.0,40.4,40.8,41.2,41.6,42.0,42.4,42.8,43.2,43.6]
  num_bins = N_ELEMENTS(lumbin)-1
  alpha = -1.3
  theta0 = 10.0^(-3.2)
  L0 = 42.15

  sfrd = 0.0
  sfrd_nodust = 0.0
  sfrd2 = 0.0

  num_in_bin = INTARR(num_bins)
  totsfr = FLTARR(num_bins)
  totsfrd = FLTARR(num_bins)
  totsfrd_nodust = FLTARR(num_bins)

  gfile=!singgdir+"/gallego_table5"

  readcol_new,gfile,name,z,snr,fhb,fha,ebv,lha, $
              COMMENT="#",/SILENT,FORMAT='(A,F,F,X,X,X,X,F,F,X,X,F,F,X)'

  FOR ii = 0,N_ELEMENTS(name)-1 DO BEGIN

    IF lha[ii] GT 0 AND ebv[ii] GT 0 THEN BEGIN
;    IF fha[ii] GT 0 AND fhb[ii] GT 0 AND ebv[ii] GT 0 AND lha[ii] GT 0 THEN BEGIN
; Valid entry.
      lum = ALOG10(lha[ii]) + 41.583 ; convert to erg/s
;      dust = ebv[ii]*3.2
      dust = 10.0^(ebv[ii])
      temp = MIN(SQRT(lum-lumbin),binnum,/NAN)
;;        PRINT,name[ii],binnum,ALOG10(lha[ii])+41.583

      IF binnum GE 0 AND binnum LE (num_bins-1) THEN BEGIN
        SFR = lha[ii]*4.0702

        Schechter,lum,[theta0,L0,alpha],theta,dtheta
;PRINT,name[ii],lha[ii],theta,binnum,dust

        num_in_bin[binnum] = num_in_bin[binnum] + 1
        totsfr[binnum] = totsfr[binnum] + SFR
        totsfrd[binnum] = totsfrd[binnum] + theta*SFR
        totsfrd_nodust[binnum] = totsfrd_nodust[binnum] + theta*SFR/dust
      ENDIF ELSE BEGIN
        PRINT,"Missing: ",name[ii],binnum,lum
      ENDELSE
    ENDIF
  ENDFOR

  FOR jj = 0,num_bins-1 DO BEGIN
    IF num_in_bin[jj] GT 0 THEN BEGIN
      weight = (lumbin[jj+1]-lumbin[jj])/num_in_bin[jj]

      PRINT,totsfrd[jj],totsfrd[jj]/num_in_bin[jj],totsfr[jj],totsfr[jj]/num_in_bin[jj],num_in_bin[jj]

      sfrd = sfrd + (totsfrd[jj]*weight)
      sfrd_nodust = sfrd_nodust + (totsfrd_nodust[jj]*weight)

; Do the crude, averaged way.
      meanlum = (lumbin[jj+1]+lumbin[jj])/2.0
      Schechter,meanlum,[theta0,L0,alpha],theta,dtheta

      sfrd2 = sfrd2 + (totsfr[jj]*theta*weight)

    ENDIF
  ENDFOR

  PRINT,sfrd,ALOG10(sfrd),sfrd2,ALOG10(sfrd2),sfrd_nodust,ALOG10(sfrd_nodust),ALOG10(sfrd_nodust)-ALOG10(sfrd)

  RETURN
END
