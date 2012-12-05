PRO perez
; Converts the luminosities of Perez-Gonzalez(2005) to SFRDs

z = [0.1,0.3,0.5,0.7,0.9,1.2,1.6,2.0,2.4]
logL12 = [9.61,9.81,10.05,10.17,10.52,10.73,11.04,11.54,11.86]
dlogL12 = [0.14,0.05,0.06,0.04,0.11,0.05,0.10,0.25,0.25]

logrho12 = [7.38,7.68,7.98,8.08,8.17,8.28,8.24,8.17,8.29]
dlogrho12 = [0.06,0.02,0.03,0.02,0.23,0.03,0.04,0.12,0.11]

logLtir = ALOG10(0.89) + 1.094*logL12
dlogLtir = SQRT((1.094*dlogL12)^2 + (0.33/0.89/ALOG(10.0))^2)
logsfr = ALOG10(1.71E-10) + logLtir
dlogsfr = dlogLtir

logrhotir = ALOG10(0.89) + 1.094*logrho12
dlogrhotir = SQRT((1.094*dlogrho12)^2 + (0.33/0.89/ALOG(10.0))^2)
logsfrd = ALOG10(1.71E-10) + logrhotir
dlogsfrd = dlogrhotir

forprint,z,logsfr,logsfrd,dlogsfrd

; Then, figures out the LIRG/ULIRG distribution
  theta = 10^(-2.31)
  refL = 9.61
  alpha = -1.23

  Lmin = refL-5.0
  Lmax = refL+5.0
  Lstep = 0.01
  num_bin = ROUND((Lmax-Lmin)/Lstep)+1
  binL = FINDGEN(num_bin)*Lstep + Lmin
  binSFR = 1.71E-10 * 0.89 * 10.0^(1.094*binL)

  totL = DBLARR(num_bin)
  th = DBLARR(num_bin)

  FOR ii = 1,num_bin-2 DO BEGIN
    logL = binL[ii] + Lstep/2.0
    Lratio = 10.0^DOUBLE(logL - refL)
    th[ii] = theta * Lratio^alpha / EXP(Lratio)

    totL[ii] = totL[ii-1] + Lstep*th[ii]*(ALOG(10)*Lratio)*(10.d0^logL)
;    PRINT,binL[ii],totL,ALOG10(totL)
  ENDFOR
;  FORPRINT,binL,binSFR,ALOG10(totL),th,ALOG10(totL),(totL/totL[num_bin-2])

  PRINT,TOTAL(th)
  indLIRG = WHERE(binSFR GT 10.0)
  PRINT,TOTAL(th[indLIRG])
  indULIRG = WHERE(binSFR GT 100.0)
  PRINT,TOTAL(th[indULIRG])
  indHyLIRG = WHERE(binSFR GT 1700.0)
  PRINT,TOTAL(th[indHyLIRG])
  
  hydist = [(12700.0/70.0),73.0,25.0]
  hivol = 4.0/3.0 * !pi * hydist^3.0
  forprint,hivol,hivol*TOTAL(th[indLIRG]),hivol*TOTAL(th[indULIRG])

return
end
