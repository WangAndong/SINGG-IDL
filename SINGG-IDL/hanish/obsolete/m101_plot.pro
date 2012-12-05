PRO m101_plot,isoflag=isoflag

  IF NOT KEYWORD_SET(isoflag) THEN BEGIN
    Rprof = "m101.con_c_ss_brightness.profile"
    Cprof = "m101ha_c_ss_brightness.profile"
    Sprof = "em101ha.93_c_ss_brightness.profile"
    Nprof = "NGA_M101-nd-int_ss_brightness.profile"
    Fprof = "NGA_M101-fd-int_ss_brightness.profile"
    file = "M101_brightness_bin.profile"
    hdrtxt = "# All data is centered around brightness peak"
  ENDIF ELSE BEGIN
    Rprof = "m101.con_c_ss_isophote.profile"
    Cprof = "m101ha_c_ss_isophote.profile"
    Sprof = "em101ha.93_c_ss_isophote.profile"
    Nprof = "NGA_M101-nd-int_ss_isophote.profile"
    Fprof = "NGA_M101-fd-int_ss_isophote.profile"
    file = "M101_isophote_bin.profile"
    hdrtxt = "# All data is centered around isophote center"
  ENDELSE

  readcol_new,Rprof,Ra,RF,RdF,RNgood,RNbad,RSB,RsigSB,RdFraw, $
                   FORMAT="F,F,F,I,I,F,F,F",SKIPLINE=15,/SILENT
  readcol_new,Cprof,Ca,CF,CdF,CNgood,CNbad,CSB,CsigSB,CdFraw, $
                   FORMAT="F,F,F,I,I,F,F,F",SKIPLINE=15,/SILENT
  readcol_new,Sprof,Sa,SF,SdF,SNgood,SNbad,SSB,SsigSB,SdFraw, $
                   FORMAT="F,F,F,I,I,F,F,F",SKIPLINE=15,/SILENT
  readcol_new,Nprof,Na,NF,NdF,NNgood,NNbad,NSB,NsigSB,NdFraw, $
                   FORMAT="F,F,F,I,I,F,F,F",SKIPLINE=15,/SILENT
  readcol_new,Fprof,Fa,FF,FdF,FNgood,FNbad,FSB,FsigSB,FdFraw, $
                   FORMAT="F,F,F,I,I,F,F,F",SKIPLINE=15,/SILENT

; scale are as_pix
  RCSxscale = 0.0005637854 * 3600.0
  UVxscale = 1.5 

;  Rexptime = 954.4761
;  CSexptime = 900.0
; No need to correct for EXPTIME, the _ss routine did that already
  RCSyscale = 1.0/RCSxscale^2
  UVyscale = 1.0/UVxscale^2

; Now, plot.
  setplotcolors
  DEVICE, RETAIN=2, DECOMPOSED=0
  WINDOW,XSIZE=800,YSIZE=800
  !P.MULTI=[0,0,1,0,0]

  dummy = FLTARR(2)

  PLOT,dummy,dummy, $
        COLOR=1,XTITLE="Radius, arcsec",YTITLE="Surface brightness, DN/arcsec^2", $
        XRANGE=[0,1000],YRANGE=[-4,-1]

  OPLOT,(Ra*RCSxscale),ALOG10(RSB*RCSyscale),COLOR=1
  OPLOT,(Ca*RCSxscale),ALOG10(CSB*RCSyscale),COLOR=!red
  OPLOT,(Sa*RCSxscale),ALOG10(SSB*RCSyscale),COLOR=!blue
  OPLOT,(Na*UVxscale),ALOG10(NSB*UVyscale),COLOR=!dgreen
  OPLOT,(Fa*UVxscale),ALOG10(FSB*UVyscale),COLOR=!orange

; Bin the data!
  binsize = 20.0 ; arcsec
  num_bins = LONG(3000.0/binsize + 1)

  xbin = FINDGEN(num_bins) * binsize

  Rbin = FLTARR(num_bins)
  Cbin = FLTARR(num_bins)
  Sbin = FLTARR(num_bins)
  Nbin = FLTARR(num_bins)
  Fbin = FLTARR(num_bins)

  Rtot = FLTARR(num_bins)
  Ctot = FLTARR(num_bins)
  Stot = FLTARR(num_bins)
  Ntot = FLTARR(num_bins)
  Ftot = FLTARR(num_bins)

  FOR ii = 0, N_ELEMENTS(RSB) - 1 DO BEGIN
; All five have the same number of bins, actually
    Rbinnum = LONG(Ra[ii]*RCSxscale/binsize)
    Rbin[Rbinnum] = Rbin[Rbinnum] + RSB[ii]
    Rtot[Rbinnum] = Rtot[Rbinnum] + 1

    Cbinnum = LONG(Ca[ii]*RCSxscale/binsize)
    Cbin[cbinnum] = Cbin[Cbinnum] + CSB[ii]
    Ctot[Cbinnum] = Ctot[Cbinnum] + 1

    Sbinnum = LONG(Sa[ii]*RCSxscale/binsize)
    Sbin[Sbinnum] = Sbin[Sbinnum] + SSB[ii]
    Stot[Sbinnum] = Stot[Sbinnum] + 1

    Nbinnum = LONG(Na[ii]*UVxscale/binsize)
    Nbin[Nbinnum] = Nbin[Nbinnum] + NSB[ii]
    Ntot[Nbinnum] = Ntot[Nbinnum] + 1

    Fbinnum = LONG(Fa[ii]*UVxscale/binsize)
    Fbin[Fbinnum] = Fbin[Fbinnum] + FSB[ii]
    Ftot[Fbinnum] = Ftot[Fbinnum] + 1
  ENDFOR

  Rbin = Rbin/Rtot * RCSyscale
  Cbin = Cbin/Ctot * RCSyscale
  Sbin = Sbin/Stot * RCSyscale
  Nbin = Nbin/Ntot * UVyscale
  Fbin = Fbin/Ftot * UVyscale

; Now, overplot the bin information
  OPLOT,xbin,ALOG10(Rbin),PSYM=SYM(1),SYMSIZE=2.0,COLOR=1
  OPLOT,xbin,ALOG10(Cbin),PSYM=SYM(1),SYMSIZE=2.0,COLOR=!red
  OPLOT,xbin,ALOG10(Sbin),PSYM=SYM(1),SYMSIZE=2.0,COLOR=!blue
  OPLOT,xbin,ALOG10(Nbin),PSYM=SYM(1),SYMSIZE=2.0,COLOR=!dgreen
  OPLOT,xbin,ALOG10(Fbin),PSYM=SYM(1),SYMSIZE=2.0,COLOR=!orange

  file = "M101_brightness_bin.profile"
  OPENW,unit,file,/GET_LUN
  PRINTF,unit,hdrtxt
  PRINTF,unit,"# "
  PRINTF,unit,"# a  SB(R)  SB(narrow)  SB(subtracted)  SB(NUV)  SB(FUV)

  FOR ii = 0,N_ELEMENTS(xbin)-1 DO BEGIN
    PRINTF,unit,xbin[ii],Rbin[ii],Cbin[ii],Sbin[ii],Nbin[ii],Fbin[ii]
  ENDFOR

  CLOSE,/ALL
  FREE_LUN,unit

END
