PRO correction,object,niicorr,dustcorr,niierr,dusterr,MODE=mode,MONTE=monte,W6583=W6583,RMAG=rmag,KNII=knii,RUN=run,FILT=filt
; Given an object name, returns the relative sizes of the [NII]
; lines as well as the dust extinction factor.
; INPUT:
;   object          Object name, to be matched to the databases
; OUTPUT:
;   niicorr[N,M+1]  [NII]/[Halpha] ratio and Halpha ratio; element 0
;   dustcorr[N,M+1]    is the baseline
;   niierr[N,2]     systematic uncertainties in
;   dusterr[N,2]      [NII] and A(Ha) ratios (+ and -)
; OPTIONAL INPUT:
;   mode            Mode flag.
;        0: Use Helmboldt method (default)
;        1: Use Tremonti method
;        other: Use flat correction
;   monte           M = Number of randomized values to return
;   run             Override run number
;   filt[2]         Override filters
; OPTIONAL OUTPUT:
;   Rmag[N]         R-band magnitude
;   Knii[N]         [NII] K-value

  IF NOT KEYWORD_SET(mode) THEN mode = 0
  IF NOT KEYWORD_SET(monte) THEN monte = 0

; First, find the values needed for this galaxy:

  refdb = "singg_sample"
  fluxdb = "singg_flux"
  headerdb = "proc3_header"

  dbopen,refdb,0
  dbext,-1,"NAME,   VHEL,   W50,   DISTANCE,EBV", $
            refname,refvhel,refw50,refdist ,ebv
  dbclose,dummy

  goodflag = NOT KEYWORD_SET(run) OR NOT KEYWORD_SET(filt)

  dbopen,fluxdb,0
  dbext,-1,"OBJECT, FILENAME,IMTYPE,RUNID,FLUX_F_ISO,FLUX_SCALE,FILTER,GALINDEX,NUMGALS", $
            fluxobj,ffile,fluxtype,runid,flux_iso,flux_scale,fluxfilt,galindex,fnumgals
  IF goodflag THEN flind = good_flux() ELSE flind = INDGEN(N_ELEMENTS(fluxobj))
  dbclose,dummy

  fluxobj = STRTRIM(fluxobj[flind],2)
  ffile = STRTRIM(ffile[flind],2)
  fluxtype = STRTRIM(fluxtype[flind],2)
  runid = STRTRIM(runid[flind],2)
  flux_iso = flux_iso[flind]
  flux_scale = flux_scale[flind]
  fluxfilt = STRTRIM(fluxfilt[flind],2)
  galindex = galindex[flind]
  fnumgals = fnumgals[flind]

  dbopen,headerdb,0
  dbext,-1,"TARGET, TELESCOP,IMTYPE,  MAGZPT1,RUNID,FILENAME", $
            headobj,telescop,headtype,hmagzpt1,hrun,hfile
  dbclose,dummy

  refname = update_name(refname)
  fluxobj = update_name(fluxobj)
  headobj = update_name(headobj)
  hrun = 'Run'+STRTRIM(hrun,2)

  refindex = WHERE(STRTRIM(STRUPCASE(refname),2) EQ STRTRIM(STRUPCASE(object),2),count)
  IF count NE 1 THEN BEGIN
    PRINT,"ERROR in correction: wrong number of matches to reference database for object ",object,count
    RETURN
  ENDIF

  vhel = refvhel[refindex[0]]
  w50 = refw50[refindex[0]]
  dist = refdist[refindex[0]]

; Find the R-band foreground correction
  wave = 6507.46 ; R-band average filter wavelength
  unred = 1.0
  ccm_unred,wave,unred,ebv[refindex[0]] ; For R-band internal extinction only

  IF goodflag THEN BEGIN
    index = WHERE(STRUPCASE(fluxobj) EQ STRTRIM(STRUPCASE(object),2) AND $
                  fluxtype EQ 'net',count)
  ENDIF ELSE BEGIN
    contchr = STRUPCASE(STRMID(filt[0],0,1))
    IF contchr EQ '6' THEN contchr = 'C'
    index = WHERE(STRUPCASE(fluxobj) EQ STRTRIM(STRUPCASE(object),2) AND $
                  runid EQ STRTRIM(run,2) AND fluxfilt EQ STRTRIM(filt[1],2) AND $
                  STRPOS(ffile,contchr+'sub_ss.fits') GE 0 AND $
                  fluxtype EQ 'net',count)
  ENDELSE

  n_gals = MAX(fnumgals[index])
  IF count NE n_gals THEN BEGIN
; It's still possible we had two different continuum filters.  Compare.
    PRINT,'ERROR in correction: must have a unique entry in good_flux ',object,count
forprint,fluxobj[index],runid[index],fluxfilt[index],fluxtype[index]
stop
    RETURN
  ENDIF

  mingalindex = MIN(galindex[index])
  maxgalindex = MAX(galindex[index])
  IF (maxgalindex - mingalindex + 1) NE n_gals THEN BEGIN
    PRINT,"ERROR in correction: inconsistent galaxy indices ",object
    FORPRINT,runid[index],fnumgals[index]
    RETURN
  ENDIF

  delmag = FLTARR(n_gals)

  niicorr = FLTARR(n_gals,monte+1)
  dustcorr = FLTARR(n_gals,monte+1)
  niierr = FLTARR(n_gals,2)
  dusterr = FLTARR(n_gals,2)

  knii = FLTARR(n_gals)
  W6583 = FLTARR(n_gals)

  Rflux = FLTARR(n_gals)
  scale = FLTARR(n_gals)
  magzpt1 = FLTARR(n_gals)

  FOR ii = 0,n_gals-1 DO BEGIN
    IF goodflag THEN BEGIN
      Rindex = WHERE(STRUPCASE(fluxobj) EQ STRTRIM(STRUPCASE(object),2) AND $
                     fluxtype EQ 'cont' AND galindex EQ (ii+mingalindex),Rcount)
    ENDIF ELSE BEGIN
      Rindex = WHERE(STRUPCASE(fluxobj) EQ STRTRIM(STRUPCASE(object),2) AND $
                     fluxfilt EQ STRTRIM(filt[0],2) AND runid EQ STRTRIM(run,2) AND $
                     fluxtype EQ 'cont' AND galindex EQ (ii+mingalindex),Rcount)
    ENDELSE

; If count=fnumgals[index], then it's one match per galaxy, so total
; them up.  If not, there's multiple R images, so take the average.
    IF Rcount GE 0 THEN BEGIN
      Rflux[ii]=TOTAL(flux_iso[Rindex])/Rcount
      scale[ii]=TOTAL(flux_scale[Rindex])/Rcount
      delmag[ii]=2.5*ALOG10(Rflux[ii]*unred/scale[ii])
    ENDIF

    magzpt2 = FLTARR(Rcount)
    FOR jj = 0,Rcount-1 DO BEGIN
      Hindex = WHERE( $ ;;STRTRIM(STRUPCASE(headobj),2) EQ STRTRIM(STRUPCASE(object),2) AND $
                     STRTRIM(hfile,2) EQ ffile[Rindex[jj]] AND $
                     hrun EQ STRTRIM(runid[Rindex[jj]],2) AND $
                     STRTRIM(headtype,2) EQ 'cont',Hcount)

; If there's multiple header matches, it's because we had two R images
; again.  Assume magzpt is the mean.

      IF Hcount EQ 0 THEN BEGIN
        PRINT,"ERROR in correction: no header matches for object ",object
        PRINT,runid[Rindex[jj]],ffile[Rindex[jj]]
        RETURN
      ENDIF ELSE BEGIN
        magzpt2[jj] = MEAN(hmagzpt1[Hindex])
      ENDELSE
    ENDFOR
; magzpt2 SHOULD be the same for every galaxy, unless it was observed
; on multiple filters AND on multiple runs AND the number of sources
; changed between runs.
    magzpt1 = MEAN(magzpt2)
  ENDFOR
  magzpt = MEAN(magzpt1)
  abmag = magzpt - delmag - 5.0*(ALOG10(dist)+5.0)
  Rmag = abmag

  IF goodflag THEN BEGIN
    Sindex = WHERE(STRUPCASE(fluxobj) EQ STRTRIM(STRUPCASE(object),2) AND $
                   fluxtype EQ "net",Scount)
  ENDIF ELSE BEGIN
    Sindex = WHERE(STRUPCASE(fluxobj) EQ STRTRIM(STRUPCASE(object),2) AND $
                   fluxfilt EQ STRTRIM(filt[1],2) AND runid EQ STRTRIM(run,2) AND $
                   STRPOS(ffile,contchr+'sub_ss.fits') GE 0 AND $
                   fluxtype EQ "net",Scount)
  ENDELSE

; Scount should just equal the number of sources.

if Scount ne n_gals then begin
 print,'Scount > 1! ',object,Scount
forprint,ffile[sindex]+' ',runid[sindex]+' ',galindex[sindex]
 return
endif

  flfilt = STRTRIM(fluxfilt[Sindex[0]],2) ;; This isn't quite correct, if we have multiple runs with different filters.

  filtdb = "filter"
  dbopen,filtdb,0
  dbext,-1,"NAME,FILENAME,FRAT",filtname,filtfile,frat
  dbclose,dummy

; Find the F/ratio appropriate to the telescope used.

  Hindex = WHERE( $ ; STRTRIM(STRUPCASE(headobj),2) EQ STRTRIM(STRUPCASE(object),2) AND $
                 STRTRIM(hfile,2) EQ STRTRIM(ffile[Sindex[0]],2) AND $
                 hrun EQ STRTRIM(runid[Sindex[0]],2) AND $
                 STRTRIM(headtype,2) EQ 'net',Hcount)

  IF Hcount NE 1 THEN BEGIN
    PRINT,'ERROR in correction: incorrect net header matches ',Hcount
    RETURN
  ENDIF

  tscope = telescop[Hindex[0]]

  CASE STRTRIM(tscope,2) OF
    'CTIO 1.5 meter telescope': fratval = 7.5
    'CTIO 0.9 meter telescope': fratval = 13.0
    'CTIO/Michigan Curtis Schmidt': fratval = 3.5
    ELSE: BEGIN
            PRINT,"ERROR in correction: invalid telescope type: ",tscope
            RETURN
          END
  ENDCASE

  index = WHERE(STRTRIM(filtname,2) EQ flfilt,count)
  IF count EQ 0 THEN BEGIN
    PRINT,"ERROR in correction: no match in filter database ",count
    RETURN
  ENDIF
  ind2 = 0
  IF count GT 1 THEN junk = MIN(ABS(ALOG(frat[index]) - ALOG(fratval)),ind2,/NAN)
  tempfile = STRTRIM(filtfile[index[ind2]],2)

  ffilt = !singgdir+"/Filters/"+tempfile
  IF NOT FILE_TEST(ffilt) THEN BEGIN
    ffilt = !singgdir+"/Filters/"+STRMID(tempfile,11,STRLEN(tempfile)-11)
    IF NOT FILE_TEST(ffilt) THEN BEGIN
      PRINT,"ERROR in correction: missing filter file: ",tempfile
      RETURN
    ENDIF
  ENDIF

  IF mode EQ 1 THEN BEGIN
    datafile = !singgdir+"/sdss_gal_binned.dat"
    readcol_new,datafile,mcent,mmin,mmax,ngal,mmid,ebvg,ebvg_sig, $
          nii,nii_sig,FORMAT='F,F,F,I,F,F,F,F,F',COMMENT="#"

    aha = ebvg * 2.547
    aha_sig = ebvg_sig * 2.547
  ENDIF
  niiflat = 0.35 * (4.0/3.0) ; the 35% was only for the 6583 line

  FOR ii = 0,n_gals-1 DO BEGIN
    CASE mode OF
      0: BEGIN
         IF Rflux[ii] GT 0.0 THEN BEGIN
           vegamag = abmag[ii] - 0.21
           NIIval = [(-0.13*vegamag)-3.2,0.23]
           Dustval = [(-0.12*vegamag)-2.5,0.23]
; This was based on the PhD work of Jansen(2000), using 196 NFGS
; galaxies.  The above uncertainties are standard deviation, while we
; want standard deviation of mean.  So, divide by SQRT(196)=14

           sigweight = [1.d0,-1.d0]

           niicorr[ii,0] = 10.d0^(NIIval[0])
           dustcorr[ii,0] = 10.d0^(Dustval[0])
           niierr[ii,*] = sigweight*niicorr[ii,0]*(10.d0^(NIIval[1]*sigweight/14.0) - 1.d0)
           dusterr[ii,*] = sigweight*dustcorr[ii,0]*(10.d0^(Dustval[1]*sigweight/14.0) - 1.d0)

           IF monte GT 0 THEN BEGIN
             ranarr = RANDOMN(seed,LONG(2.2*monte)+1,/DOUBLE)
             ind = WHERE(ABS(ranarr LT 2.0),count)
             IF count LT 2*monte THEN BEGIN
               PRINT,"ERROR in correction: not enough random numbers to fill array",FLOAT(count)/FLOAT(monte)
               RETURN
             ENDIF
             niicorr[ii,1:monte] = 10.d0^(NIIval[0] + NIIval[1]*ranarr[ind[0:monte-1]])
             dustcorr[ii,1:monte] = 10.d0^(Dustval[0] + Dustval[1]*ranarr[ind[monte:2*monte-1]])
;             niicorr[ii,1:monte] = 10.d0^(NIIval[0] + NIIval[1]*RANDOMN(seed,monte,/DOUBLE))
;             dustcorr[ii,1:monte] = 10.d0^(Dustval[0] + Dustval[1]*RANDOMN(seed,monte,/DOUBLE))
           ENDIF
         ENDIF
       END
    1: BEGIN
       niicorr[ii,0] = SPLINE(mcent,nii,abmag[ii])
       dustcorr[ii,0] = SPLINE(mcent,aha,abmag[ii])
       END
    ELSE: BEGIN
        niicorr[ii,0] = niiflat
        dustcorr[ii,0] = 10.0^(0.50/2.5) ; 0.5mag
       END
    ENDCASE
; Now, correct for the filter transmission curve.
; niicorr is for 6583+6548; niicalc uses only the 6583 value but
; returns corrected for both.
    ratio = 1.0/1.338 ; ratio of 6583 to 6583+6548
    W6583[ii] = niicorr[ii,0]*ratio
    W_in = niicorr[ii,*]*ratio
    test = 1.0-niicalc(ffilt,vhel,w50, $
            NIIFRAC=W_in,NIIERR=(niierr[ii,*]*ratio),KNII=k,SIGMA=s)
    niicorr[ii,*] = test
    knii[ii] = k[0]
    niierr[ii,*] = s
  ENDFOR

  RETURN

END
