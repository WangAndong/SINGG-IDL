PRO hipass_galex, panstarrs=panstarrs
  ;
  ; match HIPASS HICAT & NHICAT catalogs to galex gr6 tile database
  ;
  ; panstarrs -> if set then the search is done over the PanSTARRS1
  ;              footprint
  ;
  ; G. Meurer (ICRAR/UWA) 11/2011
  logstr    = 'HIPASS_GALEX: '
  flog      = 'hipass_galex.log'
  ;flog      = 'hipass_galex_highb.log'
  filo      = 'hipass_galex.out'
  ;filo      = 'hipass_galex_highb.out'
  wd        = '~/ASKAP_MeerKAT/'
  dbh1      = 'hicat_feb04'
  dbh2      = 'nhicat'
  dbg       = 'galexgr6_seibert_coadd'
  h0        = 72.0                       ; Hubble constant in km/s/Mpc
  distmin   = 1.0                        ; minimum distannce in Mpc from Hubble flow
  blim      = 0.0
  ;blim      = 30.0
  rbeam     = 15.0*0.5                   ; HIPASS beam radius in arcmin
  srad1     = (0.5*1.1*60 - rbeam)       ; GALEX tile matching radius
  hdro      = '# '+string(indgen(23)+1,format='(i2)')+' '+[$
              'HIPASS: name of HIPASS target', $
              'RA: Right ascension (J2000)', 'Dec: Declination (J2000)', $
              'Glat: galactic latitude ', 'Glong: galactic longitude', $
              'E(B-V): foreground dust reddening', 'V_hel: HI heliocentric velocity [km/s]', $
              'W50: HI line width at 50% max [km/s]', 'W20: HI line width at 20% max [km/s]', $
              'Sint: integrated HI line flux [Jy km/s]', 'Dist: distance (flow model) [Mpc]', $
              'lg_MHI: HI mass in the log [Msun]', 'nm: number of GALEX matches ', $
              'eglxb: entry GALEX tile database (deepest match)', $
              'tileb: GALEX tile name  (deepest match)', $
              'sepb: source separation form GALEX tile centre [arcmin] (deepest match)', $
              'tfuvb: FUV exposure time [s] (deepest match)', 'tnuvb: NUV exposure time [s] (deepest match)', $
              'eglxb: entry GALEX tile database (nearest match)', 'tilea: GALEX tile name  (nearest match)', $
              'sepa: source separation form GALEX tile centre [arcmin] (nearest match)', $
              'tfuva: FUV exposure time [s] (nearest match)', 'tnuva: NUV exposure time [s] (nearest match)']
  hdro     = [hdro, '# ', '#HIPASS+       RA        Dec       Glat      Glong  E(B-V) V_hel   W50   W20    '+$
              'Sint    Dist lg_MHI nm eglxb  tileb                            sepb    tfuvb    tnuvb'+$
              ' eglxa    tilea                            sepa    tfuva    tnuva']
  nhd      = n_elements(hdro)
  filj1    = 'hipass_galex_ebvdist.jpg'
  filj2    = 'hipass_galex_tfuvdist.jpg'
  filj3    = 'hipass_galex_tnuvdist.jpg'
  fmto     = '(a11,f10.4,f10.4,f10.4,f10.4,f7.3,i6,i6,i6,f9.2,f7.1,f7.2,i3,'+$
              'i6,2x,a31,f6.1,f9.1,f9.1,i6,2x,a31,f6.1,f9.1,f9.1)'
  ;
  charsize = 1.5
  symsize  = 1.1
  aspect   = 1.5
  erange   = [0.0, 1.0]
  ltrange  = [0.0, 4.6]
  debv     = 0.02
  dlt      = 0.05
  wxsize   = 1000
  wysize   = fix(float(wxsize/aspect))
  charsize = charsize*wxsize/800.0
  symsize  = symsize*wxsize/800.0
  etitle   = '!3 E(B-V)'
  ettitle  = '!3 Foreground Galactic dust reddening distribution'
  ntitle   = '!3 Number'
  lntitle  = '!3 log(Number+1)'
  lttitle  = '!3 log(t!dexposure!n + 1)'
  ltftitle = '!3 FUV exposure time distribution (deepest)'
  ltntitle = '!3 NUV exposure time distribution (deepest)'
  thick    = 2
  ;
  ; go to  working dir and open log file 
  cd, wd, current=cwd
  openw, ll, flog, /get_lun
  plog, ll, logstr, 'started in directory: '+cwd+' now working in directory: '+wd
  ;
  ; setup for doing PanSTARRS footprint search
  IF keyword_set(panstarrs) THEN BEGIN 
     plog, ll, logstr, 'will search over the PS1 footprint'
     sstr   = 'dec > -30.0, '
  ENDIF ELSE BEGIN 
     plog, ll, logstr, 'will search entire sky'
     sstr   = ''
  ENDELSE 
  ;
  ; open HICAT select galaxies
  plog, ll, logstr, 'opening HIPASS HICAT database: '+dbh1
  dbopen, dbh1
  list      = dbfind(sstr+'glat > '+numstr(blim))
  list2     = dbfind(sstr+'glat < '+numstr(-1.0*blim))
  nl1       = n_elements(list)
  nl2       = n_elements(list2)
  list      = [list, list2]
  nhi       = nl1+nl2
  plog, ll, logstr, 'will extract '+numstr(nhi)+' entries (= '+numstr(nl1)+' + '+numstr(nl2)+')'
  ;
  ; sort entries
  plog, ll, logstr, 'sorting entries'
  ss       = sort(list)
  uu       = uniq(list[ss])
  list     = list[ss[uu]]
  ;
  ; extract relevant quantities
  plog, ll, logstr, 'getting HI quantities'
  dbext, list, 'hipass_name,ra,dec,vel_50max,width_50max,width_20max,sint,glong,glat,ebv,distance,logmhi',$
         hname,ra,dec,vhel,w50,w20,shi,glong,glat,ebv,dist,lmhi
  dbclose
  entry       = list
  ;
  ; open NHICAT select galaxies
  plog, ll, logstr, 'opening HIPASS NHICAT database: '+dbh2
  dbopen, dbh2
  list3     = dbfind(sstr+'glat > '+numstr(blim))
  list4     = dbfind(sstr+'glat < '+numstr(-1.0*blim))
  nl3       = n_elements(list3)
  nl4       = n_elements(list4)
  list3     = [list3, list4]
  nhi2      = nl3+nl4
  plog, ll, logstr, 'will extract '+numstr(nhi2)+' entries (= '+numstr(nl3)+' + '+numstr(nl4)+')'
  ;
  ; sort entries
  plog, ll, logstr, 'sorting entries'
  ss       = sort(list3)
  uu       = uniq(list3[ss])
  list3    = list3[ss[uu]]
  ;
  ; extract relevant quantities
  plog, ll, logstr, 'getting HI quantities'
  dbext, list3, 'hipass_name,ra,dec,vel_50max,vel_lg,width_50max,width_20max,sint,glong,glat,ebv',$
         hname2,ra2,dec2,vhel2,vlg2,w502,w202,shi2,glong2,glat2,ebv2
  dbclose
  entry2      = list3
  ;
  ; work out rather crude distance...
  plog, ll, logstr, 'working out crude distances to NHICAT sources'
  dist2       = ((vlg2/h0) > distmin)
  lmhi2       = alog10(2.36e5*dist2^2*shi2)
  ;
  ; append arrays
  entry       = [entry, 10000+entry2]
  hname       = [hname,hname2]
  ra          = [ra,ra2]
  dec         = [dec,dec2]
  vhel        = [vhel,vhel2]
  w50         = [w50,w502]
  w20         = [w20,w202]
  shi         = [shi,shi2]
  glong       = [glong,glong2]
  glat        = [glat,glat2]
  ebv         = [ebv,ebv2]
  dist        = [dist,dist2]
  lmhi        = [lmhi,lmhi2]
  nhi         = nhi+nhi2
  ;
  ; Arrays to store things
  entgalexa   = make_array(nhi, /long, value=-1l)
  entgalexb   = make_array(nhi, /long, value=-1l)
  obsnameglxa = make_array(nhi, /string, value='')
  obsnameglxb = make_array(nhi, /string, value='')
  sepglxa     = make_array(nhi, /float, value=999.9)
  sepglxb     = make_array(nhi, /float, value=999.9)
  tfuvglxa    = make_array(nhi, /float, value=-1.0)
  tfuvglxb    = make_array(nhi, /float, value=-1.0)
  tnuvglxa    = make_array(nhi, /float, value=-1.0)
  tnuvglxb    = make_array(nhi, /float, value=-1.0)
  nmatchglx   = make_array(nhi, /int, value=0)
  umatch      = make_array(nhi, /byte, value=0b)
  ;
  ; Open Galex (GR5?) DB, get the direct entries
  plog, ll, logstr, 'opening GALEX tile database: "'+dbg+'" and cross matching'
  dbopen, dbg
  ;list        = dbfind('ow = 0')
  dbext, -1, 'entry', list
  ;
  ; loop through the master source list
  FOR ii = 0, nhi-1 DO BEGIN 
     ; 
     ; For each singg entry find
     ; - number of matching galex entries
     ; - closest galex entry (entgala)
     ; - longest FUV exposure time galex entry (entgalb)
     ; - separations for these two cases (sepglx{a,b})
     list2  = dbcircled(ra[ii], dec[ii], srad1, sep, list)
     nl2    = n_elements(list2)
     IF list2[0] EQ -1 THEN nl2 = 0
     plog, ll, logstr, 'Number of matches to '+hname[ii]+' = '+numstr(nl2)
     IF list2[0] GT -1 THEN BEGIN 
        nn              = n_elements(list2)
        nmatchglx[ii]   = nn
        dbext, list2, 'entry,obsname,texpfuv,texpnuv', ent, obsname, texpfuv, texpnuv
        jj              = sort(sep)
        entgalexa[ii]   = ent[jj[0]]
        obsnameglxa[ii] = obsname[jj[0]]
        sepglxa[ii]     = sep[jj[0]]
        tfuvglxa[ii]    = texpfuv[jj[0]]
        tnuvglxa[ii]    = texpnuv[jj[0]]
        jj              = reverse(sort(texpfuv))
        entgalexb[ii]   = ent[jj[0]]
        obsnameglxb[ii] = obsname[jj[0]]
        sepglxb[ii]     = sep[jj[0]]
        tfuvglxb[ii]    = texpfuv[jj[0]]
        tnuvglxb[ii]    = texpnuv[jj[0]]
     ENDIF 
  ENDFOR 
  ;
  ; get some stats for the matched galaxies
  ; * number of matches
  ; * number of unique galex tiles
  ; * number NOT matching galex tiles
  mtch     = where(entgalexa GT -1, nmtch)
  eglxall  = [entgalexa[mtch], entgalexb[mtch]]
  tfuvall  = [tfuvglxa, tfuvglxb]
  tnuvall  = [tnuvglxa, tnuvglxb]
  ss       = sort(eglxall)
  uu       = uniq(eglxall[ss])
  jtile    = ss[uu]
  eglb     = entgalexb[mtch]
  s0       = sort(eglb)
  u0       = uniq(eglb[s0])
  nudeep   = n_elements(u0)
  eglxall  = temporary(eglxall[jtile])
  tfuvall  = temporary(tfuvall[jtile])
  tnuvall  = temporary(tnuvall[jtile])
  nglxall  = n_elements(eglxall)
  plog, ll, logstr, 'number of HIPASS targets in GALEX tiles: '+numstr(nmtch)
  plog, ll, logstr, 'Number of HIPASS targets _outside_ GALEX tiles: '+numstr(nhi-nmtch)
  plog, ll, logstr, 'number of unique matching (nearest+deepest) GALEX tiles: '+numstr(nglxall)
  plog, ll, logstr, 'number of unique matching (deepest) GALEX tiles: '+numstr(nudeep)
  ;
  ; make array giving RA sort order for matches
  ss       = sort(ra[mtch])
  pp       = mtch[ss]
  ;
  ; store stuff in output file
  plog, ll, logstr, 'opening and writing output file: '+filo
  openw, lu, filo, /get_lun
  FOR ii = 0, nhd-1 DO BEGIN 
     printf, -1, hdro[ii]
     printf, lu, hdro[ii]
  ENDFOR 
  FOR ii = 0, nmtch-1 DO BEGIN 
     jj    = pp[ii]
     printf, -1, ljust(hname[jj],11),ra[jj],dec[jj],glong[jj],glat[jj],ebv[jj],$
             vhel[jj],w50[jj],w20[jj],shi[jj],dist[jj],lmhi[jj],nmatchglx[jj],$
             entgalexb[jj],obsnameglxb[jj],sepglxb[jj],tfuvglxb[jj],tnuvglxb[jj],format=fmto
     printf, lu, ljust(hname[jj],11),ra[jj],dec[jj],glong[jj],glat[jj],ebv[jj],$
             vhel[jj],w50[jj],w20[jj],shi[jj],dist[jj],lmhi[jj],$
             nmatchglx[jj],entgalexb[jj],ljust(obsnameglxb[jj],31),sepglxb[jj],$
             tfuvglxb[jj],tnuvglxb[jj],$
             entgalexa[jj],obsnameglxa[jj],sepglxa[jj],tfuvglxa[jj],tnuvglxa[jj],format=fmto
  ENDFOR 
  free_lun,lu
  ;
  ; make E(B-V) histograms
  keywait, 'type anything to make next plot'
  plog, ll, logstr, 'Creating E(B-V) histogram plot'
  nebvhist  = (erange[1] - erange[0])/debv
  xebv      = erange[0] + debv*(0.5 + findgen(nebvhist))
  nebv1     = histogram(ebv, binsize=debv, min=erange[0], max=erange[1])         ; all hipass targets
  nebv2     = histogram(ebv[mtch], binsize=debv, min=erange[0], max=erange[1])   ; only those with galex matches
  xebv      = [erange[0]-0.5*debv, xebv]
  nebv1     = [0.0, nebv1]
  nebv2     = [0.0, nebv2]
  lnebv1    = alog10(1.0+nebv1)
  lnebv2    = alog10(1.0+nebv2)
  nrange    = [0.0, 1.1*max(nebv1)]
  lnrange   = [0.0, 1.1*max(lnebv2)]
  window, 0, xsize=wxsize, ysize=wysize
  setplotcolors
  !p.noerase = 1
  plot, xebv, lnebv1, xrange=erange, yrange=lnrange, xstyle=1, ystyle=1, psym=10, $
        xtitle=etitle, ytitle=lntitle, title=ettitle, charsize=charsize, $
        thick=thick, xthick=thick, ythick=thick, charthick=thick, /nodata
  plotcolorfill, xebv, lnebv1, color=!lgray, /noerase
  oplot, xebv, lnebv1, psym=10, thick=thick, color=!black
  plotcolorfill, xebv, lnebv2, color=!lpurple, /noerase
  oplot, xebv, lnebv2, psym=10, thick=thick, color=!blue
  plot, xebv, lnebv1, xrange=erange, yrange=lnrange, xstyle=1, ystyle=1, psym=10, $
        xtitle=etitle, ytitle=lntitle, charsize=charsize, $
        thick=thick, xthick=thick, ythick=thick, charthick=thick, /nodata
  plog, ll, logstr, 'writing JPEG image: '+filj1
  snap_jpg, filj1 
  ;
  ; make log(texp_FUV) histogram
  keywait, 'type anything to make next plot'
  plog, ll, logstr, 'Creating FUV exposure time histogram plot'
  ltexpf    = alog10(tfuvglxb[mtch] + 1.0)
  ltexpn    = alog10(tnuvglxb[mtch] + 1.0)
  nlthist   = (ltrange[1] - ltrange[0])/dlt
  xlt       = ltrange[0] + dlt*(0.5 + findgen(nlthist))
  nltf2     = histogram(ltexpf, binsize=dlt, min=ltrange[0], max=ltrange[1])
  nltn2     = histogram(ltexpn, binsize=dlt, min=ltrange[0], max=ltrange[1])
  xlt       = [ltrange[0]-0.5*dlt, xlt]
  nltf2     = [0.0, nltf2]
  nltn2     = [0.0, nltn2]
  lnltf2    = alog10(1.0+nltf2)
  lnltn2    = alog10(1.0+nltn2)
  nrange    = [0.0, 1.1*max([nltf2,nltn2])]
  lnrange   = [0.0, 1.1*max([lnltf2,lnltn2])]
  window, 0, xsize=wxsize, ysize=wysize
  !p.noerase = 1
  plot, xlt, lnltf2, xrange=ltrange, yrange=lnrange, xstyle=1, ystyle=1, psym=10, $
        xtitle=lttitle, ytitle=lntitle, title=ltftitle, charsize=charsize, $
        thick=thick, xthick=thick, ythick=thick, charthick=thick, /nodata
  plotcolorfill, xlt, lnltf2, color=!lpurple, /noerase
  oplot, xlt, lnltf2, psym=10, thick=thick, color=!blue
  plot, xlt, lnltf2, xrange=ltrange, yrange=lnrange, xstyle=1, ystyle=1, psym=10, $
        xtitle=lttitle, ytitle=lntitle, title=ltftitle, charsize=charsize, $
        thick=thick, xthick=thick, ythick=thick, charthick=thick, /nodata
  plog, ll, logstr, 'writing JPEG image: '+filj2
  snap_jpg, filj2
  ;
  ; Do log(texp_NUV) histogram
  keywait, 'type anything to make next plot'
  window, 0, xsize=wxsize, ysize=wysize
  !p.noerase = 1
  plot, xlt, lnltn2, xrange=ltrange, yrange=lnrange, xstyle=1, ystyle=1, psym=10, $
        xtitle=lttitle, ytitle=lntitle, title=ltntitle, charsize=charsize, $
        thick=thick, xthick=thick, ythick=thick, charthick=thick, /nodata
  plotcolorfill, xlt, lnltn2, color=!lpurple, /noerase
  oplot, xlt, lnltn2, psym=10, thick=thick, color=!blue
  plot, xlt, lnltn2, xrange=ltrange, yrange=lnrange, xstyle=1, ystyle=1, psym=10, $
        xtitle=lttitle, ytitle=lntitle, title=ltntitle, charsize=charsize, $
        thick=thick, xthick=thick, ythick=thick, charthick=thick, /nodata
  plog, ll, logstr, 'writing JPEG image: '+filj3
  snap_jpg, filj3
  ;
  ; return to start directory, close log file
  plog, ll, logstr, 'returning to start directory: '+cwd
  cd, cwd
  plog, ll, logstr, 'closing log file and ending. '
  free_lun, ll
END 
