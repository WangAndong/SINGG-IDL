PRO mk_master_sample, ingest=ingest
   ;
   ; Make a database containing information on which SINGG sources 
   ; are in various samples.
   ;
   ; ingest -> if set then the data base is ingested and loaded on to
   ;           the ZDBASE directory as the new official version.
   ;
   ; G. Meurer (JHU) 05/2009 
   ; G. Meurer (ICRAR/UWA) 08/2011: 
   ;    indicate whether targets have been selected for 
   ;    SIGRID (Nicholls), MHONGOOSE (de Blok), LVHIS (Koribalski), 
   ;    S4G (Sheth et al 2010) also add /ingest switch.
   ; G. Meurer (ICRAR/UWA) 10/2011:
   ;    Add AKARI matches and fluxes.
   ;    Redo IRAS PSC matching
   ;
   ; start up logging
   logfile   = 'mk_master_sample.log'
   logstr    = 'MK_MASTER_SAMPLE: '
   openw, llog, logfile, /get_lun
   plog, llog, logstr, 'starting '
   ; setup stuff
   shome     = getenv('SHOME')
   zdbase    = getenv('ZDBASE')
   if strmid(shome,strlen(shome)-1) NE '/' then shome = shome+'/'
   wd        = shome+'WIYN/Sample/'               ; directory to start work in
   ;updatestr = 'Date: 2010 Apr 7.'               ; **** make sure to update for each new ingest!!!
   ;filk      = 'sample_kin_stat1004.dat'
   ;updatestr = 'Date: 2010 Oct 4'                 ; **** make sure to update for each new ingest!!!
   updatestr = 'Date: 2011 Nov 04'                 ; **** make sure to update for each new ingest!!!
   filk      = 'sample_kin_stat1010.dat'
   sdir      = shome+'Sample/'
   ;fsigrid   = sdir+'sigrid_singg.txt'
   fsigrid   = sdir+'SIGRID_f8shortA.dat'
   flvhis    = sdir+'lvhis.dat'
   fmgoose   = sdir+'gmd.txt'
   fs4g      = sdir+'s4g_table1.dat'
   rmatch    = 4.0  ; arcmin
   minsradak = 1.0  ; arcmin
   minsradir = 1.0  ; arcmin
   lfflag    = -99.99            ; invalid log fluxes marked with this
   notinsamp = -99l
   ; databases
   ;dbg       = 'galex_gr5all'
   dbs       = 'singg_sample'        ; SINGG sample database
   dbu       = 'sungg_sample'        ; SUNGG sample database
   dbh       = 'singg_derived'       ; SINGG measurement database
   dbf       = 'sungg_derived'       ; SUNGG measurement database
   dboc      = 'hopcat'              ; HIPASS optical catalog (HOPCAT)
   dbhi      = 'hicat_feb04'         ; HI catalog (HIPASS HICAT)
   dbg       = 'galex_all201105'     ; Galex tile database
   dbai      = 'akari_irc'           ; AKARI IRC (mid-IR) database
   dbaf      = 'akari_fis'           ; AKARI FIS (far-IR) database
   dbout     = 'supersingg_test'     ; output database
   dbir      = 'iras_psc'
   ;
   ; set up things for database
   title    = 'Super SINGG master database. '+updatestr
   item     = ['sname', 'hname', 'ra', 'dec', 'glong', 'glat', 'possrc', 'ebv', 'dist', 'lmhi',$
               'obs', 'lf09akari', 'lf90akari', 'lf90ha', 'lf90uv', $
               'sradak', 'nakirc', 'nakfis', 'niras', 'densakirc', 'densakfis', 'sepakirc', 'sepakfis', 'sepiras', $
               'haflag', 'uvflag', 'aisflaga', 'aisflagb', 'seloptrc', 'statoptrc', 's4g', $
               'idsigrid', 'obssig', 'lvhis', 'mhongoose', 'entsamp', 'entuvsamp', 'entoptphot', $
               'entfuvphot', 'entnuvphot', 'entiras', 'entgalexa', 'entgalexb', 'entakirc', 'entakfis']
   tout     = ['C', 'C', 'R', 'R', 'R', 'R', 'I', 'R', 'R', 'R', 'C', 'R', 'R', 'R', 'R', $
               'R', 'I', 'I', 'I', 'I', 'I', 'R', 'R', 'R', 'I', 'I', 'B', 'B', 'I', 'C', 'I', $
               'I', 'B', 'B', 'B', 'I', 'I', 'I', 'I', 'I', 'I', 'I', 'I', 'I', 'I']
   tlen     = [0, 0, 8, 8, 8, 8, 2, 4, 4, 4, 0, 4, 4, 4, 4, 4, 2, 2, 2, 4, 4, $
               4, 4, 4, 2, 2, 1, 1, 2, 0, 2, 2, 1, 1, 1, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4]
   descript = ['SINGG name', 'HIPASS name', 'RA (J2000) [deg]', 'Dec (J2000) [deg]', $
               'Galactic longitude [deg]', 'Galactic latitude [deg]', $
               'Position source [0-HIPASS,1-optical,2-UV,3-HOPCAT]', $
               'Galactic foreground E(B-V) (Schlegel et al) [mag]', $
               'Distance [Mpc]', 'Log(M_HI [Msun])', 'Observed for SINGG? [Y/N]', $
               'AKARI IRC 9 micron flux: log(f_09 [Jy])', 'AKARI FIS 90 micron flux: log(f_90 [Jy])', $
               'Estimated AKARI log(f_90 [Jy]) from Halpha', 'Estimated AKARI log(f_90 [Jy]) from GALEX', $
               'Search radius for AKARI matches [arcsec]', 'Number of AKARI IRC matches', $
               'Number of AKARI FIS matches', 'Number of IRAS PSC matches', $
               'Density of AKARI IRC matches', 'Density of AKARI FIS matches', $
               'Separation to AKARI IRC match [arcsec]', 'Separation to AKARI FIS match [arcsec]', $
               'Separation to IRAS PSC match [arcsec]', $
               'Halpha status flag [-1:1]', 'GALEX status flag [-1:3]', $
               'AIS flag, closest GALEX entry', 'AIS flag, deepest GALEX entry', $
               'Optical RC selection code [0:4]', 'Optical RC observation status [str]', $
               'Record number in S4G sample', 'SIGRID sample ID', 'Observed with WiFeS for SIGRID?', $
               'In LVHIS sample?', 'In MHONGOOSE sample?', $
               'Entry in SINGG_SAMPLE db', 'Entry in SUNGG_SAMPLE db', $
               'Entry in SINGG_DERIVED db', 'FUV entry in SUNGG_DERIVED db', 'NUV entry in SUNGG_DERIVED db', $
               'Entry in IRAS_FSC db', 'Closest entry in GALEX db', 'Deepest entry in GALEX db', $
               'Entry in AKARI IRC database', 'Entry in AKARI FIS database']
   indblk   = ['ra','dec','glat','glong', 'lmhi', 'entsamp', 'entuvsamp', 'entopthot', 'entakirc', 'entakfis']
   ;
   ; Go to working directory
   plog, llog, logstr, 'going to working directory '+wd
   cd, wd, current=cwd
   ;
   ; parameters needed for estimating akari 90 micron fluxes
   f90min    = 0.55              ; 90 micron detection limit
   lf90min   = alog10(f90min)    ; in the log
   lcha      = 9.809             ; see singg notebook #9
   kuv2      = 13.302            ; see singg notebook #9
   aa        = -0.6747           ; parameter for IRX-color fit
   bb        = 2.8017            ; parameter for IRX-color fit
   cc        = 0.05649           ; parameter for IRX-color fit
   ;
   srad1     = 0.5*60.0          ; galex search radius in degrees
   ;
   ; get data from singg_sample
   plog, llog, logstr, 'getting SINGG sample '
   dbopen, dbs
   dbext, -1, 'entry,name,ra,dec,ebv,distance,logmhi,observed', $
               ents,hname0,ra0,dec0,ebv0,dist0,lmhi0,obs0
   nsamp     = n_elements(hname0)
   dbclose
   plog, llog, logstr, 'number of HIPASS targets in SINGG_sample: '+strtrim(nsamp)
   ;
   ; Kludge correct name of HIPASS J1339-31A
   jj        = where(strpos(hname0,'J1339-31b') EQ 0, njj)
   hname0[jj] = 'J1339-31A'
   ;
   ; get data from singg_derived
   plog, llog, logstr, 'getting data from SINGG_derived'
   dbopen, dbh
   list      = good_derived3()
   dbext, list, 'entry,name,object,mabs_r_t,logf_ha_t,rmax_f,entry_sample,entry_iras', $
                 enth,sname0,hname1,mabsr,lfha,ropt,esamp,eiras
   dbext, list, 'ra,dec',ra1,dec1
   nsr2      = n_elements(sname0)
   dbclose
   plog, llog, logstr, 'Number of galaxies in SINGG_derived: '+numstr(nsr2)
   ;
   ; while we are working with these data might as well calculate
   ; estimated akari 90 micron flux
   lf90ha0   = lcha + lfha - 0.137*mabsr
   qq        = where(lfha LE -900.0 OR mabsr LE -900., nqq)
   IF nqq GT 0 THEN lf90ha0[qq] = lfflag
   ;
   ; mark which sources in singg_sample are in SR2 and 
   ; the entry in singg_derived of the source (or S1)
   kk        = where(strtrim(sname0,2) eq strtrim(hname1,2) or strpos(sname0,'S1') ge 7, nkk)
   jj        = sort(esamp[kk])
   uu        = uniq(esamp[kk[jj]])
   nuu       = n_elements(uu)
   pp        = kk[jj[uu]]
   esr2      = make_array(nsamp, /long, value=-1l)
   ptr01     = make_array(nsamp, /long, value=-1l)
   ptr10     = make_array(nsr2, /long, value=-1l)
   ;
   ; set some pointers
   FOR ii = 0, nuu-1 DO BEGIN 
      kk     = pp[ii]
      jj     = where(ents EQ esamp[kk], njj)
      IF njj EQ 1 THEN BEGIN 
         esr2[jj]  = enth[kk] 
         ptr01[jj] = kk
         ll        = where(esamp EQ esamp[kk], nll)
         IF nll LT 1 THEN plog, llog, logstr, '**** Huh??? nll = '+numstr(nll)
         ptr10[ll] = jj
      ENDIF ELSE BEGIN 
         plog, llog, logstr, '**** Huh??? njj = '+numstr(njj)
      ENDELSE 
   ENDFOR 
   ;
   ; make master sample: every galaxy in SR2 plus every HIPASS target
   ; in singg_sample that is not yet in SR2.
   ;
   ; first figure out sample size, make arrays
   plog, llog, logstr, 'first pass at compiling master sample'
   qq          = where(ptr01 LT 0l, nstilltodo)
   nmast       = nsr2 + nstilltodo
   plog, llog, logstr, 'Number of galaxies still to observe for SINGG : '+strtrim(nstilltodo,2)
   plog, llog, logstr, 'Number of galaxies in first pass of master sample: '+strtrim(nmast,2)
   entoptphot  = make_array(nmast, /long, value=-1l)
   hname       = make_array(nmast, /string, value='')
   sname       = make_array(nmast, /string, value='')
   entsamp     = make_array(nmast, /long, value=notinsamp)
   ra          = make_array(nmast, /float, value=-1.0)
   dec         = make_array(nmast, /float, value=-1.0)
   possrc      = make_array(nmast, /int, value=-1)
   dist        = make_array(nmast, /float, value=-1.0)
   lmhi        = make_array(nmast, /float, value=-1.0)
   ebv         = make_array(nmast, /float, value=-1.0)
   obs         = make_array(nmast, /string, value='N')
   entiras     = make_array(nmast, /long, value=-1l)
   nmiras      = make_array(nmast, /int, value=0)
   sepiras     = make_array(nmast, /float, value=0.0)
   lf90ha      = make_array(nmast, /float, value=lfflag)
   lf90uv      = make_array(nmast, /float, value=lfflag)
   haflag      = make_array(nmast, /int, value=-99)
   uvflag      = make_array(nmast, /int, value=-1)
   entuvsamp   = make_array(nmast, /long, value=-1l)
   entfuvphot  = make_array(nmast, /long, value=-1l)
   entnuvphot  = make_array(nmast, /long, value=-1l)
   seloptrc    = make_array(nmast, /int, value=0)
   statoptrc   = make_array(nmast, /string, value='')
   nobsoptrc   = make_array(nmast, /int, value=0)
   entgalexa   = make_array(nmast, /long, value=-1l)
   entgalexb   = make_array(nmast, /long, value=-1l)
   aisflaga    = make_array(nmast, /byte, value=0b)
   aisflagb    = make_array(nmast, /byte, value=0b)
   sradak      = make_array(nmast, /float, value=-1.0)
   entakfis    = make_array(nmast, /long, value=-1l)
   nentfis     = make_array(nmast, /int, value=-1)
   ndensfis    = make_array(nmast, /long, value=-1)
   sepfis      = make_array(nmast, /float, value=-1.0)
   lf90fis     = make_array(nmast, /float, value=lfflag)
   entakirc    = make_array(nmast, /long, value=-1l)
   nentirc     = make_array(nmast, /int, value=-1)
   ndensirc    = make_array(nmast, /long, value=-1)
   sepirc      = make_array(nmast, /float, value=-1.0)
   lf09irc     = make_array(nmast, /float, value=lfflag)
   ;
   ; now start populating
   mm               = nsr2-1
   entoptphot[0:mm] = enth
   hname            = ljust([hname0[ptr10], hname0[qq]],10)
   sname            = ljust([sname0, hname0[qq]],13)
   entsamp          = [esamp, ents[qq]]
   ra               = [ra1, ra0[qq]]
   dec              = [dec1, dec0[qq]]
   possrc           = [make_array(nsr2, /int, value=1), make_array(nstilltodo, /int, value=0)]
   dist             = [dist0[ptr10], dist0[qq]]
   lmhi             = [lmhi0[ptr10], lmhi0[qq]]
   obs              = [obs0[ptr10], obs0[qq]]
   entiras[0:mm]    = eiras
   lf90ha[0:mm]     = lf90ha0
   ;
   ; set haflag (the checks are done in this order): 
   ; notinsamp (=-9): was never in SINGG_SAMPLE
   ; -1: has 'N' in observed from singg_sample
   ;  0: has 'Y' in observed from singg_sample, but is not
   ;     in singg_derived (probably observed by 2.3m)
   ;  1: is in singg_derived
   ; -99: is default nothing should have this...
   kk             = where(strpos(strupcase(obs),'N') GE 0, nkk)
   IF nkk GT 0 THEN haflag[kk] = -1
   kk             = where(strpos(strupcase(obs),'Y') GE 0, nkk)
   IF nkk GT 0 THEN haflag[kk] =  0
   kk             = where(entoptphot GT 0, nkk)
   IF nkk GT 0 THEN haflag[kk] =  1
   ;
   ; ~~~~~~~~~~~~~~~~ Now on to the GALEX (GR5) database ~~~~~~~~~~~~~~~~~
   ;
   ; Arrays to store things
   obsnameglxa = make_array(nmast, /string, value='')
   obsnameglxb = make_array(nmast, /string, value='')
   sepglxa     = make_array(nmast, /float, value=999.9)
   sepglxb     = make_array(nmast, /float, value=999.9)
   tfuvglxa    = make_array(nmast, /float, value=-1.0)
   tfuvglxb    = make_array(nmast, /float, value=-1.0)
   tnuvglxa    = make_array(nmast, /float, value=-1.0)
   tnuvglxb    = make_array(nmast, /float, value=-1.0)
   nmatchglx   = make_array(nmast, /int, value=0)
   umatch      = make_array(nmast, /byte, value=0b)
   ;
   ; Open Galex (GR5?) DB, get the direct entries
   plog, llog, logstr, 'opening GALEX tile database and cross matching'
   dbopen, dbg
   list        = dbfind('ow = 0')
   ;
   ; loop through the master source list
   FOR ii = 0, nmast-1 DO BEGIN 
      ; 
      ; For each singg entry find
      ; - number of matching galex entries
      ; - closest galex entry (entgala)
      ; - longest FUV exposure time galex entry (entgalb)
      ; - separations for these two cases (sepglx{a,b})
      list2  = dbcircled(ra[ii], dec[ii], srad1, sep, list)
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
   ; determine which are in AIS
   onamea       = strlowcase(obsnameglxa)
   pp           = strpos(onamea, 'ais_')
   jj           = where(pp EQ 0) 
   aisflaga[jj] = 1b
   onameb       = strlowcase(obsnameglxb)
   pp           = strpos(onameb, 'ais_')
   jj           = where(pp EQ 0) 
   aisflagb[jj] = 1b
   ;
   ;
   ; Now find matches to sungg_derived, by name, retrieve UV data
   plog, llog, logstr, 'Opening SUNGG_derived and cross matching ...'
   dbopen, dbf
   list3 = dbfind('filter = fuv')
   entfu = dbmatch('sname',sname,list3)
   dbext, entfu, 'ra,dec,a_uv_gal,flux_brt,mag_brt,fluxrad_brt,entry_sungg_sample', raf,decf,afg,ffuv,mfuv,rfuv,esunggf
   list4 = dbfind('filter = nuv')
   entnu = dbmatch('sname',sname,list4)
   dbext, entnu, 'ra,dec,a_uv_gal,flux_brt,mag_brt,fluxrad_brt,entry_sungg_sample', ran,decn,ang,fnuv,mnuv,rnuv,esunggn
   qq1   = where(entfu GE 1, nqq1)
   IF nqq1 GE 1 THEN umatch[qq1] = 1b
   ;
   dbclose      ; close sungg_derived
   ;
   ; save entry in sungg_sample where it is valid
   jj            = where(esunggf ge 1 and esunggn ge 1, njj)
   if njj gt 1 then begin
      kk = where(esunggf[jj] ne esunggn[jj], nkk)
      if nkk gt 0 then stop, '**** warning entry_sungg_sample values do not match'
   endif 
   jj            = where(esunggf gt 0, njj)
   if njj gt 0 then entuvsamp[jj] = esunggf[jj]
   jj            = where(esunggn gt 0, njj)
   if njj gt 0 then entuvsamp[jj] = esunggn[jj]
   ;
   ; save FUV,NUV entries in sungg_derived
   jj            = where(entfu GE 1, njj)
   IF njj GT 0 THEN entfuvphot[jj] = entfu[jj]
   jj            = where(entnu GE 1, njj)
   IF njj GT 0 THEN entnuvphot[jj] = entnu[jj]
   ;
   ; calculate expected akari flux from uv data
   lf90uv        = 0.0*lf90ha - 99.99
   color         = mfuv - mnuv - afg + ang
   lfuv0         = alog10(ffuv) + 0.4*afg
   lf90uv[qq1]   = kuv2 + lfuv0[qq1] + alog10(10.0^(aa+bb*color[qq1])+cc)
   ;
   ; Set UV status flag : uvflag
   ; -1 not in GR5ALL (default)
   ;  0 in GR5ALL short (AIS) exposure
   ;  1 in GR5ALL longer exposure
   ;  2 in sungg_derived - only one of NUV, FUV
   ;  3 in sungg_derived with both NUV & FUV
   jj            = where(entgalexa ge 1 or entgalexb ge 1, njj)
   if njj gt 0 then uvflag[jj] = 0
   jj            = where((entgalexa ge 1 or entgalexb ge 1)$
                         and (aisflaga ne 1b or aisflagb ne 1b), njj)
   if njj gt 0 then uvflag[jj] = 1
   jj            = where(entnu ge 1 or entfu ge 1, njj)
   if njj gt 0 then uvflag[jj] = 2
   jj            = where(entnu ge 1 AND entfu ge 1, njj)
   if njj gt 0 then uvflag[jj] = 3
   ;
   ; Find places where UV positions are available but optical
   ; positions are not.  Insert UV positions in these cases.
   plog, llog, logstr, 'setting UV positions where need be'
   jj            = where(entnu GE 1 AND possrc EQ 0, njj)
   IF njj GE 1 THEN BEGIN 
      ra[jj]     = ran[jj]
      dec[jj]    = decn[jj]
      possrc[jj] = 2
   ENDIF 
   jj            = where(entfu GE 1 AND possrc EQ 0, njj)
   IF njj GE 1 THEN BEGIN 
      plog, llog, logstr, '**** Hmmm... seems that the number of sources with FUV and not NUV = '+numstr(njj)
      ra[jj]     = raf[jj]
      dec[jj]    = decf[jj]
      possrc[jj] = 2
   ENDIF 
   ;
   ; get hopcat positions for sources that have possrc = 0, 
   ; and reset to possrc = 3
   dbopen, dboc
   list9         = where(possrc EQ 0, n9)
   IF n9 GT 0 THEN BEGIN 
      plog, llog, logstr, 'will get HOPCAT positions for  '+strtrim(n9)+'  sources with only HIPASS positions at this stage'
      entoc       = dbmatch('name_hipass',hname[list9])
      dbext,entoc,'name_hipass,ra,dec',hnamoc,raoc,decoc
      jj          = where(entoc GT 0 AND (ra GE 0.0 AND dec GT -90.0), njj)
      IF njj GT 0 THEN BEGIN 
         plog, llog, logstr, 'replacing HIPASS positions with HOPCAT positions for N_sources = '+strtrim(njj,2)
         ii         = list9[jj]
         ra[ii]     = raoc[jj]
         dec[ii]    = decoc[jj]
         possrc[ii] = 3
      ENDIF 
   ENDIF 
   ;
   ; ~~~~~~~~~~~~~~~~~~ Find extra HIPASS sources ~~~~~~~~~~~~~~~~~~~~~~
   ;
   ; These will be defined as sources in HOPCAT that are in the unique
   ; GALEX tiles that are not already in the selection.
   ;
   ; get unique galex tiles
   plog, llog, logstr, 'Starting search for extra HIPASS targets in unique galex tiles...'
   dbopen, dbg
   entgalexc      = [entgalexa, entgalexb]
   jj             = sort(entgalexc)
   uu             = uniq(entgalexc[jj])
   entgalexc      = entgalexc[jj[uu]]
   ntile          = n_elements(entgalexc)
   plog, llog, logstr, 'will search '+strtrim(ntile,2)+' galex tiles for HOPCAT matches.'
   dbext, entgalexc, 'ra,dec', rag, decg
   dbclose
   ;
   ; loop through tiles add new HOPCAT galaxies
   nockeep        = 0
   dbopen, dboc   ; open HOPCAT
   FOR ii = 0, ntile-1 DO BEGIN 
      ;
      ; find hopcat sources in  tile
      list5 = dbcircled(rag[ii], decg[ii], srad1, sep)
      IF list5[0] GE 0 THEN BEGIN 
         ;
         ; if there are valid entries then get their entry number and 
         ; hipass name 
         nl5       = n_elements(list5)
         dbext, list5, 'entry,name_hipass', entoc, hnamoc
         FOR jj = 0, nl5-1 DO BEGIN
            ;
            ; match each hopcat target against those we have already
            str = strupcase(strtrim(hnamoc[jj],2))
            pp  = where(strpos(hname,str) EQ 0, npp)
            IF npp GT 1 THEN plog, llog, logstr, '**** warning multiple matches to existing list for: "'+str+'"'
            IF npp EQ 0 THEN BEGIN 
               ;
               ; no matches found.  Either start list of new entries
               ; or append to it depending on how many there are so 
               ; far as given by NOCKEEP.  Likewise record which tile
               ; this comes from
               IF nockeep EQ 0 THEN BEGIN 
                  entockeep = [entoc[jj]]
                  entgalexd = [entgalexc[ii]]
               ENDIF ELSE BEGIN 
                  entockeep = [entockeep, entoc[jj]]
                  entgalexd = [entgalexd, entgalexc[ii]]
               ENDELSE 
               nockeep = nockeep + 1
            ENDIF 
         ENDFOR 
      ENDIF 
   ENDFOR
   IF nockeep GT 0 THEN BEGIN 
      ;
      ; now whittle down to the unique keepers
      jj        = sort(entockeep)
      uu        = uniq(entockeep[jj])
      entockeep = entockeep[jj[uu]]
      entglaexd = entgalexd[jj[uu]]
      nockeep   = n_elements(entockeep)
      plog, llog, logstr, 'Number of extra HOPCAT targets to measure: '+numstr(nockeep)
      ;
      ; get some vital parameters from hopcat for the extra galaxies 
      ; we are keeping.
      dbext, entockeep, 'name_hipass,ra,dec,sint',hnamoc,raoc,decoc,sint
      hnamoc   = strtrim(hnamoc,2)
      dbclose
      ;
      ; now match to HIPASS catalogue to make sure we get the
      ; right distances and HI masses
      dbopen, dbhi
      dbext, -1, 'hipass_name,distance,logmhi',hnamhi,disthi,lmhihi
      dbclose
      hnamhi    = strupcase(strtrim(hnamhi,2))
      distoc    = make_array(nockeep,/float,value=-1.0)
      lmhioc    = make_array(nockeep,/float,value=-1.0)
      FOR ii = 0, nockeep-1 DO BEGIN 
         jj = where(hnamhi EQ strupcase(hnamoc[ii]), njj) 
         IF njj EQ 1 THEN BEGIN 
            distoc[ii] = disthi[jj[0]]
            lmhioc[ii] = lmhihi[jj[0]]
         ENDIF ELSE BEGIN 
            IF njj EQ 0 THEN plog, llog, logstr, '** WARNING hopcat galaxy '+hnamoc[ii]+' not matched with HI database' $
                        ELSE plog, llog, logstr, '** WARNING hopcat galaxy '+hnamoc[ii]+' has multiple matches with HI database, N = '+numstr(njj)
         ENDELSE 
      ENDFOR
      ;
      ; whittle again incase sources were discarded as unmatched
      ; or multiple matches
      keep      = where(distoc GT 0, nockeep2)
      IF nockeep2 LT nockeep THEN BEGIN 
         nockeep = nockeep2
         plog, llog, logstr, 'after rejecting cases with no or multiple HI matches, number of extra HOPCAT targets = '+strtrim(nockeep,2)
      ENDIF ELSE BEGIN 
         plog, llog, logstr, 'all extra HOPCAT targets were matched'
      ENDELSE 
      IF nockeep GT 0 THEN BEGIN 
         ;
         ; append the arrays with the new entries
         hname            = [hname, hnamoc[keep]]
         sname            = [sname, hnamoc[keep]]
         ra               = [ra, raoc[keep]]
         dec              = [dec, decoc[keep]]
         dist             = [dist, distoc[keep]]
         lmhi             = [lmhi, lmhioc[keep]]
         entsamp          = [entsamp, make_array(nockeep, /long, value=notinsamp)]
         possrc           = [possrc, make_array(nockeep, /int, value=3)]
         obs              = [obs, make_array(nockeep, /string, value='N')]
         entiras          = [entiras, make_array(nockeep, /long, value=-1)]
         nmiras           = [nmiras, make_array(nockeep,/int,value=0)]
         sepiras          = [sepiras, make_array(nockeep, /float, value=lfflag)]
         lf90ha           = [lf90ha, make_array(nockeep, /float, value=lfflag)]
         lf90uv           = [lf90uv, make_array(nockeep, /float, value=lfflag)]
         entoptphot       = [entoptphot, make_array(nockeep, /long, value=-1l)]
         ebv              = [ebv, make_array(nockeep, /float, value=-1.0)]
         haflag           = [haflag, make_array(nockeep, /int, value=notinsamp)]
         uvflag           = [uvflag, make_array(nockeep, /int, value=-1)]
         entuvsamp        = [entuvsamp, make_array(nockeep, /long, value=notinsamp)]
         entfuvphot       = [entfuvphot, make_array(nockeep, /long, value=-1l)]
         entnuvphot       = [entnuvphot, make_array(nockeep, /long, value=-1l)]
         seloptrc         = [seloptrc, make_array(nockeep, /int, value=0)]
         statoptrc        = [statoptrc, make_array(nockeep, /string, value='')]
         nobsoptrc        = [nobsoptrc, make_array(nockeep, /int, value=0)]
         entgalexa        = [entgalexa, make_array(nockeep, /long, value=-1l)]
         entgalexb        = [entgalexb, make_array(nockeep, /long, value=-1l)]
         aisflaga         = [aisflaga, make_array(nockeep, /byte, value=0b)]
         aisflagb         = [aisflagb, make_array(nockeep, /byte, value=0b)]
         sradak           = [sradak, make_array(nockeep, /float, value=-1.0)]
         entakfis         = [entakfis, make_array(nockeep, /long, value=-1l)]
         nentfis          = [nentfis, make_array(nockeep, /int, value=-1)]
         ndensfis         = [ndensfis, make_array(nockeep, /int, value=-1)]
         sepfis           = [sepfis, make_array(nockeep, /float, value=-1.0)]
         lf90fis          = [lf90fis, make_array(nockeep, /float, value=lfflag)]
         entakirc         = [entakirc, make_array(nockeep, /long, value=-1l)]
         nentirc          = [nentirc, make_array(nockeep, /int, value=-1)]
         ndensirc         = [ndensirc, make_array(nockeep, /int, value=-1)]
         sepirc           = [sepirc, make_array(nockeep, /float, value=-1.0)]
         lf09irc          = [lf09irc, make_array(nockeep, /float, value=lfflag)]
         ;
         ; update size of master sample
         nmast            = nmast+nockeep
         plog, llog, logstr, 'master sample size is now: '+strtrim(nmast,2)
      ENDIF 
   ENDIF ELSE BEGIN 
      dbclose
      plog, llog, logstr, 'No new HOPCAT targets to add'
   ENDELSE 
   plog, llog, logstr, 'setting IRAS+AKARI match radius '
   ;
   ; set IRAS+AKARI search radius
   sradak[0:nsr2-1] = ropt
   jj               = where(entnu GT 0, njj)                  ;  where there are valid NUV entries
   IF njj GT 0 THEN BEGIN                                     ; and they are bigger than the optical
      kk            = where(sradak[jj] LT rnuv[jj], nkk)      ; radius then use the NUV radius
      IF nkk GT 0 THEN sradak[jj[kk]] = rnuv[jj[kk]]
   ENDIF 
   jj               = where(entfu GT 0, njj)                  ; Do something similar with the FUV radii
   IF njj GT 0 THEN BEGIN                                  
      kk            = where(sradak[jj] LT rfuv[jj], nkk)      
      IF nkk GT 0 THEN sradak[jj[kk]] = rfuv[jj[kk]]
   ENDIF 
   jj               = where(sradak GE 0.0, njj)
   IF njj GT 0 THEN sradak[jj] = sradak[jj] + 60.0*minsradak ; add buffer 
   jj               = where(sradak LT 0.0, njj)
   IF njj GT 0 THEN sradak[jj] = 60.0*minsradak
   ;
   ; calculate galactic longitude, latitude and E(B-V)
   plog, llog, logstr, 'calculating Galactic longtitude and latitude and foreground dust absorption'
   euler, ra, dec, glong, glat, 1
   ebv             = dust_getval(glong, glat, /interp)
   ;
   ; ~~~~~~~~~ redo IRAS matching ~~~~~~~~~~~~~~~~~~~~~~~
   ;
   ; have to take care here because ra in the PSC is in hours,
   ; and in B1950 instead of J2000, so
   ; I can't get the db based matching to work.  Instead 
   ; will just extract all the arrays I need up front
   plog, llog, logstr, 'Opening IRAS database : '+dbir+'   and extracting key quantities'
   dbopen, dbir
   dbext, -1, 'entry,ra,dec,60_flux,100_flux', ecat, ra1950, dec1950, f60cat, f100cat
   ra1950         = 15.0*ra1950     ; convert from hours to degrees
   jprecess, ra1950, dec1950, racat, deccat
   ntot_iras     = 0
   entiras_old   = entiras
   entiras       = 0l*entiras - 1l
   FOR ii = 0, nmast-1 DO BEGIN 
      srad       = sradak[ii]     ; search radius in arcsec
      ;
      ; get separations using gcircd
      gcircd, 2, racat, deccat, ra[ii], dec[ii], sep
      sep        = 3600.0*sep     ; convert from deg to arcsec
      ;
      ; iras database has RA in decimal hours so use dbcircle, 
      ; rather than dbcircled
      list8      = where(sep LE srad, n8)
      IF n8 GT 0 THEN plog, llog, '  ', ljust(hname[ii],15)+'number matching = '+numstr(n8)
      ;
      ; for valid matches store the following IRAS data
      ; - entiras  = entry of closest match
      ; - nmiras   = number of irc matches
      ; - sepiras  = separation to nearest matching source
      IF list8[0] NE -1 THEN BEGIN 
         j0              = where((f60cat[list8] GT 0.0) OR (f100cat[list8] GT 0.0), nj0)
         IF nj0 GT 0 THEN BEGIN 
            ;
            ; take nearest match with 60 or 100 micron flux > 0, if available
            j1           = sort(sep[list8[j0]])
            j2           = list8[j0[j1[0]]]
            ; blah
         ENDIF ELSE BEGIN 
            ;
            ; otherwise just take nearest match
            j1           = sort(sep[list8])
            j2           = list8[j1[0]]
            ; lf09irc[ii]  = lfflag
         ENDELSE 
         entiras[ii]  = ecat[j2]
         nmiras[ii]   = n8
         sepiras[ii]  = sep[j2]
         ntot_iras    = ntot_iras + 1
      ENDIF 
   ENDFOR 
   dbclose
   ;
   ; compare old versus new IRAS entries
   jj = where(entiras_old GT 0, njj)
   plog, llog, logstr, 'Comparison of old versus new IRAS entries: '
   IF njj GT 0 THEN FOR ii = 0, njj-1 DO plog, llog, ' ', ljust(hname[jj[ii]],15)+numstr(entiras_old[jj[ii]])+'  '+numstr(entiras[jj[ii]])
   jj = where(entiras_old GT 0 AND (entiras_old NE entiras), njj)
   plog, llog, logstr, 'number of cases where entiras disagrees with entiras_old : '+strtrim(njj,2)
   IF njj GT 0 THEN FOR ii = 0, njj-1 DO plog, llog, '  ', ljust(hname[jj[ii]],15)+numstr(entiras_old[jj[ii]])+'  '+numstr(entiras[jj[ii]])
   plog, llog, logstr, 'finished matching to IRAS.  Number of matches = '+strtrim(ntot_iras,2)
   ;
   ;
   ; ~~~~~~~~~ Akari database matching ~~~~~~~~~~~~~~~~~
   ;
   ; match to AKARI IRC database
   plog, llog, logstr, 'matching to AKARI IRC database'
   dbopen, dbai
   nmirc         = 0
   FOR ii = 0, nmast-1 DO BEGIN 
      srad       = sradak[ii] / 60.0
      list6      = dbcircled(ra[ii], dec[ii], srad, sep)
      ;
      ; for valid matches store the following IRC data
      ; - entakirc = entry of closest match
      ; - nentirc  = number of irc matches
      ; - ndensirc = source density from IRC database
      ; - sepirc   = separation to nearest matching source
      ; - lf09irc  = log of 9 micron flux density (in Jy)
      IF list6[0] NE -1 THEN BEGIN 
         dbext, list6, 'entry,flux09,ndens09',ent, fl9, nd9
         j0              = where(fl9 GT 0.0, nj0)
         IF nj0 GT 0 THEN BEGIN 
            ;
            ; take nearest match with 9 micron flux > 0, if available
            j1           = sort(sep[j0])
            j2           = j0[j1[0]]
            lf09irc[ii]  = alog10(fl9[j2]) 
         ENDIF ELSE BEGIN 
            ;
            ; otherwise just take nearest match
            j1           = sort(sep)
            j2           = j1[0]
            lf09irc[ii]  = lfflag
         ENDELSE 
         entakirc[ii] = ent[j2]
         nentirc[ii]  = nj0
         ndensirc[ii] = nd9[j2]
         sepirc[ii]   = 60.0*sep[j2]
         nmirc        = nmirc + 1
      ENDIF 
   ENDFOR 
   dbclose
   plog, llog, logstr, 'finished matching to AKARI IRC.  Number of matches = '+strtrim(nmirc,2)
   ;
   ; match to AKARI FIS database
   plog, llog, logstr, 'matching to AKARI FIS database'
   dbopen, dbaf
   nmfis         = 0
   FOR ii = 0, nmast-1 DO BEGIN 
      srad       = sradak[ii] / 60.0
      list7      = dbcircled(ra[ii], dec[ii], srad, sep)
      ;
      ; for valid matches store the following FIS data
      ; - entakfis = entry of closest match
      ; - nentfis  = number of FIS matches
      ; - ndensfis = source density from FIS database
      ; - sepfis   = separation to nearest matching FIS source
      ; - lf09fis  = log of 90 micron flux density (in Jy)
      IF list7[0] NE -1 THEN BEGIN 
         dbext, list7, 'entry,flux90,ndens',ent, fl90, nd90
         j0              = where(fl90 GT 0.0, nj0)
         IF nj0 GT 0 THEN BEGIN 
            ;
            ; take nearest match with 90 micron flux > 0, if available
            j1           = sort(sep[j0])
            j2           = j0[j1[0]]
            lf90fis[ii]  = alog10(fl90[j2]) 
         ENDIF ELSE BEGIN 
            ;
            ; otherwise just take nearest match
            j1           = sort(sep)
            j2           = j1[0]
            lf90fis[ii]  = lfflag
         ENDELSE 
         entakfis[ii] = ent[j2]
         nentfis[ii]  = nj0
         ndensfis[ii] = nd90[j2]
         sepfis[ii]   = 60.0*sep[j2]
         nmfis        = nmfis + 1
      ENDIF 
   ENDFOR 
   dbclose
   plog, llog, logstr, 'finished matching to AKARI FIS.  Number of matches = '+strtrim(nmfis,2)
   ;
   ; now read in kinematics data
   fmtk          = '(i5,i5,3x,a12,1x,a12,1x,a4,i3,i3,i3,i3,f9.1,f8.2,f6.1,f9.1,1x,a8,1x,a50)'
   readfmt, filk, fmtk, esink, esunk, snamek, optidk, obsk, r1k, r2k, objk, wnk, vhelk, distk, abk, radk, telk, statk
   nkin          = n_elements(esink)
   ;
   ; translate telk into selection code
   telk          = strtrim(telk,2)
   scode         = make_array(nkin,/int,value=-1)
   jj            = where(telk EQ 'AAT',njj)
   IF njj GT 0 THEN scode[jj] = 1
   jj            = where(telk EQ 'WIYN',njj)
   IF njj GT 0 THEN scode[jj] = 2
   jj            = where(telk EQ 'WIYNgf',njj)
   IF njj GT 0 THEN scode[jj] = 3
   jj            = where(telk EQ 'WIYNxtr',njj)
   IF njj GT 0 THEN scode[jj] = 4
   ; match by singg_sample entry, check object name
   FOR kk = 0, nkin-1 DO BEGIN 
      jj = where(entsamp EQ esink[kk], njj)
      IF njj GT 0 THEN BEGIN 
         qq = where(strtrim(sname[jj],2) EQ strtrim(snamek[kk],2), nqq)
         CASE nqq OF 
            0: stop, snamek[kk]+' : No name match'
            1: BEGIN 
                  ;
                  ; store selection criteria used, status
                  ; also store n(observations) eventually...
                  seloptrc[jj[qq]]  = scode[kk]
                  statoptrc[jj[qq]] = statk[kk]
               END 
            ELSE: plog, llog, logstr, '**** '+snamek[kk]+' : multiple name matches: '+numstr(nqq)
         ENDCASE 
      ENDIF ELSE BEGIN 
         plog, llog, logstr, '**** '+snamek[kk]+' : no entry match '+numstr(njj)
      ENDELSE 
   ENDFOR
   ;
   ; set up byte arrays to indicate membership in 
   ; LVHIS, SIGRID, and MHONGOOSE
   ids4g      = make_array(nmast, /int, value=0)
   llvhis     = make_array(nmast, /byte, value=0b)
   idsigrid   = make_array(nmast, /int, value=0)
   lsigobs    = make_array(nmast, /byte, value=0b)
   lmgoose    = make_array(nmast, /byte, value=0b)
   ;
   ; read in S4G selection
   plog, llog, logstr, 'reading in S4G sample file: '+fs4g
   fmts4      = '(i,a,f,f,f,f,f,f)'
   readcol, fs4g, recs4, names4, ras4, decs4, tts4, babss4, ld25s4, bmags4, format=fmts4, comment='#'
   ns4        = n_elements(names4)
   ras4       = 15.0*ras4                   ; convert to decimal degrees
   srad       = 1.5*0.5*10.0^ld25s4 > 2.5   ; search radius in arcmin
   ;
   ; match by RA, Dec
   for ii = 0, ns4-1 do begin 
      gcircd, 2, ra, dec, ras4[ii], decs4[ii], dis
      dis     = dis*60.0
      jj      = sort(dis)
      kk      = where(dis[jj] le srad[ii], nkk)
      if nkk gt 0 then begin 
         ;
         ; nearest source is the match
         ids4g[jj[kk]] = recs4[ii]
         plog, llog, logstr, 'adopted match: '+numstr(recs4[ii])+'  '+names4[ii]+' -> '+sname[jj[0]]
         if nkk gt 1 then plog, llog, logstr, 'NOTE: S4G source='+names4[ii]+' has N = '+numstr(nkk)+' matches within search rad [arcmin] = '+numstr(srad[ii])+' all associated...'
      endif 
      ;else begin 
      ;   plog, llog, logstr, 'WARNING! S4G source='+names4[ii]+' has no matches! '
      ;endelse 
   endfor
   ;
   ; Read in LVHIS selection
   plog, llog, logstr, 'reading in LVHIS sample file: '+flvhis
   readcol, flvhis, lvname, format='(a)', comment='#'
   nlv        = n_elements(lvname)
   nlvmatch   = make_array(nlv)
   ;
   ; match LVHIS by name
   for ii = 0, nlv-1 do begin 
      str     = strtrim(lvname[ii], 2)+'*'
      jj      = where(strmatch(sname, str, /fold_case) ge 1, njj)
      if njj ge 1 then begin 
         llvhis[jj] = 1b
         if njj gt 1 then plog, llog, logstr, str+': LVHIS with multiple matches, N = '+numstr(njj)
      endif else begin
          plog, llog, logstr, str+': LVHIS source with no matches in supersingg_master'
      endelse 
   endfor
   ;
   ; Read in SIGRID selection
   plog, llog, logstr, 'reading in SIGRID sample file: '+fsigrid
   ;readcol, fsigrid, signame, format='(a)'
   readcol, fsigrid, idsig, signame, wobs, format='(i,a,a)'
   nsig       = n_elements(signame)
   lwobs      = make_array(nsig, /byte, value=0b)
   jj         = where(strmid(strtrim(strupcase(wobs),2),0,1) eq 'Y', njj)
   if njj gt 0 then lwobs[jj] = 1b
   ;
   ; match SIGRID by name
   for ii = 0, nsig-1 do begin 
      str     = strtrim(signame[ii], 2)+'*'
      jj      = where(strmatch(sname, str, /fold_case) ge 1, njj)
      if njj ge 1 then begin 
         idsigrid[jj] = idsig[ii]
         lsigobs[jj]  = lwobs[ii]
         if njj gt 1 then plog, llog, logstr, str+': SIGRID with multiple matches, N = '+numstr(njj)
      endif else begin
          plog, llog, logstr, str+': SIGRID with no matches in supersingg_master '
      endelse 
   endfor
   ;
   ; read in MHONGOOSE selection
   plog, llog, logstr, 'reading in MHONGOOSE selection file: '+fmgoose
   readcol, fmgoose, num, moptid, mra, mdec, mtyp, mvel, format='(i,a,a,a,a,i)'
   nmg        = n_elements(mra)
   mrastr     = strmid(mra,0,2)+':'+strmid(mra,3,2)+':'+strmid(mra,6,4)
   mdecstr    = strmid(mdec,0,3)+':'+strmid(mdec,4,2)+':'+strmid(mdec,7,2)
   mra        = 15.0*sexideg(mrastr)
   mdec       = sexideg(mdecstr)
   for ii = 0, nmg-1 do begin 
      gcircd, 2, ra, dec, mra[ii], mdec[ii], dis
      dis     = dis*60.0
      jj      = sort(dis)
      kk      = where(dis[jj] le rmatch, nkk)
      if nkk gt 0 then begin 
         ;
         ; nearest source is the match
         lmgoose[jj[0]] = 1b      
         plog, llog, logstr, 'adopted match '+moptid[ii]+'  '+mrastr[ii]+' '+mdecstr[ii]+' -> '+sname[jj[0]]
         if nkk gt 1 then plog, llog, logstr, 'WARNING! MHONGOOSE source='+moptid[ii]+' has more than 1 match; N = '+numstr(nkk)
         ;
         ; probably should do something for cases where there are 
         ; multiple SINGG sources to 1 HIPASS source... ****
         qq             = strpos(sname[jj[0]], ':S')
         if qq gt 0 then begin 
            pp          = where(hname eq hname[jj[0]], npp)
            if npp gt 0 then begin 
               lmgoose[pp] = 1b
               plog, llog, logstr, 'including all '+numstr(npp)+'SINGG galaxies associated with '+hname[jj[0]]+' as MHONGOOSE selected:'
               for p0 = 0, npp-1 do plog, llog, '                    ', sname[pp[p0]]
            endif else begin
            endelse 
         endif 
      endif else begin 
         plog, llog, logstr, 'WARNING! MHONGOOSE source='+moptid[ii]+' has no matches! '
      endelse 
   endfor
   ; -------------------------------------------
   ;
   ; rearrange arrays to be SNAME sorted
   kk          = sort(sname)
   sname       = temporary(strtrim(sname[kk],2))
   hname       = temporary(strtrim(hname[kk],2))
   ra          = temporary(ra[kk])
   dec         = temporary(dec[kk])
   glong       = temporary(glong[kk])
   glat        = temporary(glat[kk])
   possrc      = temporary(possrc[kk])
   ebv         = temporary(ebv[kk])
   dist        = temporary(dist[kk])
   lmhi        = temporary(lmhi[kk])
   obs         = temporary(strtrim(obs[kk],2))
   lf90ha      = temporary(lf90ha[kk])
   lf90uv      = temporary(lf90uv[kk])
   haflag      = temporary(haflag[kk])
   uvflag      = temporary(uvflag[kk])
   aisflaga    = temporary(aisflaga[kk])
   aisflagb    = temporary(aisflagb[kk])
   seloptrc    = temporary(seloptrc[kk])
   statoptrc   = temporary(strtrim(statoptrc[kk],2))
   llvhis      = temporary(llvhis[kk])
   idsigrid    = temporary(idsigrid[kk])
   lsigobs     = temporary(lsigobs[kk])
   lmgoose     = temporary(lmgoose[kk])
   entsamp     = temporary(entsamp[kk])
   entuvsamp   = temporary(entuvsamp[kk])
   entoptphot  = temporary(entoptphot[kk])
   entfuvphot  = temporary(entfuvphot[kk])
   entnuvphot  = temporary(entnuvphot[kk])
   entiras     = temporary(entiras[kk])
   nmiras      = temporary(nmiras[kk])
   sepiras     = temporary(sepiras[kk])
   entgalexa   = temporary(entgalexa[kk])
   entgalexb   = temporary(entgalexb[kk])
   sradak      = temporary(sradak[kk])
   entakfis    = temporary(entakfis[kk])
   nentfis     = temporary(nentfis[kk])
   ndensfis    = temporary(ndensfis[kk])
   sepfis      = temporary(sepfis[kk])
   lf90fis     = temporary(lf90fis[kk])
   entakirc    = temporary(entakirc[kk])
   nentirc     = temporary(nentirc[kk])
   ndensirc    = temporary(ndensirc[kk])
   sepirc      = temporary(sepirc[kk])
   lf09irc     = temporary(lf09irc[kk])
   ;
   ; set lengths of character items
   kk      = where(tlen EQ 0,nkk)
   IF nkk GT 0 THEN FOR ii = 0,nkk-1 DO result = execute('tlen[kk[ii]] = max(strlen('+item[kk[ii]]+'))') $
               ELSE stop
   ;
   ; open dbd file, write top of the file
   plog, llog, logstr, 'Writing DBD file'
   fildbd  = dbout + '.dbd'
   openw, lu, fildbd, /get_lun
   printf, lu, '#title'
   printf, lu, title
   printf, lu, ''
   printf, lu, '#maxentries'
   printf, lu, strtrim(string(nmast),2)
   printf, lu, ''
   ;
   ; write item lines of dbd file
   printf, lu, '#items'
   mc     = max(strlen(item))
   pritem = ljust(strupcase(item), mc)
   ni     = n_elements(item)
   FOR ii = 0, ni-1 DO BEGIN 
      typstr = ljust(tout[ii]+'*'+strtrim(string(tlen[ii]),2),5)
      printf,lu,pritem[ii]+'   '+typstr+'   "'+descript[ii]+'"'
   ENDFOR 
   ;
   ; write index blocks of dbd file and close
   printf,lu,'  '
   printf,lu,'#index'
   ni = n_elements(indblk)
   FOR ii = 0, ni-1 DO BEGIN 
      printf,lu,ljust(strupcase(strtrim(indblk[ii],2)),mc)+'   sort'
   ENDFOR 
   free_lun,lu   
   ;
   ; write database
   plog, llog, logstr, 'Writing database: '+dbout
   !PRIV = 2
   dbcreate, dbout, 1, 1, /external
   dbopen, dbout, 1
   dbbuild_new, sname, hname, ra, dec, glong, glat, possrc, ebv, dist, lmhi,$
                obs, lf09irc, lf90fis, lf90ha, lf90uv, sradak, nentirc, nentfis, nmiras, ndensirc, ndensfis, $
                sepirc, sepfis, sepiras, haflag, uvflag, aisflaga, aisflagb, seloptrc, statoptrc, $
                ids4g, idsigrid, lsigobs, llvhis, lmgoose, entsamp, entuvsamp, entoptphot, $
                entfuvphot, entnuvphot, entiras, entgalexa, entgalexb, entakirc, entakfis  
   dbclose,dummy
   if keyword_set(ingest) then begin 
      ;
      ; check if the database (the dbd file) exists already at $ZDBASE
      ; * if so use update_idldb
      ; * otherwise do a straight copy into $ZDBASE
      cd, zdbase
      stat     = file_info(dbout+'.dbd')
      IF stat.exists THEN BEGIN 
         update_idldb, dbout, dbdir=wd
      ENDIF ELSE BEGIN 
         spawn,'/bin/cp -f '+wd+dbout+'.db? .'
      ENDELSE 
   endif 
   ;
   ; go to starting directory, close log file and end
   plog, llog, logstr, 'returning to starting diriectory '+cwd
   cd, cwd   
   plog, llog, logstr, 'finished.'
   free_lun, llog
END 
