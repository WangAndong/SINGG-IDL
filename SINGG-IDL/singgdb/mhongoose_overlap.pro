pro mhongoose_overlap
   ;
   ; compare overlap between mhongoose and other datasets as
   ; recorded in supersingg_master database
   ;
   ; G. Meurer  (ICRAR/UWA) 03 Aug, 2011
   logstr    = 'MHONGOOSE_OVERLAP: '
   shome     = getenv('SHOME')
   sdir      = shome+'Sample/'
   fmgoose   = sdir+'gmd.txt'
   filo1     = 'mhongoose_overlap.test'
   filo2     = 'mhongoose_kin.dat'
   flog      = 'mhongoose_overlap.log'
   ;dbm       = 'supersingg_master'
   dbm       = 'supersingg_test'
   dbs       = 'singg_sample'
   dbo       = 'singg_derived'
   dbu       = 'super_sungg_derived'
   db1       = 'sr1'
   dbg       = 'galexgr6_seibert_coadd'  ; Galex tile database
   rmatch    = 4.0                  ; arcmin
   ;
   ; go to working directory
   cd, sdir
   ;
   ; open log file
   openw, ll, flog, /get_lun
   ;
   ; open database
   plog, ll, logstr, 'Opening database = '+dbm
   dbopen, dbm
   ;
   ; find galaxies marked as mhongoose selected
   plog, ll, logstr, 'finding sources'
   list      = dbfind('mhongoose = 1')
   nm        = n_elements(list)
   nammg     = make_array(nm, /string, value='<null>')
   lmgoose   = make_array(nm, /byte, value=0b)
   lwiyno    = make_array(nm, /byte, value=0b)
   nobj      = make_array(nm, /int, value=0)
   ;
   ; get relevant other parameters
   plog, ll, logstr, 'extracting quantities'
   dbext, list, 'sname,hname,ra,dec,ebv,dist,lmhi,haflag,uvflag,seloptrc,statoptrc', $
          sname, hname, ra, dec, ebv, dist, lmhi, haflag, uvflag, selrc, statrc
   dbext, list, 's4g,lvhis,idsigrid,obssig,entsamp,entuvsamp,entoptphot,entfuvphot,entnuvphot,entiras,entgalexa,entgalexb', $
          ents4g, llvhis, idsigrid, lsigobs, eosamp, euvsamp, eophot, efphot, enphot, eiras, eglxa, eglxb
   ;
   ; count number of optical matches
   plog, ll, logstr, 'counting number of optical matches'
   for ii = 0, nm-1 do begin 
      str   = strtrim(hname[ii],2)
      hlist = dbfind('hname = '+str)
      if hlist[0] le 0 then nobj[ii] = 0 else nobj[ii] = n_elements(hlist)
   endfor 
   ;forprint,sname,nobj
   ;stop
   dbclose
   ;
   ; read original mhongoose sample
   plog, ll, logstr, 'reading in MHONGOOSE selection file: '+fmgoose
   readcol, fmgoose, num, moptid, mra, mdec, mtyp, mvel, format='(i,a,a,a,a,i)'
   nmg        = n_elements(mra)
   plog, ll, logstr, 'matching by coordinates'
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
         nammg[jj[0]]   = moptid[ii]
         print, logstr+'adopted match '+moptid[ii]+'  '+mrastr[ii]+' '+mdecstr[ii]+' -> '+sname[jj[0]]
         if nkk gt 1 then plog, ll, logstr, 'WARNING! MHONGOOSE source='+moptid[ii]+' has more than 1 match; N = '+numstr(nkk)
      endif else begin 
         plog, ll, logstr, 'WARNING! MHONGOOSE source='+moptid[ii]+' has no matches! '
      endelse 
   endfor
   ;
   ; Determine the number of mhongoose sources with various properties
   plog, ll, logstr, 'finding various matches and reporting: '
   jj          = where(eosamp gt 0, nsinggsamp)
   jj          = where(euvsamp gt 0, nsunggsamp)
   jj          = where(eophot gt 0, nsr2)
   jj          = where(efphot gt 0 or enphot gt 0, nuvmeas1)
   jj          = where(efphot gt 0 and enphot gt 0, nuvmeas2)
   jj          = where(eiras gt 0, niras)
   jj          = where(uvflag eq 0, nuvflg1)
   jj          = where(uvflag le -1, nuvflg2)
   jj          = where(ents4g ge 1, ns4)
   jj          = where(selrc ge 1, ndyn)
   jj          = where(strlen(strtrim(statrc,2)) gt 0, nwiyn)
   lwiyno[jj]  = 1b
   jj          = where(llvhis eq 1, nlv)
   lsigrid     = make_array(nm, /byte, value=0b)
   jj          = where(idsigrid gt 0, nsig)
   lsigrid[jj] = 1b
   jj          = where(lsigobs eq 1, nsigo)
   jj          = where(lmgoose eq 1, nmg0)
   jj          = where(selrc eq 1, nkins)
   jj          = where(selrc eq 2, nkinw0)
   jj          = where(selrc ge 2, nkinw1)
   ;
   ; report results
   plog, ll, logstr, ' Number of sources marked MHONGOOSE                : '+numstr(nm)
   plog, ll, logstr, ' Number of sources in original MHONGOOSE sel.      : '+numstr(nmg0)
   plog, ll, logstr, ' Number of sources in SINGG sample                 : '+numstr(nsinggsamp)
   plog, ll, logstr, ' Number of sources in SUNGG sample                 : '+numstr(nsunggsamp)
   plog, ll, logstr, ' Number of sources measured in optical             : '+numstr(nsr2)
   plog, ll, logstr, ' Number of sources measured FUV or NUV             : '+numstr(nuvmeas1)
   plog, ll, logstr, ' Number of sources measured FUV and NUV            : '+numstr(nuvmeas2)
   plog, ll, logstr, ' Number of sources in IRAS FSC                     : '+numstr(niras)
   plog, ll, logstr, ' Number of sources NOT observed by GALEX           : '+numstr(nuvflg2)
   plog, ll, logstr, ' Number of sources with only short GALEX exposures : '+numstr(nuvflg1)
   plog, ll, logstr, ' Number of sources in S4G sample                   : '+numstr(ns4)
   plog, ll, logstr, ' Number of sources in SIGRID                       : '+numstr(nsig)
   plog, ll, logstr, '   Number observed with WiFeS for SIGRID           : '+numstr(nsigo)
   plog, ll, logstr, ' Number selected for dynamical analysis            : '+numstr(ndyn)
   plog, ll, logstr, '   Number earmarked for AAT (or 2.3m)              : '+numstr(nkins)
   plog, ll, logstr, '   Number in WIYN primary dynamical selection      : '+numstr(nkinw0)
   plog, ll, logstr, '   Number in WIYN primary + secondary selection    : '+numstr(nkinw1)
   plog, ll, logstr, '   Number observed with WIYN+SparsePAK             : '+numstr(nwiyn)
   plog, ll, logstr, ' Number of sources in LVHIS                        : '+numstr(nlv)
   ;
   ;  open output file
   plog, ll, logstr, 'opening output file: '+filo1
   openw, lu, filo1, /get_lun
   plog, ll, logstr, 'writing output file'   
   ;
   ; formatting
   snamo     = ljust(sname,14)
   optido    = ljust(nammg,16)
   hnamo     = ljust(hname,11)
   ;
   ; print data on the matches and write to file
   hdstr     = '# '+['MHONGOOSE precursor sample properties', '', $
                     ' 1 SINGG_name : SINGG name ', $
                     ' 2 HIPASS+    : HIPASS name', $
                     ' 3 Optical_ID : optical name (from gmd.txt)', $
                     ' 4 RA         : Right Ascension (J2000) [deg]', $
                     ' 5 Dec        : Declination (J2000) [deg]', $
                     ' 6 E(B-V)     : Galactic foreground dust reddening from Schlegel et al maps [mag]', $
                     ' 7 dist       : distance [Mpc]', $
                     ' 8 lh_MHI     : log(M_HI/M_sun) from HIPASS', $
                     ' 9 lm         : is it in MHONGOOSE selection file '+fmgoose+' (0=no, 1=yes)', $
                     '10 lv         : is it in LVHIS selection (0=no, 1=yes)', $
                     '11 ls         : is it in SIGRID selection (0=no, 1=yes)', $
                     '12 esmp       : entry in singg_sample database ', $
                     '13 euvs       : entry in sungg_sample database ', $
                     '14 uvf        : uvflag: -1 = no obs; 0 = short (AIS) exp; 1 = longer exp; 2 = one filter in sungg_derived; 3 = two filters in sungg_derived', $
                     '15 eopt       : entry in singg_derived database ', $
                     '16 efuv       : fuv entry in sungg_derived database', $
                     '17 enuv       : nuv entry in sungg_derived database', $
                     '18 eglxa      : galex tile database entry with center closest to object', $
                     '19 eglxb      : galex tile database entry containing source with longest exposure', $
                     '20 es4g       : record number in S4G sample (Sheth et al, 2010, PASP, 122, 1397)', $
                     '21 eiras      : entry in iras_fsc database', $
                     '22 fdyn       : dynamics selection flag: (0 = not sel.; 1 = AAT/ANU; 2 = WIYN original; 3,4 = WIYN extras)', $
                     '23 fwo        : WIYN+SparsePAK observation status (0 = not observed, 1 = observed)', $
                     '24 fsig       : ANU2.3m+WiFeS status of SIGRID targets', $
                     '', $
                     'SINGG_name  HIPASS+    Optical_ID           RA         Dec    E(B-V)  dist  lg_MHI lm lv ls esmp euvs uvf eopt efuv enuv  eglxa  eglxb  es4g   eiras fdyn fwo fsig', $
                     '------------------------------------------------------------------------------------------------------------------------------------------------------------------']
   nhd       = n_elements(hdstr)
   for ii = 0, nhd-1 do begin
      printf, lu, hdstr[ii]
      printf, -1, hdstr[ii]
   endfor 
   fmto1     = '(a14,a11,a16,f11.5,f11.5,f7.3,f7.2,f7.2,i3,i3,i3,i5,i5,i4,i5,i5,i5,i7,i7,i6,i8,i4,i4,i4)'
   ;
   for ii = 0, nm-1 do begin
      printf, lu, snamo[ii],hnamo[ii],optido[ii],ra[ii],dec[ii],ebv[ii],dist[ii],lmhi[ii], $
                  lmgoose[ii], llvhis[ii], lsigrid[ii], eosamp[ii], euvsamp[ii], uvflag[ii], $
                  eophot[ii], efphot[ii], enphot[ii], eglxa[ii], eglxb[ii], ents4g[ii], eiras[ii], $
                  selrc[ii], lwiyno[ii], lsigobs[ii], format=fmto1
      printf, -1, snamo[ii],hnamo[ii],optido[ii],ra[ii],dec[ii],ebv[ii],dist[ii],lmhi[ii], $
                  lmgoose[ii], llvhis[ii], lsigrid[ii], eosamp[ii], euvsamp[ii], uvflag[ii], $
                  eophot[ii], efphot[ii], enphot[ii], eglxa[ii], eglxb[ii], ents4g[ii], eiras[ii], $
                  selrc[ii], lwiyno[ii], lsigobs[ii], format=fmto1
   endfor 
   printf, lu, '# '
   printf, lu, '# Number of sources marked MHONGOOSE                : '+numstr(nm)
   printf, lu, '# Number of sources in original MHONGOOSE sel.      : '+numstr(nmg0)
   printf, lu, '# Number of sources in SINGG sample                 : '+numstr(nsinggsamp)
   printf, lu, '# Number of sources in SUNGG sample                 : '+numstr(nsunggsamp)
   printf, lu, '# Number of sources measured in optical             : '+numstr(nsr2)
   printf, lu, '# Number of sources measured FUV or NUV             : '+numstr(nuvmeas1)
   printf, lu, '# Number of sources measured FUV and NUV            : '+numstr(nuvmeas2)
   printf, lu, '# Number of sources in IRAS FSC                     : '+numstr(niras)
   printf, lu, '# Number of sources NOT observed by GALEX           : '+numstr(nuvflg2)
   printf, lu, '# Number of sources with only short GALEX exposures : '+numstr(nuvflg1)
   printf, lu, '# Number of sources in S4G sample                   : '+numstr(ns4)
   printf, lu, '# Number of sources in SIGRID                       : '+numstr(nsig)
   printf, lu, '#   Number observed with WiFeS for SIGRID           : '+numstr(nsigo)
   printf, lu, '# Number selected for dynamical analysis            : '+numstr(ndyn)
   printf, lu, '#   Number earmarked for AAT (or 2.3m)              : '+numstr(nkins)
   printf, lu, '#   Number in WIYN primary dynamical selection      : '+numstr(nkinw0)
   printf, lu, '#   Number in WIYN primary + secondary selection    : '+numstr(nkinw1)
   printf, lu, '#   Number observed with WIYN+SparsePAK             : '+numstr(nwiyn)
   printf, lu, '# Number of sources in LVHIS                        : '+numstr(nlv)
   ;
   plog, ll, logstr, 'finished, closing main output file...'
   free_lun, lu
   ;
   ; Now get more data so that we can write data for kinematics table
   ;
   ; first open sample database, get obs, vhel, dist
   plog, ll, logstr, 'Opening optical sample database: '+dbs+' to extract obs status, vhel, distance'
   dbopen, dbs
   dbext, eosamp, 'name,observed,vhel,distance',nametest1,obs,vhel,dist
   dbclose
   ;forprint, snamo+nametest1,eosamp,vhel,dist
   ;
   ; hmmm, there is a bug in supersingg_master:
   ; two entries for J1303-17b, of course we pick the 
   ; wrong one.  We will need to fix mk_master_sample.pro
   ; ignore for now. --  GRM 23/4/2012.
   ;
   ; open optical measurement database and get axial ratio and optical 
   ; aperture size
   plog, ll, logstr, 'Opening optical photometry datanase: '+dbo+' to extract axial ratio, optical size'
   dbopen, dbo
   dbext, eophot, 'name,axerat,rmax_f',nametest2,axerat,ropt
   ;
   ; match hipass names to "good" names to get number of optical 
   ; sources
   dbclose
   ;forprint, snamo+nametest2,axerat,rmax_f
   ;
   ; open uv measurement database match nuv entries by 
   ; optical photometry entry (I think this should work...)
   ; get UV sizes
   plog, ll, logstr, 'Opening and matching to UV database: '+dbu+' to get UV size'
   dbopen, dbu
   nlist = dbfind('filter = nuv')
   ulist = dbmatch('ENTRY_SINGG_DERIVD',eophot,nlist)
   dbext, ulist, 'sname,fluxrad_brt,entry_othfilt',nametest3,rnuv,flist
   dbext, flist, 'fluxrad_brt',rfuv
   dbclose
   ; forprint,snamo+nametest3,rnuv,ulist,flist
   ;
   ; Set maximum radius
   plog, ll, logstr, 'Determining maximum radius'
   rmax = ropt > rnuv
   rmax = rmax > rfuv
   ;forprint, ljust(snamo,15), ropt, rnuv, rfuv, rmax
   ;
   ; open and match to SR1 database
   plog, ll, logstr, 'Opening Release 1 database: '+db1+' and matching'
   l1   = make_array(nm,/byte,value=0b)
   tel  = make_array(nm,/string,value='ANU2.3m')
   dbopen, db1
   for ii = 0, nm-1 do begin
      str = strtrim(snamo[ii],2)
      slist = dbmatch('name',str)
      if slist[0] gt 0 then l1[ii] = 1b
   endfor
   ;forprint, snamo, l1
   dbclose
   ;
   ; open galex tile database and get exposure times
   plog, ll, logstr, 'Opening Galex tile database: '+dbg+' to get exposure times'
   dbopen, dbg
   dbext, eglxb, 'texpnuv,texpfuv',tnuv,tfuv
   dbclose
   tuv  = tnuv > tfuv
   ;
   ; some tidying
   ; 1. set whether source will be in SR2
   l2   = make_array(nm,/byte,value=1b)
   kk   = where(eophot le 0, nkk)
   if nkk gt 0 then l2[kk] = 0b
   ;
   ; 2. set telescope by SELOPTRC in supersingg_master => variable
   kk  = where(selrc eq 2, nkk)
   if nkk gt 0 then tel[kk] = 'WIYN'
   kk  = where(selrc eq 3, nkk)
   if nkk gt 0 then tel[kk] = 'WIYNgf'
   kk  = where(selrc eq 4, nkk)
   if nkk gt 0 then tel[kk] = 'WIYNxtr'
   ;
   ; 3. set reported sungg sample with various flags
   ;      - 777 if not in DB but already in galex arch with texp>1000s
   ;      - 888 if not in DB but already in galex arch with texp = 500s - 1000s
   ;      - 999 if not in DB but texp < 500s, or planned not yet observed
   euvsamp2 = euvsamp
   kk       = where(euvsamp2 le 0 and tuv ge 1000.0)
   if nkk gt 0 then euvsamp2[kk] = 777
   kk       = where(euvsamp2 le 0 and tuv ge 500.0)
   if nkk gt 0 then euvsamp2[kk] = 888
   kk       = where(euvsamp2 le 0 and tuv ge 0.1)
   if nkk gt 0 then euvsamp2[kk] = 999
   ;
   ; 4. set reported rotcurve staus to SIGobs for those marked
   ;    Observed for SIGRID
   statrc2 = statrc
   kk      = where(lsigobs ge 1 and strtrim(statrc2,2) eq '', nkk)
   if nkk gt 0 then statrc2[kk] = 'SG.|'
   kk      = where(lsigobs ge 1 and strtrim(statrc2,2) ne '' and strpos(strtrim(statrc2,2),'SG') lt 0, nkk)
   if nkk gt 0 then statrc2[kk] = 'SG,'+statrc2[kk]
   ;
   ;  open second output file
   plog, ll, logstr, 'opening output file: '+filo2
   openw, lu, filo2, /get_lun
   plog, ll, logstr, 'writing output file'   
   ;
   ; write second outpout file
   fmt2  = '(i5,i5,3x,a13,a12,1x,a4,i3,i3,i3,i3,f9.1,f8.2,f6.1,f9.1,1x,a8,1x,a40)'
   for ii = 0, nm-1 do begin
      printf, -1, eosamp[ii], euvsamp2[ii], ljust(snamo[ii],13), ljust(optido[ii],12), ljust(obs[ii],4), $
                  l1[ii], l2[ii], nobj[ii], lwiyno[ii], vhel[ii], dist[ii], axerat[ii], rmax[ii], $
                  ljust(tel[ii],8), ljust(statrc2[ii],40), format=fmt2
      printf, lu, eosamp[ii], euvsamp2[ii], ljust(snamo[ii],13), ljust(optido[ii],12), ljust(obs[ii],4), $
                  l1[ii], l2[ii], nobj[ii], lwiyno[ii], vhel[ii], dist[ii], axerat[ii], rmax[ii], $
                  ljust(tel[ii],8), ljust(statrc2[ii],40), format=fmt2
   endfor 
   ;
   free_lun, lu
   free_lun, ll
end
