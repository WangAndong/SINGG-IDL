pro preassemble_data_gr6   ;, filo, flog
  ;
  ; filo   -> output file that will be used to assemble data.  
  ;           It will have columns as follows
  ;           1 HIPASS target name
  ;           2 UV directory
  ;           3 UV mask directory
  ;           4 Optical directory
  ;           5 FUV file name
  ;           6 NUV file name
  ;           7 R (continuum) image name
  ;           8 Halpha image name
  ;           9 R mask file name
  ;           10 Halpha mask file name
  ;           11 UV mask file name
  ; flog   -> log file name
  ;
  ; NB, because of inaccuracies in the DBs filo may require hand
  ; tweaking.
  ;
  ; G. Meurer (ICRAR/UWA) 5/2010 
  ;    + Originally written
  ; G. Meurer (ICRAR/UWA) 12/2011
  ;    + adapted for new masks and GR6 coadds
  ;    + uses as input data file created by mkall_ivymasks_gr6.pro
  udb1       = 'sungg_derived_2010'
  odb        = 'singg_derived'
  hdb        = 'proc3_header'
  baseu1     = '/Volumes/data/meurer/SUNGG/GR6/'
  baseo      = '/Volumes/data/meurer/SINGG/'
  baseum1    = '/Volumes/data/meurer/SUNGG/GR6/'
  fili1      = 'maskmatch_single.dat'
  fili2      = 'maskmatch_multiple.dat'
  filo       = 'ssoup_assembly_gr6.dat'
  fmti1      = '(a,a,a,a,a,a,a)'
  fmti2      = '(a,a,a,a,i)'
  wd         = baseu1
  suffm      = '_uv_mask.fits'                    ; output masks will have this suffix
  prog       = 'PREASSEMBLE_DATA_GR6: '
  flog       = 'preassemble_data_gr6.log'
  ;
  ; open log file
  printf, -1, prog+'opening log file: '+flog
  openw, ll, flog, /get_lun
  ;
  ; go to working directory
  plog, ll, prog, 'going to working directory: '+wd
  cd, wd, current=cwd
  ;
  ; read in data on the "single masks"
  plog, ll, prog, 'reading in "single mask" matches'
  readcol, fili1, sname, hname, optid, dum, fnamef, fnamen, fmskuv, $
           format=fmti1
  ;
  ; determine number of matches
  plog, ll, prog, 'finding HIPASS sources with multiple masks (which will be merged)'
  ss         = sort(hname)
  uu         = uniq(hname[ss])
  uhname     = hname[ss[uu]]
  nuh        = n_elements(uhname)
  nhmatch    = make_array(nuh, /int, value=0)
  for ii = 0, nuh-1 do begin 
     jj          = where(hname eq uhname[ii], njj)
     nhmatch[ii] = njj
  endfor 
  mult       = where(nhmatch gt 1, nmult)
  plog, ll, prog, 'number of HIPASS sources with multiple masks:  '+numstr(nmult)
  sngl       = where(nhmatch eq 1, nsngl)
  plog, ll, prog, 'number of HIPASS sources with single masks  :  '+numstr(nsngl)
  ;
  ; save the single matches
  plog, ll, prog, 'saving the single matches'
  sngl       = ss[uu[sngl]] 
  hname      = hname[sngl]
  fnamef     = fnamef[sngl]
  fnamen     = fnamen[sngl]
  fmskuv     = fmskuv[sngl]
  ngaluv     = make_array(nsngl, /int, value=1)
  ;
  ; read in multiple matches
  plog, ll, prog, 'reading in "multiple mask" matches'
  readcol, fili2, hname2, fnamef2, fnamen2, fmskuv2, ngaluv2, $
           format=fmti2
  nmult      = n_elements(hname2)
  plog, ll, prog, 'Number of HIPASS galaxies with multiple masks: '+numstr(nmult)
  ;
  ; append to single matches
  hname      = [hname, hname2]
  fnamef     = [fnamef, fnamef2]
  fnamen     = [fnamen, fnamen2]
  fmskuv     = [fmskuv, fmskuv2]
  ngaluv     = [ngaluv, ngaluv2]
  ntot       = nsngl + nmult
  ;
  ; sort and resave
  plog, ll, prog, 'sorting...'
  ss         = sort(hname)
  hname      = strtrim(hname[ss],2)
  fnamef     = fnamef[ss]
  fnamen     = fnamen[ss]
  fmskuv     = fmskuv[ss]
  ngaluv     = ngaluv[ss]
  ;
  ; open optical database, get good measurements
  plog, ll, prog, 'opening optical database "'+odb+'" and getting good entries'
  dbopen, odb
  goodo      = good_derived3()
  ;
  ; make arrays to store stuff
  listhr     = make_array(ntot, /int, value=-1)
  listhh     = make_array(ntot, /int, value=-1)
  runid      = make_array(ntot, /string, value='NULL')
  hnameo     = make_array(ntot, /string, value='NULL')
  ngalopt    = make_array(ntot, /int, value=-1)
  ;
  ; loop through entries get data on sources
  plog, ll, prog, 'getting essential data on sources'
  for ii = 0, ntot-1 do begin 
     list     = dbfind('object='+hname[ii], goodo,/silent)
     ngalopt[ii] = n_elements(list)
     if ngalopt[ii] eq 1 and list[0] le 0 then ngalopt[ii] = 0
     if ngalopt[ii] gt 0 then begin 
        dbext, list, 'name,object,runid,entry_hdr_r,entry_hdr_s',sname0,hnameo0,runid0,listhr0,listhh0
        sname0  = strtrim(sname0,2)
        hnameo0 = strtrim(hnameo0,2)
        runid0  = strtrim(runid0,2)
        if ngalopt[ii] gt 1 then begin
           ;
           ; check that the results are consistent, i.e.
           ; the came from the same image set
           k0 = where(runid0 ne runid0[0], nk0)
           k1 = where(listhr0 ne listhr0[0], nk1)
           k2 = where(listhh0 ne listhh0[0], nk2)
           k3 = where(strpos(sname0,hname[ii]) ne 0, nk3)
           k4 = where(hnameo0 ne hnameo0[0], nk4)
           if max([nk0,nk1,nk2,nk3,nk4]) gt 0 then begin 
              plog, ll, prog, '**** caution multiple optical matches for '+hname[ii]+' are not consistent'
              for jj = 0, ngalopt[ii]-1 do plog, ll, prog, '   '+sname0[jj]+'  '+hnameo0[jj]+'  '+runid0[jj]+'   '+numstr(listhr0[jj])+'  '+numstr(listhh0[jj])
           endif 
        endif 
        runid[ii]  = runid0[0]
        hnameo[ii] = hnameo0[0]
        listhr[ii] = listhr0[0]
        listhh[ii] = listhh0[0]
     endif 
     plog, ll, prog, 'number of optical matches for '+hname[ii]+' = '+numstr(ngalopt[ii])
  endfor 
  ;
  ; keep only entries with both optical and uv
  jj         = where(ngalopt ge 1, ntot)
  plog, ll, prog, 'saving only the entries with both optical and UV data, ntot = '+numstr(ntot)
  if ntot le 0 then stop, 'huh...., no cases with both optical and UV data???? '
  hname      = hname[jj]
  hnameo     = hnameo[jj]
  fnamef     = fnamef[jj]
  fnamen     = fnamen[jj]
  fmskuv     = fmskuv[jj]
  ngaluv     = ngaluv[jj]
  ngalopt    = ngalopt[jj]
  runid      = runid[jj]
  listhr     = listhr[jj]
  listhh     = listhh[jj]
  ;
  ; get filenames from header database
  plog, ll, prog, 'getting info from header database: '+hdb
  dbopen, hdb
  dbext, listhr, 'filename', filer
  dbext, listhh, 'filename', fileh
  filer      = strtrim(filer,2)
  fileh      = strtrim(fileh,2)
  dbclose
  ;
  ; derive directory names and file names for UV data
  plog, ll, prog, 'setting UV directory and file names'
  uvdirf     = make_array(ntot, /string, value='')
  uvdirn     = uvdirf
  fnamf      = make_array(ntot, /string, value='')
  fnamn      = fnamf
  for jj = 0, ntot-1 do begin 
     uvdirf[jj] = baseu1
     uvdirn[jj] = baseu1
     fnamf[jj]  = fnamef[jj]
     fnamn[jj]  = fnamen[jj]
     fnamef[jj] = uvdirf[jj]+fnamf[jj]
     fnamen[jj] = uvdirn[jj]+fnamn[jj]
     fmskuv[jj] = fmskuv[jj]
  endfor 
  ;
  ; derive directory and filenames for optical data
  plog, ll, prog, 'deriving optical directory and file names'
  pathr      = baseo+strtrim(runid,2)+'/Proc4/'+strtrim(filer,2)
  pathh      = baseo+strtrim(runid,2)+'/Proc4/'+strtrim(fileh,2)
  ;
  ; arrays to store whether file exists and is a symbolic link
  plog, ll, prog, 'resolving symbolic links for optical images '
  existr     = make_array(ntot, /byte, value=0b)
  existh     = existr
  slinkr     = make_array(ntot, /byte, value=0b)
  slinkh     = slinkr
  for ii = 0, ntot-1 do begin
     infr       = file_info(pathr[ii])
     infh       = file_info(pathh[ii])
     existr[ii] = infr.exists
     existh[ii] = infh.exists
     slinkr[ii] = infr.symlink
     slinkh[ii] = infh.symlink
  endfor 
  ;
  ; resolve file names for the symbolic links
  pr         = where(slinkr eq 1b, npr)
  ph         = where(slinkh eq 1b, nph)
  oldpathr   = pathr
  oldpathh   = pathh
  if npr gt 0 or nph gt 0 then begin
     plog, ll, prog, 'will try to resolve '+strtrim(string(npr+nph),2)+' symbolic links'
     for ii = 0, npr-1 do begin
        pth           = oldpathr[pr[ii]]
        pathr[pr[ii]] = resolve_symlink(pth)
     endfor 
     for ii = 0, nph-1 do begin
        pth           = oldpathh[ph[ii]]
        pathh[ph[ii]] = resolve_symlink(pth)
     endfor 
  endif 
  ;
  ; extract directory from optical paths
  odirr       = file_dirname(pathr)+'/'
  odirh       = file_dirname(pathh)+'/'
  ;
  ; derive optical mask file name
  plog, ll, prog, 'deriving optical mask file names'
  fmskr       = make_array(ntot, /string, value='NULL')
  fmskh       = make_array(ntot, /string, value='NULL')
  for ii = 0, ntot-1 do begin 
     jj       = strpos(fileh[ii], '_ss.fits')
     if jj gt 0 then begin
        ftry  = strmid(fileh[ii], 0, jj)+'_mask.fits'
        inf   = file_info(odirh[ii]+ftry)
        if inf.exists then fmskh[ii] = ftry
        ;stop
     endif
     jj       = strpos(filer[ii], '_ss.fits')
     if jj gt 0 then begin 
        ftry  = strmid(filer[ii], 0, jj)+'_mask.fits'
        inf   = file_info(odirr[ii]+ftry)
        if inf.exists then fmskr[ii] = ftry
        ;stop        
     endif 
     if fmskr[ii] eq 'NULL' then begin
        jj    = strpos(filer[ii], '_')
        if jj gt 0 then begin
           ftry = strmid(filer[ii], 0, jj)+'_mask.fits'
           inf  = file_info(odirr[ii]+ftry)
           if inf.exists then fmskr[ii] = ftry
           ;stop        
        endif 
     endif
  endfor 
  ;
  ; Note cases that don't make sense, need  further work
  nerr        = make_array(ntot, /long, value=0l)   ; store number of errors here
  ;
  ; + fuv and nuv entry in singg_derived do not match
  plog, ll, prog, 'file name error checking'
  ;
  ; + hipass name does not match in optical and uv database
  pp         = where(hname ne hnameo, npp)
  if npp gt 0 then begin 
     plog, ll, prog, 'Cases where HIPASS name not the same in the UV and optical databases (UV name | optical name):'
     for ii = 0, npp-1 do begin
        plog, ll, prog,'   '+hname[pp[ii]]+'  |  '+hnameo[pp[ii]]
     endfor 
     nerr[pp] = nerr[pp]+1l
  endif 
  ;
  ; + UV directories don't match
  ;pp         = where(uvdirf ne uvdirn, npp)
  ;if npp gt 0 then begin
  ;   printf, -1, prog+'The following cases do not have matching fuv, nuv directories (format: fuvdir | nuvdir)'
  ;   printf, ll, prog+'The following cases do not have matching fuv, nuv directories (format: fuvdir | nuvdir)'
  ;   for ii = 0, npp-1 do begin
  ;      printf, -1, uvdirf[pp[ii]]+' | '+uvdirn[pp[ii]]
  ;      printf, ll, uvdirf[pp[ii]]+' | '+uvdirn[pp[ii]]
  ;   endfor 
  ;   nerr[pp] = nerr[pp]+1l
  ;endif 
  ;
  ; + optical image directories don't match
  pp          = where(odirr ne odirh, npp)
  if npp gt 0 then begin
     plog, ll, prog, 'The following cases do not have matching directories for optical images (format: R_dir | Ha_dir)'
     for ii = 0, npp-1 do begin
        plog, ll, prog, '     '+odirh[pp[ii]]+' | '+odirh[pp[ii]]
     endfor 
     nerr[pp] = nerr[pp]+1
  endif 
  ;
  ; + do not have .fits extensions
  pf         = where(strpos(fnamf,'.fits') lt 0, npf)
  pn         = where(strpos(fnamn,'.fits') lt 0, npn)
  pr         = where(strpos(filer,'.fits') lt 0, npr)
  ph         = where(strpos(fileh,'.fits') lt 0, nph)
  if max([npf,npn,npr,nph] gt 0) then begin 
     printf, ll,prog,'the following files do not have ".fits" in their names'
     for ii = 0, npf-1 do begin
        plog, ll, prog, '      fuv image: '+fnamef[pf[ii]]
        nerr[pf[ii]] = nerr[pf[ii]]+1l
     endfor 
     for ii = 0, npn-1 do begin
        plog, ll, prog, '      nuv image: '+fnamen[pn[ii]]
        nerr[pn[ii]] = nerr[pn[ii]]+1l
     endfor 
     for ii = 0, npr-1 do begin
        plog, ll, prog, '      R image: '+filer[pr[ii]]
        nerr[pr[ii]] = nerr[pfr[ii]]+1l
     endfor 
     for ii = 0, nph-1 do begin
        plog, ll, prog, '   Halpha image: '+fileh[ph[ii]]
        nerr[ph[ii]] = nerr[ph[ii]]+1l
     endfor 
  endif 
  ;
  ; + strip .gz from filename
  qf         = strpos(fnamf,'.gz')
  qn         = strpos(fnamn,'.gz')
  qr         = strpos(filer,'.gz')
  qh         = strpos(fileh,'.gz')
  pf         = where(qf ge 0, npf)
  pn         = where(qn ge 0, npn)
  pr         = where(qr ge 0, npr)
  ph         = where(qh ge 0, nph)
  if max([npf,npn,npr,nph] gt 0) then begin 
     plog, ll, prog, 'the following files had ".gz" stripped from their db names'
     for ii = 0, npf-1 do begin
        fnamf[pf[ii]] = strmid(fnamf[pf[ii]],0,qf[pf[ii]])
        plog, ll, prog, '    fuv image: '+uvdirf[pf[ii]]+fnamf[pf[ii]]
        nerr[pf[ii]] = nerr[pf[ii]]+1l
     endfor
     for ii = 0, npn-1 do begin
        fnamn[pn[ii]] = strmid(fnamn[pn[ii]],0,qn[pn[ii]])
        plog, ll, prog, '    nuv image: '+uvdirn[pn[ii]]+fnamn[pn[ii]]
        nerr[pn[ii]] = nerr[pn[ii]]+1l
     endfor 
     for ii = 0, npr-1 do begin
        filer[pr[ii]] = strmid(filer[pr[ii]],0,qr[pr[ii]])
        plog, ll, prog, '    R image: '+odirr[pr[ii]]+filer[pr[ii]]
        nerr[pr[ii]] = nerr[pr[ii]]+1l
     endfor 
     for ii = 0, nph-1 do begin
        fileh[ph[ii]] = strmid(fileh[ph[ii]],0,qh[ph[ii]])
        plog, ll, prog, '    Halpha image: '+odirh[ph[ii]]+fileh[ph[ii]]
        nerr[ph[ii]] = nerr[ph[ii]]+1l
     endfor 
  endif 
  ;
  ; + check if file and/or gzipped version exist
  existf     = make_array(ntot, /byte, value=0b)
  existn     = make_array(ntot, /byte, value=0b)
  existr     = make_array(ntot, /byte, value=0b)
  existh     = make_array(ntot, /byte, value=0b)
  existzf    = make_array(ntot, /byte, value=0b)
  existzn    = make_array(ntot, /byte, value=0b)
  existzr    = make_array(ntot, /byte, value=0b)
  existzh    = make_array(ntot, /byte, value=0b)
  for ii = 0, ntot-1 do begin
     inff        = file_info(uvdirf[ii]+fnamf[ii])
     infn        = file_info(uvdirn[ii]+fnamn[ii])
     infr        = file_info(odirr[ii]+filer[ii])
     infh        = file_info(odirh[ii]+fileh[ii])
     infzf       = file_info(uvdirf[ii]+fnamf[ii]+'.gz')
     infzn       = file_info(uvdirn[ii]+fnamn[ii]+'.gz')
     infzr       = file_info(odirr[ii]+filer[ii]+'.gz')
     infzh       = file_info(odirh[ii]+fileh[ii]+'.gz')
     existf[ii]  = inff.exists
     existn[ii]  = infn.exists
     existr[ii]  = infr.exists
     existh[ii]  = infh.exists
     existzf[ii] = infzf.exists
     existzn[ii] = infzn.exists
     existzr[ii] = infzr.exists
     existzh[ii] = infzh.exists
  endfor
  ;
  ; + neither file nor gzipped version exist
  pf         = where(existf eq 0b and existzf eq 0b, npf)
  pn         = where(existn eq 0b and existzn eq 0b, npn)
  pr         = where(existr eq 0b and existzr eq 0b, npr)
  ph         = where(existh eq 0b and existzh eq 0b, nph)
  if max([npf,npn,npr,nph] gt 0) then begin 
     plog, ll, prog, 'The following files (or its gzipped version) do not exist where they are supposed to'
     for ii = 0, npf-1 do begin
        plog, ll, prog, '    fuv image: '+uvdirf[pf[ii]]+fnamf[pf[ii]]
        nerr[pf[ii]] = nerr[pf[ii]]+1l
     endfor 
     for ii = 0, npn-1 do begin
        plog, ll, prog, '    nuv image: '+uvdirn[pn[ii]]+fnamn[pn[ii]]
        nerr[pn[ii]] = nerr[pn[ii]]+1l
     endfor 
     for ii = 0, npr-1 do begin
        plog, ll, prog, '    R image: '+odirr[pr[ii]]+filer[pr[ii]]
        nerr[pr[ii]] = nerr[pr[ii]]+1l
     endfor 
     for ii = 0, nph-1 do begin
        plog, ll, prog, '    Halpha image: '+odirh[ph[ii]]+fileh[ph[ii]]
        nerr[ph[ii]] = nerr[ph[ii]]+1l
     endfor 
  endif 
  ;
  ; + gzipped version exists but not original
  pf         = where(existf eq 0b and existzf eq 1b, npf)
  pn         = where(existn eq 0b and existzn eq 1b, npn)
  pr         = where(existr eq 0b and existzr eq 1b, npr)
  ph         = where(existh eq 0b and existzh eq 1b, nph)
  if max([npf,npn,npr,nph] gt 0) then begin 
     plog, ll, prog, 'The following files are gzipped and should be gunzipped:'
     ;
     ; assemble unique file names...
     if npf gt 0 then begin 
        ufnf = uvdirf[pf]+fnamf[pf]+'.gz'
        kk   = sort(ufnf)
        uu   = uniq(ufnf[kk])
        ufnf = ufnf[kk[uu]]
        npf  = n_elements(ufnf)
        nerr[pf] = nerr[pf]+1l
     endif 
     if npn gt 0 then begin 
        ufnn = uvdirn[pn]+fnamn[pn]+'.gz'
        kk   = sort(ufnn)
        uu   = uniq(ufnn[kk])
        ufnn = ufnn[kk[uu]]
        npn  = n_elements(ufnn)
        nerr[pn] = nerr[pn]+1l
     endif 
     if npr gt 0 then begin 
        ufnr = odirr[pr]+filer[pr]+'.gz'
        kk   = sort(ufnr)
        uu   = uniq(ufnr[kk])
        ufnr = ufnr[kk[uu]]
        npr  = n_elements(ufnr)
        nerr[pr] = nerr[pr]+1l
     endif 
     if nph gt 0 then begin 
        ufnh = odirh[ph]+fileh[ph]+'.gz'
        kk   = sort(ufnh)
        uu   = uniq(ufnh[kk])
        ufnh = ufnh[kk[uu]]
        nph  = n_elements(ufnh)
        nerr[ph] = nerr[ph]+1l
     endif 
     ;
     ; now warn
     for ii = 0, npf-1 do begin
        plog, ll, prog, '    fuv image: '+ufnf[ii]
     endfor 
     for ii = 0, npn-1 do begin
        plog, ll, prog, '    nuv image: '+ufnn[ii]
     endfor 
     for ii = 0, npr-1 do begin
        plog, ll, prog, '    R image: '+ufnr[ii]
     endfor 
     for ii = 0, nph-1 do begin
        plog, ll, prog, '    Halpha image: '+ufnh[ii]
     endfor 
  endif 
  ;
  ; + uvmask does not exist
  existm     = make_array(ntot, /byte, value=0b)
  for ii = 0, ntot-1 do begin 
     inf        = file_info(fmskuv[ii])
     existm[ii] = inf.exists
  endfor 
  jj          = where(existm eq 0b, njj)
  if njj gt 0 then begin
     plog, ll, prog, 'could not find the UV masks for the following cases: (hname | FUV file name | expected mask name)'
     for ii = 0, njj-1 do begin
        kk    = jj[ii]
        plog, ll, prog, '    '+hname[kk]+' | '+fnamf[kk]+' | '+fmskuv[kk]
     endfor 
  endif
  ;
  ; + NULL in optical mask name
  pr          = where(strpos(fmskr, 'NULL') ge 0, npr)
  ph          = where(strpos(fmskh, 'NULL') ge 0, nph)
  if npr gt 0 then begin
     plog, ll, prog, 'The following cases have "NULL" for R mask image: '
     for ii = 0, npr-1 do begin
        plog, ll, prog, '    '+hname[pr[ii]]+' '+fmskr[pr[ii]]
     endfor 
     nerr[pr] = nerr[pr]+1
  endif 
  if npr gt 0 then begin
     plog, ll, prog, 'The following cases have "NULL" for Halpha mask image: '
     for ii = 0, npr-1 do begin
        plog, ll, prog, '     '+hname[ph[ii]]+' '+fmskh[ph[ii]]
     endfor 
     nerr[ph] = nerr[ph]+1
  endif 
  ;
  ; prepare for output, sort alphabetically
  ; by hname but put those with errs at the end
  srtstr  = strtrim(string(nerr),2)+' '+hname
  k1      = sort(srtstr)
  plog, ll, prog, 'writing output file: '+filo
  openw, lu, filo, /get_lun
  for ii = 0, ntot-1 do begin 
     kk = k1[ii]
     str = hname[kk]+' '+uvdirf[kk]+' '+odirr[kk]+' '+baseum1+' '+fnamf[kk]+' '+fnamn[kk]+$
           ' '+filer[kk]+' '+fileh[kk]+' '+fmskr[kk]+' '+fmskh[kk]+' '+fmskuv[kk]
     plog, ll, prog, str+'   nerr='+numstr(nerr[kk])
     printf, lu, str
  endfor
  free_lun, lu
  ;
  ; return to starting directory, close files and finish
  ; ---------------------------------------------------------------
  ; finish
  plog, ll, prog, 'returning to starting directory: '+cwd
  cd, cwd
  plog, ll, prog, 'finished'
  free_lun, ll
end 
