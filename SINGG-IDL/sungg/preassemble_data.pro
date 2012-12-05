pro preassemble_data, filo, flog
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
  ; G. Meurer 5/2010
  udb1       = 'sungg_derived_2010'
  odb        = 'singg_derived'
  hdb        = 'proc3_header'
  baseu1     = '/Volumes/data/meurer/SUNGG/data2_sungg_iwong/GALEX_DATA/'
  baseo      = '/Volumes/data/meurer/SINGG/'
  baseum1    = '/Volumes/data/meurer/SUNGG/Masks/'
  suffm      = '_uv_mask.fits'                    ; output masks will have this suffix
  prog       = 'PREASSEMBLE_DATA: '
  ;
  ; open log file
  printf, -1, prog+'opening log file: '+flog
  openw, ll, flog, /get_lun
  ;
  ; get filenames from UV database
  printf, -1, prog+'getting data from database: '+udb1
  printf, ll, prog+'getting data from database: '+udb1
  dbopen, udb1
  listf      = dbfind('filter = fuv, entry_singg_derivd > 0')
  dbext, listf, 'hipname,filename,entry_othfilt,entry_singg_derivd', hname, fnamef, listn, listof
  dbext, listn, 'filename, entry_singg_derivd', fnamen, liston
  nf         = n_elements(fnamef)
  dbclose
  ;
  ; pare down to unique hipass names (and sort)
  jj         = sort(hname)
  uu         = uniq(hname[jj])
  uu         = jj[uu]
  hname      = hname[uu]
  fnamef     = fnamef[uu]
  listn      = listn[uu]
  listof     = listof[uu]
  fnamen     = fnamen[uu]
  liston     = liston[uu]
  nf         = n_elements(hname)
  printf, -1, prog+'sorted and kept '+strtrim(string(nf),2)+' HIPASS targets'
  printf, ll, prog+'sorted and kept '+strtrim(string(nf),2)+' HIPASS targets'
  ;
  ; get information from optical database
  printf, -1, prog+'getting info from optical database: '+odb
  printf, ll, prog+'getting info from optical database: '+odb
  dbopen, odb
  dbext, listof, 'object,runid,entry_hdr_r,entry_hdr_s', hnameo, runid, listhr, listhh
  dbclose
  ;
  hname      = strtrim(hname,2)
  hnameo     = strtrim(hnameo,2)
  ;
  ; get filenames from header database
  printf, -1, prog+'getting info from header database: '+hdb
  printf, ll, prog+'getting info from header database: '+hdb
  dbopen, hdb
  dbext, listhr, 'filename', filer
  dbext, listhh, 'filename', fileh
  filer = strtrim(filer,2)
  fileh = strtrim(fileh,2)
  dbclose
  ;
  ; derive directory names and file names for UV data
  printf, -1, prog+'deriving UV directory and file names'
  printf, ll, prog+'deriving UV directory and file names'
  uvdirf     = make_array(nf, /string, value='')
  uvdirn     = uvdirf
  fnamf      = make_array(nf, /string, value='')
  fnamn      = fnamf
  fmskuv     = make_array(nf, /string, value='')
  iif        = strpos(fnamef, '/', /reverse_search)
  iin        = strpos(fnamen, '/', /reverse_search)
  for jj = 0, nf-1 do begin 
     uvdirf[jj] = baseu1 + strmid(fnamef[jj], 0, iif[jj]+1)
     fnamf[jj]  = strtrim(strmid(fnamef[jj], iif[jj]+1),2)
     uvdirn[jj] = baseu1 + strmid(fnamen[jj], 0, iin[jj]+1)
     fnamn[jj]  = strtrim(strmid(fnamen[jj], iin[jj]+1),2)
     fmskuv[jj] = hname[jj]+suffm
  endfor 
  ;
  ; derive directory and filenames for optical data
  printf, -1, prog+'deriving UV directory and file names'
  printf, ll, prog+'deriving UV directory and file names'
  pathr      = baseo+strtrim(runid,2)+'/Proc4/'+strtrim(filer,2)
  pathh      = baseo+strtrim(runid,2)+'/Proc4/'+strtrim(fileh,2)
  ;
  ; arrays to store whether file exists and is a symbolic link
  printf, -1, prog+'resolving symbolic links for optical images '
  printf, ll, prog+'resolving symbolic links for optical images '
  existr     = make_array(nf, /byte, value=0b)
  existh     = existr
  slinkr     = make_array(nf, /byte, value=0b)
  slinkh     = slinkr
  for ii = 0, nf-1 do begin
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
     printf, -1, prog+'will try to resolve '+strtrim(string(npr+nph),2)+' symbolic links'
     printf, ll, prog+'will try to resolve '+strtrim(string(npr+nph),2)+' symbolic links'
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
  printf, -1, prog+'deriving optical mask file names'
  printf, ll, prog+'deriving optical mask file names'
  fmskr       = make_array(nf, /string, value='NULL')
  fmskh       = make_array(nf, /string, value='NULL')
  for ii = 0, nf-1 do begin 
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
  nerr        = make_array(nf, /long, value=0l)   ; store number of errors here
  ;
  ; + fuv and nuv entry in singg_derived do not match
  pp         = where(listof ne liston, npp)
  if npp gt 0 then begin 
     printf, -1, prog+'Cases that do not have matching pointers in '+udb1+'to singg_derived for nuv and fuv'
     printf, ll, prog+'Cases that do not have matching pointers in '+udb1+'to singg_derived for nuv and fuv'
     for ii = 0, npp-1 do begin
        printf, -1, hname[pp[ii]]+'  ', listf[pp[ii]], listn[pp[ii]], listof[pp[ii]], liston[pp[ii]]
        printf, ll, hname[pp[ii]]+'  ', listf[pp[ii]], listn[pp[ii]], listof[pp[ii]], liston[pp[ii]]
     endfor 
     nerr[pp] = nerr[pp]+1l
  endif 
  ;
  ; + hipass name does not match in optical and uv database
  pp         = where(hname ne hnameo, npp)
  if npp gt 0 then begin 
     printf, -1, prog+'Cases where HIPASS name not the same in the UV and optical databases (UV name | optical name):'
     printf, ll, prog+'Cases where HIPASS name not the same in the UV and optical databases (UV name | optical name):'
     for ii = 0, npp-1 do begin
        printf, -1, hname[pp[ii]]+'  |  '+hnameo[pp[ii]]
        printf, ll, hname[pp[ii]]+'  |  '+hnameo[pp[ii]]
     endfor 
     nerr[pp] = nerr[pp]+1l
  endif 
  ;
  ; + UV directories don't match
  pp         = where(uvdirf ne uvdirn, npp)
  if npp gt 0 then begin
     printf, -1, prog+'The following cases do not have matching fuv, nuv directories (format: fuvdir | nuvdir)'
     printf, ll, prog+'The following cases do not have matching fuv, nuv directories (format: fuvdir | nuvdir)'
     for ii = 0, npp-1 do begin
        printf, -1, uvdirf[pp[ii]]+' | '+uvdirn[pp[ii]]
        printf, ll, uvdirf[pp[ii]]+' | '+uvdirn[pp[ii]]
     endfor 
     nerr[pp] = nerr[pp]+1l
  endif 
  ;
  ; + optical image directories don't match
  pp          = where(odirr ne odirh, npp)
  if npp gt 0 then begin
     printf, -1, prog+'The following cases do not have matching directories for optical images (format: R_dir | Ha_dir)'
     printf, ll, prog+'The following cases do not have matching directories for optical images (format: R_dir | Ha_dir)'
     for ii = 0, npp-1 do begin
        printf, -1, odirr[pp[ii]]+' | '+odirr[pp[ii]]
        printf, ll, odirh[pp[ii]]+' | '+odirh[pp[ii]]
     endfor 
     nerr[pp] = nerr[pp]+1l
  endif 
  ;
  ; + do not have .fits extensions
  pf         = where(strpos(fnamf,'.fits') lt 0, npf)
  pn         = where(strpos(fnamn,'.fits') lt 0, npn)
  pr         = where(strpos(filer,'.fits') lt 0, npr)
  ph         = where(strpos(fileh,'.fits') lt 0, nph)
  if max([npf,npn,npr,nph] gt 0) then begin 
     printf, -1,'the following files do not have ".fits" in their names'
     printf, ll,'the following files do not have ".fits" in their names'
     for ii = 0, npf-1 do begin
        printf, -1, 'fuv image: '+fnamef[pf[ii]]
        printf, ll, 'fuv image: '+fnamef[pf[ii]]
        nerr[pf[ii]] = nerr[pf[ii]]+1l
     endfor 
     for ii = 0, npn-1 do begin
        printf, -1, 'nuv image: '+fnamen[pn[ii]]
        printf, ll, 'nuv image: '+fnamen[pn[ii]]
        nerr[pn[ii]] = nerr[pn[ii]]+1l
     endfor 
     for ii = 0, npr-1 do begin
        printf, -1, 'R image: '+filer[pr[ii]]
        printf, ll, 'R image: '+filer[pr[ii]]
        nerr[pr[ii]] = nerr[pfr[ii]]+1l
     endfor 
     for ii = 0, nph-1 do begin
        printf, -1, 'Halpha image: '+fileh[ph[ii]]
        printf, ll, 'Halpha image: '+fileh[ph[ii]]
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
     printf, -1,'the following files had ".gz" stripped from their db names'
     printf, ll,'the following files had ".gz" stripped from their db names'
     for ii = 0, npf-1 do begin
        fnamf[pf[ii]] = strmid(fnamf[pf[ii]],0,qf[pf[ii]])
        printf, -1, 'fuv image: '+uvdirf[pf[ii]]+fnamf[pf[ii]]
        printf, ll, 'fuv image: '+uvdirf[pf[ii]]+fnamf[pf[ii]]
        nerr[pf[ii]] = nerr[pf[ii]]+1l
     endfor
     for ii = 0, npn-1 do begin
        fnamn[pn[ii]] = strmid(fnamn[pn[ii]],0,qn[pn[ii]])
        printf, -1, 'nuv image: '+uvdirn[pn[ii]]+fnamn[pn[ii]]
        printf, ll, 'nuv image: '+uvdirn[pn[ii]]+fnamn[pn[ii]]
        nerr[pn[ii]] = nerr[pn[ii]]+1l
     endfor 
     for ii = 0, npr-1 do begin
        filer[pr[ii]] = strmid(filer[pr[ii]],0,qr[pr[ii]])
        printf, -1, 'R image: '+odirr[pr[ii]]+filer[pr[ii]]
        printf, ll, 'R image: '+odirr[pr[ii]]+filer[pr[ii]]
        nerr[pr[ii]] = nerr[pr[ii]]+1l
     endfor 
     for ii = 0, nph-1 do begin
        fileh[ph[ii]] = strmid(fileh[ph[ii]],0,qh[ph[ii]])
        printf, -1, 'Halpha image: '+odirh[ph[ii]]+fileh[ph[ii]]
        printf, ll, 'Halpha image: '+odirh[ph[ii]]+fileh[ph[ii]]
        nerr[ph[ii]] = nerr[ph[ii]]+1l
     endfor 
  endif 
  ;
  ; + check if file and/or gzipped version exist
  existf     = make_array(nf, /byte, value=0b)
  existn     = make_array(nf, /byte, value=0b)
  existr     = make_array(nf, /byte, value=0b)
  existh     = make_array(nf, /byte, value=0b)
  existzf    = make_array(nf, /byte, value=0b)
  existzn    = make_array(nf, /byte, value=0b)
  existzr    = make_array(nf, /byte, value=0b)
  existzh    = make_array(nf, /byte, value=0b)
  for ii = 0, nf-1 do begin
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
     printf, -1,'The following files (or its gzipped version) do not exist where they are supposed to'
     printf, ll,'The following files (or its gzipped version) do not exist where they are supposed to'
     for ii = 0, npf-1 do begin
        printf, -1, 'fuv image: '+uvdirf[pf[ii]]+fnamf[pf[ii]]
        printf, ll, 'fuv image: '+uvdirf[pf[ii]]+fnamf[pf[ii]]
        nerr[pf[ii]] = nerr[pf[ii]]+1l
     endfor 
     for ii = 0, npn-1 do begin
        printf, -1, 'nuv image: '+uvdirn[pn[ii]]+fnamn[pn[ii]]
        printf, ll, 'nuv image: '+uvdirn[pn[ii]]+fnamn[pn[ii]]
        nerr[pn[ii]] = nerr[pn[ii]]+1l
     endfor 
     for ii = 0, npr-1 do begin
        printf, -1, 'R image: '+odirr[pr[ii]]+filer[pr[ii]]
        printf, ll, 'R image: '+odirr[pr[ii]]+filer[pr[ii]]
        nerr[pr[ii]] = nerr[pr[ii]]+1l
     endfor 
     for ii = 0, nph-1 do begin
        printf, -1, 'Halpha image: '+odirh[ph[ii]]+fileh[ph[ii]]
        printf, ll, 'Halpha image: '+odirh[ph[ii]]+fileh[ph[ii]]
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
     printf, -1,'The following files are gzipped and should be gunzipped:'
     printf, ll,'The following files are gzipped and should be gunzipped:'
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
        printf, -1, 'fuv image: '+ufnf[ii]
        printf, ll, 'fuv image: '+ufnf[ii]
     endfor 
     for ii = 0, npn-1 do begin
        printf, -1, 'nuv image: '+ufnn[ii]
        printf, ll, 'nuv image: '+ufnn[ii]
     endfor 
     for ii = 0, npr-1 do begin
        printf, -1, 'R image: '+ufnr[ii]
        printf, ll, 'R image: '+ufnr[ii]
     endfor 
     for ii = 0, nph-1 do begin
        printf, -1, 'Halpha image: '+ufnh[ii]
        printf, ll, 'Halpha image: '+ufnh[ii]
     endfor 
  endif 
  ;
  ; + uvmask does not exist
  existm     = make_array(nf, /byte, value=0b)
  for ii = 0, nf-1 do begin 
     inf        = file_info(baseum1+fmskuv[ii])
     existm[ii] = inf.exists
  endfor 
  jj          = where(existm eq 0b, njj)
  if njj gt 0 then begin
     printf, -1, prog+'could not find the UV masks for the following cases: (hname | FUV file name | expected mask name)'
     printf, ll, prog+'could not find the UV masks for the following cases: (hname | FUV file name | expected mask name)'
     for ii = 0, njj-1 do begin
        kk    = jj[ii]
        printf, -1, hname[kk]+' | '+fnamf[kk]+' | '+fmskuv[kk]
        printf, ll, hname[kk]+' | '+fnamf[kk]+' | '+fmskuv[kk]
     endfor 

  endif

  ;
  ; + NULL in optical mask name
  pr          = where(strpos(fmskr, 'NULL') ge 0, npr)
  ph          = where(strpos(fmskh, 'NULL') ge 0, nph)
  if npr gt 0 then begin
     printf, -1, 'The following cases have "NULL" for R mask image: '
     printf, ll, 'The following cases have "NULL" for R mask image: '
     for ii = 0, npr-1 do begin
        printf, -1, hname[pr[ii]]+' '+fmskr[pr[ii]]
        printf, ll, hname[pr[ii]]+' '+fmskr[pr[ii]]
     endfor 
     nerr[pr] = nerr[pr]+1
  endif 
  if npr gt 0 then begin
     printf, -1, 'The following cases have "NULL" for Halpha mask image: '
     printf, ll, 'The following cases have "NULL" for Halpha mask image: '
     for ii = 0, npr-1 do begin
        printf, -1, hname[ph[ii]]+' '+fmskh[ph[ii]]
        printf, ll, hname[ph[ii]]+' '+fmskh[ph[ii]]
     endfor 
     nerr[ph] = nerr[ph]+1
  endif 
  ;
  ; prepare for output, sort alphabetically
  ; by hname but put those with errs at the end
  srtstr  = strtrim(string(nerr),2)+' '+hname
  k1      = sort(srtstr)
  printf, -1, prog+'writing output file: '+filo
  printf, ll, prog+'writing output file: '+filo
  openw, lu, filo, /get_lun
  for ii = 0, nf-1 do begin 
     kk = k1[ii]
     str = hname[kk]+' '+uvdirf[kk]+' '+odirr[kk]+' '+baseum1+' '+fnamf[kk]+' '+fnamn[kk]+$
           ' '+filer[kk]+' '+fileh[kk]+' '+fmskr[kk]+' '+fmskh[kk]+' '+fmskuv[kk]
     printf, -1, strtrim(string(nerr[kk]),2)+'  '+str
     printf, lu, str
  endfor
  ;
  ; close files and finish
  printf, -1, prog+'closing files and exitting'
  printf, ll, prog+'closing files and exitting'
  free_lun, lu
  free_lun, ll
end 
