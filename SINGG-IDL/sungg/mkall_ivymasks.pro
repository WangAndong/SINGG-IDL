pro mkall_ivymasks
  ;
  ; Turn all of Ivy Wong's savesets into mask files
  ;
  ; G. Meurer 6/2010
  ;
  wd        = '/Volumes/data/meurer/SUNGG/Masks' ; working directory
  udb       = 'sungg_derived_2010'               ; data that has been processed
  sstr      = 'mask.uv.dat'                      ; how to recognize the savesets
  suffm     = '_uv_mask.fits'                    ; output masks will have this suffix
  baseu1    = '/Volumes/data/meurer/SUNGG/data2_sungg_iwong/GALEX_DATA/'  ; starting tree for UV images
  prog      = 'MKALL_IVYMASKS: '
  flog      = 'mkall_ivymasks.log'
  ;
  ; open log, start
  printf, -1, prog+'starting, opening '+flog
  openw, ll, flog, /get_lun
  ;
  ; go to workingdirectory
  printf, -1, prog+'going to working directory '
  printf, ll, prog+'going to working directory '
  cd, wd, current=cwd
  ;
  ; find rpc files
  resu   = file_search('*'+sstr+'*',count=nres)
  nr     = n_elements(resu)
  printf, -1, prog+'found '+numstr(nr)+' savesets '
  printf, ll, prog+'found '+numstr(nr)+' savesets '
  ;
  ; get HIPASS names from udb
  printf, -1, prog+'getting object names from database'
  printf, ll, prog+'getting object names from database'
  dbopen, udb
  kfl    = dbfind('filter = fuv')
  dbext, kfl, 'sname,hipname,name,optid,filename',sname,hname,cname,oname,fnamef
  dbclose
  ;
  ; kull to unique hipass names
  jj      = sort(hname)
  uu      = uniq(hname[jj])
  uhname  = strtrim(hname[jj[uu]],2)
  ucname  = strtrim(cname[jj[uu]],2)
  uoname  = strtrim(oname[jj[uu]],2)
  ufnamef = strtrim(fnamef[jj[uu]],2)
  nh      = n_elements(uhname)
  ;
  ; Loop through all unique HIPASS targets 
  ; + find matches to saveset
  ; + find template galex image
  ; + make mask
  fnamf   = make_array(nh, /string, value='NULL')
  fmasko  = make_array(nh, /string, value='NULL')
  nmatch  = make_array(nh, /int, value=0)   ; to store number of matches
  nused   = make_array(nr, /int, value=0)   ; to tell how many times each saved set was used
  for ii = 0, nh-1 do begin
     printf, -1, prog+'working on matches for '+uhname[ii]
     printf, ll, prog+'working on matches for '+uhname[ii]
     p1         = strpos(resu,uhname[ii])
     j1         = where(p1 GE 0, nj1)
     p2         = strpos(resu,ucname[ii])
     j2         = where(p2 GE 0, nj2)
     p3         = strpos(resu,uoname[ii])
     j3         = where(p3 GE 0, nj3)
     ;
     ; combine matches
     pp         = [j1, j2, j3]
     jj         = where(pp ne -1, njj)
     ;if uhname[ii] eq 'J1051-17' or uhname[ii] eq 'J2357-32' then stop
     if njj gt 0 then begin
        ;
        ; get unique matches 
        pp         = pp[jj]
        jj         = sort(pp)
        uu         = uniq(pp[jj])
        pp         = pp[jj[uu]]
        npp        = n_elements(pp)
        nmatch[ii] = npp
        printf, -1, prog+'Number of saveset matches for '+uhname[ii]+' = '+numstr(npp)
        printf, ll, prog+'Number of saveset matches for '+uhname[ii]+' = '+numstr(npp)
        ;
        ; derive matching FUV image
        fname      = baseu1+ufnamef[ii]
        inf        = file_info(fname)
        if inf.exists then begin 
           printf, -1, prog+'Matching fits image exists and is: '+fname
           printf, ll, prog+'Matching fits image exists and is: '+fname
           fnamf[ii] = fname
           ;
           ; read header from template
           printf, -1, prog+'reading header from fits image: '+fname
           printf, ll, prog+'reading header from fits image: '+fname
           fits_read, fname, img, hdr, /header_only
           ;
           ; make mask
           fmasko[ii] = uhname[ii]+suffm
           printf, -1, prog+'calling CREATE_IVY_MASK to make mask image: '+fmasko[ii]
           printf, ll, prog+'calling CREATE_IVY_MASK to make mask image: '+fmasko[ii]
           create_ivy_mask, resu[pp], hdr, mask, fmasko=fmasko[ii]
           ;
           nused[pp]  = nused[pp]+1
        endif else begin
           printf, -1, prog+'The matching fits image should be '+fname+' but it does not exist '
           printf, ll, prog+'The matching fits image should be '+fname+' but it does not exist '
        endelse 
     endif else begin 
        printf, -1, prog+'No saveset matches for object: '+uhname[ii]
        printf, ll, prog+'No saveset matches for object: '+uhname[ii]
     endelse 
  endfor
  ;
  ; summarize results for log file
  printf, -1, prog+'results of the mask creation: (HIPASSname | maskfile | number of savesets used)'
  printf, ll, prog+'results of the mask creation: (HIPASSname | maskfile | number of savesets used)'
  for ii = 0, nh-1 do begin
     str = uhname[ii]+' | '+fmasko[ii]+' | '+numstr(nmatch[ii])
     printf, -1, str
     printf, ll, str
  endfor 
  ;
  ; summarize usage of savesets
  kk     = where(nused eq 1, nkk)
  printf, -1, prog+'number of savesets that were used just once: ', nkk
  printf, ll, prog+'number of savesets that were used just once: ', nkk
  kk     = where(nused eq 0, nkk)
  if nkk gt 0 then begin
     printf, -1, prog+'the following savesets were not used at all (total not used = '+numstr(nkk)+'): '
     printf, ll, prog+'the following savesets were not used at all (total not used = '+numstr(nkk)+'): '
     for ii = 0, nkk-1 do begin
        jj = kk[ii]
        printf, -1, resu[kk[ii]]
        printf, ll, resu[kk[ii]]
     endfor
  endif
  kk     = where(nused gt 1, nkk)
  if nkk gt 0 then begin
     printf, -1, prog+'the following savesets were used multiple times (total multiply used = '+numstr(nkk)+'): '
     printf, ll, prog+'the following savesets were used multiple times (total multiply used = '+numstr(nkk)+'): '
     for ii = 0, nkk-1 do begin
        jj = kk[ii]
        printf, -1, resu[kk[ii]]+' | '+numstr(nused[kk[ii]])
        printf, ll, resu[kk[ii]]+' | '+numstr(nused[kk[ii]])
     endfor
  endif
  ;
  ; match names to rpc files
  ;printf, -1, prog+'matching names to rpc files'
  ;printf, ll, prog+'matching names to rpc files'
  ;nmtchh = make_array(nh, /long, value=0l)
  ;nmtchc = nmtchh
  ;nmtcho = nmtchh
  ;
  ;FOR ii = 0, nh-1 DO BEGIN 
  ;   pp         = strpos(resu,uhname[ii])
  ;   jj         = where(pp GE 0, njj)
  ;   nmtchh[ii] = njj
  ;   pp         = strpos(resu,ucname[ii])
  ;   jj         = where(pp GE 0, njj)
  ;   nmtchc[ii] = njj
  ;   pp         = strpos(resu,uoname[ii])
  ;   jj         = where(pp GE 0, njj)
  ;   nmtcho[ii] = njj
  ;ENDFOR 
  ;

end 
