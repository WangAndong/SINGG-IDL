pro test_ivy_masknames
  wd         = '/Volumes/data/meurer/SUNGG/GR6/'
  udb1       = 'sungg_derived_2010'
  sstrmsk    = 'mask.uv.dat'
  sstrfuv    = '-fd-int.fits'
  glxrad     = 0.65
  ;
  ; special matches 
  snamsp     = ['J0403-43:S1', 'J0403-43:S2', 'J1339-31:S1', 'J1339-31:S2']
  efuvsp     = [260, 261, 348, 347]
  enuvsp     = [66, 67, 154, 153]
  ;
  ; go to working directory
  cd, wd
  ;
  ; get mask file names
  film       = file_search('*'+sstrmsk, count=nfilm)
  print, 'number of mask files = '+numstr(nfilm)
  ;
  ; trim object name guess from first part of file name
  oname      = make_array(nfilm, /string, value='')
  entfuv     = make_array(nfilm, /int, value=-1)
  nfuv       = make_array(nfilm, /int, value=0)
  for ii = 0, nfilm-1 do begin 
     tstr    = film[ii]
     jpos    = strpos(tstr,sstrmsk)
     if jpos ge 0 then begin 
        oname[ii] = strmid(tstr,0,jpos)
        ;print,tstr+'  '+oname[ii]
     endif 
     if strmid(oname[ii],0,4) eq 'NGA_' then oname[ii] = strmid(oname[ii],4)
  endfor 
  ;
  ; get data from uv database
  dbopen, udb1
  listf      = dbfind('filter = fuv')
  dbext, listf, 'hipname,sname,optid,filename,entry_othfilt,entry_singg_derivd', hname, sname, optid, fnamef, listn, listof
  dbext, listn, 'filename, entry_singg_derivd', fnamen, liston
  nf         = n_elements(fnamef)
  print, 'number of FUV entries in '+udb1+' : '+numstr(nf)
  ;
  ; match objects
  for ii = 0, nfilm-1 do begin 
     ;
     ; Try by matching optical names
     jj      = strpos(optid,oname[ii])
     kk      = where(jj ge 0, nkk)
     if nkk gt 0 then begin 
        entfuv[ii] = listf[kk[0]]
        nfuv[ii]   = nkk
        if nkk gt 1 then begin 
           ll = where(listof[kk] gt 0, nll)
           if nll eq 1 then begin 
              ll = kk[ll]
              entfuv[ii] = listf[ll]
              nfuv[ii]   = nll
           endif
        endif
     endif
     if nfuv[ii] le 0 then begin
        ;
        ; try by matching singg name
        jj   = strpos(sname,oname[ii])
        kk   = where(jj ge 0, nkk)
        if nkk gt 0 then begin 
           entfuv[ii] = listf[kk[0]]
           nfuv[ii]   = nkk
           if nkk gt 1 then begin 
              ll = where(listof[kk] gt 0, nll)
              if nll eq 1 then begin 
                 ll = kk[ll]
                 entfuv[ii] = listf[ll]
                 nfuv[ii]   = nll
              endif
           endif
        endif
     endif 
     if nfuv[ii] le 0 then begin
        ;
        ; try by matching filename
        jj   = strpos(fnamef,oname[ii])
        kk   = where(jj ge 0, nkk)
        if nkk gt 0 then begin 
           entfuv[ii] = listf[kk[0]]
           nfuv[ii]   = nkk
           if nkk gt 1 then begin 
              ll = where(listof[kk] gt 0, nll)
              if nll eq 1 then begin 
                 ll = kk[ll]
                 entfuv[ii] = listf[ll]
                 nfuv[ii]   = nll
              endif
           endif
        endif
     endif
     if nfuv[ii] le 0 then begin 
        ;
        ; try special matches
        jj   = strpos(snamsp,oname[ii])
        kk   = where(jj ge 0, nkk)
        if nkk gt 0 then begin 
           entfuv[ii] = efuvsp[kk]
           nfuv[ii]   = nkk
        endif
     endif 
     print, film[ii]+'  '+oname[ii], entfuv[ii], nfuv[ii]
  endfor
  kk = where(nfuv ne 1, nkk)
  forprint,kk, '   '+film[kk]+'  '+oname[kk], nfuv[kk]
  ;
  ; rextract quantities from UV database
  dbext, entfuv, 'hipname,sname,optid,filename,entry_othfilt,entry_singg_derivd,ra,dec', hname, sname, optid, fnamef, entnuv, entof, ra, dec
  dbext, entnuv, 'filename,entry_singg_derivd', fnamen, enton
  dbclose
  ;
  ; save entries where we know RA & Dec
  jj      = where(entfuv gt 0, nsrc)
  film    = film[jj]
  oname   = oname[jj]
  entfuv  = entfuv[jj]
  hname   = hname[jj]
  sname   = sname[jj]
  optid   = optid[jj]
  fnamef  = fnamef[jj]
  entnuv  = entnuv[jj]
  entof   = entof[jj]
  ra      = ra[jj]
  dec     = dec[jj]
  fnamen  = fnamen[jj]
  enton   = enton[jj]
  ;
  ; grab all FUV file names from GR6 directory
  filfuv  = file_search('*'+sstrfuv, count=nffuv)
  print, 'Number of FUV files found = '+numstr(nffuv)
  ;
  ; arrays to record stuff
  ;  idxf   -  index from source name to FUV file with center nearest
  ;            to source
  ;  minsep -  (min) separation source to filed center
  ;  nmtch  -  number of matches
  idxf     = make_array(nsrc, /int, value=-1)
  minsep   = make_array(nsrc, /float, value=999.99)
  nmtch    = make_array(nsrc, /int, value=0)
  ;
  ; loop through UV files
  for ii = 0, nffuv-1 do begin
     ;
     ; read file header
     fits_read, filfuv[ii], img, hdr, /header_only
     ;
     ; get field center
     ractr  = sxpar(hdr, 'RA_CENT')
     decctr = sxpar(hdr, 'DEC_CENT')
     ;
     ; Determine which sources are in this file
     gcircd,2,ractr,decctr,ra,dec,dis
     jj     = where(dis le glxrad, njj)
     if njj gt 0 then begin 
        ;
        ; update matches that are the closest
        kk  = where(dis[jj] le minsep[jj], nkk)
        print, 'number of closest matches in file - '+filfuv[ii]+' = '+numstr(nkk)
        if nkk gt 0 then begin 
           minsep[jj[kk]] = dis[jj[kk]]
           idxf[jj[kk]]   = ii
           nmtch[jj[kk]]  = nmtch[jj[kk]]+1
        endif 
     endif 
  endfor
  ;
  ; report cases of no matches
  jj       = where(nmtch gt 0, njj)
  print, 'Number of cases with matches: '+numstr(njj)
  if njj gt 0 then begin 
     forprint, ljust(film[jj],30)+'  '+ljust(oname[jj],20)+'  '+ljust(filfuv[idxf[jj]],30),nmtch[jj],minsep[jj],idxf[jj] 
  endif
  jj       = where(nmtch le 0, njj)
  print, 'Number of cases with no matches: '+numstr(njj)
  if njj gt 0 then begin 
     forprint, ljust(film[jj],30)+'  '+ljust(oname[jj],20)+'  ',nmtch[jj],minsep[jj],idxf[jj] 
  endif
  stop
end
