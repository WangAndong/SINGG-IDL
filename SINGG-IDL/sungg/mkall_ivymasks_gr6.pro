pro mkall_ivymasks_gr6
  ;
  ; make Ivy's mask for GR6 data.
  ;
  ; Much of this code was prototyped in test_ivy_masknames.pro
  ;
  ; G. Meurer (ICRAR/UWA) 12/2011
  ;
  prog       = 'MKALL_IVYMASKS_GR6: '
  flog       = 'mkall_ivymasks_gr6.log'
  wd         = '/Volumes/data/meurer/SUNGG/GR6/'
  udb1       = 'sungg_derived_2010'
  sstrmsk    = 'mask.uv.dat'
  sstrfuv    = '-fd-int.fits'
  sstrnuv    = '-nd-int.fits'
  sfxmasko   = '_uv_mask.fits'
  sfxmaska   = '_all_uv_mask.fits'
  filo1      = 'maskmatch_single.dat'
  filo2      = 'maskmatch_multiple.dat'
  glxrad     = 0.65
  ;
  ; special matches (hardwired)
  snamsp     = ['J0403-43:S1', 'J0403-43:S2', 'J1339-31:S1', 'J1339-31:S2']
  efuvsp     = [260, 261, 348, 347]
  enuvsp     = [66, 67, 154, 153]
  ;
  ; open log, start
  printf, -1, prog+'starting, opening '+flog
  openw, ll, flog, /get_lun
  ;
  ; go to workingdirectory
  plog, ll, prog, 'going to working directory: '+wd
  cd, wd, current=cwd
  ;
  ; get mask file names
  plog, ll, prog, 'looking for masks using search string: '+sstrmsk
  film       = file_search('*'+sstrmsk, count=nfilm)
  plog, ll, prog, 'number of mask files found: '+numstr(nfilm)
  ;
  ; trim object name guess from first part of file name
  plog, ll, prog, 'guessing object names from mask names'
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
  plog, ll, prog, 'opening database: '+udb1
  dbopen, udb1
  plog, ll, prog, 'getting properties of all FUV entries...'
  listf      = dbfind('filter = fuv')
  dbext, listf, 'hipname,sname,optid,filename,entry_othfilt,entry_singg_derivd', hname, sname, optid, fnamef, listn, listof
  dbext, listn, 'filename, entry_singg_derivd', fnamen, liston
  nf         = n_elements(fnamef)
  plog, ll, prog, 'number of FUV entries in '+udb1+' : '+numstr(nf)
  ;
  ; match objects
  plog, ll, prog, 'matching masks to database '
  for ii = 0, nfilm-1 do begin 
     meth = '*UNMATCHED*'
     ;
     ; Try by matching optical names
     jj      = strpos(optid,oname[ii])
     kk      = where(jj ge 0, nkk)
     if nkk gt 0 then begin 
        entfuv[ii] = listf[kk[0]]
        nfuv[ii]   = nkk
        if nkk gt 1 then begin 
           mm = where(listof[kk] gt 0, nmm)
           if nmm eq 1 then begin 
              mm = kk[mm]
              entfuv[ii] = listf[mm]
              nfuv[ii]   = nmm
           endif
        endif
        meth = 'optid'
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
              mm = where(listof[kk] gt 0, nmm)
              if nmm eq 1 then begin 
                 mm = kk[mm]
                 entfuv[ii] = listf[mm]
                 nfuv[ii]   = nmm
              endif
           endif
           meth = 'sname'
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
              mm = where(listof[kk] gt 0, nmm)
              if nmm eq 1 then begin 
                 mm = kk[mm]
                 entfuv[ii] = listf[mm]
                 nfuv[ii]   = nmm
              endif
           endif
           meth = 'filename'
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
           meth = 'special'
        endif
     endif 
     plog, ll, prog, 'result of matching: file = '+film[ii]+' | method = '+meth+' | FUV entry = '+numstr(entfuv[ii])+' | nmatches = '+numstr(nfuv[ii])
  endfor
  ;
  kk = where(nfuv ne 1, nkk)
  if nkk gt 0 then begin 
     plog, ll, prog, 'the following cases have nmatches NE 1: '
     for ii = 0, nkk-1 do begin
        plog, ll, prog, '    '+film[kk[ii]]+'  '+oname[kk[ii]]+'  '+numstr(nfuv[kk[ii]])
     endfor 
  endif 
  ;
  ; rextract quantities from UV database
  plog, ll, prog, 're-extracting quantities from database: '
  dbext, entfuv, 'hipname,sname,optid,filename,entry_othfilt,entry_singg_derivd,ra,dec', hname, sname, optid, fnamef, entnuv, entof, ra, dec
  dbext, entnuv, 'filename,entry_singg_derivd', fnamen, enton
  dbclose
  ;
  ; save entries where we know RA & Dec
  jj      = where(entfuv gt 0, nsrc)
  plog, ll, prog, 'saving data on '+numstr(nsrc)+' mask files with good matches' 
  film    = strtrim(film[jj],2)
  oname   = strtrim(oname[jj],2)
  entfuv  = entfuv[jj]
  hname   = strtrim(hname[jj],2)
  sname   = sname[jj]
  optid   = optid[jj]
  fnamef  = strtrim(fnamef[jj],2)
  entnuv  = entnuv[jj]
  entof   = entof[jj]
  ra      = ra[jj]
  dec     = dec[jj]
  fnamen  = strtrim(fnamen[jj],2)
  enton   = enton[jj]
  ;
  ; grab all FUV file names from GR6 directory
  plog, ll, prog, 'getting fuv file names in working directory using search string: '+sstrfuv
  filfuv  = file_search('*'+sstrfuv, count=nffuv)
  plog, ll, prog, 'Number of FUV files found = '+numstr(nffuv)
  ;
  ; check for NUV matches
  plog, ll, prog, 'checking for NUV matches'
  filnuv  = make_array(nffuv, /string, value='')
  for ii = 0, nffuv-1 do begin 
     jj   = strpos(filfuv[ii],sstrfuv)
     test = strmid(filfuv[ii],0,jj)+sstrnuv
     filn = file_search(test, count=nfiln)
     case nfiln of 
        0 : plog, ll, prog, 'no NUV match to: '+filfuv[ii]
        1 : filnuv[ii] = filn[0]
        else : begin
              filnuv[ii] = filn[0]
              plog, ll, prog, 'warning multiple NUV file matches to '+filfuv[ii]
           end 
     endcase
  endfor 
  ;
  ; arrays to record stuff
  ;  idxf   -  index from source name to FUV file with center nearest
  ;            to source
  ;  minsep -  (min) separation source to filed center
  ;  nmtch  -  number of matches
  idxf     = make_array(nsrc, /int, value=-1)
  minsep   = make_array(nsrc, /float, value=999.99)
  nmtch    = make_array(nsrc, /int, value=0)
  fmasko   = make_array(nsrc, /string, value='')
  ;
  ; loop through UV files
  plog, ll, prog, 'looping through fits images and finding which sources from database are in them'
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
        plog, ll, prog, 'number of closest matches in file - '+filfuv[ii]+' = '+numstr(nkk)
        if nkk gt 0 then begin 
           minsep[jj[kk]] = dis[jj[kk]]
           idxf[jj[kk]]   = ii
           nmtch[jj[kk]]  = nmtch[jj[kk]]+1
        endif 
     endif 
  endfor
  ;
  ; say something about the case that are not working so well
  kk        = where(nmtch ne 1, nkk)
  if nkk gt 0 then begin 
     plog, ll, prog, 'the following cases have nmatches NE 1: '
     for ii = 0, nkk-1 do begin
        plog, ll, prog, '    '+film[kk[ii]]+'  '+numstr(idxf[kk[ii]])+'  '+numstr(nmtch[kk[ii]])
     endfor 
     kk      = where(nmtch le 0, nkk)
     plog, ll, prog, 'Number of cases with no matches = '+numstr(nkk)
  endif else begin 
     plog, ll, prog, 'all cases have nmatches EQ 1'
  endelse 
  ;
  ; loop through cases and make individual masks
  ; and write first output file on the
  ; mask matching of single files
  plog, ll, prog, 'making individual masks...'
  plog, ll, prog, 'will write mask matching results for single masks in: '+filo1
  openw, lu, filo1, /get_lun
  for ii = 0, nsrc-1 do begin 
     fmasko[ii] = oname[ii]+sfxmasko
     printf, lu, ljust(sname[ii],14)+ljust(hname[ii],11)+ljust(optid[ii],31)+ljust(film[ii],30)+$
             ljust(filfuv[idxf[ii]],40)+ljust(filnuv[idxf[ii]],40)+ljust(fmasko[ii],30),$
             format='(a)'
     plog, ll, prog, 'creating mask:  '+fmasko[ii]+'  from file:  '+film[ii]+'  based on fits image:  '+filfuv[idxf[ii]]
     fits_read, filfuv[idxf[ii]], img, hdr, /header_only
     create_ivy_mask, film[ii], hdr, mask, fmasko=fmasko[ii]
  endfor 
  free_lun, lu
  ;
  ; index files by hipass name,
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
  plog, ll, prog, 'number of HIPASS sources with multiple masks: '+numstr(nmult)
  ;
  ; make multiple masks
  if nmult gt 0 then begin 
     plog, ll, prog, 'will write results of multiple mask creation to file: '+filo2
     plog, ll, prog, 'making multiple masks...'
     openw, lu, filo2, /get_lun
     fmaska = make_array(nmult, /string, value='')
     fffits = make_array(nmult, /string, value='')
     fnfits = make_array(nmult, /string, value='')
     for ii = 0, nmult-1 do begin 
        jj = where(hname eq uhname[mult[ii]], njj) 
        ; print, ii, '   '+uhname[mult[ii]]+'   ', njj
        if njj gt 1 then begin 
           fssets     = film[jj]
           fmaska[ii] = uhname[mult[ii]]+sfxmaska
           fffits[ii] = filfuv[idxf[jj[0]]]
           fnfits[ii] = filnuv[idxf[jj[0]]]
           kk         = where(filfuv[idxf[jj]] ne fffits[ii], nkk)
           if nkk gt 0 then plog, ll, prog, 'caution not all fits files match '+fffits[ii]+' nmismatch = '+numstr(nkk)
           printf, lu, ljust(uhname[mult[ii]],11)+ljust(fffits[ii],40)+ljust(fnfits[ii],40)+ljust(fmaska[ii],28),njj,nkk,$
             format='(a,i3,i3)'
           plog, ll, prog, 'creating mask:  '+fmaska[ii]+'  from njj = '+numstr(njj)+' saveset files based on fits image:  '+fffits[ii]
           fits_read, fffits[ii], img, hdr, /header_only
           create_ivy_mask, fssets, hdr, mask, fmasko=fmaska[ii]
        endif else begin
           plog, ll, prog, "that's odd.  "+uhname[mult[ii]]+"   should be multiple but njj = "+numstr(njj)
        endelse 
     endfor
     free_lun, lu
  endif 
  ; ---------------------------------------------------------------
  ; finish
  plog, ll, prog, 'returning to starting directory: '+cwd
  cd, cwd
  plog, ll, prog, 'finished'
  free_lun, ll
end 
