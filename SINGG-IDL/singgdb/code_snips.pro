pro code_snips
  ;
  ;  This is bits of code from mk_master_sample.pro which are used to match 
  ;  positions with IRAS and akari positions.  I don't guarantee that 
  ;  these snips work in a standalone basis, but I think they do in 
  ;  mk_master_sample.pro.
  ;
  ; note 
  ; * plog writes a line of text to the screen and a log file, which has
  ;        logical unit llog.
  ;
   minsradfir = 1.0  ; arcmin  (buffer to FIR search radius).
   minsradir  = 1.0  ; arcmin
   dbai      = 'akari_irc'               ; AKARI IRC (mid-IR) database
   dbaf      = 'akari_fis'               ; AKARI FIS (far-IR) database
   dbir      = 'iras_psc'                ; IRAS point source catalog
   ;
   sradfir      = make_array(nmast, /float, value=-1.0)
   sradfir[0:nsr2p-1] = ropt    ; ropt is the individual optical sizes and indecis 0:n2sr2p are the ones with optical matches
   jj               = where(entnu GT 0, njj)  ; where there are valid NUV entries (entnu is the NUV entries)
   IF njj GT 0 THEN BEGIN                                     ; and they are bigger than the optical
      kk            = where(sradfir[jj] LT rnuv[jj], nkk)      ; radius then use the NUV radius rnuv
      IF nkk GT 0 THEN sradfir[jj[kk]] = rnuv[jj[kk]]
   ENDIF 
   jj               = where(entfu GT 0, njj)                  ; Do something similar with the FUV radii
   IF njj GT 0 THEN BEGIN                                  
      kk            = where(sradfir[jj] LT rfuv[jj], nkk)      
      IF nkk GT 0 THEN sradfir[jj[kk]] = rfuv[jj[kk]]
   ENDIF 
   jj               = where(sradfir GE 0.0, njj)
   IF njj GT 0 THEN sradfir[jj] = sradfir[jj] + 60.0*minsradfir ; add buffer 
   jj               = where(sradfir LT 0.0, njj)
   IF njj GT 0 THEN sradfir[jj] = 60.0*minsradfir
   ;
   ; so at this stage I hvae a bunch of individual search radii 
   ; equal to the bigger of the UV or optical radius plus a buffer
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
   entiras_old   = entiras    ; these are the old version of iras matches
   entiras       = 0l*entiras - 1l  ; reset iras entries
   FOR ii = 0, nmast-1 DO BEGIN 
      srad       = sradfir[ii]     ; search radius in arcsec for this galaxy
      ;
      ; get separations using gcircd
      gcircd, 2, racat, deccat, ra[ii], dec[ii], sep
      sep        = 3600.0*sep     ; convert from deg to arcsec
      ;
      list8      = where(sep LE srad, n8)  ; get matches within search radius
      IF n8 GT 0 THEN plog, llog, '  ', ljust(hname[ii],15)+'number matching = '+numstr(n8)
      ;
      ; for valid matches store the following IRAS data
      ; - entiras  = entry of closest match
      ; - nmiras   = number of iras matches
      ; - sepiras  = separation to nearest matching source
      IF list8[0] NE -1 THEN BEGIN 
         j0              = where((f60cat[list8] GT 0.0) OR (f100cat[list8] GT 0.0), nj0)
         IF nj0 GT 0 THEN BEGIN 
            ;
            ; take nearest match with 60 or 100 micron flux > 0, if available
            j1           = sort(sep[list8[j0]])
            j2           = list8[j0[j1[0]]]
         ENDIF ELSE BEGIN 
            ;
            ; otherwise just take nearest match
            j1           = sort(sep[list8])
            j2           = list8[j1[0]]
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
      srad       = sradfir[ii] / 60.0
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
      srad       = sradfir[ii] / 60.0
      ; 
      ; Here we can do the searching with dbcircled, but have
      ; to pass the search radius in arcmin
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
   
end
