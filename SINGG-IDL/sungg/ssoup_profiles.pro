PRO ssoup_profiles, ll, fimages, fmask, hname, fprof, ngal, verbose=verbose, shapepar=shapepar
  ;
  ;  ll       -> logical unit of log file
  ;  fimages  -> array list of input fits images.
  ;              These should be aligned, and presumably are at
  ;              different wavelengths.
  ;  fmask    -> name of fits image of mask to apply to all images.
  ;  hname    -> hipass name of object to analyze, 
  ;              this can include multiple ELGs
  ;  fprofs   -> name of output ascii profile files, one for each file
  ;              in fimages.
  ;  shapepar -> If set determines the basis for getting the shape 
  ;              parameters.  This is a string (which is converted 
  ;              to uppercase) with the following options
  ;              OPT : take optical parameters from singg_derived
  ;                    (default)
  ;              UV  : take uv parameters from sungg_derived
  ;              AVG : take average of optical and UV
  ; ngal      <- number of galaxies detected
  ;
  ; G. Meurer (ICRAR/UWA) 5/2010 - first written
  ; G. Meurer (ICRAR/UWA) 6/2011 - fixed bug aa ,bb, abin need to be 
  ;                                in pixels when passed to calc_growth
  ;
  ; prelims
  dbo      = 'singg_derived'       ; get optical measurements from this DB
  dbu      = 'sungg_derived'       ; get UV measurements from this DB
  fmult    = 1.05                  ; at large radii apertures increase in size by this factor
  zmax     = 1.20                  ; maximum aperture size compared to galaxy size
  asdeg    = 3600.0                ; arcsec / degree
  sky      = 0.0                   ; kludged...
  proftype = 'aligned'
  headline = '# profiles from aligned images convolved to common seeing and pixel grid'
  prog     = 'SSOUP_PROFILES: '
  COMMON bands, band, nband, bandnam, bandavail, nbandavail
  ;
  plog,ll,prog,'------------------------- starting '+prog+'---------------------------------'
  ;
  vb       = keyword_set(verbose)
  ;
  ; decide on shape parameter
  spdef    = 'OPT'
  sp       = spdef      ; for now just taking optical size
  if keyword_set(shapepar) then begin 
     sp    = strupcase(shapepar)
     if sp ne 'UV' and sp ne 'AVG' and sp ne 'OPT' then sp = spdef
  endif 
  ;
  ; get a fiducial header
  fid = where(bandavail eq band.R, /null)
  plog,ll,prog,'reading fiducial header from '+fimages[fid]
  fits_read, fimages[fid], img, hdr, /header_only
  ;
  ; get some quantities from header: seeing, pixel scale
  seeing  = sxpar(hdr, 'seeing')
  IF seeing LE 0.0 THEN seeing = 5.3
  minstep = 0.5*seeing
  getrot, hdr, dum, cdelt
  as_pix  = asdeg*sqrt(abs(cdelt[0]*cdelt[1]))
  ;
  ; open singg measurements database
  plog,ll,prog,'getting optical quantities from '+dbo
  dbopen, dbo
  list    = good_derived3()     
  ;
  ; get all source names and corresponding HIPASS name
  dbext, list, 'name,object', sname,object  
  object   = strtrim(object,2)
  sname    = strtrim(sname,2)
  ;
  ; find matching hipass name
  pp       = where(object EQ hname, nelg)
  IF nelg LE 0 THEN plog,ll,prog,' **** No galaxies to measure' $
               ELSE plog,ll,prog,'Found '+strtrim(nelg,2)+' galaxies to measure'
  ;
  ; get list of ELGs sorted by name
  qq       = sort(sname[pp])
  ento     = list[pp[qq]]
  sname    = sname[pp[qq]]
  ;
  ; get optical parameters
  dbext, ento, 'ra,dec,axerat,pa,rmax_f', ra,dec,arat,pa,rmax
  ;
  ; close optical database, open UV database
  dbclose
  plog,ll,prog,'getting UV quantities from '+dbu
  dbopen, dbu
  ;
  ; check for matches in UV database
  list     = dbfind('filter = fuv')
  entu     = dbmatch('entry_singg_derivd', ento, list)
  qq       = where(entu GT 0, nqq)
  plog,ll,prog,'number of matches in UV database = '+strtrim(nqq,2)
  ;
  IF nqq GT 0 THEN BEGIN 
     ;
     ; get UV parameters
     dbext, entu,'ra,dec,axerat,pa,fluxrad_brt',rau,decu,aratu,pau,rmaxu
     ;
     ; settle on parameters for extraction
     jj    = where(entu GT 0 AND rmaxu GT rmax, njj)    ; take maximum of optical
     IF njj GT 0 THEN rmax[jj] = rmaxu[jj]              ; and UV radius
     jj    = where(entu GT 0, njj)     
     ;
     ; adjust aperture shape if shapepar NE OPT, 
     ; and there are good UV ap shapes
     IF njj GT 0 and sp ne 'OPT' THEN BEGIN
        ;
        ; we are already set to use OPT shape parameters,
        ; change to UV or AVG if requested
        if sp eq 'AVG' then begin 
           arat[jj] = 0.5*(aratu[jj] + arat[jj])        ; average optical & UV axial ratio
           pa[jj]   = 0.5*(pau[jj] + pa[jj])            ; and position angle
        endif else begin 
           arat[jj] = aratu[jj]                         ; take UV aperture ax ratio
           pa[jj]   = pau[jj]                           ; and position angle
        endelse 
     ENDIF 
  ENDIF 
  nelg    = n_elements(ra)
  ;
  ; close UV database
  dbclose
  ;
  ; check that objects are in FOV of input images
  plog,ll,prog,'checking that sources are in images'
  adxy, hdr, ra, dec, xc, yc
  nx       = sxpar(hdr, 'naxis1')
  ny       = sxpar(hdr, 'naxis2')
  jj       = where(xc ge 0.0 and xc le float(nx-1) and yc ge 0.0 and yc le float(ny-1), njj)
  plog,ll,prog,'number of sources in image = '+strtrim(njj,2)
  if njj lt nelg then begin 
     sname = sname[jj]
     ento  = ento[jj]
     entu  = entu[jj]
     ra    = ra[jj]
     dec   = dec[jj]
     arat  = arat[jj]
     pa    = pa[jj]
     rmax  = rmax[jj]
     rau   = rau[jj]
     decu  = decu[jj]
     artau = artau[jj]
     pau   = pau[jj]
     rmaxu = rmaxu[jj]
     xc    = xc[jj]
     yc    = yc[jj]
     nelg  = njj
  endif
  ;
  ; read in mask
  plog,ll,prog,'reading in mask image '+fmask
  fits_read, fmask, mask, hdm
  ;
  ; Loop through images
  FOR jj = 0, nbandavail-1 DO BEGIN 
     plog,ll,prog,'Working on image: '+fimages[jj]
     ;
     ; read in image
     fits_read, fimages[jj], img, hdr
     ;
     ; get some stuff from header
     IF bandavail[jj] EQ band.HALPHA THEN BEGIN 
        fscale = sxpar(hdr, 'photflux', count=nmtch) 
        units = 'erg/cm^2/s'
     ENDIF ELSE BEGIN 
        fscale = sxpar(hdr, 'photflam', count=nmtch)
        units = 'erg/cm^2/Angstrom/s'
     ENDELSE 
     IF nmtch LE 0 THEN BEGIN 
        units = 'unknown'
        fscale = 1.0
     ENDIF 
     ;
     ; open output ascii file
     plog,ll,prog,'opening output ASCII file for this image: '+fprof[jj]
     openw, lu, fprof[jj], /get_lun
     ;
     ; write header for output file
     write_profile_header, lu, fimages[jj], as_pix, units, fscale, proftype, headline, nelg
     FOR ii = 0, nelg-1 DO BEGIN 
        plog,ll,prog,'measuring source number: '+strtrim(ii,2)
        ;
        ; make scaled aperture radii array 
        rrmax    = max(rmax)                 ; take max radius, for calculating size of arrays 
        zbin     = create_zbin(minstep, fmult, rmax[ii], zmax)
        abin     = zbin*rmax[ii]/as_pix      ; convert to pixels
        aa       = rmax[ii]/as_pix           ; convert to pixels
        bb       = rmax[ii]/(as_pix*arat[ii]) ; convert to pixels
        ;
        ; call calc_growth to get profiles
        netsky   = 0.0
        plog,ll,prog,'calling calc_growth'
        calc_growth, img, mask, zbin, xc[ii], yc[ii], netsky, pa[ii], aa, bb, $
                     ngood, nbad, flux, flux_good, sb, sbsig
        ;
        ; write profiles
        sky      = [0.0, 0.0, 0.0]  ; kludge
        fout     = [0.0, 0.0, 0.0]  ; kludge
        del_pa   = 0.0
        plog,ll,prog,'calling growth_profile'
        growth_profile, lu, ii, xc[ii], yc[ii], arat[ii], pa[ii], del_pa, sky, aa, aa, aa, $
                        abin, flux, ngood, nbad, flux_good, sb, sbsig, fout
     ENDFOR 
     plog,ll,prog,'closing output ASCII file'
     free_lun, lu
  ENDFOR
  ngal = nelg
END 
