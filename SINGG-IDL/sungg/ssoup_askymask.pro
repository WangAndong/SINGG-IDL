function ssoup_askymask, ll, hname, hdr, buffer, mask
  ;
  ; Create a sky mask: all pixel eligible to be used in sky 
  ; are marked 0b, the rest are 1b.  These include objects passed in
  ; input mask, hipass targets indicated by their HIPASS name,
  ; and edge pixels.
  ;
  ;  hname    -> name of HIPASS targets expected in FOV.  Used to mask
  ;              them out of the sky measurement
  ;  img      -> image array to be measured
  ;  hdr      -> header of the image to be measured
  ;  buffer   -> number of pixels along edge of frame to mask
  ;  mask     -> input mask, bad=1b, good=0b
  ;
  sdb       = 'singg_derived'
  udb1      = 'sungg_derived_2010'
  udb2      = 'sungg_derived_zz'
  prog      = 'SSOUP_ASKYMASK: '
  ;
  ; default sky mask = input mask 
  skymask   = mask
  ;
  ; get apertures from databases
  plog,ll,prog,'-------------------- starting '+prog+'-----------------------------'
  plog,ll,prog,'getting apertures from databases'
  dbopen, sdb
  listo     = good_derived3()
  listo     = dbmatch('object',hname,listo)
  nobj      = n_elements(listo)
  if nobj le 0 then stop, 'stopping no objects in optical database'
  dbext, listo, 'name,ra,dec,axerat,pa,rmax_s,rmax_c',snameo,ra,dec,ab,pa,rmax,r2
  snameo    = strtrim(snameo,2)
  sname     = snameo
  src       = make_array(nobj,/string,value='SINGG')
  dbclose
  ;
  ; take maximum of flux and S/N apert
  jj        = where(r2 gt rmax, njj)
  if njj gt 0 then rmax[jj] = r2[jj]
  ; 
  ; get matching objects from UV database1
  dbopen, udb1
  ;listu     = dbmatch('sname', snameo)
  listu     = dbfind('filter=FUV')
  listu     = dbmatch('hipname', hname, listu)
  jj        = where(listu gt 0, nobju)
  if nobju gt 0 then begin
     dbext, listu[jj], 'sname,ra,dec,axerat,pa,fluxrad_brt', snameu,rau,decu,abu,pau,rmaxu
     srcu   = make_array(nobj,/string,value='SUNGG1')
     ;
     ; concatenate with other aps
     sname  = [sname, snameu]
     ra     = [ra, rau]
     dec    = [dec, decu]
     ab     = [ab, abu]
     pa     = [pa, pau]
     rmax   = [rmax, rmaxu]
     src    = [src, srcu]
  endif 
  dbclose
  ; 
  ; get matching objects from UV database2
  dbopen, udb2
  ;listu     = dbmatch('sname', snameo)
  listu     = dbfind('filter=FUV')
  listu     = dbmatch('hipname', hname, listu)
  jj        = where(listu gt 0, nobju)
  if nobju gt 0 then begin
     dbext, listu[jj], 'sname,ra,dec,axerat,pa,fluxrad_brt', snameu,rau,decu,abu,pau,rmaxu
     srcu   = make_array(nobj,/string,value='SUNGG2')
     ;
     ; concatenate with other aps
     sname  = [sname, snameu]
     ra     = [ra, rau]
     dec    = [dec, decu]
     ab     = [ab, abu]
     pa     = [pa, pau]
     rmax   = [rmax, rmaxu]
     src    = [src, srcu]
  endif 
  dbclose
  ;
  ; note final list of objects
  nobj      = n_elements(sname)
  plog,ll,prog,'The following are the apertures that will be blanked (dbsrc name ra dec ab pa rmax):'
  for ii = 0, nobj-1 do begin
     printf,-1,'',src[ii]+' '+sname[ii]+' ',ra[ii],dec[ii],ab[ii],pa[ii],rmax[ii]
  endfor 
  ;
  ; get aperture parameters in pixel units
  plog,ll,prog,'converting apertures to pixel coords'
  adxy, hdr, ra, dec, xc, yc         ; transform aperture center
  getrot,hdr,rot,cdelt               ; get pixel scales and rotation of coord sys
  as_pix = 3600.0*sqrt(abs(cdelt[0]*cdelt[1]))  ; convert scale to arcsec/pixel
  plog,ll,prog,'  platescale = '+numstr(as_pix)+'  rotation = '+numstr(rot)
  pa0    = pa + rot                  ; convert PA to be relative to pixel grid
  rmax0  = rmax/as_pix               ; maximum radii in pixels
  sz     = size(mask)
  nxny   = [sz[1],sz[2]]
  ;
  ; Loop through objects and mask
  for ii = 0, nobj-1 do begin 
     plog,ll,prog,'determining object pixels to add to sky mask for obj# '+numstr(ii)
     ;
     ; make array of elliptical distances
     dist_ellipse, dist, nxny, xc[ii], yc[ii], ab[ii], pa0[ii]
     ;
     ; new pixels to mask - within aperture and not yet masked
     jj      = where(dist le rmax0[ii] and (skymask eq 0b), njj)
     if njj gt 0 then skymask[jj] = 1b
     plog,ll,prog,'  added '+numstr(njj)+' pixels to sky mask'
  endfor
  ;
  ; mask edge
  if buffer gt 0 then begin
     plog,ll,prog,'will mask a buffer of '+numstr(buffer)+' pixels around edges'
     ntot    = long(nxny[0])*long(nxny[1])
     xp      = lindgen(ntot) mod nxny[0]
     yp      = lindgen(ntot) / nxny[0]
     jj      = where(xp le buffer and skymask eq 0b, njj0)           ; left edge
     if njj0 gt 0 then skymask[jj] = 1b              
     jj      = where(xp ge nxny[0]-buffer-1 and skymask eq 0b, njj1) ; right edge
     if njj1 gt 0 then skymask[jj] = 1b              
     jj      = where(yp le buffer and skymask eq 0b, njj2)           ; bottom edge
     if njj2 gt 0 then skymask[jj] = 1b              
     jj      = where(yp ge nxny[1]-buffer-1 and skymask eq 0b, njj3) ; top edge
     if njj3 gt 0 then skymask[jj] = 1b              
     njj     = njj0+njj1+njj2+njj3
     plog,ll,prog,'  added '+numstr(njj)+' pixels to sky mask'
  endif 
  ;
  RETURN, skymask
end 
