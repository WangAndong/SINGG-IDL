pro ssoup_psp_indecis, ll, nam, rad, sm, esm, flag, eflag, lflag, uflag, $
                       rmax, irmax, ij, ik, il, im, nij, nik, nil, nim
  ;
  ; Detemine which indecis of a surface brightness profile should
  ; be plotted and how.  This code is required multiple times in 
  ; ssoup_plotsprofs_new so is broken into its own procedure
  ;
  ;  ll    -> logical unit for log file
  ;  nam   -> name of band or colour
  ;  rad   -> radii
  ;  sm    -> surface brightness or colour (logarithmic), or flag
  ;  esm   -> error on sm or flag
  ;  flag  -> elements of sm having this value are bad
  ;  eflag -> elements of esm having this value indicate 
  ;           sm is plottable but not good 
  ;           (i.e. an upper limt in linear surface brightness)
  ;  lflag -> elements of esm having this value indicate 
  ;           sm is a lower limit
  ;  uflag -> elements of esm having this value indicate 
  ;           sm is an upper limit
  ;  rmax  <- maximum radius of plottable points
  ;  irmax <- index of maximum plottable point
  ;  ij    <- indecis where sm is plottable (good or at a limit)
  ;  ik    <- indecis where sm is good
  ;  il    <- indecis where sm is at a lower limit
  ;  im    <- indecis where sm is at an uppet limit
  ;  nij   <- number of elements in ij
  ;  nik   <- number of elements in ik
  ;  nil   <- number of elements in il
  ;  nim   <- number of elements in im
  ;
  ; G. Meurer (ICRAR/UWA) 5/2011
  prog      = 'SSOUP_PSP_INDECIS: '
  plog,ll,prog,'--------------------- starting '+prog+'---------------------------------'
  plog,ll,prog,'processing band/colour = '+nam
  ij        = where(sm ne flag, nij)
  ik        = where(esm ne eflag and esm ne lflag and esm ne uflag, nik)
  il        = where(esm eq lflag, nil)
  im        = where(esm eq uflag, nim)
  IF nij GT 0 THEN BEGIN 
     qq     = reverse(sort(rad[ij]))
     irmax  = ij[qq[0]]
  ENDIF ELSE BEGIN 
     irmax = n_elements(rad)-1
  ENDELSE 
  rmax      = rad[irmax]
  plog,ll,prog,'finished'
end 
