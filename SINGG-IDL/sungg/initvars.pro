; Initializes global variables, so they only have to be updated once (here)

pro initvars
  COMMON bands, band, nband
  band      = ['R', 'HALPHA', 'NUV', 'FUV']
  nband     = n_elements(band)
  
  ; define input structure
  ; ...
  RETURN
END