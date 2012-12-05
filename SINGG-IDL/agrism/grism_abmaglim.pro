FUNCTION grism_abmaglim, sigsky, nsig, ncd, ndisp, exptime, angstpix, sensfile, $
                         lampeak=lampeak, speak=speak
   ;
   ; calculate limiting AB magnitude for continuum observations at the
   ; peak throughput of the grism
   ;
   ; sigsky    -> noise or rms of background in grism image
   ; nsig      -> significance of detection 
   ;              (e.g. nsig=3 calculates 3 sigma detection level)
   ; ncd       -> aperture dimension in cross dispersion direction
   ; ndisp     -> aperture dimension in dispersion direction
   ; exptime   -> exposure time in sec.
   ; angstpix  -> dispersion of order in angstroms/pixel
   ; sensfile  -> fits table file containing sensitivity curve.
   ; lampeak   <- if set, returned as the wavelength of peak 
   ; speak     <- if set, returned as peak sensitivity
   ;
   ; G. Meurer 07/2004
   ;
   s       = mrdfits(sensfile,1,hsens)
   lams    = s.wavelength
   sens    = s.sensitivity
   k       = reverse(sort(sens))
   lampeak = lams[k[0]]
   speak   = sens[k[0]]
   lim     = -21.1 - 2.5*alog10(sigsky*float(nsig)*sqrt(float(ncd)/float(ndisp))*(lampeak/5470.)^2/$
                                (exptime*speak*angstpix))
   return,lim
END 
