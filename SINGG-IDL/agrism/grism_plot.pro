PRO grism_plot, file, ext, id, prefix, hdr, bintab, orient=orient
   ; Extract a grism spectrum from binary table and plot it
   ;
   ; file   -> fits file containing binary fits table extensions, 
   ;           one for each spectrum.
   ; ext    -> the extension number to extract
   ; id     -> id #.
   ; prefix -> begining of file name for output .png file.  
   ;           If prefix = '' then no hardcopy is made.
   ; hdr    <- header of extension
   ; bintab <- binary table structure
   ; orient -> if set then contains a three element vector 
   ;           [a, b, theta], where a & b are the
   ;           semi-minor and semi-major axis lengths
   ;           and theta is the angle wrt to the spectrum trace.
   ;
   xrange  = [5600.0, 10200.0]
   cmaxdef = 150.0
   xtitle  = 'Wavelength [Angstroms]'
   ytitle  = 'Counts'
   count   = 0
   ;
   bintab  = mrdfits(file, ext, hdr)
   grism_plot1d, bintab, id, orient=orient, lrange=xrange
END 
