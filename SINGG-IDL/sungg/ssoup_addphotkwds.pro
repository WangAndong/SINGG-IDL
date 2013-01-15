pro ssoup_addphotkwds, bname, hdr
  ;
  ; add photometric keywords to a header based on the observed band
  ;
  ; G. Meurer 6/2010
  COMMON bands, band
  mag0     = [18.82, 20.08]
  emag0    = [0.05, 0.03]
  plam     = [1535.1, 2300.8]
  photflam = 10.0^(-0.4*(mag0 + 21.1))*(5500.0/plam)^2.0
  ; see http://wise2.ipac.caltech.edu/docs/release/allsky/expsup/sec2_3f.html and
  ; http://wise2.ipac.caltech.edu/docs/release/allsky/expsup/sec4_4h.html#WISEZMA
  photwise = [8.1787e-12*10^(-0.4*20.5), 2.4150e-12*10^(-0.4*19.5), 6.5151e-14*10^(-0.4*18), 5.0901e-15*10^(-0.4*13.5) ]
  ;
  case bname of
     band.FUV: begin
        fxaddpar, hdr, 'FILTNAME', band.FUV, 'Filter name'
        fxaddpar, hdr, 'MAGSYS1', 'ABMAG', 'Magnitude System'
        fxaddpar, hdr, 'MAGZPT1', mag0[0], 'Magnitude zeropoint'
        fxaddpar, hdr, 'ERRZPT1', emag0[0], 'rms uncertainty in zeropoint'
        fxaddpar, hdr, 'MAGREF1', '2007ApJS..173..682M', 'Bibcode reference for photometric calibration'
        fxaddpar, hdr, 'EXTCOEF1', 0.0, 'Extinction coeficient [mag/airmass]'
        fxaddpar, hdr, 'PHOTPLAM', plam[0], 'Filter pivot wavelength [Angstrom]'
        fxaddpar, hdr, 'PHOTFLAM', photflam[0], 'Unit response [(erg/cm^2/Angstrom/s)/(count/s)]'
        fxaddpar, hdr, 'PHOTMTRC', 'T', 'Observed in photometric conditions (always T for GALEX)'
     end
     band.NUV: begin
        fxaddpar, hdr, 'FILTNAME', band.NUV, 'Filter name'
        fxaddpar, hdr, 'MAGSYS1', 'ABMAG', 'Magnitude System'
        fxaddpar, hdr, 'MAGZPT1', mag0[1], 'Magnitude zeropoint'
        fxaddpar, hdr, 'ERRZPT1', emag0[1], 'rms uncertainty in zeropoint'
        fxaddpar, hdr, 'MAGREF1', '2007ApJS..173..682M', 'Bibcode reference for photometric calibration'
        fxaddpar, hdr, 'EXTCOEF1', 0.0, 'Extinction coeficient [mag/airmass]'
        fxaddpar, hdr, 'PHOTPLAM', plam[1], 'Filter pivot wavelength [Angstrom]'
        fxaddpar, hdr, 'PHOTFLAM', photflam[1], 'Unit response [(erg/cm^2/Angstrom/s)/(count/s)]'
        fxaddpar, hdr, 'PHOTMTRC', 'T', 'Observed in photometric conditions (always T for GALEX)'
     end
     band.mir_W1: begin
        fxaddpar, hdr, 'PHOTFLAM', photwise[0], 'Unit response [(erg/cm^2/Angstrom/s)/(count/s)]'
     end
     band.mir_W2: begin
        fxaddpar, hdr, 'PHOTFLAM', photwise[1], 'Unit response [(erg/cm^2/Angstrom/s)/(count/s)]'
     end
     band.mir_W3: begin
        fxaddpar, hdr, 'PHOTFLAM', photwise[2], 'Unit response [(erg/cm^2/Angstrom/s)/(count/s)]'
     end
     band.mir_W4: begin
        fxaddpar, hdr, 'PHOTFLAM', photwise[3], 'Unit response [(erg/cm^2/Angstrom/s)/(count/s)]'
     end
     else:   begin
        fxaddpar, hdr, 'MAGREF1', '2006ApJS..165..307M', 'Bibcode reference for photometric calibration'
     end 
  endcase 
  fxaddpar, hdr, 'BUNIT', 'counts/sec', 'pixel values are in these units'
end 
