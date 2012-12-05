PRO zp_detim
   ;
   ; calculate zeropoint for a detection image assuming a flat f_nu 
   ; spectrum.
   ;
   ; m_detect = m_0,detect - 2.5*log(C_detect/t_total)  [ABmag]
   ;
   ; where m_0,detect is the detection image ABmag zeropoint
   ;
   ; the detection image is inverse variance weighted so that the total
   ; weight = 1.0
   ;
   ; w_i = rms_i^(-2)/total_over_j(rms_j^(-2))
   ;
   ; take
   ; t_i = exposure time
   ; lambda_i = pivot wavelength of filter in angstroms
   ; plam_i = photflam of filter
   ;
   ; then it is easy to show that
   ;
   ; m_0,detect = - 48.60 + 2.5*log(total(w_i*t_i/pnu_i)/total(t_i))
   ; 
   ; where pnu_i = 1.0e-11*(5470/lambda_i)^2
   ;
   filt    = ['F775W ', 'F850LP']
   rms     = [15.72, 19.32]
   lambda  = [7699.3, 9054.3]
   t       = [4500.0, 6800.0]
   plam    = [1.0179476E-19, 1.5341358E-19]
   c       = 2.9979e18
   ;
   pnu     = 1.0e-11*plam*(5470.0/lambda)^2
   mab0    = -48.60 - 2.5*alog10(pnu)
   w       = rms^(-2)
   wtot    = total(w)
   w       = w / wtot
   f       = w*t/pnu
   zp2     = -48.60 + 2.5*alog10(total(f))
   zp      = zp2 - 2.5*alog10(total(t))
   forprint, filt, rms, w, lambda, t, pnu, mab0, f
   print, zp, zp2
END 
