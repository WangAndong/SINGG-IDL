PRO calc_abmaglim
   ;
   ; drives grism_abmaglim for a variety of useful cases
   ;
   exptime  = 6870.0
   sigsky   = 28.6
   ncd      = 5
   nsig     = 3.
   ndisp    = [5, 5, 5, 3, 5, 5, 5]
   sdir     = '/home/meurer/ACS/Grism/'
   sfile    = ['ACS.WFC.-3rd.sens.1.fits', $
               'ACS.WFC.-2nd.sens.1.fits', $
               'ACS.WFC.-1st.sens.1.fits', $
               'ACS.WFC.0th.sens.1.fits', $
               'ACS.WFC.1st.sens.5.fits', $
               'ACS.WFC.2nd.sens.5.fits', $
               'ACS.WFC.3rd.sens.1.fits']
   lampeak  = make_array(7,/float)
   speak    = make_array(7,/float)
   ablim    = make_array(7,/float)
   ord      = -3 + indgen(7)
   angstpix = [10.0, 20.0, 40.0, 650.0, 40.0, 20.0, 10.0]
   FOR i = 0, 6 DO BEGIN 
      sf = sdir + sfile[i]
      nd = ndisp[i]
      disp = angstpix[i]
      lim = grism_abmaglim(sigsky, nsig, ncd, nd, exptime, disp, sf, $
                             lampeak=lp, speak=sp)
      ablim[i] = lim
      lampeak[i] = lp
      speak[i] = sp
   ENDFOR 
   ncdarr = make_array(7,/int,value=ncd)
   forprint, ord, ncdarr, ndisp, angstpix, lampeak, speak, ablim
END 
