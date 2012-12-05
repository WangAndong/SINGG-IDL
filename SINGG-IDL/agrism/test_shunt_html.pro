PRO test_shunt_html
   fcat = 'ps1_squash_shunt.cat'
   fhtml = 'ps1_shunt.html'
   title = 'SHunt results for PEARS field PS1 (cdf-s-pears-1-gpa069) '
   pfx_plots = 'ps1_shunt'
   pfx_dstamp = 'ps1_shunt_direct_stamp'
   readcol, fcat, id, xsq, ysq, mag, spec_class, ra, dec, format='(l,f,f,f,a,x,x,x,x,x,x,x,x,f,f)'
   spec_class = strtrim(spec_class,2)
   shunt_html_out, fhtml, fcat, title, id, xsq, ysq, spec_class, mag, ra, dec, pfx_plots, pfx_dstamp=pfx_dstamp
END 
