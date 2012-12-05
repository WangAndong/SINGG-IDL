FUNCTION axe_wl, dx, xim, yim, dldp_a_0, dldp_a_1, dldp_a_2, xref=xref, yref=yref
   ;
   ; Returns the wavelength from the aXe Wavelength solution.
   ;
   ; dx       -> x displacement from direct image
   ; xim,yim  -> coordinates of source in direct image.
   ; dldp_a_0 -> zeropt term for wavelength solution
   ; dldp_a_1 -> dispersion term for wavelength solution
   ; dldp_a_2 -> quadratic term for wavelength solution
   ;
   ; G. Meurer - late 2002 : originally written
   ;           - 7/2004 : updated to use arbitrary aXe style 2d
   ;                      polynomial
   ;
   a0  = eval_axe_poly(dldp_a_0, xim, yim, xref=xref, yref=yref)
   a1  = eval_axe_poly(dldp_a_1, xim, yim, xref=xref, yref=yref)
   a2  = eval_axe_poly(dldp_a_2, xim, yim, xref=xref, yref=yref)
   return, a0 + dx*(a1 + dx*a2)
END 

