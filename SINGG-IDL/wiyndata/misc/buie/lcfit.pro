;+
; NAME: 
;  lcfit
; PURPOSE: 
;  Fit a lightcurve function (Fourier series plus phase coefficient).
; DESCRIPTION:
; CATEGORY:
;  Function fitting
; CALLING SEQUENCE:
; INPUTS:
;  lon    - Longitude of sub-earth point (0 to 360).
;  phang  - Phase angle (Sun-Object-Earth angle in degrees).
;  data   - Measured values.
;  sig    - Uncertainties of the data.
;  nterms - Number of fourier terms to fit.  (2*nterms+1 unknowns)
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
; OUTPUTS:
;  c - fourier series coefficients (see fourfunc)
;  csig - uncertainties of the coefficients
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  94/10/10, Written by Marc W. Buie, Lowell Observatory
;-
pro lcfit,lon,in_phang,data,sig,nterms,c,csig, $
      YFIT=yfit,CHISQ=chisq,TITLE=title,DYMF=dymf

   common lc_com,phang

   if badpar(title,[0,7],0,CALLER='LCFIT (title): ',default='') THEN return
   if badpar(dymf,[0,1,2,3,4,5,6],0,CALLER='LCFIT (title): ',default=1.0) THEN return

   phang = in_phang

   w = 1.0/sig^2
   guess = fltarr(nterms*2+2)
   guess[0] = mean(data)

   yfit = curvefit(lon,data,w,guess,sigterms,function_name="LCFUN")

   chisq = total( ((data-yfit)/sig)^2 ) / float(n_elements(data)-n_elements(c))

   c=guess
   csig=sigterms

   lonran=[0,360]
   dym=[0.2,-0.2]
   synlon=findgen(361.0)
   lcfun,synlon,c,syn,phang=0.0
   lcfun,lon,c,synd,phang=0.0
   
   setwin,0,xsize=540,ysize=320
   data_1 = data - c[0]*in_phang
   ploterror,lon,data_1,sig,psym=4,xr=lonran,yr=mean(minmax(data_1))+dym, $
      xtit='East longitude (degrees)',ytit='Phase corrected magnitude', $
      tit=title
   oplot,synlon,syn

   setwin,1,xsize=540,ysize=320
   data_2 = data - synd + c[1]
   synphase = [0.,max(in_phang)]
   synp     = c[1] + c[0]*synphase

   ploterror,in_phang,data_2,sig,psym=4, $
      xr=[0.,max(in_phang)],yr=[max(data_2+sig),min([c[1],data_2-sig])], $
      xtit='Phase angle (deg)',ytit='Rotation corrected magnitude', $
      tit=title
   oplot,[0],[c[1]],psym=5
   oplot,synphase,synp

   setwin,2,xsize=540,ysize=320
   ploterror,lon,data-yfit,sig,psym=4,xr=lonran,yr=mean(minmax(data-yfit))+dym/dymf, $
      xtit='East longitude (degrees)',ytit='Residuals', $
      tit=title

end
