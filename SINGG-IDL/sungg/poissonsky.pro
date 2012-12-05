pro poissonsky, skyarr, skymean=skymean, skysigma=skysigma, skysdom=skysdom, $
                clipstart=clipstart, used=used, converge=converge, fit=fit,$
                verbose=verbose, plot=plot

; Works on the assumption that for Poisson distribution:
;  - all values are positive
;  - mean = variance
; and that the true sky value can be obtianed by iteratively clipping 
; values > medain+N*sigma untill variance is within 10% of mean and 
; remaining values will have a Poission distribution
;
; skayarr = input vlaues to find Poisson sky
;           must be positive integers
; skymean = computed sky mean
; skysigma = sqrt(skymean) not true value (i.e. sqrt(varaince))
; clipstart = N for intial clipping (N*sigma) default=5
; used = indicies of final skyarr values used for sky estimate
; converge = max fractional difference limit between mean amd variance
;            default=0.02 (2%)
; verbose = print status and plot
;
; software from Don Neill and Mark Seibert sent to GRM 11 Feb 2011
; do not share without permission.

pre='POISSONSKY: '

if not keyword_set(clipstart) then clipstart=5
if not keyword_set(converge) then converge=0.02

skymean=!values.f_nan
skysigma=!values.f_nan
skysdom=!values.f_nan
used=-1

;type=size(skyarr,/type)
;if type ne 2 and type ne 3 then begin
; print,pre+"Input array must be of type INTEGER or LONG."
; return
;endif

s=where(finite(skyarr),count)
if count gt 30 then begin
 used=s
 skyvals=skyarr[s]
endif else begin
 print,pre+"Input array has too few finite values. Minimum of 30 required."
 return 
endelse

a=where(skyvals - long(skyvals) ne 0, count)
if count gt 0 then begin
 print,pre+"Input array appears to have non-integer or long values."
 return
endif

a=where(skyvals lt 0,count)
if count gt 0 then begin
 print,pre+"Input array has negative values. Not a Poisson distribution."
 return
endif

mom=moment(skyvals)
if mom[1] le mom[0] then print,pre+ "Warning: variance is less than mean."
 
if mom[1] le (1.0+converge)*mom[0] then begin
 print,pre+'Input array meets requirements without clipping'
 skymean=mom[0]
 skysigma=sqrt(mom[1])
 skysdom=skysigma/sqrt(n_elements(skyvals))
 goto, skipclip 
endif

if mom[1] lt mom[0] then begin
 print,pre+ "Warning: variance is less than mean. But not within 10% of mean. Canceling."
 return
endif

iter=0
clip=clipstart
momlast=mom

;;;;;;;;;;;;;;;;;;;;
; clipping loop
;;;;;;;;;;;;;;;;;;;;

while mom[1] gt (1.0+converge)*mom[0] do begin

 if clip lt 2.0 then begin
  print,pre+'clipsigma has dropped below 2 without convergence. Quiting.'   
  skymean = !values.f_nan
  skysigma = !values.f_nan
  skysdom = !values.f_nan
  ;used=-1 
  return
 endif

 iter = iter+1
 posmeanclip, skyvals, skymean, skysigma, sub = s, clip=clip;, conv=0.01

 if skymean le 0 or skysigma le 0 then begin
  mom=moment(skyvals)
  skymean=mom[0]
  skysigma=sqrt(mom[1])
  return
 endif

 skyvals=skyvals[s]
 used=used[s]
 mom=moment(skyvals)
 ;skysigma=sqrt(skymean)
 skysdom=skysigma/sqrt(n_elements(skyvals))  

 if keyword_set(verbose) then print,pre+'mean:',mom[0],'var:',mom[1],$
   'clipsig:',clip,'iter:',iter,format='(a17,f12.5,a5,f12.5,a9,f5.2,a6,i)'
 
 if mom[0] eq momlast[0] and mom[1] eq momlast[1] then clip=clip-0.05
 momlast=mom 

endwhile

;;;;;;;;;;;;;;;;;;;;
; end clipping loop
;;;;;;;;;;;;;;;;;;;;

skipclip:

if keyword_set(fit) then begin

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;fit a poisson distribution
;;;;;;;;;;;;;;;;;;;;;;;;;;;

;bin into distribution and normalize

plothist,skyvals,x,y,/halfbin,/noplot
x=x*1D
y=y*1D
yerr=y*0 ; errors? what errors :)
y=1.0*y/max(y)

;weight the smallest counts the most

weights=(1.0/(1+x))

;start at 85% of skymean and alow it to roam between 80-120% of skymean

parinfo = replicate({value:0.D, fixed:0, limited:[0,0], $
                 limits:[0.D,0]}, 1)
parinfo[0].value=skymean
parinfo[0].limited=[1,1]
parinfo[0].limits=[skymean*0.8,skymean*1.2]
p0 = [1D*skymean*0.99]

if keyword_set(verbose) then quiet=0 else quiet=1

p = mpfitfun('poisson',x,y,yerr,p0,parinfo=parinfo,weights=weights,$
             status=status,yfit=yfit,quiet=quiet, errmsg=errmsg,bestnorm=bestnorm)

if keyword_set(verbose) then print,pre+'best fit = ',p

if errmsg eq "" then begin
 skymean=p[0]  
 skysigma=sqrt(p[0]) 
endif else print,pre+ 'Problem with Poisson fit. Using simple mean. "+errmsg


endif

;;;;;;;;;;;;;;;;;;;;;;;;;;
; end fit
;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;
; make a plot
;;;;;;;;;;;;;;;;;;;;;;;;;;

if keyword_set(plot) then begin

 setplotcolors

 Result = RANDOMN(randomn(seed), n_elements(skyvals), POISSON=skymean)

 plothist,skyvals,x1,y1,/halfbin,/noplot;,/peak,yr=[0,1.1]
 plothist,result,x2,y2,/halfbin,/noplot; ,/peak,color=!blue,/over

 plot,x1,1.0*y1/max(y1),yr=[0,1.1],/ys,xr=[0,max(result)+1],/xs,$
  xtit='skyval (counts)',tit='Normaized Distribution',psym=10
 oplot,x2,1.0*y2/max(y2),color=!blue,psym=-2,thick=2

 lambda=skymean
 k=lindgen(ceil(lambda)*3 > 5)
 f= ((1d*lambda)^(k) * exp(-lambda))/factorial(k)
 oplot,k,1.0*f/max(f),color=!green,psym=-4

 if n_elements(yfit) gt 0 then $
 oplot,x,yfit,color=!red,psym=-4,thick=2,line=2

endif

;;;;;;;;;;;;;;;;;;;;;;;;;;
; end plot
;;;;;;;;;;;;;;;;;;;;;;;;;;


end
