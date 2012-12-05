pro medhdrval,file,fitsfile,nsig,iterations

; medhdrval is called by medhdr.  The inputs are all defined by medhdr.
; The file is the .stars file which medhdr deems appropriate while the
; fitsfiles is the .fits file whose header will be edited.  nsig is the
; number of standard deviations for the outlier rejection and iterations
; is the number of time the program will loop to reject points.
; medhdrval first finds an appropriate trunction point, as described in
; medhdrREADME.  It then eliminates the outliers based on the user
; defined parameters.  Once the outliers are eliminated, the program
; calculates the median value of the remaining data points and prints it
; to the header file.  Before this occurs, however, the program pauses
; to allow the user to examine the histograms to make sure that the
; program functioned properly.  A .cont resumes the program, which will
; then work on the next file in the list.

   ; 1/2003 G.R. Meurer: 
   ;    * Sextractor file format added.
   ;    * binwidth lowered to 0.25
   ;    * psym=10 added to histogram
   ;    * set f properly
   ;    * use colors
   ;    * plot mean, median, mean+/-1sig

   fmt = '(i,f,f,f,i,f,f,f,f,f)'
   truncmin=.5
   truncmax=2.5
   binwidth=0.2

   PRINT,file
   READCOL,file,number,x_im,y_im,mag_best,flags,a_im,b_im,$
    elongation,FWHM_im1,class_star,format=fmt,/SILENT


   WINDOW,TITLE=file+' GRAPHICS',XSIZE=900,YSIZE=800
   setplotcolors
   !P.MULTI=[0,1,5]

   locs=WHERE(FWHM_im1 gt truncmin AND FWHM_im1 lt truncmax)
   histlocs=HISTOGRAM(FWHM_im1(locs),BINSIZE=binwidth)
   ;PRINT,histlocs
   minhist=WHERE(histlocs EQ MIN(histlocs))
   ;PRINT,minhist

   trunc=truncmin+binwidth*minhist(0)
   PRINT,'trunc=',trunc

; determines the trunction point(ie eliminates the cosmic ray peak) by
; finding the local minimum in the histogram.  this process is further
; described in the medhdrREADME file.

; PRINT,N_ELEMENTS(histlocs)

; PRINT,FWHM_im1(WHERE(FWHM_im1 gt trunc))
; PRINt,trunc
  FWHM_im=FWHM_im1(WHERE(FWHM_im1 gt trunc))
;reads in the columns, we're interested in FWHM_im

  sigorig=STDDEV(FWHM_im)
  median1=MEDIAN(FWHM_im)
  mean1=MEAN(FWHM_im)

;defines an original sigma, median, and mean on which to base the outlier filter
  sig=sigorig
  meanfin = mean1

  FOR i=0,iterations-1 DO BEGIN
     FWHMnoouts=WHERE(FWHM_im lt meanfin+nsig*sig AND FWHM_im $
                      gt meanfin-nsig*sig,count)
     FWHMrej=WHERE(FWHM_im lt mean1-nsig*sig OR FWHM_im gt mean1+nsig*sig)
;	PRINT,"Rejected Values"
;	PRINT,FWHM_im(FWHMrej)
;	PRINT,FWHMrej
;	PRINT,'stats after',i+1,' iterations'
     sig=STDDEV(FWHM_im(FWHMnoouts))
     medianfin=MEDIAN(FWHM_im(FWHMnoouts))
     meanfin=MEAN(FWHM_im(FWHMnoouts))	
;	PRINT,sig,meanfin,medianfin


     total=N_ELEMENTS(FWHM_im)
     rejected=total-count
;	PRINT,rejected
  ENDFOR
;finds total number, removes outliers For loop allows a user defined
;number of iterations to be run.  This rejects outliers for different
;sigma values.  For example, far outliers, such as a value of 100 when
;the max of the feature is closer to 4, will inflate the sigma value.
;because of this, FWHM's of 20 pixels may not be rejected, when they
;clearly should be.  Running more than one iteration will first reject
;the far outliers, deflating the sigma.


;PRINT,FWHMrej

  meanfin=MEAN(FWHM_im(FWHMnoouts))
  medianfin=MEDIAN(FWHM_im(FWHMnoouts))
  sigfin=STDDEV(FWHM_im(FWHMnoouts))

;takes statistics on outlier filtered data

  omin=MIN(FWHM_im)
;PRINT,omin
  omax=MAX(FWHM_im)
  fmin=MIN(FWHM_im(FWHMnoouts))
  fmax=MAX(FWHM_im(FWHMnoouts))
  f0  = -50.0
  f1  =  50.0
  histnotrunc=HISTOGRAM(FWHM_im1,BINSIZE=binwidth,min=f0,max=f1)
;PRINT,histnotrunc
  histoorig=HISTOGRAM(FWHM_im,BINSIZE=binwidth,min=f0,max=f1)
;PRINT,histoorig

;plots a bunch of histograms to check trunc as well as look at rejected outliers
  F=f0 + 0.5*binwidth + binwidth*findgen((f1 - f0)/binwidth)
  yr = [0.0, 1.2*max([histnotrunc, histoorig])]
  PLOT,F,histnotrunc,TITLE='Pre-Truncation Histogram', $
   XTITLE='FWHM Bin',YTITLE='Number of Data Points',CHARSIZE=2.0, $
   XRANGE=[-50,50],yrange=yr,xstyle=1,ystyle=1,psym=10
  PLOT,F,histnotrunc,TITLE='Pre-Truncation Histogram', $
   XTITLE='FWHM Bin',YTITLE='Number of Data Points',CHARSIZE=2.0, $
   XRANGE=[-8,8],yrange=yr,xstyle=1,ystyle=1,psym=10
  PLOT,F,histnotrunc,TITLE='Area Examined for Truncation Point', $
   XTITLE='FWHM Bin',YTITLE='Number of Data Points',CHARSIZE=2.0, $
   XRANGE=[truncmin,truncmax],yrange=yr,xstyle=1,ystyle=1,psym=10

;shows the pretrunction data in various zooms

  PLOT,F,histoorig,TITLE='Truncated Data Showing Outliers in Bold', $
   XTITLE='FWHM Bin',YTITLE='Number of Data Points',CHARSIZE=2.0,psym=10, $
   xrange=[fmin,fmax],xstyle=1,ystyle=1,yrange=yr
  histofin=HISTOGRAM(FWHM_im(FWHMnoouts),BINSIZE=binwidth,min=f0,max=f1)
  IF fwhmrej[0] NE -1 THEN BEGIN 
     historej=HISTOGRAM(FWHM_im(FWHMrej),binsize=binwidth,min=f0,max=f1)*5
     OPLOT,F,historej,THICK=3
     ENDIF 
;PRINT,fmin
;PRINT,fmax

;shows the truncated data as well as teh rejected points

  PLOT,F,histofin,TITLE='Histogram of Values Used to Calculated Charcteristic Value', $
   XTITLE='FWHM Bin',YTITLE='Number of Data Points',CHARSIZE=2.0,psym=10, $
   xrange=[fmin,fmax],yrange=yr,xstyle=1,ystyle=1
  ;
  ; overplot lines for median (thick solid, green)
  ;                    mean   (dashed, green)
  ;                    mean +/- sig (dotted, green)
  xpl = medianfin*[1., 1.]
  ypl = yr
  oplot,xpl,ypl,linestyle=0,thick=3.0,color=!dgreen
  xpl = meanfin*[1., 1.]
  oplot, xpl, ypl, linestyle=2,color=!dgreen,thick=2.0
  xpl = (meanfin - sig)*[1., 1.]
  oplot, xpl, ypl, linestyle=1,color=!dgreen,thick=2.0
  xpl = (meanfin + sig)*[1., 1.]
  oplot, xpl, ypl, linestyle=1,color=!dgreen,thick=2.0

;shows the values used to calculate the median

  PRINT,file,' ',fitsfile,' ',medianfin
;PRINT,sigorig
  PRINT,"type .cont to continue"
  STOP

;stops as the user examines the plots.  a ".cont" resumes.

;PRINT,fitsfile
  header=STRING(REPLICATE(32B,80))
  readfitsfile=READFITS(fitsfile,header)
  FXADDPAR,header,"MEDIAN",medianfin,"Median value of FWHM_image"
;;FXADDPR(header,"MEAN",meanfin,"Mean value of FWHM_image")
  WRITEFITS,fitsfile,readfitsfile,header
;opens the fits file and adds the "MEDIAN" value to the header

;writes teh median value to the header

;adds median to file as defined in medhdr.pro

  total=N_ELEMENTS(FWHM_im)
;PRINT,total

  PRINTF,1,''
  PRINTF,1,FORMAT='(A25,3X,A25,3X,F5.3,3X,F8.5,3X,F5.3,3X,F7.5,3X,I2,3X,F7.5,3X,F5.3,X,F7.5)', $
   file,fitsfile,trunc,sigorig,median1,mean1,rejected,sigfin,medianfin,meanfin

;prints to filelist.out, for tabular inspection of results.

END
