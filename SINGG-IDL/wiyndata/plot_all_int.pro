PRO plot_all_int, ps=ps, folder=folder

   IF keyword_set(ps) THEN BEGIN
       outfolder = 'ps'
       extension = '.ps'
   ENDIF ELSE BEGIN
       outfolder = 'jpg'
       extension = '.jpg'
   ENDELSE
   IF keyword_set(folder) THEN outfolder=folder
   filelist   = 'exptimes.dat'
   exptimes   = 'exptimes.dat'
   format1    = '(a,a,a,a,i,f)'
   format2    = '(a,a,a,a,a,a,i,f)'
   folders    = ['Run0410_n','Run0503_n','Run0603_n']
   winratio   = 1.0/1.12
   charsize   = 1.5
   titlesize  = 2.0 * charsize
   positions  = [[0.10, 0.80/1.12, 0.95, 1.02/1.12], $
                 [0.10, 0.49/1.12, 0.95, 0.71/1.12], $
                 [0.10, 0.06/1.12, 0.95, 0.40/1.12], $
                 [0.10, 0.18/1.12, 0.95, 0.40/1.12], $
                 [0.10, 0.062/1.12, 0.95, 0.17/1.12]]
   ytitlepos  = [[0.03, 0.23/1.12], $
                 [0.03, 0.60/1.12], $
                 [0.03, 0.91/1.12]]
   titlepos   = [0.5, 1.07/1.12]
   xstep      = 20.0
   baddata    = [40, 26, 26]
   ysteps3    = [0.01, 0.025, 0.05, 0.1, 0.25, 0.5, 1.0, 2.5, 5.0, 10.0]
   ;
   ; set plot parameters
   IF keyword_set(ps) THEN BEGIN 
       xs = 6.0
       ys = xs/winratio
       yoff=1.0
       xoff=1.0
       thick=2
       charsize  = 0.4*charsize
       titlesize = 0.4*titlesize
   ENDIF ELSE BEGIN 
       wxsize   = 900
       wysize   = fix(float(wxsize)/winratio)
       thick    = 1
   ENDELSE 
   ;
   ; load list of filenames
   readcol, filelist, hipassid, galid1, galid2, filename, $
     nexp, texp, format=format1, /silent
   readcol, filelist, temphi, tempga1, tempga2, tempga3, tempga4, $
     tempfile, tempnexp, temptexp, format=format2, /silent
   hipassid   = [hipassid, temphi]
   galid      = [galid1+' '+galid2, $
                 tempga1+' '+tempga2+' '+tempga3+' '+tempga4]
   filename   = [filename, tempfile]
   texp       = [texp, temptexp]
   nfiles     = n_elements(filename)
   ;
   ; match filenames to folders
   fileno     = strmid(filename, 3)
   runno      = fix(strmid(fileno, 1, 1))
   nightno    = strmid(fileno, 3, 1)
   ;
   ; create filename and title strings
   fintspec   = '../'+folders(runno-1)+nightno+'/intspec'+fileno+'.cr.ms.fits'
   outfile    = outfolder+'/'+'intspec'+fileno+extension
   title      = hipassid+' ('+galid+')'
   ;
   ; read exposure times
   readcol, exptimes, expfilename, nexp, expt, $
     format='(x,x,x,a,i,f)', /silent
   ;
   ; plot each set of spectra
   FOR ii = 0, nfiles-1 DO BEGIN
       ;
       ; initialise plots
       IF keyword_set(ps) THEN BEGIN 
           set_plot,'ps',/copy, /interpolate
           device,/portrait,/inches,xs=xs,ys=ys,yo=yoff,xo=xoff,/color
       ENDIF ELSE BEGIN 
           window, 0, xsize=wxsize, ysize=wysize
       ENDELSE 
       ;
       ; open fits file
       spectra  = readfits(fintspec(ii), fitsheader)
       specdim  = size(spectra, /dimensions)
       ;
       ; read information from header
       delt1loc = where(strmid(fitsheader, 0, 6) EQ 'CDELT1')
       disper   = float(strmid(fitsheader(delt1loc), $
                             strpos(fitsheader(delt1loc), '=')+1))
       disper   = disper(0)
       crvalloc = where(strmid(fitsheader, 0, 6) EQ 'CRVAL1')
       crval    = float(strmid(fitsheader(crvalloc), $
                             strpos(fitsheader(crvalloc), '=')+1))
       crval    = crval(0)
       crpixloc = where(strmid(fitsheader, 0, 6) EQ 'CRPIX1')
       crpix    = float(strmid(fitsheader(crpixloc), $
                             strpos(fitsheader(crpixloc), '=')+1))
       crpix    = crpix(0)
       restfactorloc = where(strmid(fitsheader, 0, 8) EQ 'ISRESTFA')
       eqpos         = strpos(fitsheader(restfactorloc), '=')
       restfactor    = float(strmid(fitsheader(restfactorloc), eqpos + 1, $
                             strpos(fitsheader(restfactorloc), '/') - $
                             eqpos - 1))
       restfactor    = restfactor(0)
       ;
       ; convert spectra to nice units
       spectra  = spectra / (texp(ii) * disper)
       ;
       ; set x axes
       lamda1   = ((findgen(n_elements(spectra(*,0))) - (crpix - 1)) * $
                     disper) + crval
       lamda2   = lamda1 / restfactor
       lamda1   = lamda1(baddata(runno(ii)-1):specdim(0)-baddata(runno(ii)-1)-1)
       lamda2   = lamda2(baddata(runno(ii)-1):specdim(0)-baddata(runno(ii)-1)-1)
       spectra  = spectra(baddata(runno(ii)-1):specdim(0)-baddata(runno(ii)-1)-1, *)
       xmin1    = xstep*floor(min(lamda1) / xstep)
       xmax1    = xstep*ceil(max(lamda1) / xstep)
       xmin2    = xmin1 / restfactor
       xmax2    = xmax1 / restfactor
       ;
       ; set y range for 3rd plot
       midpoint = median(spectra(*,2))
       rms      = stddev(spectra(*,2))
       FOR loop=0,2 DO rms = stddev(spectra(where(spectra(*,2) LT midpoint+3*rms),2))
       yrange3  = midpoint + 3.0*rms*[-1,1]
       compare  = (6.0 * rms / 3.0) / ysteps3
       compare  = abs(compare - 1.0)
       mindif   = min(compare, mindifindex)
       ytickinterval3 = ysteps3(mindifindex)
;       ytickinterval3 = 0.02*round(2.5 * rms / 0.02)
       ;
       ; plot spectra
       plot, lamda1, spectra(*,0), position=positions(*,0), $
         xtitle='Wavelength (!3'+angstsym()+'!6)', $
;         ytitle='Intensity (counts s!U-1!N !3'+angstsym()+'!6!U-1!N)', $
         title='Non-sky fibers', xstyle=1, xrange=[xmin1, xmax1], $
         charsize=charsize
       plot, lamda1, spectra(*,1), position=positions(*,1), $
         xtitle='Wavelength (!3'+angstsym()+'!6)', $
;         ytitle='Intensity (counts s!U-1!N !3'+angstsym()+'!6!U-1!N)', $
         title='Fibers with H!7a!6', xstyle=1, xrange=[xmin1, xmax1], $
         charsize=charsize, /noerase
       plot, lamda2(baddata(runno(ii)-1):specdim(0)-3*baddata(runno(ii)-1)-1), $
         spectra(baddata(runno(ii)-1):specdim(0)-3*baddata(runno(ii)-1)-1,2), $
         position=positions(*,2), $
         xtitle='Wavelength (!3'+angstsym()+'!6)', $
         title='Fibers with any detection, shifted to rest wavelength', $
         xstyle=1, xrange=[xmin2, xmax2], charsize=charsize, /noerase, /nodata, $
         yticks=1, ytickformat='(A1)', ystyle=1
       plot, lamda2(baddata(runno(ii)-1):specdim(0)-3*baddata(runno(ii)-1)-1), $
         spectra(baddata(runno(ii)-1):specdim(0)-3*baddata(runno(ii)-1)-1,2), $
         position=positions(*,3), $
         xstyle=5, xrange=[xmin2, xmax2], charsize=charsize, /noerase
       plot, lamda2(baddata(runno(ii)-1):specdim(0)-3*baddata(runno(ii)-1)-1), $
         spectra(baddata(runno(ii)-1):specdim(0)-3*baddata(runno(ii)-1)-1,2), $
         position=positions(*,4), $
         yrange=yrange3, ystyle=1, ytickinterval=ytickinterval3, $
         xstyle=5, xrange=[xmin2, xmax2], charsize=charsize, /noerase
       FOR jj=0,2 DO xyouts, ytitlepos(0,jj), ytitlepos(1,jj), $
         'Intensity (counts s!U-1!N !3'+angstsym()+'!6!U-1!N)', $
         alignment=0.5, charsize=charsize, /normal, orientation=90
       ;
       ; print title
       xyouts, titlepos(0), titlepos(1), title(ii), color=0, $
         alignment=0.5, /normal, charsize=titlesize
       ;
       ; make output file
       IF keyword_set(ps) THEN BEGIN
           psend, outfile(ii), /noprint, /clobber
           set_plot,'x'
       ENDIF ELSE BEGIN
           im = tvrd(true=3)
           WRITE_JPEG,outfile(ii),im,TRUE=3,QUALITY=100
       ENDELSE
       
   ENDFOR 


END
