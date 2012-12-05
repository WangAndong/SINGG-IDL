PRO fit_plot_vel, fileno, inguess, method=method
   ; method = 1   Run fitfibvel once with given input
   ; method = 2   Run fitfibvel once with velocities fixed, then once 
   ;              with all other parameters fixed (and v(0)=0)
   IF NOT(keyword_set(method)) THEN method = 1
   folders    = ['Run0410_n','Run0503_n','Run0603_n']
   runno      = fix(strmid(fileno, 1, 1))
   nightno    = strmid(fileno, 3, 1)
   filelist = 'exptimes.dat'
   format1  = '(a,a,a,a,i,f)'
   format2  = '(a,a,a,a,a,a,i,f)'
   ;
   ; load list of filenames
   readcol, filelist, hipassid, galid1, galid2, filename, $
     nexp, texp, format=format1, /silent
   readcol, filelist, temphi, tempga1, tempga2, tempga3, tempga4, $
     tempfile, tempnexp, temptexp, format=format2, /silent
   hipassid = [hipassid, temphi]
   galid    = [galid1+' '+galid2, $
               tempga1+' '+tempga2+' '+tempga3+' '+tempga4]
   filename = [filename, tempfile]
   thisfile   = where(strmid(filename,3) EQ fileno)
   ;
   ; make strings
   fgaus      = '../'+folders(runno-1)+nightno+'/gauss'+fileno+'.dat'
   ffit       = '../'+folders(runno-1)+nightno+'/vrot'+fileno+'.dat'
   fcomb      = '../'+folders(runno-1)+nightno+'/comb'+fileno+'.dat'
   fad        = '../'+folders(runno-1)+nightno+'/obj'+fileno+'.co.cr.ms.fits'
   fps        = 'ps/vrot'+fileno+'.ps'
   fjpg       = 'jpg/vrot'+fileno+'.jpg'
   background = hipassid(thisfile)
   title      = hipassid(thisfile) + ' (' + galid(thisfile) + ')'
   ;
   ; do it
   IF method EQ 1 THEN fitresult = $
     fitfibvel(fgaus, inguess, outfile=ffit, uncert=uncert) $
   ELSE BEGIN
       npar        = n_elements(inguess)
       nrr         = (npar - 5) / 2
       fixed       = make_array(npar, /integer)
       limited     = make_array(2, npar, /integer)
       limits      = make_array(2, npar)
       fixed(5:*)  = 1
       limited(1,1)   = 1
       limited(*,2:3) = 1
       limits(1,1)    = 90.0
       limits(0,2:3)  = -5.0
       limits(1,2:3)  = 5.0
       fitresult   = fitfibvel(fgaus, inguess, outfile=ffit, fixed=fixed, $
                               limited=limited, limits=limits)
       fixed(0)    = 1
       fixed(2:4)  = 1
       fixed(6+nrr:*) = 0
       limited(0,6+nrr:*) = 1
       fitresult(5+nrr) = 0.0
       fitresult   = fitfibvel(fgaus, fitresult, outfile=ffit, fixed=fixed, $
                               limited=limited, limits=limits, uncert=uncert)
   ENDELSE
   ;
   ; plot it


   plot_rot, fitresult, errors=uncert, hardfile=fps, background=background, $
     fieldad=fad, title=title, fcomb=fcomb, plottype=3
   resetplot
   plot_rot, fitresult, errors=uncert, jpg=fjpg, background=background, $
     fieldad=fad, title=title, fcomb=fcomb, plottype=3
   



END
