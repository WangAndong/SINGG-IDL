PRO plot_vel_profile, ps=ps
   IF keyword_set(ps) THEN BEGIN
       outfolder = 'ps'
       extension = '.ps'
   ENDIF ELSE BEGIN
       outfolder = 'jpg'
       extension = '.jpg'
   ENDELSE
   lamzero    = 6562.8169
   cvel       = 299792.458
   filelist   = 'exptimes.dat'
   exptimes   = 'exptimes.dat'
   fhelio     = 'helio_vel.dat'
   format1    = '(a,a,a,a,i,f)'
   format2    = '(a,a,a,a,a,a,i,f)'
   folders    = ['Run0410_n','Run0503_n','Run0603_n']
   charsize   = 1.5
   titlesize  = 2.0 * charsize
   winratio   = 1.5
   xtitle     = 'Heliocentric velocity (km/s)'
   ytitle     = 'Intensity'
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
   fintspec   = '../'+folders(runno-1)+nightno+'/intspec'+fileno+'.dat'
   fhispec    = '../'+folders(runno-1)+nightno+'/hivel'+fileno+'.dat'
   outfile    = outfolder+'/'+'velprof'+fileno+extension
   outascii   = '../'+folders(runno-1)+nightno+'/velprof'+fileno+'.dat'
   title      = hipassid+' ('+galid+')'
   ;
   ; read heliocentric velocities
   readcol, fhelio, helfilename, helvel, format='(a,f)', /silent
   ;
   ; set plot parameters
   IF keyword_set(ps) THEN BEGIN 
       xs = 6.0
       ys = xs/winratio
       yoff=1.0
       xoff=1.0
       thick=2
       charsize  = 0.6 * charsize
       titlesize = 0.6 * titlesize
   ENDIF
   wxsize   = 900
   wysize   = fix(float(wxsize)/winratio)
   thick    = 1
   ;
   ; loop over files
   FOR ii=0, nfiles-1 DO BEGIN
       ;
       ; initialise plots
       loadct, 0
       window, 0, xsize=wxsize, ysize=wysize
       ;
       ; read HI and Halpha spectra
       readcol, fintspec(ii), lamda, ha, format='(x,x,f,f,x,x)', /silent
       readcol, fhispec(ii), hivel, hi, format='(x,f,f)', /silent
       havel    = (lamda / lamzero - 1) * cvel
       ;
       ; find correct heliocentric velocity, and edit havel
       havel    = havel + (helvel(where(helfilename EQ filename(ii))))(0)
       ;
       ; plot Halpha spectrum and ask for input on Halpha position
       plot, ha
       read, 'Lower limit: ', lower
       read, 'Upper limit: ', upper
       ;
       ; find peaks and scale appropriately
       hapeak   = max(ha(lower:upper), hapeakloc)
       hapeakloc= hapeakloc + lower
       hilook   = where(hivel GT havel(hapeakloc) - 400.0 AND $
                        hivel LT havel(hapeakloc) + 400.0)
       halook   = where(havel GT havel(hapeakloc) - 400.0 AND $
                        havel LT havel(hapeakloc) + 400.0)
       hipeak   = max(hi(hilook))
       hahi     = hapeak / hipeak
       ;
       ; set ranges for plot
       xrange   = havel(hapeakloc) + 400.0 * [-1,1]
       yrange   = [min([ha(halook), hahi * hi(hilook), 0]), hapeak]
       ;
       ; make plot
       loadct, 0
       IF keyword_set(ps) THEN BEGIN 
           set_plot,'ps',/copy, /interpolate
           device,/portrait,/inches,xs=xs,ys=ys,yo=yoff,xo=xoff,/color
       ENDIF ELSE BEGIN 
           window, 0, xsize=wxsize, ysize=wysize
       ENDELSE 
       plot, havel, ha, xrange=xrange, yrange=yrange, /nodata, $
         xstyle=1, xtitle=xtitle, ytitle=ytitle, title=title(ii), $
         charsize=charsize
       loadct, 40
       oplot, havel, ha, color=70
       oplot, hivel, hi * hahi, color=250
       legend, ['H!7a!6', 'HI'], linestyle=0, colors=[70,250], $
         charsize=charsize, textcolors=[0,0]
       ;
       ; make output file
       IF keyword_set(ps) THEN BEGIN
           psend, outfile(ii), /noprint, /clobber
           set_plot,'x'
       ENDIF ELSE BEGIN
           im = tvrd(true=3)
           WRITE_JPEG,outfile(ii),im,TRUE=3,QUALITY=100
       ENDELSE
       ;
       ; make ascii file
       hakeep = where(havel GE xrange(0) AND havel LE xrange(1))
       hikeep = where(hivel GE xrange(0) AND hivel LE xrange(1))
       havel  = havel(hakeep)
       ha     = ha(hakeep)
       hivel  = hivel(hikeep)
       hi     = hi(hikeep)
       openw, 1, outascii(ii)
       printf, 1, '#       velocity    Ha intensity        velocity' + $
         '    HI intensity'
       nha    = n_elements(havel)
       nhi    = n_elements(hivel)
       FOR jj = 0, max([nha, nhi])-1 DO BEGIN
           IF jj LT nha AND jj LT nhi THEN printf, 1, havel(jj), ha(jj), $
             hivel(jj), hi(jj), format='(4f16)' $
           ELSE IF jj LT nha THEN printf, 1, havel(jj), ha(jj), $
             format='(2f16)' $
           ELSE printf, 1, '                                ', $
             hivel(jj), hi(jj), format='(a32,2f16)'
       ENDFOR
       close, 1
   ENDFOR 
   loadct, 0
END
