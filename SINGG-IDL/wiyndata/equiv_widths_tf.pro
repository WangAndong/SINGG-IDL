PRO equiv_widths_tf

   filelist = 'exptimes.dat'
   exptimes = 'exptimes.dat'
   rangeout = 'eqwranges.dat'
   format1  = '(a,a,a,i,f)'
;   format1  = '(a,a,a,a,i,f)'
;   format2  = '(a,a,a,a,a,a,i,f)'
   fmtg     =  '(i,a,a,x,x,x,f,f,f,f,f,f,f,i,f,f)'
   fmtout   = '(i5,a20,a1,a3,i8,f16,f16,f16,f16,a8)'
   ;folders  = ['Run0410_n','Run0503_n','Run0603_n']
   ;
   ; load list of filenames
    readcol, filelist, hipassid, galid, filename, nexp, texp, format=format1
;   readcol, filelist, hipassid, galid1, galid2, filename, $
;     nexp, texp, format=format1
;   readcol, filelist, temphi, tempga1, tempga2, tempga3, tempga4, $
;     tempfile, tempnexp, temptexp, format=format2
;   hipassid = [hipassid, temphi]
;   galid    = [galid1+' '+galid2, $
;               tempga1+' '+tempga2+' '+tempga3+' '+tempga4]
;   filename = [filename, tempfile]
   nfiles   = n_elements(filename)
   ;
   ; match filenames to folders
   ;fileno   = strmid(filename, 3)
   ;runno    = fix(strmid(fileno, 1, 1))
   ;nightno  = strmid(fileno, 3, 1)
   ;
   ; create input and output filename strings
   dp       = strpos(filename,'.')
   pfx      = make_array(nfiles,/string,value='')
   for ii = 0, nfiles-1 do pfx[ii] = strmid(filename[ii],0,dp[ii])
   ;
   ; Changed to Tommy Fergusson's naming scheme
   gausfile = pfx+'.dat'
   contfile = filename+'.cont.fits'
   datafile = filename+'.con.fits' 
   outfile  = pfx+'_eqw.dat' 
   ;
   ; read exposure times
   ;readcol, exptimes, expfilename, nexp, expt, $
   ;  format='(x,x,x,a,i,f)', /silent
   ;
   ; open file to output ranges
   openw, lr, rangeout, /get_lun
   ;
   ; loop over all files
   FOR ii = 0, nfiles-1 DO BEGIN
       ;
       ; load gaussian curve data
       readcol, gausfile(ii), row, filstr, filstr2, chisq, rms, c1, c2, c3, $
         c4, c5, linnum, zz, rvel, format=fmtg, /silent
       
       nr       = n_elements(row)
       ;
       ; decode apertures from filstr
       id = make_array(nr, /int, value=0)
       FOR jj = 0, nr-1 DO BEGIN 
           k2   = strpos(filstr2[jj],']',/reverse_search)
           istr = strmid(filstr2[jj],0,k2)
           id[jj] = fix(istr)
       ENDFOR 
       ;
       ; find the exposure time
       ;time     = 1800.0
       ;thisfile = 'obj'+strmid(gausfile(ii),strpos(gausfile(ii),'gauss')+5,13)
       ;fileline = where(expfilename EQ thisfile)
       ;IF fileline NE -1 THEN time = expt(fileline)
       ;time     = time(0)
       ;
       ; better way to do exp times
       time     = nexp[ii]*texp[ii]
       ;
       ; read calibration data from fits header
       contfits = readfits(contfile[ii], contheader)
       ;
       ; this is re-inventing the wheel..
       ;delt1loc = where(strmid(contheader,0,6) EQ 'CDELT1')
       ;disper   = float(strmid(contheader(delt1loc), $
       ;                      strpos(contheader(delt1loc), '=')+1))
       ;disper   = disper(0)
       ;crvalloc = where(strmid(contheader,0,6) EQ 'CRVAL1')
       ;crval    = float(strmid(contheader(crvalloc), $
       ;                      strpos(contheader(crvalloc), '=')+1))
       ;crval    = crval(0)
       ;crpixloc = where(strmid(contheader,0,6) EQ 'CRPIX1')
       ;crpix    = float(strmid(contheader(crpixloc), $
       ;                      strpos(contheader(crpixloc), '=')+1))
       ;crpix    = crpix(0)
       ;
       ; this is the better way
       disper   = sxpar(contheader, 'CDELT1')
       crval    = sxpar(contheader, 'CRVAL1')
       crpix    = sxpar(contheader, 'CRPIX1')
       xx       = fix(round((c4 - crval)/disper + crpix) - 0.5)
       ;
       ; calculate flux and continuum
       flux     = 1.0647*c3*c5/(disper*time)
       cont     = (c1 + contfits[xx,id-1])/(disper*time)
       ;
       ; calculate rms, and use it where rms > continuum
       datafits = readfits(datafile[ii])
       rmsdata  = make_array(nr)
       lower    = 2.0*c5/disper
       upper    = lower + 5.0/disper
       FOR jj = 0, nr-1 DO BEGIN
           neardata    = [datafits[round(xx[jj]-upper[jj]):$
               round(xx[jj]-lower[jj]),id[jj]-1],datafits[round(xx[jj]+$
               lower[jj]):round(xx[jj]+upper[jj]),id[jj]-1]]
           rmsdata[jj] = stddev(neardata)
       ENDFOR
       rmsdata  = rmsdata/(disper*time)   ; is this right??? probabalyshould be rmsdata*some_threshold
       toosmall = where(cont LT rmsdata*disper, nreplace)
       print, toosmall
       ;
       contuse  = cont
       used     = make_array(nr, /string, value='c')
       ;
       IF nreplace GT 0 THEN BEGIN
           contuse[toosmall] = rmsdata[toosmall]
           used[toosmall] = 'r'
       ENDIF
       ;
       eqwidth  = flux / contuse
       ;
       ; open output file and print header
       openw, lo, outfile(ii), /get_lun
       printf, lo, '# row file                       line            flux'+$
         '       continuum             rms    equiv. width    used'
       ;
       ; print data
       fmtout   = '(i5,2x,a30,i8,f16.6,f16.6,f16.6,f16.6,a8)'
       FOR jj = 0, nr-1 DO BEGIN
           printf, lo, row[jj], ljust(filstr[jj]+','+filstr2[jj],30), linnum[jj], flux[jj], $
             cont[jj], rmsdata[jj]*disper, eqwidth[jj], used[jj], format=fmtout
           printf, -1, row[jj], ljust(filstr[jj]+','+filstr2[jj],30), linnum[jj], flux[jj], $
             cont[jj], rmsdata[jj]*disper, eqwidth[jj], used[jj], format=fmtout
        ENDFOR
       free_lun, lo
       FOR line = 1, 6 DO BEGIN
           thisline = where(linnum EQ line, nthisline)
           IF nthisline GT 0 THEN begin 
              printf, lr, min(eqwidth[thisline]), max(eqwidth[thisline])
              printf, -1, min(eqwidth[thisline]), max(eqwidth[thisline])
           endif
       ENDFOR
       printf, lr, ''
   ENDFOR
   free_lun, lr
END
