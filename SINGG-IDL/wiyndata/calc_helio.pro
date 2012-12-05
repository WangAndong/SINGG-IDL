PRO calc_helio
   filelist   = 'exptimes.dat'
   fout       = 'helio_vel.dat'
   format1    = '(a,a,a,a,i,f)'
   format2    = '(a,a,a,a,a,a,i,f)'
   folders    = ['Run0410_n','Run0503_n','Run0603_n']
   cvel       = 299792.458
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
   ; create filename strings
   ffits      = '../'+folders(runno-1)+nightno+'/'+filename+'.co.cr.ms.fits'
   ;
   ; make output array
   helio_vel  = make_array(nfiles)
   ;
   ; loop over files
   FOR ii=0, nfiles-1 DO BEGIN
       ;
       ; read header and decode date
       fitsheader = headfits(ffits(ii))
       jdloc      = where(strmid(fitsheader, 0, 9) EQ 'JD      =')
       q1         = strpos(fitsheader(jdloc), "'")
       q2         = strpos(fitsheader(jdloc), "'", /reverse_search)
       jd         = float(strmid(fitsheader(jdloc), q1 + 1, q2 - q1 - 1))
       ;
       ; calculate heliocentric velocity
       baryvel, jd, 2000, vh, vb
       ;
       ; find ra and dec in radians
       raloc      = where(strmid(fitsheader, 0, 3) EQ 'RA ')
       quote1     = strpos(fitsheader(raloc), "'")
       quote2     = strpos(fitsheader(raloc), "'", /reverse_search)
       rastring   = strmid(fitsheader(raloc), quote1 + 1, quote2 - quote1 - 1)
       decloc     = where(strmid(fitsheader, 0, 4) EQ 'DEC ')
       quote1     = strpos(fitsheader(decloc), "'")
       quote2     = strpos(fitsheader(decloc), "'", /reverse_search)
       decstring  = strmid(fitsheader(decloc), quote1 + 1, quote2 - quote1 - 1)
       adstring   = rastring + ' ' + decstring
       adstring   = adstring(0)
       get_coords, fieldad, instring = adstring
       raoffloc   = where(strmid(fitsheader, 0, 7) EQ 'RAOFFST')
       quote1     = strpos(fitsheader(raoffloc), "'")
       quote2     = strpos(fitsheader(raoffloc), "'", /reverse_search)
       raoffstring= strmid(fitsheader(raoffloc), quote1 + 1, quote2 - quote1 - 1)
       decoffloc  = where(strmid(fitsheader, 0, 8) EQ 'DECOFFST')
       quote1     = strpos(fitsheader(decoffloc), "'")
       quote2     = strpos(fitsheader(decoffloc), "'", /reverse_search)
       decoffstring = strmid(fitsheader(decoffloc), quote1 + 1, quote2 - quote1 - 1)
       adoffstring  = raoffstring + ' ' + decoffstring
       adoffstring  = adoffstring(0)
       get_coords, offsetad, instring = adoffstring
       fieldad    = fieldad - offsetad
       IF fieldad(0) LT -24.0 THEN fieldad(0) = fieldad(0) + 24.0
       IF fieldad(0) GT 24.0 THEN fieldad(0) = fieldad(0) - 24.0
       fieldad(0) = fieldad(0) * 15.0
       fieldad    = fieldad/!radeg
       ;
       ; project velocity
       helio_vel(ii) = vh(0) * cos(fieldad(1)) * cos(fieldad(0)) + $
                     vh(1) * cos(fieldad(1)) * sin(fieldad(0)) + $
                     vh(2) * sin(fieldad(1))
   ENDFOR 
   ;
   ; make output file
   openw, 1, fout
   printf, 1, '#       filename     helio. vel.'
   FOR ii=0, nfiles-1 DO printf, 1, filename(ii), helio_vel(ii), format='(a16,f16)'
   close, 1
END
