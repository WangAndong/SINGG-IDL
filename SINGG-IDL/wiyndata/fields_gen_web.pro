PRO fields_gen_web, folder=folder
   filelist  = 'exptimes.dat'
   fflux     = '../line_fluxes2.dat'
   fhelvel   = 'helio_vel.dat'
   fw20      = '../widths20.dat'
   fw50      = '../widths50.dat'
   finrange  = 'inrange.dat'
   fnotes    = 'obj_notes.dat'
   fmtv      =  '(i,f,f,f,f,f,f,f,f,f,f,f,f,f,f)'
   format1   = '(a,a,a,a,i,f)'
   format2   = '(a,a,a,a,a,a,i,f)'
   fmtf      = '(a,a,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f)'
   fmtw      = '(a,a,x,x,f,f)'
   fmtr      = '(a,i,i,i,i,i,i)'
   folders   = ['Run0410_n','Run0503_n','Run0603_n']
   dates     = [['01/02 Oct 04','28 Feb/01 Mar 05','20/21 Mar 06'], $
                ['02/03 Oct 04','01/02 Mar 05','21/22 Mar 06']]
   outfolder = 'html'
   IF keyword_set(folder) THEN outfolder=folder
   linenames = ['[NII] 6548','H&#945; 6563','[NII] 6584',$
                 'HeI 6678','[SII] 6716','[SII] 6731']
   nlines    = n_elements(linenames)
   paramsnames = ['Systematic velocity', 'Heliocentric velocity', $
                  'Inclination', 'R. A.', 'Dec', 'R. A. (2000)', 'Dec (2000)', $
                  'Position angle', 'Residual RMS', 'Mean velocity error']
   paramsunits = [' km/s', ' km/s', ' degrees', ' arcseconds', ' arcseconds', '', $
                  '', ' degrees', ' km/s', ' km/s']
   nparams     = n_elements(paramsnames)
   specprop  = ['W<sub>50</sub><sup>max</sup>', 'W<sub>50</sub><sup>min</sup>', $
                'W<sub>20</sub><sup>max</sup>', 'W<sub>20</sub><sup>min</sup>', $
                'H&#945; heliocentric velocity']
   cvel      = 299792.458
   halam     = 6562.8169
   ;
   ; load list of filenames
   readcol, filelist, hipassid, galid1, galid2, filename, $
     nexp, texp, format=format1, /silent
   readcol, filelist, temphi, tempga1, tempga2, tempga3, tempga4, $
     tempfile, tempnexp, temptexp, format=format2, /silent
   hipassid  = [hipassid, temphi]
   galid     = [galid1+' '+galid2, $
                tempga1+' '+tempga2+' '+tempga3+' '+tempga4]
   filename  = [filename, tempfile]
   nfiles    = n_elements(filename)
   ;
   ; sort list into hipass name order
   hipass1   = fix(strmid(hipassid, 1, 4))
   order     = sort(hipass1)
   hipassid  = hipassid(order)
   galid     = galid(order)
   filename  = filename(order)
   nexp      = nexp(order)
   texp      = texp(order)
   ;
   ; create filename strings
   fileno    = strmid(filename, 3)
   runno     = fix(strmid(fileno, 1, 1))
   nightno   = strmid(fileno, 3, 1)
   fhtml     = outfolder+'/field'+fileno+'.html'
   fhtmlps   = outfolder+'/field'+fileno+'ps.html'
   fcomb     = '../'+folders(runno-1)+nightno+'/comb'+fileno+'.dat'
   ffit      = '../'+folders(runno-1)+nightno+'/vrot'+fileno+'.dat'
   fobj      = '../'+folders(runno-1)+nightno+'/obj'+fileno+'.co.cr.ms.fits'
   fhome     = 'html/index.html'
   fhomeps   = 'html/indexps.html'
   indexcells   = make_array(nfiles, 2, /string)
   indexline    = make_array(nfiles, /string)
   indexcellsps = make_array(nfiles, 2, /string)
   indexlineps  = make_array(nfiles, /string)
   hasfit       = make_array(nfiles, /integer)
   ;
   ; read heliocentric velocities and put list in order
   readcol, fhelvel, vfilename, helvel, format='(a,f)'
   vorder    = make_array(nfiles)
   FOR ii = 0, nfiles-1 DO vorder(ii) = where(vfilename EQ filename(ii))
   vfilename = vfilename(vorder)
   helvel    = helvel(vorder)
   ;
   ; read flux data
   readcol, fflux, ffilename, fhipassid, l1, w1, f1, l2, w2, f2, l3, w3, f3, $
     l4, w4, f4, l5, w5, f5, l6, w6, f6, format=fmtf
   nfiles    = n_elements(filename)
   lgrid     = make_array(6, nfiles)
   wgrid     = make_array(6, nfiles)
   fgrid     = make_array(6, nfiles)
   FOR ii = 0, 5 DO BEGIN
       scratch   = execute('lgrid(ii,*) = l' + strcompress(string(ii+1), /remove_all))
       scratch   = execute('wgrid(ii,*) = w' + strcompress(string(ii+1), /remove_all))
       scratch   = execute('fgrid(ii,*) = f' + strcompress(string(ii+1), /remove_all))
   ENDFOR
   ;
   ; sort flux list into hipass name order
   fhipass1  = fix(strmid(fhipassid, 1, 4))
   forder    = sort(fhipass1)
   ffilename = ffilename(forder)
   fhipassid = fhipassid(forder)
   lgrid     = lgrid(*, forder)
   wgrid     = wgrid(*, forder)
   fgrid     = fgrid(*, forder)
   havel     = (lgrid(1,*) / halam - 1) * cvel - helvel
   ;
   ; read widths and put lists in order
   readcol, fw20, w20filename, w20hipassid, w20max, w20min, format=fmtw
   readcol, fw50, w50filename, w50hipassid, w50max, w50min, format=fmtw
   whipass1  = fix(strmid(w20hipassid, 1, 4))
   worder    = sort(whipass1)
   w20filename = w20filename(worder)
   w20hipassid = w20hipassid(worder)
   w20max    = w20max(worder)
   w20min    = w20min(worder)
   w50filename = w50filename(worder)
   w50hipassid = w50hipassid(worder)
   w50max    = w50max(worder)
   w50min    = w50min(worder)
   ;
   ; read which lines were in range, and put lists in order
   readcol, finrange, rfilename, ir1, ir2, ir3, ir4, ir5, ir6, format=fmtr
   irgrid    = make_array(6, nfiles, /integer)
   FOR ii = 0, 5 DO $
     scratch = execute('irgrid(ii,*) = ir' + strcompress(string(ii+1), /remove_all))
   rorder    = make_array(nfiles)
   FOR ii = 0, nfiles-1 DO rorder(ii) = where(rfilename EQ filename(ii))
   rfilename = rfilename(rorder)
   irgrid    = irgrid(*, rorder)
   ;
   ; read notes about each galaxy, and put in order
   notes     = make_array(nfiles, /string)
   nfilename = make_array(nfiles, /string)
   openr, 1, fnotes
   FOR ii = 0, nfiles-1 DO BEGIN
       temp      = ''
       readf, 1, temp
       nfilename(ii) = strmid(temp, 0, 16)
       hashloc   = strpos(temp, '#')
       notes(ii) = strmid(temp, hashloc(0)+2)
   ENDFOR
   close, 1
   norder    = make_array(nfiles)
   FOR ii = 0, nfiles-1 DO norder(ii) = where(nfilename EQ filename(ii))
   nfilename = nfilename(norder)
   notes     = notes(norder)
   ;
   ; create html page for each galaxy
   FOR ii = 0, nfiles-1 DO BEGIN
       print, hipassid(ii)
       ;
       ; find out which lines are present
       readcol, fcomb(ii), apid, z1, f1, z2, f2, z3, f3, z4, f4, z5, f5, z6, f6, $
         meanz, meanv, format=fmtv, /silent
       nap        = n_elements(apid)
       zgrid      = make_array(6,nap)
       zgrid(0,*) = z1
       zgrid(1,*) = z2
       zgrid(2,*) = z3
       zgrid(3,*) = z4
       zgrid(4,*) = z5
       zgrid(5,*) = z6
       linedata   = where(total(zgrid, 2) GT 0, nlinedata)
       ;
       ; find out which lines have fluxes, and make table
       fluxtable  = '<tr><td>' + linenames + '</td>'
       FOR jj = 0, nlines-1 DO BEGIN
           IF fgrid(jj, ii) GT 0 THEN fluxtable(jj) = fluxtable(jj) + '<td>' + $
             strcompress(string(lgrid(jj, ii), format='(f12.2)'), /remove_all) + '</td><td>' + $
             strcompress(string(wgrid(jj, ii), format='(f12.3)'), /remove_all) + '</td><td>' + $
             strcompress(string(fgrid(jj, ii), format='(f12.3)'), /remove_all) + '</td></tr>' $
           ELSE IF irgrid(jj, ii) EQ 0 THEN fluxtable(jj) = fluxtable(jj) + $
             '<td COLSPAN=3>Out of range</td></tr>' $
           ELSE fluxtable(jj) = fluxtable(jj) + $
             '<td COLSPAN=3>Too faint to be measured</td></tr>'
       ENDFOR
       ;
       ; see if a fit is available, and read data if yes
       openr, 1, ffit(ii), error=openerr
       close, 1
       IF openerr EQ 0 THEN BEGIN
           hasfit(ii) = 1
           readcol, ffit(ii), col1, col2, col3, col4, format='(a,f,a,f)', /silent
           vsysloc    = where(col1 EQ 'vsys', nvsys)
           vsysloc    = vsysloc(nvsys-1)
           incloc     = where(col1 EQ 'inc', ninc)
           incloc     = incloc(ninc-1)
           params     = make_array(nparams, /string)
           paramserr  = make_array(nparams)
           params(0)  = strcompress(string(col2(vsysloc), format='(f10.2)'), /remove_all)
           paramserr(0)   = col4(vsysloc)
           params(1)  = strcompress(string(col2(vsysloc) - helvel(ii), $
                                           format='(f10.2)'), /remove_all)
           paramserr(1)   = paramserr(0)
           params(2)  = strcompress(string(col2(incloc), format='(f10.2)'), /remove_all)
           paramserr(2)   = col4(incloc)
           params([3,4,7])    = strcompress(string(col2(vsysloc+2:vsysloc+4), $
                                                   format='(f10.2)'), /remove_all)
           paramserr([3,4,7]) = col4(vsysloc+2:vsysloc+4)
           readcol, ffit(ii), col1, col2, col3, format='(a,a,f)', /silent
           meanloc    = where(col1 EQ 'mean', nmean)
           meanloc    = meanloc(nmean-1)
           params(8:9) = strcompress(string(col3([meanloc+1,meanloc]), $
                                            format='(f10.2)'), /remove_all)
           ;
           ; get ra and dec information from fits header
           fitsheader = headfits(fobj(ii))
           raloc      = where(strmid(fitsheader, 0, 3) EQ 'RA ')
           quote1     = strpos(fitsheader(raloc), "'")
           quote2     = strpos(fitsheader(raloc), "'", /reverse_search)
           rastring   = strmid(fitsheader(raloc), quote1 + 1, quote2 - quote1 - 1)
           decloc     = where(strmid(fitsheader, 0, 4) EQ 'DEC ')
           quote1     = strpos(fitsheader(decloc), "'")
           quote2     = strpos(fitsheader(decloc), "'", /reverse_search)
           decstring  = strmid(fitsheader(decloc), quote1 + 1, quote2 - quote1 - 1)
           radecstring = rastring + ' ' + decstring
           radecstring = radecstring(0)
           get_coords, fieldad, instring = radecstring
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
           ra         = fieldad(0) + params(3) / (15.0 * 3600.0 * cos(fieldad(1)*!pi/180.0))
           dec        = fieldad(1) + params(5) / 3600.0
           ra         = ra * 15.0
           radec      = adstring(ra, dec, 2)
           params(5)  = strmid(radec, 1, 12)
           params(6)  = strmid(radec, 15)
           ;
           ; get rotation curve
           readcol, ffit(ii), rr, vrot, err, format='(f,f,x,f)', /silent
           rrloc      = 0
           FOR jj = 1, n_elements(rr)-1 DO IF rr(jj) LT rr(jj-1) THEN rrloc = jj
           rr         = [0.0, rr(rrloc:*)]
           vrot       = [0.0, vrot(rrloc:*)]
           err        = [0.0, err(rrloc:*)]
           nrr        = n_elements(rr)
           ;
           ; make an html table
           fittable    = make_array(max([nrr, nparams]) + 1, /string)
           fittable(0) = '<tr><th>Radius (arcseconds)</th><th>V<sub>rot</sub> (km/s)</th>' + $
             '<th>Property</th><th>Value</th></tr>'
           FOR jj = 1, max([nrr, nparams]) DO BEGIN
               IF jj LE nrr THEN BEGIN
                   fittable(jj) = '<tr><td>' + $
                     strcompress(string(rr(jj-1), format='(i)'), /remove_all) + '</td><td>' + $
                     strcompress(string(vrot(jj-1), format='(f10.1)'), /remove_all)
                   IF err(jj-1) NE 0.0 THEN fittable(jj) = fittable(jj) + ' +/- ' + $
                     strcompress(string(err(jj-1), format='(f10.1)'), /remove_all)
                   fittable(jj) = fittable(jj) + '</td>'
               ENDIF ELSE IF jj EQ nrr+1 THEN fittable(jj) = '<tr><td COLSPAN=2 ROWSPAN=' + $
                 strcompress(string(nparams-nrr, format='(i)'), /remove_all) + '> </td>' $
               ELSE fittable(jj) = '<tr>'
               IF jj LE nparams THEN BEGIN
                   fittable(jj) = fittable(jj) + '<td>' + paramsnames(jj-1) + '</td><td>' + $
                     params(jj-1)
                   IF paramserr(jj-1) NE 0.0 THEN fittable(jj) = fittable(jj) + ' +/- ' + $
                     strcompress(string(paramserr(jj-1), format='(f10.2)'), /remove_all)
                   fittable(jj) = fittable(jj) + paramsunits(jj-1) + '</td></tr>'
               ENDIF ELSE IF jj EQ nparams+1 THEN fittable(jj) = fittable(jj) + $
                 '<td COLSPAN=2 ROWSPAN=' + $
                 strcompress(string(nrr-nparams, format='(i)'), /remove_all) + '> </td></tr>' $
               ELSE fittable(jj) = fittable(jj) + '</tr>'
           ENDFOR
       ENDIF
       ;
       ; make line for index page
       indexcells(ii, *)   = ''
       indexcellsps(ii, *) = ''
       FOR jj = 0, nlinedata-1 DO BEGIN
           IF indexcells(ii, 0) NE '' THEN indexcells(ii, *) = indexcells(ii, *) + ', '
           indexcells(ii, 0) = indexcells(ii, 0) + $
             '<a href="images/field' + fileno(ii) + '_' + strcompress(string(linedata(jj)+1), $
             /remove_all) + '.jpg">' + linenames(linedata(jj)) + '</a>'
           indexcells(ii, 1) = indexcells(ii, 1) + $
             '<a href="images/fieldpic' + fileno(ii) + '_' + strcompress(string(linedata(jj)+1), $
             /remove_all) + '.jpg">' + linenames(linedata(jj)) + '</a>'
           IF indexcellsps(ii, 0) NE '' THEN indexcellsps(ii, *) = indexcellsps(ii, *) + ', '
           indexcellsps(ii, 0) = indexcellsps(ii, 0) + $
             '<a href="ps/field' + fileno(ii) + '_' + strcompress(string(linedata(jj)+1), $
             /remove_all) + '.ps">' + linenames(linedata(jj)) + '</a>'
           indexcellsps(ii, 1) = indexcellsps(ii, 1) + $
             '<a href="ps/fieldpic' + fileno(ii) + '_' + strcompress(string(linedata(jj)+1), $
             /remove_all) + '.ps">' + linenames(linedata(jj)) + '</a>'
       ENDFOR 
       date          = dates(runno(ii)-1, fix(nightno(ii))-1)
       date          = date(0)
       indexline(ii) = '<tr><td><a href="field' + fileno(ii) + '.html">' + hipassid(ii) + $
         '</a></td><td>' + galid(ii) + '</td><td>' + date + $
         '</td><td><a href="images/meanvel' + fileno(ii) + '.jpg">mean</a>, ' + $
         '<a href="images/vel' + fileno(ii) + '.jpg">all</a></td><td>' + $
         '<a href="images/meanvelpic' + fileno(ii) + '.jpg">mean</a>, ' + $
         '<a href="images/velpic' + fileno(ii) + '.jpg">all</a></td><td>' + $
         indexcells(ii, 0) + $
         '</td><td>' + indexcells(ii, 1) + '</td><td><a href="images/intspec' + fileno(ii) + $
         '.jpg">plots</a>, <a href="data/intspec' + fileno(ii) + '.dat">ASCII</a></td>' + $
         '<td><a href="images/velprof' + fileno(ii) + $
         '.jpg">plot</a>, <a href="data/velprof' + fileno(ii) + '.dat">ASCII</a></td>'
       IF hasfit(ii) EQ 1 THEN indexline(ii) = indexline(ii) + '<td><a href="images/vrot' + $
         fileno(ii) + '.jpg">plots</a>, <a href="data/vrot' + fileno(ii) + $
         '.dat">parameters</a></td>'
       indexline(ii) = indexline(ii) + '</tr>'
       indexlineps(ii) = '<tr><td><a href="field' + fileno(ii) + 'ps.html">' + hipassid(ii) + $
         '</a></td><td>' + galid(ii) + '</td><td>' + date + $
         '</td><td><a href="ps/meanvel' + fileno(ii) + '.ps">mean</a>, ' + $
         '<a href="ps/vel' + fileno(ii) + '.ps">all</a></td><td>' + $
         '<a href="ps/meanvelpic' + fileno(ii) + '.ps">mean</a>, ' + $
         '<a href="ps/velpic' + fileno(ii) + '.ps">all</a></td><td>' + $
         indexcellsps(ii, 0) + $
         '</td><td>' + indexcellsps(ii, 1) + '</td><td><a href="ps/intspec' + fileno(ii) + $
         '.ps">plots</a>, <a href="data/intspec' + fileno(ii) + '.dat">ASCII</a></td>' + $
         '<td><a href="ps/velprof' + fileno(ii) + $
         '.ps">plot</a>, <a href="data/velprof' + fileno(ii) + '.dat">ASCII</a></td>'
       IF hasfit(ii) EQ 1 THEN indexlineps(ii) = indexlineps(ii) + '<td><a href="ps/vrot' + $
         fileno(ii) + '.ps">plots</a>, <a href="data/vrot' + fileno(ii) + $
         '.dat">parameters</a></td>'
       indexlineps(ii) = indexlineps(ii) + '</tr>'
       ;
       ; create table entries
       cells      = [[linenames], [linenames]]
       FOR jj = 0, nlinedata-1 DO BEGIN
           cells(linedata(jj),0) = $
             '<a href="images/field'+fileno(ii)+'_'+strcompress(string(linedata(jj)+1), $
             /remove_all)+'.jpg">'+cells(linedata(jj),0)+'</a>'
           cells(linedata(jj),1) = $
             '<a href="images/fieldpic'+fileno(ii)+'_'+strcompress(string(linedata(jj)+1), $
             /remove_all)+'.jpg">'+cells(linedata(jj),1)+'</a>'
       ENDFOR 
       cellsps    = [[linenames], [linenames]]
       FOR jj = 0, nlinedata-1 DO BEGIN
           cellsps(linedata(jj),0) = $
             '<a href="ps/field'+fileno(ii)+'_'+strcompress(string(linedata(jj)+1), $
             /remove_all)+'.ps">'+cellsps(linedata(jj),0)+'</a>'
           cellsps(linedata(jj),1) = $
             '<a href="ps/fieldpic'+fileno(ii)+'_'+strcompress(string(linedata(jj)+1), $
             /remove_all)+'.ps">'+cellsps(linedata(jj),1)+'</a>'
       ENDFOR 
       ;
       ; create integrated spectrum properties table entries
       specval    = strcompress(string([w50max(ii), w50min(ii), w20max(ii), w20min(ii), $
                                        havel(ii)], format='(f10.2)'), /remove_all) + ' km/s'
       ;
       ; write html for jpg page
       openw, 1, fhtml(ii)
       printf, 1, '<HTML>'
       printf, 1, '<HEAD>'
       printf, 1, '<TITLE>'+hipassid(ii)+' ('+galid(ii)+')</TITLE>'
       printf, 1, '</HEAD>'
       printf, 1, ''
       printf, 1, '<BODY BGCOLOR="#FFFFFF">'
       printf, 1, ''
       printf, 1, '<P><TABLE WIDTH=100%>'
       printf, 1, '<tr><td>jpg version</td><td align=center><a href="index.html">' + $
         'Return to index</a></td><td align=right><a href="field'+fileno(ii)+ $
         'ps.html">ps version</a></td></tr>'
       printf, 1, '</TABLE></P>'
       printf, 1, ''
       printf, 1, '<P>'
       printf, 1, '<img src="images/meanvelpic'+fileno(ii)+'.jpg">'
       printf, 1, '</P>'
       printf, 1, ''
       printf, 1, '<P>'
       printf, 1, notes(ii)
       printf, 1, '</P>'
       printf, 1, ''
       IF hasfit(ii) EQ 1 THEN BEGIN
           printf, 1, '<P><TABLE BORDER=1 CELLPADDING=5>'
           printf, 1, '<tr><th COLSPAN=4>Rotation curve fit</th></tr>'
           FOR jj = 0, max([nrr, nparams]) DO printf, 1, fittable(jj)
           printf, 1, '<tr><td COLSPAN=2><a href="images/vrot'+fileno(ii)+$
             '.jpg">Rotation curve plots</a></td><td COLSPAN=2><a href="data/vrot' + $
             fileno(ii) + '.dat">Parameter table</a></td></tr>'
           printf, 1, '</TABLE></P>'
           printf, 1, ''
       ENDIF
       printf, 1, '<P><TABLE BORDER=1 CELLPADDING=5>'
       printf, 1, '<tr><th COLSPAN=2>Velocity Fields</th><th COLSPAN=2>Velocity, '+$
         'Flux, FWHM and Equivalent Width Fields</th></tr>'
       printf, 1, '<tr><th>Without Background Image</th><th>With Background Image</th>'+$
         '<th>Without Background Image</th><th>With Background Image</th></tr>'
       printf, 1, '<tr><td ROWSPAN=6><a href="images/meanvel'+fileno(ii)+'.jpg">mean</a>, '+$
         '<a href="images/vel'+fileno(ii)+'.jpg">all</a>'+$
         '</td><td ROWSPAN=6><a href="images/meanvelpic'+fileno(ii)+'.jpg">mean</a>, '+$
         '<a href="images/velpic'+fileno(ii)+'.jpg">all</a></td>' + $
         '<td>' + cells(0,0) + '</td><td>' + cells(0,1) + '</td></tr>'
       FOR jj = 1, 5 DO printf, 1, '<tr><td>'+cells(jj,0)+'</td><td>'+cells(jj,1)+$
         '</td></tr>'
       printf, 1, '</TABLE></P>'
       printf, 1, ''
       printf, 1, '<P><TABLE BORDER=1 CELLPADDING=5>'
       printf, 1, '<tr><th COLSPAN=2>Other plots</th></tr>'
       printf, 1, '<tr><th>Plots</th><th>ASCII</th></tr>'
       printf, 1, '<tr><td><a href="images/intspec'+fileno(ii)+'.jpg">' + $
         'Integrated spectra</a></td><td><a href="data/intspec'+fileno(ii)+$
         '.dat">Integrated spectra</a></td></tr>'
       printf, 1, '<tr><td><a href="images/velprof'+fileno(ii)+$
         '.jpg">H&#945; / HI velocity profiles</a></td><td><a href="data/velprof'+fileno(ii)+$
         '.dat">H&#945; / HI velocity profiles</a></td></tr>'
       printf, 1, '</TABLE></P>'
       printf, 1, ''
       printf, 1, '<P><TABLE BORDER=1 CELLPADDING=5>'
       printf, 1, '<tr><th COLSPAN=4>Integrated spectrum emission lines</th></tr>'
       printf, 1, '<tr><th>Emission line</th><th>Wavelength (&#197;)</th>' + $
         '<th>FWHM (&#197;)</th><th>Flux (counts/s)</th></tr>'
       FOR jj = 0, 5 DO printf, 1, fluxtable(jj)
       printf, 1, '</TABLE></P>'
       printf, 1, ''
       printf, 1, '<P><TABLE BORDER=1 CELLPADDING=5>'
       printf, 1, '<tr><th COLSPAN=2>Integrated H&#945; properties</th></tr>'
       printf, 1, '<tr><th>Property</th><th>Value</th></tr>'
       FOR jj = 0, 4 DO printf, 1, '<tr><td>' + specprop(jj) + '</td><td>' + $
         specval(jj) + '</td></tr>'
       printf, 1, '</TABLE></P>'
       printf, 1, ''
       printf, 1, '<P><TABLE WIDTH=100%>'
       printf, 1, '<tr><td>jpg version</td><td align=center><a href="index.html">' + $
         'Return to index</a></td><td align=right><a href="field'+fileno(ii)+ $
         'ps.html">ps version</a></td></tr>'
       printf, 1, '</TABLE></P>'
       printf, 1, ''
       printf, 1, '<P><small>Page generated automatically by fields_gen_web.pro on ' + $
         systime() + '</P>'
       printf, 1, ''
       printf, 1, '</BODY>'
       printf, 1, '</HTML>'
       printf, 1, ''
       close, 1
       ;
       ; write html for ps page
       openw, 2, fhtmlps(ii)
       printf, 2, '<HTML>'
       printf, 2, '<HEAD>'
       printf, 2, '<TITLE>'+hipassid(ii)+' ('+galid(ii)+')</TITLE>'
       printf, 2, '</HEAD>'
       printf, 2, ''
       printf, 2, '<BODY BGCOLOR="#FFFFFF">'
       printf, 2, ''
       printf, 2, '<P><TABLE WIDTH=100%>'
       printf, 2, '<tr><td><a href="field'+fileno(ii)+'.html">jpg version</a></td>'+$
         '<td align=center><a href="indexps.html">' + $
         'Return to index</a></td><td align=right>ps version</td></tr>'
       printf, 2, '</TABLE></P>'
       printf, 2, ''
       printf, 2, '<H2>'+hipassid(ii)+' ('+galid(ii)+')</H2>'
       printf, 2, ''
       printf, 2, '<P>'
       printf, 2, notes(ii)
       printf, 2, '</P>'
       printf, 2, ''
       IF hasfit(ii) EQ 1 THEN BEGIN
           printf, 2, '<P><TABLE BORDER=1 CELLPADDING=5>'
           printf, 2, '<tr><th COLSPAN=4>Rotation curve fit</th></tr>'
           FOR jj = 0, max([nrr, nparams]) DO printf, 2, fittable(jj)
           printf, 2, '<tr><td COLSPAN=2><a href="ps/vrot'+fileno(ii)+$
             '.ps">Rotation curve plots</a></td><td COLSPAN=2><a href="data/vrot' + $
             fileno(ii) + '.dat">Parameter table</a></td></tr>'
           printf, 2, '</TABLE></P>'
           printf, 2, ''
       ENDIF
       printf, 2, '<P><TABLE BORDER=1 CELLPADDING=5>'
       printf, 2, '<tr><th COLSPAN=2>Velocity Fields</th><th COLSPAN=2>Velocity, '+$
         'Flux, FWHM and Equivalent Width Fields</th></tr>'
       printf, 2, '<tr><th>Without Background Image</th><th>With Background Image</th>'+$
         '<th>Without Background Image</th><th>With Background Image</th></tr>'
       printf, 2, '<tr><td ROWSPAN=6><a href="ps/meanvel'+fileno(ii)+'.ps">mean</a>, '+$
         '<a href="ps/vel'+fileno(ii)+'.ps">all</a>'+$
         '</td><td ROWSPAN=6><a href="ps/meanvelpic'+fileno(ii)+'.ps">mean</a>, '+$
         '<a href="ps/velpic'+fileno(ii)+'.ps">all</a></td>' + $
         '<td>' + cellsps(0,0) + '</td><td>' + cellsps(0,1) + '</td></tr>'
       FOR jj = 1, 5 DO printf, 2, '<tr><td>'+cellsps(jj,0)+'</td><td>'+cellsps(jj,1)+$
         '</td></tr>'
       printf, 2, '</TABLE></P>'
       printf, 2, ''
       printf, 2, '<P><TABLE BORDER=1 CELLPADDING=5>'
       printf, 2, '<tr><th COLSPAN=2>Other plots</th></tr>'
       printf, 2, '<tr><th>Plots</th><th>ASCII</th></tr>'
       printf, 2, '<tr><td><a href="ps/intspec'+fileno(ii)+'.ps">' + $
         'Integrated spectra</a></td><td><a href="data/intspec'+fileno(ii)+$
         '.dat">Integrated spectra</a></td></tr>'
       printf, 2, '<tr><td><a href="ps/velprof'+fileno(ii)+$
         '.ps">H&#945; / HI velocity profiles</a></td><td><a href="data/velprof'+fileno(ii)+$
         '.dat">H&#945; / HI velocity profiles</a></td></tr>'
       printf, 2, '</TABLE></P>'
       printf, 2, ''
       printf, 2, '<P><TABLE BORDER=1 CELLPADDING=5>'
       printf, 2, '<tr><th COLSPAN=4>Integrated spectrum emission lines</th></tr>'
       printf, 2, '<tr><th>Emission line</th><th>Wavelength (&#197;)</th>' + $
         '<th>FWHM (&#197;)</th><th>Flux (counts/s)</th></tr>'
       FOR jj = 0, 5 DO printf, 2, fluxtable(jj)
       printf, 2, '</TABLE></P>'
       printf, 2, ''
       printf, 2, '<P><TABLE BORDER=1 CELLPADDING=5>'
       printf, 2, '<tr><th COLSPAN=2>Integrated H&#945; properties</th></tr>'
       printf, 2, '<tr><th>Property</th><th>Value</th></tr>'
       FOR jj = 0, 4 DO printf, 2, '<tr><td>' + specprop(jj) + '</td><td>' + $
         specval(jj) + '</td></tr>'
       printf, 2, '</TABLE></P>'
       printf, 2, ''
       printf, 2, '<P><TABLE WIDTH=100%>'
       printf, 2, '<tr><td><a href="field'+fileno(ii)+'.html">jpg version</a></td>'+$
         '<td align=center><a href="indexps.html">' + $
         'Return to index</a></td><td align=right>ps version</td></tr>'
       printf, 2, '</TABLE></P>'
       printf, 2, ''
       printf, 2, '<P><small>Page generated automatically by fields_gen_web.pro on ' + $
         systime() + '</P>'
       printf, 2, ''
       printf, 2, '</BODY>'
       printf, 2, '</HTML>'
       printf, 2, ''
       close, 2
   ENDFOR 
   ;
   ; create index page for jpg pages
   openw, 3, fhome
   printf, 3, '<HTML>'
   printf, 3, '<HEAD>'
   printf, 3, '<TITLE>WIYN Sparsepak kinematics</TITLE>'
   printf, 3, '</HEAD>'
   printf, 3, ''
   printf, 3, '<BODY BGCOLOR="#FFFFFF">'
   printf, 3, ''
   printf, 3, '<P><TABLE WIDTH=100%>'
   printf, 3, '<tr><td>jpg version</td><td align=right><a href="indexps.html">'+$
     'ps version</a></td></tr>'
   printf, 3, '</TABLE></P>'
   printf, 3, ''
   printf, 3, '<P><TABLE border=1 cellpadding=3>'
   printf, 3, '<tr><th ROWSPAN=2>HIPASS Name</th><th ROWSPAN=2>Optical ID</th>'+$
     '<th ROWSPAN=2>Date Observed</th><th COLSPAN=2>Velocity Fields</th>'+$
     '<th COLSPAN=2>Velocity, Flux, FWHM and Equivalent Width Fields</th>'+$
     '<th ROWSPAN=2>Integrated Spectra</th>'+$
     '<th ROWSPAN=2>H&#945; / HI Velocity Profiles</th>' + $
     '<th ROWSPAN=2>Rotation curve fit</th></tr>'
   printf, 3, '<tr><th>Without Background Image</th><th>With Background Image</th>'+$
     '<th>Without Background Image</th><th>With Background Image</th></tr>'
   FOR ii = 0, nfiles-1 DO printf, 3, indexline(ii)
   printf, 3, '</TABLE></P>'
   printf, 3, ''
   printf, 3, '<P><TABLE border=1 cellpadding=3>'
   printf, 3, '<tr><th>Plots</th><th>ASCII</th></tr>'
   printf, 3, '<tr><td><a href="images/widths50.jpg">H&#945; / HI linewidths ' +$
     '(W<sub>50</sub>)</a></td><td><a href="data/widths50.dat">' + $
     'H&#945; / HI linewidths (W<sub>50</sub>)</a></td></tr>'
   printf, 3, '<tr><td><a href="images/widths20.jpg">H&#945; / HI linewidths ' +$
     '(W<sub>20</sub>)</a></td><td><a href="data/widths20.dat">' + $
     'H&#945; / HI linewidths (W<sub>20</sub>)</a></td></tr>'
   printf, 3, '<tr><td><a href="images/rmag_lineratio.jpg">R band magnitude / linewidth ' +$
     'ratio</a></td><td><a href="data/rmag_lineratio.dat">R band magnitude / linewidth ' +$
     'ratio</a></td></tr>'
   printf, 3, '<tr><td></td><td><a href="data/line_fluxes2.dat">Line fluxes</a></td></tr>'
   printf, 3, '</TABLE></P>'
   printf, 3, ''
   printf, 3, '<P><TABLE border=1 cellpadding=3>'
   printf, 3, '<tr><th COLSPAN=2>Documentation</th></tr>'
   printf, 3, '<tr><td><a href="docs/irafreport.ps">Extracting multiple spectra in IRAF' + $
     '</a></td><td><a href="docs/irafreport.tex">latex</a></td></tr>'
   printf, 3, '<tr><td><a href="docs/idlreport.ps">IDL plotting of velocity fields and ' + $
     'related data</a></td><td><a href="docs/idlreport.tex">latex</a></td></tr>'
   printf, 3, '</TABLE></P>'
   printf, 3, ''
   printf, 3, '<P><TABLE WIDTH=100%>'
   printf, 3, '<tr><td>jpg version</td><td align=right><a href="indexps.html">'+$
     'ps version</a></td></tr>'
   printf, 3, '</TABLE></P>'
   printf, 3, ''
   printf, 3, '<P><small>Page generated automatically by fields_gen_web.pro on ' + $
     systime() + '</P>'
   printf, 3, ''
   printf, 3, '</BODY>'
   printf, 3, '</HTML>'
   close, 3
   ;
   ; create index page for ps pages
   openw, 4, fhomeps
   printf, 4, '<HTML>'
   printf, 4, '<HEAD>'
   printf, 4, '<TITLE>WIYN Sparsepak kinematics</TITLE>'
   printf, 4, '</HEAD>'
   printf, 4, ''
   printf, 4, '<BODY BGCOLOR="#FFFFFF">'
   printf, 4, ''
   printf, 4, '<P><TABLE WIDTH=100%>'
   printf, 4, '<tr><td><a href="index.html">jpg version</a></td><td align=right>'+$
     'ps version</td></tr>'
   printf, 4, '</TABLE></P>'
   printf, 4, ''
   printf, 4, '<P><TABLE border=1 cellpadding=3>'
   printf, 4, '<tr><th ROWSPAN=2>HIPASS Name</th><th ROWSPAN=2>Optical ID</th>'+$
     '<th ROWSPAN=2>Date Observed</th><th COLSPAN=2>Velocity Fields</th>'+$
     '<th COLSPAN=2>Velocity, Flux, FWHM and Equivalent Width Fields</th>'+$
     '<th ROWSPAN=2>Integrated Spectra</th>'+$
     '<th ROWSPAN=2>H&#945; / HI Velocity Profiles</th>' + $
     '<th ROWSPAN=2>Rotation curve fit</th></tr>'
   printf, 4, '<tr><th>Without Background Image</th><th>With Background Image</th>'+$
     '<th>Without Background Image</th><th>With Background Image</th></tr>'
   FOR ii = 0, nfiles-1 DO printf, 4, indexlineps(ii)
   printf, 4, '</TABLE></P>'
   printf, 4, ''
   printf, 4, '<P><TABLE border=1 cellpadding=3>'
   printf, 4, '<tr><th>Plots</th><th>ASCII</th></tr>'
   printf, 4, '<tr><td><a href="ps/widths50.ps">H&#945; / HI linewidths ' +$
     '(W<sub>50</sub>)</a></td><td><a href="data/widths50.dat">' + $
     'H&#945; / HI linewidths (W<sub>50</sub>)</a></td></tr>'
   printf, 4, '<tr><td><a href="ps/widths20.ps">H&#945; / HI linewidths ' +$
     '(W<sub>20</sub>)</a></td><td><a href="data/widths20.dat">' + $
     'H&#945; / HI linewidths (W<sub>20</sub>)</a></td></tr>'
   printf, 4, '<tr><td><a href="ps/rmag_lineratio.ps">R band magnitude / linewidth ' +$
     'ratio</a></td><td><a href="data/rmag_lineratio.dat">R band magnitude / linewidth ' +$
     'ratio</a></td></tr>'
   printf, 4, '<tr><td></td><td><a href="data/line_fluxes2.dat">Line fluxes</a></td></tr>'
   printf, 4, '</TABLE></P>'
   printf, 4, ''
   printf, 4, '<P><TABLE border=1 cellpadding=3>'
   printf, 4, '<tr><th COLSPAN=2>Documentation</th></tr>'
   printf, 4, '<tr><td><a href="docs/irafreport.ps">Extracting multiple spectra in IRAF' + $
     '</a></td><td><a href="docs/irafreport.tex">latex</a></td></tr>'
   printf, 4, '<tr><td><a href="docs/idlreport.ps">IDL plotting of velocity fields and ' + $
     'related data</a></td><td><a href="docs/idlreport.tex">latex</a></td></tr>'
   printf, 4, '</TABLE></P>'
   printf, 4, ''
   printf, 4, '<P><TABLE WIDTH=100%>'
   printf, 4, '<tr><td><a href="index.html">jpg version</a></td><td align=right>'+$
     'ps version</td></tr>'
   printf, 4, '</TABLE></P>'
   printf, 4, ''
   printf, 4, '<P><small>Page generated automatically by fields_gen_web.pro on ' + $
     systime() + '</P>'
   printf, 4, ''
   printf, 4, '</BODY>'
   printf, 4, '</HTML>'
   close, 4


END
