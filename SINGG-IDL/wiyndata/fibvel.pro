FUNCTION fibvel, vsys, inc, xpos, ypos, pa, rr, vrot, fibers=fibers, $
                 radians=radians, ppas=ppas, fast=fast
   IF NOT(keyword_set(radians)) THEN BEGIN
       incrad     = inc / !radeg
       parad      = -pa / !radeg
   ENDIF ELSE BEGIN
       incrad     = inc
       parad      = -pa
   ENDELSE
   IF NOT(keyword_set(ppas)) THEN ppas = 5.0
   ffpos      =  'fiberpos.txt'
   fibdia     = 4.687        ; fiber diameter in arcseconds
   ;
   ; read fiber positions
   readcol, ffpos, fid, dxfib, dyfib, format='(i,f,f)', /silent
   nfibers    = n_elements(fid)
   IF NOT(keyword_set(fibers)) THEN plotfibers = indgen(nfibers) $
   ELSE IF datatype(fibers) EQ 'STR' THEN BEGIN
       readcol, fibers, apid, format='(i)'
       plotfibers = apid - 1
   ENDIF ELSE plotfibers = fibers - 1
   fid        = fid(plotfibers)
   dxfib      = dxfib(plotfibers)
   dyfib      = dyfib(plotfibers)
   nfibers    = n_elements(fid)
   ;
   ; for fast calculation just take the central value
   IF keyword_set(fast) THEN BEGIN
       ;
       ; calculate distance from galactic center at each fiber
       xx         = dxfib
       yy         = dyfib
       aa         = -(xx - xpos) * sin(parad) + (yy - ypos) * cos(parad)
       bb         = ( -(xx - xpos) * cos(parad) - (yy - ypos) * sin(parad) ) / $
         cos(incrad)
       rgrid      = sqrt( aa^2 + bb^2 )
       ;
       ; calculate vrot at each fiber
       vrotgrid   = make_array(nfibers)
       minr       = min(rr, minrpos)
       maxr       = max(rr, maxrpos)
       FOR jj = 0, nfibers-1 DO BEGIN
           IF rgrid(jj) LE minr THEN vrotgrid(jj) = vrot(minrpos) ELSE BEGIN
               IF rgrid(jj) GE maxr THEN vrotgrid(jj) = vrot(maxrpos) ELSE BEGIN
                   rfloorpos    = (where(rr GT rgrid(jj)))(0) - 1
                   vrotgrid(jj) = vrot(rfloorpos) + (rgrid(jj) - rr(rfloorpos)) * $
                     (vrot(rfloorpos+1) - vrot(rfloorpos)) / $
                     (rr(rfloorpos+1) - rr(rfloorpos))
               ENDELSE
           ENDELSE
       ENDFOR
       ;
       ; calculate vfib
       rnonzero   = where(rgrid NE 0, complement=rzero, ncomplement=nrzero)
       costheta   = make_array(nfibers)
       costheta(rnonzero) = aa(rnonzero) / rgrid(rnonzero)
       IF nrzero GT 0 THEN costheta(rzero)    = 0.0
       vfib       = vsys + vrotgrid * costheta * sin(incrad)
   ENDIF ELSE BEGIN
       ;
       ; define positions to sample for each fiber
       npix       = 1 + 2*ceil(0.5*fibdia*ppas)
       fibxx      = (findgen(npix) - (npix - 1)/2) / ppas
       fibxx      = fibxx # make_array(npix, value=1)
       fibyy      = (findgen(npix) - (npix - 1)/2) / ppas
       fibyy      = make_array(npix, value=1) # fibyy
       keeppix    = where(fibxx^2 + fibyy^2 LE (0.5*fibdia)^2, nkeeppix)
       fibxx      = fibxx(keeppix)
       fibyy      = fibyy(keeppix)
       ;
       ; calculate mean velocity in each fiber
       vfib       = make_array(nfibers)
       FOR ii = 0, nfibers-1 DO BEGIN
           ;
           ; calculate distance from galactic center at each point
           xx         = fibxx + dxfib(ii)
           yy         = fibyy + dyfib(ii)
           aa         = -(xx - xpos) * sin(parad) + (yy - ypos) * cos(parad)
           bb         = ( -(xx - xpos) * cos(parad) - (yy - ypos) * sin(parad) ) / $
             cos(incrad)
           rgrid      = sqrt( aa^2 + bb^2 )
           ;
           ; calculate vv at each point
           vrotgrid   = make_array(nkeeppix)
           minr       = min(rr, minrpos)
           maxr       = max(rr, maxrpos)
           FOR jj = 0, nkeeppix-1 DO BEGIN
               IF rgrid(jj) LE minr THEN vrotgrid(jj) = vrot(minrpos) ELSE BEGIN
                   IF rgrid(jj) GE maxr THEN vrotgrid(jj) = vrot(maxrpos) ELSE BEGIN
                       rfloorpos    = (where(rr GT rgrid(jj)))(0) - 1
                       vrotgrid(jj) = vrot(rfloorpos) + (rgrid(jj) - rr(rfloorpos)) * $
                         (vrot(rfloorpos+1) - vrot(rfloorpos)) / $
                         (rr(rfloorpos+1) - rr(rfloorpos))
                   ENDELSE
               ENDELSE
           ENDFOR
           rnonzero   = where(rgrid NE 0, complement=rzero, ncomplement=nrzero)
           costheta   = make_array(nkeeppix)
           costheta(rnonzero) = aa(rnonzero) / rgrid(rnonzero)
           IF nrzero GT 0 THEN costheta(rzero)    = 0.0
           vv         = vsys + vrotgrid * costheta * sin(incrad)
           ;
           ; take mean vv as velocity seen by fiber
           vfib(ii)   = mean(vv)
       ENDFOR
   ENDELSE
   RETURN, vfib
END
