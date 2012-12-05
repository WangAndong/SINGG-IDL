PRO blem_flgstars, xstar, ystar, idstar, dydg, ap, dxbeam, $
                   xg, yg, qualcut, csexid, xd, yd, nflg, dxdg=dxdg
   ;
   ; Flag sources in grism catalog as stars given the 
   ; direct image position of the star.
   ;
   ; xstar   -> x position of identified star in direct image
   ; ystar   -> y position of identified star in direct image
   ; idstar  -> id of identified star
   ; dydg    -> difference in y coordinate y_direct - y_grism
   ;            at position of star
   ; ap      -> aperture width adopted for grism extraction.
   ; dxbeam  -> limits in x of beam for extraction, relative
   ;            to direct image
   ; xg      -> x position of sources in grism image
   ; yg      -> y position of sources in grism image
   ; qualcut <> quality comments from the cutouts.  Sources
   ;            overlapping the 1st order extraction aperture
   ;            for the star are updated to have qualcut=3
   ; csexid  <> sextractor id in direct image of sources. 
   ;            Sources overlapping the 1st order extraction 
   ;            aperture for the star have this updated to 
   ;            have csexid = idstar
   ; xd      <> for grism sources in extraction ap, updated 
   ;            to xstar.
   ; yd      <> for grism sources in extraction ap, updated 
   ;            to ystar.
   ; nflg    <- number of sources flagged
   ; dxdg    -> difference in x coordinate x_direct - x_grism
   ;            at position of star (needed to handle padded
   ;            direct images).
   ;
   ; G. Meurer 07/2004
   ; G. Meurer 07/2006 - added dxdg parameter to handle pears data.
   ;
   IF NOT keyword_set(dxdg) THEN dxdg = 0.0
   xgs1      = xstar[0] + float(dxbeam[0]) - dxdg
   xgs2      = xgs1 + float(dxbeam[1] - dxbeam[0])
   ygs1      = ystar[0] - dydg - 0.5*float(ap)
   ygs2      = ystar[0] - dydg + 0.5*float(ap)
   ;
   ; find sources in grism catalog in that area
   flg       = where(xg GE xgs1 AND xg LE xgs2 AND yg GE ygs1 AND yg LE ygs2, nflg)
   print, 'BLEM_FLGSTARS: search area = ', xgs1, xgs2, ygs1, ygs2
   print, 'BLEM_FLGSTARS: number of grism sources corresponding to star # '$
    +strtrim(string(idstar))+' : '+strtrim(string(nflg))
   ;
   ; update relevant array positions
   IF nflg GT 0 THEN BEGIN 
      qualcut[flg] = '3'
      csexid[flg]  = strtrim(string(idstar[0]),2)
      xd[flg]      = xstar[0]
      yd[flg]      = ystar[0]
   ENDIF 
END 
