PRO grism_boundbox, hdr, boxx, boxy, warn
   ;
   ; Read bounding box for spectrum from spectrum header
   ;
   ; G.R. Meurer 9/02
   ;
   boxx    = make_array(4, value=0.0)
   boxy    = make_array(4, value=0.0)
   boxx[0] = sxpar(hdr, 'c0x', count=c1)
   boxy[0] = sxpar(hdr, 'c0y', count=c2)
   boxx[1] = sxpar(hdr, 'c1x', count=c3)
   boxy[1] = sxpar(hdr, 'c1y', count=c4)
   boxx[2] = sxpar(hdr, 'c2x', count=c5)
   boxy[2] = sxpar(hdr, 'c2y', count=c6)
   boxx[3] = sxpar(hdr, 'c3x', count=c7)
   boxy[3] = sxpar(hdr, 'c3y', count=c8)
   check   = where([c1, c2, c3, c4, c5, c6, c7, c8] LE 0,nbad)
   warn    = nbad GT 0
   IF warn THEN print,'**** Warning bounding back not properly read in '
END 

