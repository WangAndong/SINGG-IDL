PRO blem_sexids, segm, nid, id, imin, imax
   ;
   ; determine Sextractor direct image IDs and their column 
   ; position ranges
   ;
   ; segm -> section of segmentation image
   ; nid  -> number of sources found
   ; id   -> IDs of sources found
   ; imin -> min column 
   ; imax -> max column
   ;
   ; G.R. Meurer  12/02
   tmp  = segm[uniq(segm, sort(segm))]
   ; 
   k    = where(tmp GT 0, nid)
   IF nid GT 0 THEN BEGIN 
      id   = tmp[k]
      imin = id*0
      imax = id*0
      sz = size(segm)
      FOR j = 0, nid-1 DO BEGIN 
         kk = where(segm EQ id[j], nkk)
         ii = kk MOD sz[1]
         imin[j] = min(ii)
         imax[j] = max(ii)
      ENDFOR 
   ENDIF ELSE BEGIN 
      id    = -1
      imin  = -1
      imax  = -1
   ENDELSE 
END 

