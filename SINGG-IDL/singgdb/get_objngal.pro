PRO get_objngal, ddb, objun, mngal, ents, fblank=fblank
   ;
   ; determine the HIPASS object names and max number 
   ; of Halpha sources for that source
   ;
   ; ddb    -> name of database.  It should be "like" 
   ;           singgg_derived
   ; objun  <- array of unique HIPASS object names
   ; mngal  <- max Halpha sources for that source
   ; ents   <- entry into singg_sample
   ; fblank -> name of file containing list of blank fields
   ;
   ; G. Meurer (JHU) 01/2007
   ; G. Meurer (ICRAR/UWA) 11/2011 
   ;    Improved documentation, use sure fire method to count galaxies
   ;
   dbopen, ddb 
   IF strpos(strupcase(ddb),'SUPERSINGG') LT 0 THEN BEGIN 
      ; 
      ; this big chunk of code for the case of a 'singg_derived' like
      ; database.
      dbext,-1,'name,runid,filter_r,filter_n,object,logmhi,entry_sample',$
            name,runid,filtr,filtn,hname,lmhi,entsamp
      dbclose
      ;
      k0    = sort(hname)
      k1    = uniq(hname[k0])
      k0    = k0[k1]
      ;
      ; objun are the unique HIPASS objects
      objun = hname[k0]
      lmhi  = lmhi[k0]
      nobj  = n_elements(k0)
      mngal = make_array(nobj, /int, value=0)
      ents  = make_array(nobj, /int, value=0)
      ;
      ; make a string that should be unique for each dataset
      str   = ljust(runid,6)+ljust(filtr,11)+ljust(filtn,11)   
      FOR kk = 0, nobj-1 DO BEGIN 
         jj = where(hname EQ objun[kk], njj)
         IF njj LE 1 THEN BEGIN 
            ;
            ; zero or 1 matches, this should be easy...
            mngal[kk] = njj
            IF njj EQ 1 THEN ents[kk] = entsamp[jj[0]]
         ENDIF ELSE BEGIN 
            ;
            ; now need to sort out how many datasets
            ; and how many objects in each
            k0   = sort(str[jj])
            k1   = uniq(str[jj[k0]])
            ;
            ; unique datasets are stored in strs, nrr = number of these datasets
            strs = str[jj[k0[k1]]]    
            nrr  = n_elements(strs)
            ents[kk]  = entsamp[jj[0]]
   ;         IF nrr EQ njj THEN BEGIN 
   ;            ;
   ;            ; number of datasets = number of objects matched 
   ;            ; (probably) means only 1 galaxy detected in each
   ;            mngal[kk] = 1
   ;         ENDIF ELSE BEGIN 
            ;
            ; this should always work - count the number of sources 
            ; in each dataset and take the maximum
            ngal = make_array(nrr, /int, value=0)
            FOR jj = 0, nrr-1 DO BEGIN 
               ii = where(hname EQ objun[kk] AND str EQ strs[jj], nii)
               ngal[jj] = nii
            ENDFOR 
            mngal[kk] = max(ngal)
   ;         ENDELSE
         ENDELSE 
      ENDFOR 
      ;stop
      ;
      ; look through blank fields list, 
      ; set mngal = 0 for those in objun
      IF keyword_set(fblank) THEN BEGIN 
         readcol, fblank, nambl, format='(a)'
         nbl   = n_elements(nambl)
         nambl = strtrim(nambl,2)
         FOR ii = 0, nbl-1 DO BEGIN 
            jj = strpos(objun, nambl[ii])
            kk = where(jj GE 0, nkk)
            IF nkk GT 0 THEN BEGIN 
               print, nambl[ii]+'    ',lmhi[kk[0]],nkk,kk
               mngal[kk] = 0
            ENDIF 
         ENDFOR 
      ENDIF 
      ;
   ENDIF ELSE BEGIN 
      ;
      ; this chunk of code for 'supersingg_master' like databases
      ;
      ; get required quantities from the database
      list1 = dbfind('entoptphot > 1')
      list2 = dbfind('entoptphot = -10')
      list  = [list1, list2]
      dbext,list,'sname,hname,lmhi,entsamp,fhadetect',name,hname,lmhi,entsamp,fhadetect
      ;
      ; find unique hipass names
      k0    = sort(hname)
      k1    = uniq(hname[k0])
      k0    = k0[k1]
      ;
      ; objun are the unique HIPASS objects
      objun = hname[k0]
      lmhi  = lmhi[k0]
      ents  = entsamp[k0]
      fhad  = fhadetect[k0]
      nobj  = n_elements(k0)
      mngal = make_array(nobj, /int, value=0)
      ;
      ; count the number of sources that correspond to 
      ; each unique target
      FOR jj = 0, nobj-1 DO BEGIN 
         ii        = where(hname EQ objun[jj], nii)
         IF nii LE 0 THEN stop, 'no matches of object = '+objun[jj]+', but this can not be....'
         mngal[jj] = nii
      ENDFOR 
      ;
      ; set mngal to 0 where the flag fhadetect <= 0 
      ; (not detected or not observed in Halpha)
      k2    = where(fhad LE 0, nk2)
      IF nk2 GT 0 THEN mngal[k2] = 0
   ENDELSE
   ;
   ; sort by hipass name so both methods give same results
   ss    = sort(objun)
   objun = strtrim(temporary(objun[ss]),2)
   lmhi  = temporary(lmhi[ss])
   ents  = temporary(ents[ss])
   mngal = temporary(mngal[ss])
   ;
   dbclose,ddb
END
 
