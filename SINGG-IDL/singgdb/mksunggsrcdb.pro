PRO mksunggsrcdb, usedir=usedir, calcflow=calcflow
   ;
   ; Make database for SUNGG sample.  The IDL database is written
   ; to the files sungg_sample.db? in the directory sdir.  sdir is
   ; currently hard wired but can be overwridden with the usedir
   ; keyword.
   ;
   ;   usedir   -> If set this is the directory where data files are 
   ;               read from and the directory that the database is 
   ;               written to.
   ;   calcflow -> If set, the multi-attractor flow model JPHFLOW is
   ;               run on the final positions and velocities.  This
   ;               results in new distances. (ie. use calcflow=1)

   
   ;
   ; G. Meurer 08/2004 - or earlier... program originally written.
   ; G. Meurer 12/2004 - adjusted to insert E(B-V) and flag value from 
   ;                     Schlegel et al. (1998, ApJ, 500, 525) into
   ;                     database.  Databse now has E(b-V) in its sort 
   ;                     block.
   ;                   - Added calcflow keyword
   ;
   ; Later butchered by I. Wong (May 2005) 
   ; *** note that in this version, one has to run jphflow separately beforehand
   ; before you can set calcflow=1 ***


   ; set parameters
   ;
   c        = 2.9978d5                    ; speed of light
   h0       = 70.0                        ; km/s/Mpc
   distmin  = 1.0                         ; minimum distance allowed from vshap
   invrad   = 3.141592654/180.      ; radians / degree
   vmin     = -999.9
   ;
   ; input file
   IF keyword_set(usedir) THEN sdir = usedir $
     ELSE sdir = './'
   fil      =  'readsamp_input.txt.new2'      ; singg sample name
   fild     = sdir+'distance_override_sungg.dat' ; over-ride distances
   fill     = sdir+'mksinggsampdb.log'    ; report results here
   invers   = 5                           ; sungg sample version
   sfil     = sdir+fil
  
   ;
   ;  Databases to match.  Order given is inverted priority
   ;  (last match is used for optical ID if relevant).
   dblis    = ['singg_sample','hipass_sccc', 'hipass_avcc', 'hipass_bgc', $
               'hicat_feb04', 'hipass_remeasure']
   dbnam    = ['SINGG','SCCC', 'AVCC', 'BGC', 'HICAT', 'REMEASURE']
   shnam    = ['','SCC', 'AVC', 'BGC', 'HCT', 'REM']
   vnam     = ['vhel','vhel', 'vlsr', 'vhel', 'vel_50max', 'vhel']
   wnam     = ['w50','w50', 'w50', 'w50', 'width_50max', 'w50']
   vsnam    = ['vshap','', '', 'vshap', 'vshap', 'vshap']
   pknam    = ['sp','sp', 'sp', 'sp', 'sp', 'sp']
   flxnam   = ['sint','sint', 'sint', 'sint', 'sint', 'sint']
   ebvnam   = ['','', '', '', '', '']
   onam     = ['optid','optid', 'optid', 'optid', '', 'optid']
   nnam     = ['NAME','NAME', '', 'NAME', 'HIPASS_NAME', 'NAME']
   updpar   = [0, 0, 0b, 1b, 1b, 1b]
   ;
   tolv     = 0.5
   tolpk    = 0.0005
   tolflx   = 0.1
   tolebv   = 0.001
   tolp     = 5.0/3600.0
   tollm    = 0.005
   ;
   ndb      = n_elements(dblis)
   ;
   ; database matching criteria
   rmatch   = 4.0                         ; arcmin
   vmatch   = 400.0                       ; km/s
   ;
   item = ['field','name','optid','catalog','ra','dec','glat','glong',$
           'sep','vhel','vsource','vlg','vcmb','vtonry','vshap',$
           'w50','sp','sint','distance','logmhi',$
           'ebv','mask_ebv','original','entry_'+dbnam]
   tout     = ['C','C','C','C','R','R','R','R',$
               'R','R','C','R','R','R','R',$
               'R','R','R','R','R',$
               'R','B','B',make_array(ndb,/string,value='I')]

   tlen  = [0,0,0,0,8,8,4,4,$
            4,4,0,4,4,4,4,$
            4,4,4,4,4,$
            4,1,1,make_array(ndb,/int,value=2)]

   dstring = 'a,a,a,a,d,d,f,f,f,f,a,f,f,f,f,f,f,f,f,f,f,b,b'
   FOR ii = 0,ndb-1 DO dstring = dstring+',i'

   descript = ['SUNGG Field','HIPASS name','Optical ID', 'HI source catalog', $
               'Right Ascension (J2000) [deg]','Declination (J2000) [deg]',$
               'Galactic latitude [deg]', 'Galactic longitude [deg]', $
               'Separation of source & field center (arcmin)',$
               'HI Heliocentric radial velocity [km/s]', $
               'Source of velocity measurement', $
               'Local group corrected radial velocity', $
               'Cosmic Microwave Background corrected radial velocity', $
               'Tonry flow model corrected radial velocity', $
               'Huchra flow model corrected radial velocity', $
               'HI FWHM velocity width [km/s]', 'HI peak flux density [Jy]', $
               'HI integrated flux [Jy km/s]',$
               'Distance - multipole attractor model [Mpc]', $
               'Logarithmic HI mass [Log(M_sun)]', $
               'E(B-V)g Gal. foreground reddening (Schlegel et al) [mag]',$
               'Byte mask from Schlegel et al',$
               'Original SUNGG selected source?', $
               'Entry number in '+dblis+' database']


;   title    = 'SINGG sample selection'
   title = 'SUNGG source selection'
;   filo     = 'singg_sample'
   filo = 'sungg_source'
;   indblk   = ['ra','dec','vhel','logmhi','sp','sint','glat','glong','ebv']
   indblk = ['ra','dec','vhel','logmhi','sp','sint','glat','glong','ebv']
   ;
   ; open log file
   openw, ll, fill, /get_lun
   ;
   ; read in sample
   singg_readsamp, sfil, field,name, catalog, rastr, decstr, optid, sep, sp,$
     sint, w50, vhel,vsource,comment, ra, dec, vshap,ebv,logmhi, $
     version=invers 
   original=BYTARR(n_elements(name))


   FOR vy=0, n_elements(name)-1 DO BEGIN
       IF comment[vy] EQ 'original' THEN BEGIN
           original[vy]=1b
       ENDIF  ELSE BEGIN
           original[vy]=0b
;           print,comment[vy]
       ENDELSE
   ENDFOR

   ra       =  15.0*ra      ; convert RA from hours to deg

; note that sp already comes in Jy
;   sp       =  0.001*sp     ; convert sp from mJy to Jy

   ns       =  n_elements(name)
   ;
   ; log number of entries read in
   printf, ll, 'MKSUNGGSSRCDB: Number of entries read with sungg_readsamp: ', ns
   ;
   ; make arrays to store old quantities
   oldcat    =  catalog
   catalog   =  make_array(ns,/string,value='---')
   oldra     =  ra
   olddec    =  dec
   oldvhel   =  vhel
   oldw50    =  w50
;   oldvshap  =  vshap
   oldsp     =  sp
   oldsint   =  sint
;   oldebv    =  ebv
   oldname   =  name
;   oldlogmhi =  logmhi
   ;
   ; Loop through databases and record matches


   FOR jj = 0, ndb-1 DO BEGIN 
       
       print, 'Opening database : '+dblis[jj]
       printf, ll, 'MKSUNGGSRCDB: Opening database : '+dblis[jj]
       dbopen, dblis[jj]
       entry = make_array(ns, /int, value=-1)
                                ;
      ; loop through database entries
           
      FOR ii = 0, ns -1 DO BEGIN 
          list = dbcircled(oldra[ii], olddec[ii], rmatch, dis)
          nl   = n_elements(list)
         ;
         ; proceed if there is at least one position match
;          if nl eq 1 and list[0] eq -1 then stop
         IF nl GE 1 AND list[0] NE -1 THEN BEGIN 
            dbext, list, vnam[jj], vdb
            good = where(abs(vdb - oldvhel[ii]) LE vmatch, ngood)
            ;
            ; store closest entry if there is at least one vel match
     
;;;;;;;;;;;;kludging for j2318-42b;;;;;;;
            nametest=strmatch(strtrim(name[ii]),'J2318-42b',/fold_case)
            IF nametest eq 1 THEN sp[ii]=sp[ii]/10.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

            IF ngood GE 1 AND nametest lt 1.0 THEN entry[ii] = list[good[0]]
            
         ENDIF 
      ENDFOR 

      qq     = where(entry NE -1, nent)
      print, '   Number of matched entries in database '+dblis[jj]+' : '+strtrim(string(nent),2)
      printf, ll, 'MKSUNGGSRCDB:  Number of matched entries in database '+dblis[jj]+' : '+strtrim(string(nent),2)
      ;
      ; Things to do with matching entries...
      
 
     IF nent GT 0 AND jj NE 0 THEN BEGIN 
         ;
         ; reset catalog
         catalog[qq] = shnam[jj]
         IF updpar[jj] THEN BEGIN 
            ;
            ; reset ra,dec
            dbext, entry[qq], 'ra,dec',newra,newdec
            ra[qq]   =  newra
            dec[qq]  =  newdec
            ;
            ; reset velocities
            dbext, entry[qq], vnam[jj], newvhel
  ;          dbext, entry[qq], vsnam[jj], newvshap
            vhel[qq]  =  newvhel
  ;          vshap[qq] =  newvshap
            ;
            ; reset widths
            dbext, entry[qq], wnam[jj], neww50
            w50[qq]   =  neww50
            ;
            ; reset sp,sint
            dbext, entry[qq], pknam[jj], newsp
            dbext, entry[qq], flxnam[jj], newsint
            
            sp[qq]    =  newsp
            sint[qq]  =  newsint
            ;
            ; reset E(B-V)
            IF strlen(strtrim(ebvnam[jj],2)) GT 0 THEN BEGIN 
               dbext, entry[qq], ebvnam[jj], newebv
               ebv[qq]   =  newebv
            ENDIF 
            ;
            ; reset names
            dbext, entry[qq], nnam[jj], newname
            name[qq]  =  newname
         ENDIF 
         ;
         ; get optical ids if they are supported in the database
         IF onam[jj] NE '' THEN BEGIN
            oldid  = optid[qq]
            oldnam = name[qq]
            dbext, entry[qq], 'name,'+onam[jj], newnam, newid
            newid  = strcompress(newid, /remove_all)
            oldid  = strcompress(oldid, /remove_all)
            kk     = where(strlen(newid) GT 1)
            kj     = where(strlen(newid) GT 1 AND strlen(oldid) GT 1 $
                           AND strupcase(newid) NE strupcase(oldid), nrep)
            ;
            ; print cases where updated names differ
            print, 'Optical IDs do not agree in '+strtrim(string(nrep),2)+' cases'
            printf, ll, 'MKSUNGGSRCDB: Optical IDs do not agree in '+strtrim(string(nrep),2)+' cases'
            IF nrep GT 0 THEN BEGIN 
               FOR ii =0, nrep-1 DO printf, ll, ljust(oldnam[kj[ii]],15), ljust(oldid[kj[ii]],20), $
                ' | ', ljust(newnam[kj[ii]],15), ljust(newid[kj[ii]],20)
            ENDIF 
            ;
            ; update optical IDs
            optid[qq[kk]] = newid[kk]
         ENDIF 
      ENDIF 
      ;
      ; set pointer to database
      cmd = 'entry_'+dbnam[jj]+' = entry'
      result = execute(cmd)
      dbclose
   ENDFOR 
   ;
   ; now prepare quantities for database ...
   printf, ll, 'MKSUNGGSRCDB: recalculating GLONG, GLAT, EBV, MASK_EBV, VLG.'
   ;
   ; calculate galactic coords
   glactc, ra, dec, 2000.0, glong, glat, 1, /degree
   ;
   ; get new reddening values
   ebv       = dust_getval(glong, glat, /interp, outfile=febv1)
   mask_ebv  = dust_getval(glong, glat, map='mask', outfile=febv2)
   ;
   ; recompute local group velocity
   vlg       = float(vhel) - 79.0*cos(invrad*glong)*cos(invrad*glat) + 296.0*sin(invrad*glong)*cos(invrad*glat) - 36.0*sin(invrad*glat)

   IF keyword_set(calcflow) THEN BEGIN 
      ;
      ; compute flow model velocities

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
       ; the following have been commented out because we ran 
       ; jphflow separately as it's a fortran program
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

     ; filvo  = sdir+'flow_'+strtrim(filo,2)+'.out'
       filvo='flow.out'
      ; filvs  = sdir+'flow_'+strtrim(filo,2)+'.simple'
       filvs='flow.simple.new'
      ;vuse   = vhel
      ;badv   = where(vuse LT vmin, nb)
      ;IF nb GT 0 THEN vuse[badv] = vmin
      ;printf, ll, 'MKSUNGGSRCDB: preparing data for distance calculation using JPHFLOW'
      ;printf, ll, '      Output files: '+filvo+'  ,  '+filvs
      ;mkjphflowin, name, ra, dec, vuse
      ;spawn, '/home/meurer/bin/jphflow'
      ;printf, ll, 'MKSUNGGSRCDB: spawn of JPHFLOW completed'
      ;spawn, 'mv flow.out '+filvo
      ;spawn, 'mv flow.simple '+filvs
      ;
      ; read in derived velocities
       print,"got in here"
;      readcol,filvs,dum1,dum2,dum3,vhin,vlg,vcmb,vvir,vga,vshap,sty,iau,dum4,dum5,vtonry,$
;       format='(a12,f,f,f,f,f,f,f,f,f,f,f,f,f)'
       readfmt,filvs,'a12,f8.5,f9.5,f8.5,f8.5,f8.5,f8.5,f8.5,f8.5,f8.5,f8.5,f8.5,f8.5,f8.5',$
         dum1,dum2,dum3,vhin,vlg,vcmb,vvir,vga,vshap,sty,iau,dum4,dum5,vtonry
        print,'***CHECK',vhin,vlg

      ;printf, ll, 'MKSUNGGSRCDB: velocities from JPHFLOW read in'
   ENDIF 
   ;
   ; calculate distances
   printf, ll, 'MKSUNGGSRCDB: recalculating distances'
   distance  =  vshap / h0
   kk        =  where(distance LT distmin,nk)
   IF nk GT 0 THEN distance[kk] = distmin
   ;
   printf, ll, 'MKSUNGGSRCDB: number of sources with distances set to distmin: '+strtrim(string(nk),2)
   ;
   ; Override distances (perhaps I should do this only when making sample...?)
   get_distoverride, fild, namd, newd, logu=ll
   distoverride, namd, newd, oldname, distance, logu=ll
   ;
   ; Recalculate log(M_HI)
   printf, ll, 'MKSUNGGSRCDB: recalculating LOGMHI'
   logmhi   = alog10(2.36e5*sint*distance*distance)
   ;
   ; report places where arrays have changed significantly
   report_diff, ll, tolp, oldra, ra, quant='RA', name=name, /detail
   report_diff, ll, tolp, olddec, dec, quant='DEC', name=name, /detail
   report_diff, ll, tolv, oldvhel, vhel, quant='VHEL', name=name, /detail
   report_diff, ll, tolv, oldw50, w50, quant='W50', name=name, /detail
;   report_diff, ll, tolv, oldvshap, vshap, quant='VSHAP', name=name, /detail
   report_diff, ll, tolpk, oldsp, sp, quant='SP', name=name, /detail
   report_diff, ll, tolflx, oldsint, sint, quant='SINT', name=name, /detail
;   report_diff, ll, tolebv, oldebv, ebv, quant='EBV', name=name, /detail
;   report_diff, ll, tollm, oldlogmhi, logmhi, quant='LOGMHI', name=name, /detail
   ;
   ; find entries that have not been matched
   qq  = where(catalog EQ '---', nqq)
   printf, ll, 'MKSUNGGSRCDB: number of sources not matched to any database : '+strtrim(string(nqq),2)
   IF nqq GT 0 THEN FOR jj = 0, nqq-1 DO printf, ll, '    '+ljust(name[qq[jj]], 20)
   ;
   ; find entries that have not been matched to any of the 
   ; key databases
   mtch = make_array(ns, /byte, value=0b)
   FOR jj = 0, ndb-1 DO BEGIN 
      IF updpar[jj] THEN BEGIN 
         result = execute('mtch = (mtch EQ 1b OR entry_'+dbnam[jj]+' NE -1)')
      ENDIF 
   ENDFOR 
   qq   = where(mtch EQ 0b, nqq)
   printf, ll, 'MKSUNGGSRCDB: number of sources not matched to the "good" databases : '+strtrim(string(nqq),2)
   IF nqq GT 0 THEN FOR jj = 0, nqq-1 DO printf, ll, '    '+ljust(name[qq[jj]], 20)
   ;
   ; Find entries with HIPASS name changes
   qq = where(strtrim(name,2) NE strtrim(oldname,2), nqq)
   printf, ll, 'MKSUNGGSRCDB: number of sources with HIPASS name changes : '+strtrim(string(nqq),2)
   IF nqq GT 0 THEN FOR jj = 0, nqq-1 DO printf, ll, '    old : '+ljust(oldname[qq[jj]], 20)+' | new : '+ljust(name[qq[jj]], 20)
   ;
   ; Kludge name of J1339-31b to remain the same
   jj = strpos(oldname, 'J1339-31b')
   qq = where(jj GE 0, nqq)
   IF nqq EQ 1 THEN BEGIN 
      name[qq] = oldname[qq]
      printf, ll, 'MKSUNGGSRCDB: kludging name of J1339-31b back to old value...'
   ENDIF 
   ;
   ; now do database creation stuff ...
   ;
   ; set lengths of character items
   kk       = where(tlen EQ 0,nk)
   IF nk GT 0 THEN FOR ii = 0,nk-1 DO result = execute('tlen[kk[ii]] = max(strlen('+item[kk[ii]]+'))') $
              ELSE stop
   ;
   ; create structure
;   create_struct2, singg, 'SINGG', item, dstring, dimen=ns
   create_struct2,sungg,'SUNGG',item,dstring,dimen=ns 
  ;
   ; open dbd file, write top of the file
   fildbd  = filo + '.dbd'
   openw, lu, fildbd, /get_lun
   printf, lu, '#title'
   printf, lu, title
   printf, lu, ''
   printf, lu, '#maxentries'
   printf, lu, strtrim(string(ns),2)
   printf, lu, ''
   printf, lu, '#items'
   ;
   ; Fill singg structure, write item lines of dbd file
   mc     = max(strlen(item))
   pritem = ljust(strupcase(item), mc)
   ni     = n_elements(item)
   FOR ii = 0, ni-1 DO BEGIN 
 ;     cmd = "singg."+strtrim(item[ii],2)+" = "+strtrim(item[ii],2)
       cmd = "sungg."+strtrim(item[ii],2)+" = "+strtrim(item[ii],2)
     result = execute(cmd)
      typstr = ljust(tout[ii]+'*'+strtrim(string(tlen[ii]),2),5)
      printf,lu,pritem[ii]+'   '+typstr+'   "'+descript[ii]+'"'
;      print,cmd
;      print,pritem[ii]+'   '+typstr+'   "'+descript[ii]+'"'
   ENDFOR 
   ;
   ; write index blocks of dbd file and close
   printf,lu,'  '
   printf,lu,'#index'
   ni = n_elements(indblk)
   FOR ii = 0, ni-1 DO BEGIN 
      printf,lu,ljust(strupcase(strtrim(indblk[ii],2)),mc)+'   sort'
   ENDFOR 
   free_lun,lu   
   ;
   ; write database
   !PRIV = 2
   dbcreate, filo, 1, 1, /external
   dbopen, filo, 1
   dbbuildstruct, sungg
   dbclose,dummy
   ;
   free_lun, ll
END 
