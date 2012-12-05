;+
; NAME:
;  reductor
; PURPOSE:
;  Automated photometry reduction tool.
; DESCRIPTION:
; CATEGORY:
;  Photometry
; CALLING SEQUENCE:
;  reductor
; INPUTS:
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
;  CALIBPATH-Directory path pointing to calibration information,
;               default=datadir+'calib/'  (datadir dymnamically determined
;               by program).
;  DEBUG      - Turn on debug information output, default=OFF
;  DUMPOBJ    - Name of object to dump complete reduction information on.
;                 If set to 'all', all objects are dumped.
;  FLUSH_INFO - Flush all cached information, force a clean start
;  FORCE      - Flag, if true, ignores "ok" in instructions
;  FULL       - Flag passed to colorsol that generates a more involved listing
;                 of the reductions.
;  MAGRESID   - Edit bad values by mag residual, not sigma residuals
;  NOEDIT     - Suppress all bad point editing.
;  PRINT      - Send all plot output to the default printer (PORTRAIT.PRO),
;                  The display device is set to X (with a call to DISPLAY.PRO)
;                  upon exit.  If FORCE is also set, then DATAMON.pro will be
;                  called at the end to generate a plot from the .log1 file.
;  RESETBAD   - Flag, if true, turns off all bad flags.
; OUTPUTS:
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
;   COM_REDUCTOR - This is used to save information between multiple runs
;                    of this program.  Considerable information is cached to
;                    speed execution on the same night of data.
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
;  The operation of the program is controlled by the contents of a file
;     named reduc.inf that is in the current directory.  If not found, this
;     program will attempt to build a relevant file to get started.  In normal
;     use you will run this program many times on the way to getting your
;     photometry reduced.  What steps are done and how they are done are
;     controlled by lines in the reduc.inf file.
;
;  The contents of reduc.inf are as follows:
;     Line 1 - REDUCTOR v1.0          File id line
;     Line 2 - LCCD/OSU2              Instrument id (no spaces)
;     Line 3 - /cdrom/lo_mwb_0022/    Directory where RAW image data reside
;     Line 4 - 920611                 YYMMDD, UT date code
;     Line 5 - 15.0 25.0 100.0 39.4   Photometry extraction parameters,
;                                     Object radius, inner sky, outer sky, gain.
;     Line 6 to end, these are "rule" lines that invoke different steps of
;        reduction operations.  In general, these lines start with a two
;        character operation code followed by the information needed for that
;        step.
;
;     The operators are:
;          2c - Two color lightcurve against all sky standards
;          dp - Differential photometric reduction (like Pluto)
;          lc - Single color lightcurve against all sky standards.
;          sl - Block for reducing Stars (in Landolt system) (color unknown).
;          tr - Transformation against Landolt standard stars.
;
;     In addition to the stated arguments, all operator lines can be terminated
;        with the word "ok" (no quotes).  Any line marked in this way is
;        ignored unless /FORCE is specified.
;
; ------------------------------------------------------------------------------
; 2c - Two color lightcurve against all sky standards
; ------------------------------------------------------------------------------
; 2c OBJECT SERNO FILCOD FILNAM STDFIL COLOR1 COLOR2
;                 FILCOD FILNAM STDFIL COLOR1 COLOR2 STDCOL COLERR
; 1  OBJECT - Standard object code name to reduce
; 2  SERNO  - Serial number of ojbect to reduce (usually 0)
; 3  FILCOD - filter code (string) as in photometry log file
; 4  FILNAM - filter name (best if 1 character)
; 5  STDFIL - color code for Landolt system 01234 = UBVRI
; 6  COLOR1 - First color code for color to reduce against
; 7  COLOR2 - Second color code (ex: B-V is color1=1, color2=2)
; 8  FILCOD - filter code (string) as in photometry log file
; 9  FILNAM - filter name (best if 1 character)
;10  STDFIL - color code for Landolt system 01234 = UBVRI
;11  COLOR1 - First color code for color to reduce against
;12  COLOR2 - Second color code (ex: B-V is color1=1, color2=2)
;
; ------------------------------------------------------------------------------
; dp - Differential photometric reduction (like Pluto)
; ------------------------------------------------------------------------------
; dp FILCOD FILNAM COLNAM OBJECT SERNO COLOR COMP [k2 V1 E1] [ct V2 E2] [k V1 E1] [td DATE]
; 1  FILCOD - filter code (string) as in photometry log file
; 2  FILNAM - filter name (1 character)
; 3  COLNAM - name of color (2 character, ie., BV or VR)
; 4  OBJECT - Standard object code name to reduce
; 5  SERNO  - Serial number of ojbect to reduce (usually 0)
; 6  COLOR  - Standard color of object
; 7  COMP   - Standard object name for comparison star (replace blanks with _)
;  argument flags:
;    k2     - override second order extinction coefficient.
;    ct     - Override color term.
;    k      - Override extinction.
;    td     - Override transformation with values from another date.
;  optional
;    V1,E1  - Second order extinction and error
;    V2,E2  - Color term
;    DATE   - YYMMDD for date to use transformation from
; 
; ------------------------------------------------------------------------------
; lc - Single color lightcurve against all sky standards.
; ------------------------------------------------------------------------------
; lc OBJECT SERNO FILCOD FILNAM STDFIL COLOR1 COLOR2 STDCOL COLERR
; 1  OBJECT - Standard object code name to reduce
; 2  SERNO  - Serial number of ojbect to reduce (usually 0)
; 3  FILCOD - filter code (string) as in photometry log file
; 4  FILNAM - filter name (best if 1 character)
; 5  STDFIL - color code for Landolt system 01234 = UBVRI
; 6  COLOR1 - First color code for color to reduce against
; 7  COLOR2 - Second color code (ex: B-V is color1=1, color2=2)
; 8  STDCOL - Standard color of object
; 9  COLERR - Uncertainty of color
;
; ------------------------------------------------------------------------------
; sl - Block for reducing Stars (in Landolt system) (color unknown)
; ------------------------------------------------------------------------------
; sl FILCOD1 FILCOD2 FILNAM1 FILNAM2 STDFIL1 STDFIL2
; 1  FILCOD1 - filter code (string) as in photometry log file for color 1
; 2  FILCOD2 - filter code (string) as in photometry log file for color 2
; 3  FILNAM1 - filter name (best if 1 character) for color 1
; 4  FILNAM2 - filter name (bset if 1 character) for color 2
; 5  STDFIL1 - color code for Landolt system 01234 = UBVRI for color 1
; 6  STDFIL2 - color code for Landolt system 01234 = UBVRI for color 2
;
; ------------------------------------------------------------------------------
; tr - Transformation against Landolt standard stars.
; ------------------------------------------------------------------------------
; tr FILCOD FILNAM STDFIL COLOR1 COLOR2 [k2 V1 E1] [ct V2 E2] [kt]
; 1  FILCOD - filter code (string) as in photometry log file
; 2  FILNAM - filter name (best if 1 character)
; 3  STDFIL - color codes for Landolt system 01234 = UBVRI
; 4  COLOR1 - First color code for color to reduce against
; 5  COLOR2 - Second color code (ex: B-V is color1=1, color2=2)
;
;  argument flags:
;    k2     - override second order extinction coefficient.
;    ct     - Override color term.
;    kt     - turn on time dependent extinction.
;  optional
;    V1,E1  - Second order extinction and error
;    V2,E2  - Color term
;
; MODIFICATION HISTORY:
;  Written by Marc W. Buie, 96/10/16, Lowell Observatory
;  96/10/31, MWB, added "bad" flag support from rdphalt
;  97/01/24, MWB, added MAGRESID flag
;  92/02/27, MWB, added "dp" rule for differential photometry reductions.
;  2000/01/25, John Mattox, Boston University, added missing dumpall clause.
;  2000/07/06, MWB, added '9 = M = Methane' filter to "dp" rule.
;  2000/07/14, MWB, fixed bug that was killing new sky signal in log file.
;                 Also added printing of datamon.pro plot if force and print set.
;  2000/10/16, MWB, fixed bug that was ignoring forced extintion in dp rule.
;  2001/11/06, MWB, allowed for cached ephemeris calculations
;  2003/10/01, MWB, converted my Delfile call to system file_delete routine
;                   converted my Mkdir calls to IDL file_mkdir calls
;  2004/02/09, MWB, changed fixed paths to data files and reduction areas
;
;-
pro reductor, DEBUG=debug, DUMPOBJ=fulldump, FLUSH_INFO=flush_info, $
              FORCE=force, FULL=full, MAGRESID=magresid, PRINT=printit, $
              RESETBAD=resetbad, NOEDIT=noedit, CALIBPATH=calibpath

   common com_reductor, info, pdata

   if badpar(debug,[0,1,2,3],0,caller='REDUCTOR: (DEBUG) ',default=0) then return
   if badpar(fulldump,[0,7],0,caller='REDUCTOR: (DUMPOBJ) ', $
             default='[[[none]]]') then return
   if badpar(full,[0,1,2,3],0,caller='REDUCTOR: (FULL) ',default=0) then return
   if badpar(printit,[0,1,2,3],0,caller='REDUCTOR: (PRINTIT) ',default=0) then return
   if badpar(noedit,[0,1,2,3],0,caller='REDUCTOR: (NOEDIT) ',default=0) then return

   IF printit THEN BEGIN
      portrait
   ENDIF

   setusym,1
   !x.style=3
   !y.style=3
   !z.style=3

   infofile='reduc.inf'
   indexfile='/frakir/raid/buie/archive/index.dat'
   in_date=''
   in_cd=''
   in_info=''
   inst=''
   ddir=''
   data=''
   plotsdone=0
   badisdirty=0
   landfil=['U','B','V','R','I']
   blanks='                     '

   ; This is the GPS position for the 42", derived 1993 Sep 08
   lat = (35.0+5.0/60.0+48.740/3600.0)/180.0*!pi
   lon = (111.0+32.0/60.0+10.601/3600.0)/180.0*!pi

   ; First, look for reduc.inf in the current directory
   ; if not found, ask questions to create one
   if not exists(infofile) then begin
      ; Get current directory, this is used for the data directory
      cd,'.',current=cdir
      data=strmid(cdir,rstrpos(cdir,'/')+1,99)
      ; Check for the date in the archive index to find CD of data.
      nfound=0
      openr,lun,indexfile,/get_lun
      while not(eof(lun)) do begin
         readf,lun,in_date,in_cd,in_info,format='(a6,1x,a11,1x,a80)'
         if in_date eq data then begin
            print,in_date,' ',in_cd,' ',strtrim(in_info,2)
            if nfound eq 0 then $
              cdlist=in_cd $
            else $
              cdlist=[cdlist,in_cd]
            nfound=nfound+1
         endif
      endwhile
      free_lun,lun
      if nfound eq 1 then begin
         ddir='/cdrom/'+strlowcase(cdlist)
      endif else if nfound gt 1 then begin
         ddir='/cdrom/'+picker(strlowcase(cdlist))
      endif else begin
         read,prompt='Directory pointer for raw data ',ddir
      endelse

      ddir=addslash(ddir)

      read,prompt='Instrument? ',inst
      if inst eq 'PCCD' then begin
         newrad    =  15.0
         newsky1   =  25.0
         newsky2   = 130.0
         newgain   =   2.60
      endif else if inst eq 'Loral/2.5' then begin
         newrad    =  15.0
         newsky1   =  25.0
         newsky2   = 100.0
         newgain   =   2.56
      endif else begin
         newrad    =   5.0
         newsky1   =  25.0
         newsky2   =  40.0
         newgain   =   1.00
      endelse
      openw,lun,infofile,/get_lun
      printf,lun,'REDUCTOR v1.0'
      printf,lun,inst
      printf,lun,ddir
      printf,lun,data
      printf,lun,newrad,newsky1,newsky2,newgain
      printf,lun,'tr 1 B 1 1 2 ok'
      printf,lun,'tr 2 V 2 1 2 k2 0. 0. ok'
      printf,lun,'tr 2 V 2 2 3 k2 0. 0. ok'
      printf,lun,'tr 3 R 3 2 3 k2 0. 0. ok'
      printf,lun,'sl 1 2 B V 1 2 ok'
      printf,lun,'sl 2 3 V R 2 3 ok'
      printf,lun,'2c a???? 0 2 V 2 2 3 3 R 3 2 3 ok'
      printf,lun,'lc a???? 0 3 R 3 2 3 0.4? 0.0? ok'
      printf,lun,'dp 1 B BV p9 0 0.842 SAO_160288 ok'
      printf,lun,'dp 2 V BV p9 0 0.842 SAO_160288 ok'
      printf,lun,'dp 9 M BV p9 0 0.0   SAO_160288 k2 0. 0. ct 0. 0. ok'
      free_lun,lun
      print,'The file ',infofile, $
         ' has been created, review/edit and run REDUCTOR again'
      return
   endif

; If common is empty, fill in the structure with empty stuff.
   if keyword_set(flush_info) then info=0
   sz_info=size(info)
   if sz_info[n_elements(sz_info)-2] ne 8 then begin
      info = { $
         cwd:        '', $
         data:       '', $
         ddir:       '', $
         inst:       '', $
         newrad:    0.0, $
         newsky1:   0.0, $
         newsky2:   0.0, $
         newgain:   0.0, $
         oplist:    strarr(50), $
         needcoord:   0, $
         logdate: '' $
         }
   endif

; Is the directory the same as the last time?  If so, reset some of the info.
   cd,'.',current=cwd
   if cwd ne info.cwd then info.logdate=''
   info.cwd = cwd

   ; Okay, load the info from the information file.
   openr,lun,infofile,/get_lun
   version=''
   readf,lun,version
   if version ne 'REDUCTOR v1.0' then begin
      print,'ERROR!  bad version id [',version,'] in information file ',infofile
      return
   endif
   readf,lun,inst
   readf,lun,ddir
   readf,lun,data
   readf,lun,newrad,newsky1,newsky2,newgain

   newop=''
   j=0
   while not eof(lun) do begin
      readf,lun,newop,format='(a100)'
      info.oplist[j] = strtrim(newop,2)
      j=j+1
   endwhile
   numop=j

   info.inst=inst
   info.ddir=addslash(ddir)
   info.data=data
   info.newrad=newrad
   info.newsky1=newsky1
   info.newsky2=newsky2
   info.newgain=newgain

   free_lun,lun

   logfile=info.data+'.log'
   log1file=info.data+'.log1'
   namesfile=info.data+'.names'
   fncache=info.data+'.eph'

   ; Check for .log file, can't proceed if not present
   if not exists(logfile) then begin
      print,'ERROR! ',logfile,' does not exist, need to run CCDPHOT'
      if exists(fncache) then file_delete,fncache,/quiet,/noexpand_path
      return
   endif

   ; Check for .log1 file, see if need to run REPHOT
   if not exists(log1file) then begin
      redo_phot=1
   endif else begin
      rdphalt,log1file,filename,obj,fil,jd,exptime,gain,rad,sky1,sky2,NUMLINE=1
      if fix(gain[0]*100.0+0.5) ne fix(info.newgain[0]*100.0+0.5) or $
         fix(rad[0]*100.0+0.5) ne fix(info.newrad[0]*100.0+0.5) or $
         fix(sky1[0]*100.0+0.5) ne fix(info.newsky1[0]*100.0+0.5) or $
         fix(sky2[0]*100.0+0.5) ne fix(info.newsky2[0]*100.0+0.5) then redo_phot=1 $
      else redo_phot=0
      if redo_phot then begin
         ans=''
         read,prompt='log1 file needs to be recreated, do you want to proceed? ',ans
         IF ans eq 'yes' THEN BEGIN
            file_delete,log1file,/quiet,/noexpand_path
         ENDIF ELSE return
      endif
   endelse

   ; Rerun REPHOT if required.
   if redo_phot then begin
      if exists(fncache) then file_delete,fncache,/quiet,/noexpand_path
      rephot,logfile,log1file,'files.cal',path=info.ddir+info.data, $
         newgain=info.newgain, newrad=info.newrad, pscale=1.0, $
         newsky1=info.newsky1, newsky2=info.newsky2, /silent, $
         calibpath=calibpath
      if not exists(log1file) then return
   endif

   ; Check to see if log1 file needs to be loaded
   spawn,'ls -l '+log1file,result
   if result[0] ne info.logdate then begin
      print,'(re)loading photometry data'
      rdphalt,log1file,filename,obj,fil,jd,exptime,gain, $
         rad,sky1,sky2,serial,xpos,ypos,fwhm,maxcnt,sky,skyerr,mag,err,bad
      idx=lindgen(n_elements(jd))
      pdata = { $
         filename: filename, $
         obj:      obj, $
         fil:      fil, $
         jd:       jd, $
         exptime:  exptime, $
         gain:     gain, $
         rad:      rad, $
         sky1:     sky1, $
         sky2:     sky2, $
         serial:   serial, $
         xpos:     xpos, $
         ypos:     ypos, $
         fwhm:     fwhm, $
         maxcnt:   maxcnt, $
         sky:      sky, $
         skyerr:   skyerr, $
         mag:      mag, $
         err:      err, $
         bad:      bad, $
         idx:      idx, $
         ra:       fltarr(n_elements(err)), $
         dec:      fltarr(n_elements(err)), $
         stand:    strarr(n_elements(err)), $
         time:     fltarr(n_elements(err)), $
         am:       fltarr(n_elements(err)) $
         }
      info.logdate=result[0]
      info.needcoord=1
   endif

   if keyword_set(resetbad) then begin
      badisdirty=1
      pdata.bad[*]=0
   endif

   ; Check to see if the names correspondence file exists.
   if not exists(namesfile) then begin
      objlist=pdata.obj[uniq(pdata.obj,sort(pdata.obj))]
      origlist=objlist
      ; Try to clean up the names based on some simple rules
      for i=0,n_elements(objlist)-1 do begin
         if strmid(objlist[i],0,2) eq 'HD' and $
            strmid(objlist[i],2,1) ne ' ' then $
            objlist[i] = 'HD '+strmid(objlist[i],2,99) $
         else if strmid(objlist[i],0,3) eq 'RU ' then $
            objlist[i] = 'Rubin '+strmid(objlist[i],3,99) $
         else if strmid(objlist[i],0,3) eq 'SAO' and $
                 strmid(objlist[i],3,1) ne ' ' then $
            objlist[i] = 'SAO '+strmid(objlist[i],3,99) $
         else if strmid(objlist[i],0,2) eq 'PG' and $
                 strmid(objlist[i],2,1) ne ' ' then $
            objlist[i] = 'PG '+strmid(objlist[i],2,99) $
         else if strmid(objlist[i],0,2) eq 'SA' and $
                 strmid(objlist[i],2,1) ne ' '  and $
                 strmid(objlist[i],2,1) ne 'O' then $
            objlist[i] = 'SA '+strmid(objlist[i],2,99) $
         else if strmid(objlist[i],0,3) eq 'M67' and $
                 strmid(objlist[i],0,5) ne 'M67 I' and $
                 strmid(objlist[i],0,5) ne 'M67 F' then $
            objlist[i] = 'M67 F'+strmid(objlist[i],4,99)
      endfor
      getstars,objlist,tra,tdec,found,std=std_names
      z=where(found ne 1,count)
      if count ne 0 then begin
         for i=0,count-1 do begin
            ans=''
            if objlist[z[i]] eq 'Pluto' then begin
               ans='p9'
            endif else begin
               read,ans,prompt='Standard name for '+objlist[z[i]]+' ',format='(a)'
            endelse
            std_names[z[i]]=ans
         endfor
      endif
      idx=sort(std_names)
      wrmatch,std_names[idx],origlist[idx],namesfile
   endif

   ; Generate object coordinates, if needed
   if info.needcoord then begin
      coord,pdata.jd,688,pdata.obj,namesfile,ra,dec, $
         stand=stand,/ofdate,DEBUG=debug,/cache,fncache=fncache
      sz=size(ra)
      if sz[0] eq 0 then return
      info.needcoord=0
      pdata.ra    = ra
      pdata.dec   = dec
      pdata.stand = stand
      pdata.time  = (pdata.jd - long(pdata.jd[0]-0.5d0)-0.50d0)*24.0
      pdata.am    = airmass(pdata.jd,pdata.ra,pdata.dec,lat,lon,0.6,594.0,0.0,0.50)
   endif

   ; Check to see if all is ok with airmass information
   z=where(pdata.am lt 1.0 or pdata.am gt 9.0,count)
   if count ne 0 then begin
      print,'Bad airmass values, something needs to be fixed.'
      for i=0,count-1 do begin
         rastr,pdata.ra[z[i]],1,str1
         rastr,pdata.dec[z[i]],0,str2
         print,i,pdata.stand[z[i]],pdata.time[z[i]], $
            str1,str2,pdata.am[z[i]], $
            format='(i3,1x,a16,1x,f8.5,1x,a,1x,a,1x,g10.3)'
      endfor
      info.needcoord=1
      return
   endif

   ;Cross check the filter reduction information against what is in the
   ;  photometry log file.
   newfils = pdata.fil[uniq(pdata.fil,sort(pdata.fil))]
   objlist = pdata.stand[uniq(pdata.stand,sort(pdata.stand))]

   print,info.data,', data in filters: ',newfils

   IF fulldump ne '[[[none]]]' THEN BEGIN
      IF fulldump eq 'all' THEN BEGIN
         for io=0,n_elements(objlist)-1 do begin
            z=where(pdata.stand eq objlist[io],count)
            for i=0,count-1 do begin
               print,pdata.filename[z[i]],pdata.stand[z[i]],pdata.time[z[i]], $
                     pdata.am[z[i]],pdata.fil[z[i]], $
                     pdata.mag[z[i]],pdata.err[z[i]],pdata.bad[z[i]], $
                     format='(a,1x,a16,1x,f8.5,1x,f5.3,1x,a2,1x,f7.4,1x,f6.4,1x,i1)'
            endfor
         endfor
      ENDIF ELSE BEGIN
         z=where(pdata.stand eq fulldump,count)
         FOR i=0,count-1 DO BEGIN
            print,pdata.filename[z[i]],pdata.stand[z[i]],pdata.time[z[i]], $
                  pdata.am[z[i]],pdata.fil[z[i]], $
                  pdata.mag[z[i]],pdata.err[z[i]],pdata.bad[z[i]], $
                  format='(a,1x,a16,1x,f8.5,1x,f5.3,1x,a2,1x,f7.4,1x,f6.4,1x,i1)'
         ENDFOR
      ENDELSE
   ENDIF

   for i=0,numop-1 do begin
      print,i,' ',info.oplist[i]
   endfor

   ; Now do the work required
   for i=0,numop-1 do begin

      op = strsplit(strcompress(strtrim(info.oplist[i],2)),' ',/extract)
      if op[n_elements(op)-1] eq 'ok' then begin
         if keyword_set(force) then $
            op = op[0:n_elements(op)-2] $
         else $
            goto,skipit
      endif

      CASE op[0] OF


; ------------------------------------------------------------------------------
; tr - Transformation against Landolt standard stars.
; ------------------------------------------------------------------------------

         'tr': BEGIN
            ; Parameter validation
            ; enough arguments?
            if n_elements(op) lt 6 then begin
               print,'bad rule: ',info.oplist[i]
               goto,skipit
            endif

            ; data in the selected filter
            zfil = where(pdata.fil eq op[1],count)
            if count eq 0 then begin
               print,'No data found in filter ',op[1]
               goto,skipit
            endif

            ; regularize the optional arguments
            ; #6 could be k2 or ct, followed by two numbers
            forcek2=0
            forcect=0
            forcekt=0
            opnum=6
            k2=[999.0,0.]
            ct=[999.0,0.]
            WHILE opnum lt n_elements(op) DO BEGIN
               IF op[opnum] eq 'k2' and not forcek2 and $
                                        opnum+2 lt n_elements(op) THEN BEGIN
                  forcek2 = 1
                  k2=[float(op[opnum+1:opnum+2])]
                  opnum = opnum+3
               ENDIF ELSE IF op[opnum] eq 'ct' and $
                             not forcect and opnum+2 lt n_elements(op) THEN BEGIN
                  forcect=1
                  ct=[float(op[opnum+1:opnum+2])]
                  opnum = opnum+3
               ENDIF ELSE IF op[opnum] eq 'kt' and not forcekt THEN BEGIN
                  forcekt=1
                  opnum = opnum+1
               ENDIF ELSE BEGIN
                  print,'bad rule args: ',info.oplist[i]
                  goto,skipit
               ENDELSE
            ENDWHILE

            if !d.name eq 'PS' then setpage,/portrait

            tmpbad=pdata.bad[zfil]
            transf,pdata.stand[zfil],pdata.jd[zfil], $
               fix(op[3]),fix(op[4]),fix(op[5]), $
               pdata.am[zfil],pdata.mag[zfil],pdata.err[zfil],tmpbad, $
               title=info.inst+' '+info.data,other=pdata.time[zfil],olab='UT Time', $
               k2=k2, cterm=ct, ktime=forcekt,NOEDIT=noedit, $
               /chi,tagdate=info.data,taginst=info.inst,/histfile,magresid=magresid
            z1=where(tmpbad ne pdata.bad[zfil],countchange)
            if countchange gt 0 and not badisdirty then badisdirty=1
            pdata.bad[zfil]=tmpbad

            plotsdone=1

            ; Just for information, look at what is NOT a standard and summarize
            ;  to the screen.
            stdcheck,'/frakir/raid/buie/photometry/landolt/landphot.dat', $
               pdata.stand[zfil], $
               [ [ replicate(fix(op[3]),count) ], $
                  [ replicate(fix(op[4]),count) ], $
                  [ replicate(fix(op[5]),count) ]    ], $
               stdflg
            z=where(stdflg eq 0, count0)
            if count0 ne 0 then begin
               tmpnam=pdata.stand[zfil[z]]
               tmpnam=tmpnam[uniq(tmpnam,sort(tmpnam))]
               print,' Objects to reduce for ',landfil[fix(op[3])], $
                  ' wrt (',landfil[fix(op[4])],'-',landfil[fix(op[5])],')'
               for j=0,n_elements(tmpnam)-1 do print,' (0)  ',tmpnam[j]
            endif
            z=where(stdflg eq 3, count3)
            if count3 ne 0 then begin
               tmpnam=pdata.stand[zfil[z]]
               tmpnam=tmpnam[uniq(tmpnam,sort(tmpnam))]
               if count0 eq 0 then $
                  print,' Objects to reduce for ',landfil[fix(op[3])], $
                     ' wrt (',landfil[fix(op[4])],'-',landfil[fix(op[5])],')'
               for j=0,n_elements(tmpnam)-1 do print,' (3)  ',tmpnam[j]
            endif
         ENDCASE ; Transformation block

; ------------------------------------------------------------------------------
; lc - Single color lightcurve against all sky standards.
; ------------------------------------------------------------------------------

         'lc': BEGIN
            if n_elements(op) ne 10 then begin
               print,'bad rule: ',info.oplist[i]
               goto,skipit
            endif

            ; First, must fetch the transformation coefficients for this obsn
            gettran,info.inst,info.data,fix(op[5]),fix(op[6]),fix(op[7]), $
               tran,transig,jdref
            sz=size(tran)
            if sz[0] eq 0 then return
            transig[3]=0. ; turn off zeropoint error

            ; Now, generate lightcurve
            lnam=strlowcase(op[1])
            dnam=info.data+'.'+lnam
            oldbad=pdata.bad
            if !d.name eq 'PS' then setpage,/landscape
            tmpbad=pdata.bad
            ltcrv2,pdata.stand,pdata.fil,pdata.jd,pdata.time,pdata.am, $
                  pdata.serial,pdata.mag,pdata.err, $
                  float(op[8]),float(op[9]),op[1],fix(op[2]),op[3], $
                  tran,transig,jdref, $
                  filtname=op[4],file=dnam,badflags=tmpbad
            pdata.bad=tmpbad
            z1=where(oldbad ne pdata.bad,countchange)
            if countchange gt 0 and not badisdirty then badisdirty=1
            plotsdone=1

            ; Park the output files in the final data area
            onam=strmid(lnam,1,99)
            ; Numbered asteroid
            if strcompress(string(fix(onam)),/remove_all) eq onam then $
               fdir = '/frakir/raid/buie/neo/a'+onam $
            ; unnumbered asteroid
            else $
               fdir = '/frakir/raid/buie/neo/'+onam
            if not exists(fdir) then file_mkdir,fdir
            cmd='cp -p '+dnam+' '+fdir+'/'+info.data+'.001'
            print,cmd
            spawn,cmd
         ENDCASE ; single color lightcurve

; ------------------------------------------------------------------------------
; 2c - Two color lightcurve against all sky standards
; ------------------------------------------------------------------------------

         '2c': BEGIN
            if n_elements(op) ne 13 then begin
               print,'bad rule: ',info.oplist[i]
               goto,skipit
            endif

            ; First, must fetch the transformation coefficients for this obsn
            gettran,info.inst,info.data,fix(op[5]),fix(op[6]),fix(op[7]), $
               tran,transig,jdref
            sz=size(tran)
            if sz[0] eq 0 then return
            transig[3]=0. ; turn off zeropoint error
            gettran,info.inst,info.data,fix(op[10]),fix(op[11]),fix(op[12]), $
               tran1,transig1,jdref1
            sz=size(tran1)
            if sz[0] eq 0 then return
            transig1[3]=0. ; turn off zeropoint error
            tran=[[tran],[tran1]]
            transig=[[transig],[transig]]
            jdref=[jdref,jdref1]

            ; Now, generate lightcurve
            lnam=strlowcase(op[1])
            dnam=info.data+'.'+lnam
            oldbad=pdata.bad
            if !d.name eq 'PS' then setpage,/portrait
            tmpbad=pdata.bad
            ltcrv2c,pdata.stand,pdata.fil,pdata.jd,pdata.time,pdata.am, $
                  pdata.serial,pdata.mag,pdata.err, $
                  op[1],fix(op[2]),[op[3],op[8]],tran,transig,jdref, $
                  color,colorsig,badflags=tmpbad, $
                  filtname=[op[4],op[9]],error=error,file=[dnam,dnam]
            pdata.bad=tmpbad
            if error then goto,bailout
            z1=where(oldbad ne pdata.bad,countchange)
            if countchange gt 0 and not badisdirty then badisdirty=1

            ; Save the final color to a file.
            colorfile=lnam+'.'+op[4]+'-'+op[9]
            colorinfo=string(info.data,' ',color,colorsig)
            openw,lun,colorfile,/get_lun
            printf,lun,colorinfo
            free_lun,lun
            print,'Final color written to file ',colorfile
            plotsdone=1

            ; Park the output files in the final data area
            onam=strmid(lnam,1,99)
            ; Numbered asteroid
            if strcompress(string(fix(onam)),/remove_all) eq onam then $
               fdir = '/frakir/raid/buie/neo/a'+onam $
            ; unnumbered asteroid
            else $
               fdir = '/frakir/raid/buie/neo/'+onam
            if not exists(fdir) then file_mkdir,fdir
            cmd='cp -p '+dnam+' '+fdir+'/'+info.data+'.001'
            print,cmd
            spawn,cmd

            ; Update average color for this object for this night
            print,'update ',fdir,'/',colorfile
            repwrite,fdir+'/'+colorfile,info.data,colorinfo
         ENDCASE

; ------------------------------------------------------------------------------
; sl - Block for reducing Stars (in Landolt system) (color unknown)
; ------------------------------------------------------------------------------

         'sl': BEGIN
            if n_elements(op) ne 7 then begin
               print,'bad rule: ',info.oplist[i]
               goto,skipit
            endif

            fil1 = fix(op[5])
            fil2 = fix(op[6])

            ; B, B-V,   V, B-V
            if fil1 eq 1 and fil2 eq 2 then begin
               fa=1
               fb=2
               c1=1
               c2=2
               f1='B'
               f2='V'
            endif else $

            ; V, V-R,   R, V-R
            if fil1 eq 2 and fil2 eq 3 then begin
               fa=2
               fb=3
               c1=2
               c2=3
               f1='V'
               f2='R'
            endif else goto,skipit

            ; First, must fetch the transformation coefficients for this obsn
            gettran,info.inst,info.data,fa,c1,c2,tran1,tran1sig,jdref1
            gettran,info.inst,info.data,fb,c1,c2,tran2,tran2sig,jdref2
            sz=size(tran1sig)
            if sz[0] eq 0 then return
            sz=size(tran2sig)
            if sz[0] eq 0 then return
            tran1sig[3]=0. ; turn off zeropoint error
            tran2sig[3]=0. ; turn off zeropoint error

            ; Next select observations to reduce.

            ; Get data in the selected filters that are not planetary
            zfil = where( (pdata.fil eq op[1] or pdata.fil eq op[2]) and $
                          strmid(pdata.stand,0,1) ne 'a' and $
                          strmid(pdata.stand,0,1) ne 'c' and $
                          strmid(pdata.stand,0,1) ne 'p' ,count)
            if count eq 0 then begin
               print,'No data found in filter ',op[1],' and ',op[2]
               goto,skipit
            endif

            ; Now must remove those that are standards
            stdcheck,'/frakir/raid/buie/photometry/landolt/landphot.dat', $
               pdata.stand[zfil], $
               [ [ replicate(c1,count) ], $
                 [ replicate(c2,count) ]    ], $
               stdflg

            z = where(stdflg ne 1,countdo)
            if countdo eq 0 then begin
               print,'No non-standard objects to reduce in ',op[1],' and ',op[2]
               goto,skipit
            endif
            zfil=zfil[z]

            ; Reduce `em
            oldbad=pdata.bad
            tmpbad=pdata.bad[zfil]
            colorsol,pdata.stand[zfil],pdata.fil[zfil],pdata.jd[zfil], $
               pdata.am[zfil],pdata.serial[zfil],pdata.mag[zfil], $
               pdata.err[zfil],op[1],op[2],tran1,tran1sig,jdref1, $
               tran2,tran2sig,jdref2,date=info.data,NOEDIT=noedit, $
               filter1=f1,filter2=f2,/save,badflags=tmpbad, $
               path='/frakir/raid/buie/photometry/stars',FULL=full
            pdata.bad[zfil]=tmpbad
            z1=where(oldbad ne pdata.bad,countchange)
            if countchange gt 0 and not badisdirty then badisdirty=1
         ENDCASE

; ------------------------------------------------------------------------------
; dp - Differential photometric reduction (like Pluto)
; ------------------------------------------------------------------------------

         'dp': BEGIN
            if n_elements(op) lt 8 then begin
               print,'bad rule: ',info.oplist[i]
               goto,skipit
            endif

            CASE op[2] OF
               'B': fa=1
               'V': fa=2
               'R': fa=3
               'M': fa=9
               ELSE: BEGIN
                     print,'bad rule: ',info.oplist[i]
                     print,'Unrecognized filter [',op[2],']'
                     goto,skipit
                  ENDELSE
            ENDCASE

            c1=strmid(op[3],0,1)
            CASE c1 OF
               'B': c1=1
               'V': c1=2
               'R': c1=3
               ELSE: BEGIN
                     print,'bad rule: ',info.oplist[i]
                     print,'Unrecognized filter in color [',op[3],']'
                     goto,skipit
                  ENDELSE
            ENDCASE

            c2=strmid(op[3],1,1)
            CASE c2 OF
               'B': c2=1
               'V': c2=2
               'R': c2=3
               ELSE: BEGIN
                     print,'bad rule: ',info.oplist[i]
                     print,'Unrecognized filter in color [',op[3],']'
                     goto,skipit
                  ENDELSE
            ENDCASE


            ; regularize the optional arguments

            loadtrans=1
            transdate=info.data
            forcek2=0
            forcect=0
            forcek =0
            k2=[999.,0.]
            ct=[999.,0.]
            k =[999.,0.]
            opnum=8
            WHILE opnum lt n_elements(op) DO BEGIN
               CASE op[opnum] OF

                  ; Override second order extinction, turns off auto load of
                  ;   transformation values, color set to zero if not already
                  ;   overridden.  Can't combine with td.
                  'k2': BEGIN
                        IF loadtrans and info.data ne transdate THEN BEGIN
                           print,'bad rule: ',info.oplist[i]
                           print,'Cannot combine "td" with "k2"'
                           goto,skipit
                        ENDIF ELSE IF opnum+2 ge n_elements(op) THEN BEGIN
                           print,'bad rule: ',info.oplist[i]
                           print,'Need two arguments for "k2"'
                           goto,skipit
                        ENDIF ELSE BEGIN
                           loadtrans=0
                           forcek2 = 1
                           k2=[float(op[opnum+1:opnum+2])]
                           opnum = opnum+3
                           IF not forcect THEN BEGIN
                              forcect=1
                              ct=[0.,0.]
                           ENDIF
                        ENDELSE
                     END
                  'ct': BEGIN
                        IF loadtrans and info.data ne transdate THEN BEGIN
                           print,'bad rule: ',info.oplist[i]
                           print,'Cannot combine "td" with "ct"'
                           goto,skipit
                        ENDIF ELSE IF opnum+2 ge n_elements(op) THEN BEGIN
                           print,'bad rule: ',info.oplist[i]
                           print,'Need two arguments for "ct"'
                           goto,skipit
                        ENDIF ELSE BEGIN
                           loadtrans=0
                           forcect = 1
                           ct=[float(op[opnum+1:opnum+2])]
                           opnum = opnum+3
                           IF not forcek2 THEN BEGIN
                              forcek2=1
                              k2=[0.,0.]
                           ENDIF
                        ENDELSE
                     END
                  'k': BEGIN
                        IF opnum+2 ge n_elements(op) THEN BEGIN
                           print,'bad rule: ',info.oplist[i]
                           print,'Need two arguments for "k"'
                           goto,skipit
                        ENDIF ELSE IF forcek THEN BEGIN
                           print,'bad rule: ',info.oplist[i]
                           print,'"k" can only be specified once'
                           goto,skipit
                        ENDIF ELSE BEGIN
                           forcek = 1
                           k=[float(op[opnum+1:opnum+2])]
                           opnum = opnum+3
                        ENDELSE
                     END
                  'td': BEGIN
                        IF opnum+1 ge n_elements(op) THEN BEGIN
                           print,'bad rule: ',info.oplist[i]
                           print,'Need one argument for "td"'
                           goto,skipit
                        ENDIF ELSE IF loadtrans and info.data ne transdate THEN BEGIN
                           print,'bad rule: ',info.oplist[i]
                           print,'"td" can only be specified once'
                           goto,skipit
                        ENDIF ELSE IF forcek2 or forcect THEN BEGIN
                           print,'bad rule: ',info.oplist[i]
                           print,'Cannot combine "td" with "k2" or "ct"'
                           goto,skipit
                        ENDIF ELSE BEGIN
                           transdate = op[opnum+1]
                           opnum = opnum+2
                        ENDELSE
                     END
                  ELSE: BEGIN
                        print,'bad rule args: ',info.oplist[i]
                        print,'Unrecognized optional argument: [',op[opnum],']'
                        goto,skipit
                     ENDELSE
               ENDCASE
            ENDWHILE

            IF fa ne 9 and loadtrans THEN BEGIN
               gettran,info.inst,transdate,fa,c1,c2,tran,transig,jdref
               sz=size(tran)
               if sz[0] eq 0 then begin
                  print,'No transformation found for ',transdate,', ', $
                     op[2],' - ',op[3]
                  return
               endif
               k2 = [tran[1],transig[1]]
               ct = [tran[2],transig[2]]
            ENDIF

            ; Load my standards and see if the magnitude is there and valid
            if fa ne 9 then begin
               rdphocat,'/frakir/raid/buie/photometry/buiestd.dat',lname,lmags,lcod
               cleanname = repchar(op[7],'_',' ')
               zstd = where(cleanname eq lname)
               IF zstd[0] ge 0 THEN BEGIN
                  smag = lmags[fa-1,zstd[0]]
                  scolor = lmags[c1-1,zstd[0]]-lmags[c2-1,zstd[0]]
               ENDIF ELSE BEGIN
                  rdland2,lname,lmags,lcod
                  zstd = where(cleanname eq lname)
                  IF zstd[0] ge 0 THEN BEGIN
                     smag = lmags[fa-1,zstd[0]]
                     scolor = lmags[c1-1,zstd[0]]-lmags[c2-1,zstd[0]]
                  ENDIF ELSE BEGIN
                     print,'bad rule args: ',info.oplist[i]
                     print,'Standard star ',cleanname,' not found.'
                     goto,skipit
                  ENDELSE
               ENDELSE
            endif else begin
               smag = 0.0
               scolor = 0.0
            endelse

            stdname = repchar(op[7],'_',' ')
            objname=naifname(op[4])
            dnam=info.data+'_'+op[2]+'.'+objname

            if not forcek then k = [999.,0.]

            ltcrv,pdata.stand,pdata.fil,pdata.jd,pdata.am,pdata.serial, $
               pdata.mag,pdata.err,op[4],fix(op[5]),stdname,op[1],jdout, $
               K2=k2,CTERM=ct,OCOLOR=[float(op[6]),0.],SCOLOR=[scolor,0.], $
               STDMAG=[smag,0.],FILTNAME=op[2],OBJNAME=objname, $
               FILE=dnam,BAD=pdata.bad,FORCE=k

            sz=size(jdout)
            if sz[0] eq 0 then return
            plotsdone=1

            IF objname eq 'Pluto' THEN BEGIN
               ; Put the output photometry in the final data area
               fdir='/frakir/raid/buie/pluto/Photometry/'
               cmd='cat '+info.data+'_*.'+objname+' | sort > '+fdir+info.data+'.001'
               print,cmd
               spawn,cmd
               ; Update the master history file for this dataset
               histname=fdir+'master.zpt'
               tag=string(info.data,op[2]+blanks,format='(a,1x,a2)')
               outinfo=string(info.inst+blanks,op[7]+blanks,smag,scolor, $
                                 format='(1x,a10,1x,a16,1x,f7.4,1x,f6.4)')
               print,tag+outinfo
               repwrite,histname,tag,tag+outinfo
            ENDIF
         END

         ELSE: BEGIN
            print,'unknown rule: ',info.oplist[i]
         ENDELSE
      ENDCASE

skipit:
   endfor

   if keyword_set(force) and printit then begin
      datamon,log1file,/print,/noqueue
      plotsdone=1
   endif
   
   if !d.name eq 'PS' and plotsdone then begin
      hardcopy
      print,'Plot sent to the default printer.'
   endif
   IF printit THEN BEGIN
      display
   ENDIF

bailout:
   if badisdirty then begin
      print,'Updating ',log1file,' with new "bad" flags'
      fmt1 = '(a,1x,"''",a,"''",1x,a,1x,f13.5,1x,f8.3,1x,f6.2,1x,f7.3,1x,f7.3,' + $
             '1x,f7.3,1x,i4.4,1x,f8.3,1x,f8.3,1x,f5.2,1x,f7.1,1x,f8.2,1x,f6.2,' + $
             '1x,f8.4,1x,f7.4,1x,i1)'
      openw,lun,log1file,/get_lun
      printf,lun,'PHOTFILE v1.0'
      for i=0,n_elements(pdata.jd)-1 do begin
         printf,lun,FORMAT=fmt1, $
         pdata.filename[i], pdata.obj[i], pdata.fil[i], pdata.jd[i], $
         pdata.exptime[i], pdata.gain[i], pdata.rad[i], $
         pdata.sky1[i], pdata.sky2[i], pdata.serial[i], $
         pdata.xpos[i], pdata.ypos[i], pdata.fwhm[i], $
         pdata.maxcnt[i], pdata.sky[i], pdata.skyerr[i], $
         pdata.mag[i], pdata.err[i], pdata.bad[i]
      endfor
      free_lun,lun
      spawn,'ls -l '+log1file,result
      info.logdate=result[0]
   endif

end
