;+
; NAME:
;  plpedit
; PURPOSE:
;  Interactive editing of photometry data files.
; DESCRIPTION:
;  This is a non-blocking widget tool that edits .001 and .log* files and
;    views .dat files.  You can have multiple copies running, just don't
;    edit the same file in two tools (just like a regular editor).
; CATEGORY:
;  Photometry
; CALLING SEQUENCE:
;  plpedit
; INPUTS:
;
; OPTIONAL INPUT PARAMETERS:
;
; KEYWORD INPUT PARAMETERS:
;
; OUTPUTS:
;
; KEYWORD OUTPUT PARAMETERS:
;
; COMMON BLOCKS:
;
; SIDE EFFECTS:
;
; RESTRICTIONS:
;
; PROCEDURE:
;
; MODIFICATION HISTORY:
;  Written by Marc W. Buie, Lowell Observatory, 2000/07/19
;  2000/08/30, MWB, Added option for manipulating log files.
;
;-
pro plpedit_cleanup, tlb

   ; get pointer to state structure
   widget_control, tlb, get_uvalue=state

   ; Free up any dynamically allocated space with the state structure
   ptr_free,(*state).bad
   ptr_free,(*state).err
   ptr_free,(*state).exptime
   ptr_free,(*state).fil
   ptr_free,(*state).fil_list
   ptr_free,(*state).filename
   ptr_free,(*state).fwhm
   ptr_free,(*state).gain
   ptr_free,(*state).jd
   ptr_free,(*state).lon
   ptr_free,(*state).mag
   ptr_free,(*state).maxcnt
   ptr_free,(*state).obj
   ptr_free,(*state).rad
   ptr_free,(*state).serial
   ptr_free,(*state).sky1
   ptr_free,(*state).sky2
   ptr_free,(*state).sky
   ptr_free,(*state).skyerr
   ptr_free,(*state).time
   ptr_free,(*state).xpos
   ptr_free,(*state).ypos

   ; Free up the state structure itself.
   ptr_free, state

end

function plpedit_filecheck, state

   if (*state).dirty then begin
      text = [ $
         'The bad flags in current file have been modified but the file has', $
         'not yet been saved.  If you choose to proceed you will lose all', $
         'changes.  If you do not wish to lose the changes, then cancel and', $
         'save the file before proceeding.']
      return,qannounc(text,FALSE='Abort',TITLE='File Modified', $
                  TRUE='Continue, abondon changes',xsize=72,ysize=4)
   endif

end

pro plpedit_filesave, state

   if (*state).fn eq '' then return

   if (*state).type eq '001' then begin

      ; save the data with the modified bad flags.
      wrphot,(*(*state).jd), (*(*state).fil),(*(*state).mag), $
               (*(*state).err),(*state).fn,bad=(*(*state).bad)

      ; write a marker file.
      openw,lun,'lastedit.tag',/get_lun
      printf,lun,(*state).fn
      free_lun,lun

      (*state).dirty=0

   endif else if (*state).type eq 'log' then begin
      wrphalt,(*state).fn,(*(*state).filename),(*(*state).obj),(*(*state).fil), $
         (*(*state).jd),(*(*state).exptime),(*(*state).gain), $
         (*(*state).rad),(*(*state).sky1),(*(*state).sky2), $
         (*(*state).serial),(*(*state).xpos),(*(*state).ypos), $
         (*(*state).fwhm),(*(*state).maxcnt),(*(*state).sky), $
         (*(*state).skyerr),(*(*state).mag),(*(*state).err),(*(*state).bad)
      (*state).dirty=0
   endif

end

pro plpedit_plot, state

   if (*state).fn eq '' then return

   if not (*state).hardcopy then begin
      widget_control, (*state).drawwin, get_value=winnum
      wset,winnum
   endif

   if (*state).type eq '001' then begin
      z = where( (*(*state).bad) eq 0 and $
                 (*(*state).fil) eq (*state).curfil,count)
      if count ne 0 then begin
         if (*state).scaleall then begin
            zs = where( (*(*state).fil) eq (*state).curfil )
            xr = minmax( (*(*state).time)[zs] )
            yr = [ max( (*(*state).mag)[zs]+(*(*state).err)[zs] ), $
                   min( (*(*state).mag)[zs]-(*(*state).err)[zs] ) ]
         endif else begin
            xr = minmax( (*(*state).time)[z] )
            yr = [ max( (*(*state).mag)[z]+(*(*state).err)[z] ), $
                   min( (*(*state).mag)[z]-(*(*state).err)[z] ) ]
         endelse
         avger,(*(*state).time)[z],(*(*state).mag)[z],(*(*state).err)[z] > 0.0001,6, $
            1.3,avgtime,avgmag,avgerr,verbose=(*state).verbose,xspread=1.0, $
            dataerr=derr
         if (*state).select ge 0 then setusym,-1 else setusym,1
         ploterror,(*(*state).time)[z],(*(*state).mag)[z],(*(*state).err)[z],psym=8, $
            xtitle='UT time (hours)',ytitle=(*state).curfil+' Magnitude', $
            title=(*state).fn,xr=xr,yr=yr
         oploterror,avgtime,avgmag,derr,psym=5,symsize=1.5
         if (*state).select ge 0 and not (*state).hardcopy then begin
            setusym,1
            oploterror,(*(*state).time)[(*state).select], $
                       (*(*state).mag)[(*state).select], $
                       (*(*state).err)[(*state).select],psym=8
         endif
         z = where( (*(*state).bad) eq 1 and (*(*state).fil) eq (*state).curfil,count)
         if count ne 0 then begin
            oploterror,(*(*state).time)[z], $
                       (*(*state).mag)[z], $
                       (*(*state).err)[z],psym=7,charsize=1.8
         endif
      endif else begin
         erase
      endelse
   endif else if (*state).type eq 'dat' then begin

      ; Set y-range of plot which will include error bars
      yr = [ max( (*(*state).mag)+(*(*state).err) ), $
               min( (*(*state).mag)-(*(*state).err) ) ]

      ; Setup basic axis with no data
      plot,[0],[1],xtitle='East Longitude (degrees)',ytitle='Magnitude', $
         title=(*state).fn,xr=[0,360],yr=yr,/nodata

      ; If no objects are selected, then just plot everything with
      ;    filled circles.
      if (*state).select lt 0 or (*state).hardcopy then begin
         oploterror,(*(*state).lon),(*(*state).mag),(*(*state).err),psym=8

      ; Handle plot with a selected object
      endif else begin

         ; This case is a bit more complicated, there are three types of
         ;   points to be distinguished:
         ;   1 - The selected single point
         ;   2 - All points taken within 12 hours of the selected point.
         ;   3 - All other points
         zn = where( abs((*(*state).jd)-(*(*state).jd)[(*state).select]) le 0.5 and $
                     (*(*state).jd) ne (*(*state).jd)[(*state).select] )
         zo = where( abs((*(*state).jd)-(*(*state).jd)[(*state).select]) gt 0.5 )
         if zo[0] ne -1 then begin
            setusym,6
            oploterror,(*(*state).lon)[zo], $
                        (*(*state).mag)[zo], $
                        (*(*state).err)[zo],psym=8,/nohat,symsize=0.8
            setusym,1
         endif
         if zn[0] ne -1 then begin
            setusym,-1
            oploterror,(*(*state).lon)[zn], $
                        (*(*state).mag)[zn], $
                        (*(*state).err)[zn],psym=8,symsize=1.2,/nohat
            setusym,1
         endif
         oploterror,(*(*state).lon)[(*state).select], $
                     (*(*state).mag)[(*state).select], $
                     (*(*state).err)[(*state).select],psym=8,symsize=1.2,/nohat
      endelse

   endif else if (*state).type eq 'log' then begin
      z = where( (*(*state).bad) eq 0 and $
                 (*(*state).fil) eq (*state).curfil and $
                 (*(*state).obj) eq (*state).curobj,count)
      if count ne 0 then begin
         if (*state).scaleall then begin
            zs = where( (*(*state).fil) eq (*state).curfil and $
                        (*(*state).obj) eq (*state).curobj )
            xr = minmax( (*(*state).time)[zs] )
            yr = [ max( (*(*state).mag)[zs]+(*(*state).err)[zs] ), $
                   min( (*(*state).mag)[zs]-(*(*state).err)[zs] ) ]
         endif else begin
            xr = minmax( (*(*state).time)[z] )
            yr = [ max( (*(*state).mag)[z]+(*(*state).err)[z] ), $
                   min( (*(*state).mag)[z]-(*(*state).err)[z] ) ]
         endelse
         if (*state).select ge 0 then setusym,-1 else setusym,1
         if count eq 1 then begin
            xr[1]=xr[1]+1
            z=[z,z]
         endif

         ploterror,[(*(*state).time)[z]],[(*(*state).mag)[z]],[(*(*state).err)[z]],psym=8, $
            xtitle='UT time (hours)',ytitle=(*state).curfil+' Magnitude', $
            title=(*state).curobj+'   '+(*state).fn,xr=xr,yr=yr
         if (*state).select ge 0 and not (*state).hardcopy then begin
            setusym,1
            oploterror,(*(*state).time)[(*state).select], $
                       (*(*state).mag)[(*state).select], $
                       (*(*state).err)[(*state).select],psym=8
         endif
         z = where( (*(*state).bad) eq 1 and $
                    (*(*state).fil) eq (*state).curfil and $
                    (*(*state).obj) eq (*state).curobj,count)
         if count ne 0 then begin
            oploterror,(*(*state).time)[z], $
                       (*(*state).mag)[z], $
                       (*(*state).err)[z],psym=7,charsize=1.8
         endif
      endif else begin
         erase
      endelse
   endif else begin
      print,'This is never supposed to happen.  (*state).type=',(*state).typ
   endelse
end

pro plpedit_updateinfo,state

   if (*state).fn eq '' then return

   widget_control, (*state).mainbase, UPDATE=0
   if (*state).select lt 0 then begin
      widget_control, (*state).obj_jd_id, SET_VALUE='             '
      widget_control, (*state).obj_mag_id, SET_VALUE='       '
      widget_control, (*state).obj_err_id, SET_VALUE='       '
   endif else begin
      jdstr,(*(*state).jd)[(*state).select],0,str
      widget_control, (*state).obj_jd_id, $
         SET_VALUE=str+'  '+string((*(*state).jd)[(*state).select],format='(1x,f13.5,1x)')
      widget_control, (*state).obj_mag_id, $
         SET_VALUE=string((*(*state).mag)[(*state).select],format='(1x,f7.4,1x)')
      widget_control, (*state).obj_err_id, $
         SET_VALUE=string((*(*state).err)[(*state).select],format='(1x,f6.4,1x)')
   endelse
   widget_control, (*state).mainbase, UPDATE=1

end

pro plpedit_eve, event

   widget_control, event.top, GET_UVALUE=state

   if event.id eq (*state).mainbase then $
      event_name = 'Mainbase' $
   else $
      widget_control, event.id,  GET_UVALUE=event_name, /HOURGLASS

   exit = event_name eq 'THE_MENU'
   if exit then exit = event.value eq 'Exit'

   case event_name of

      'THE_MENU': begin
         case event.value of

            '001 Files': begin
               (*state).filefilter='*.001'
            end

            'dat Files': begin
               (*state).filefilter='*.dat'
            end

            'log Files': begin
               (*state).filefilter='*.log*'
            end

            'Autosave Off': begin
               (*state).autosave=0
            end

            'Autosave On': begin
               (*state).autosave=1
            end

            'Open' : begin
               if (*state).dirty then begin
                  if (*state).autosave then plpedit_filesave,state $
                  else if not plpedit_filecheck(state) then return
               endif

               cd,current=current
               fn = dialog_pickfile( group=event.top, TITLE='Select file', $
                                     FILTER=(*state).filefilter, /must_exist, path=current)

               if fn ne '' then begin

                  print,'Loading file ',fn
                  fdecomp,fn,disk,dir,name,suf
                  suffix=strmid(suf,0,3)
                  if suffix eq '001' then begin
                     rdphot3,fn,jd,fil,mag,err,bad
                     fil=strtrim(fil,2)
                     newfils = fil[uniq(fil,sort(fil))]
                     widget_control, (*state).fillistid, set_value=newfils, $
                        set_droplist_select=0, /sensitive
                     (*state).curfil = newfils[0]
                     (*state).curobj = ''
                     jd0  = long(jd[0]-0.5d0)+0.50d0
                     time = (jd - jd0)*24.0

                     ptr_free,(*state).jd
                     ptr_free,(*state).fil
                     ptr_free,(*state).mag
                     ptr_free,(*state).err
                     ptr_free,(*state).bad
                     ptr_free,(*state).fil_list
                     ptr_free,(*state).time

                     (*state).jd    = ptr_new(jd)
                     (*state).fil   = ptr_new(fil)
                     (*state).mag   = ptr_new(mag)
                     (*state).err   = ptr_new(err)
                     (*state).bad   = ptr_new(bad)
                     (*state).fil_list=ptr_new(newfils)
                     (*state).jd0   = jd0
                     (*state).time  = ptr_new(time)
                     (*state).fn    = fn
                     (*state).dirty = 0
                     (*state).select = -1

                     widget_control,(*state).flagid,sensitive=0
                     widget_control,(*state).objsel,sensitive=0
                     (*state).type   = suffix
                     (*state).suffix = suf
                  endif else if suffix eq 'dat' then begin
                     readcol,fn,jd,lon,mag,err,format='d,x,x,f,x,x,x,x,x,x,f,f',/silent

                     ptr_free,(*state).jd
                     ptr_free,(*state).lon
                     ptr_free,(*state).mag
                     ptr_free,(*state).err

                     (*state).jd  = ptr_new(jd)
                     (*state).lon = ptr_new(lon)
                     (*state).mag = ptr_new(mag)
                     (*state).err = ptr_new(err)

                     (*state).fn    = fn
                     (*state).dirty = 0
                     (*state).select = -1
                     (*state).type   = suffix
                     (*state).suffix = suf
                     (*state).curobj = ''
                     widget_control,(*state).flagid,sensitive=0
                     widget_control,(*state).fillistid,sensitive=0
                     widget_control,(*state).objsel,sensitive=0
                  endif else if suffix eq 'log' then begin
                     rdphalt,fn,filename,obj,fil,jd,exptime,gain,rad,sky1,sky2, $
                        serial,xpos,ypos,fwhm,maxcnt,sky,skyerr,mag,err,bad
                     newfils = fil[uniq(fil,sort(fil))]
                     widget_control, (*state).fillistid, set_value=newfils, $
                        set_droplist_select=0, /sensitive
                     (*state).curfil = newfils[0]
                     (*state).curobj = ''
                     jd0  = long(jd[0]-0.5d0)+0.50d0
                     time = (jd - jd0)*24.0

                     ptr_free,(*state).filename
                     ptr_free,(*state).obj
                     ptr_free,(*state).fil
                     ptr_free,(*state).jd
                     ptr_free,(*state).exptime
                     ptr_free,(*state).gain
                     ptr_free,(*state).rad
                     ptr_free,(*state).serial
                     ptr_free,(*state).sky1
                     ptr_free,(*state).sky2
                     ptr_free,(*state).xpos
                     ptr_free,(*state).ypos
                     ptr_free,(*state).fwhm
                     ptr_free,(*state).maxcnt
                     ptr_free,(*state).sky
                     ptr_free,(*state).skyerr
                     ptr_free,(*state).mag
                     ptr_free,(*state).err
                     ptr_free,(*state).bad
                     ptr_free,(*state).fil_list
                     ptr_free,(*state).time

                     (*state).filename = ptr_new(filename)
                     (*state).obj      = ptr_new(obj)
                     (*state).fil      = ptr_new(fil)
                     (*state).jd       = ptr_new(jd)
                     (*state).exptime  = ptr_new(exptime)
                     (*state).gain     = ptr_new(gain)
                     (*state).rad      = ptr_new(rad)
                     (*state).serial   = ptr_new(serial)
                     (*state).sky1     = ptr_new(sky1)
                     (*state).sky2     = ptr_new(sky2)
                     (*state).xpos     = ptr_new(xpos)
                     (*state).ypos     = ptr_new(ypos)
                     (*state).fwhm     = ptr_new(fwhm)
                     (*state).maxcnt   = ptr_new(maxcnt)
                     (*state).sky      = ptr_new(sky)
                     (*state).skyerr   = ptr_new(skyerr)
                     (*state).mag      = ptr_new(mag)
                     (*state).bad      = ptr_new(bad)
                     (*state).err      = ptr_new(err)
                     (*state).fil_list = ptr_new(newfils)
                     (*state).jd0      = jd0
                     (*state).time     = ptr_new(time)
                     (*state).fn    = fn
                     (*state).dirty    = 0
                     (*state).select   = -1

                     widget_control,(*state).flagid,sensitive=0
                     (*state).type     = suffix
                     (*state).suffix   = suf
                     widget_control,(*state).objsel,sensitive=1
                  endif else begin
                     print,'Unrecognized suffix [',suffix,']'
                     widget_control,(*state).objsel,sensitive=0
                  endelse
                  plpedit_plot, state
                  plpedit_updateinfo,state
               endif

            end

            'Print' : begin
               if (*state).fn eq '' then return
               (*state).hardcopy = 1
               landscap
               plpedit_plot, state
               hardcopy
               display
               (*state).hardcopy = 0
               plpedit_plot, state
            end

            'Save' : begin
               plpedit_filesave,state
            end

            'Scale All' : begin
               (*state).scaleall=1
               plpedit_plot, state
            end

            'Scale Good' : begin
               (*state).scaleall=0
               plpedit_plot, state
            end

            'Terse' : begin
               (*state).verbose=0
            end

            'Verbose' : begin
               (*state).verbose=1
               plpedit_plot, state
            end

            'Exit' : begin
               if (*state).dirty then begin
                  if (*state).autosave then plpedit_filesave,state $
                  else if not plpedit_filecheck(state) then return
               endif
               widget_control, event.top, /DESTROY
               return
            end

            else: begin
               message, 'Unknown menu event:', /INFO
               help, event, /STRUCTURE
            end

         endcase

      end ; THE_MENU

      'Change Object' : begin
         z = where( (*(*state).fil) eq (*state).curfil )
         obj = (*(*state).obj)[z]
         objectlist = obj[uniq(obj,sort(obj))]
         newobj = picker( objectlist, group=event.top, $
                          title='Select Object')
         if newobj ne '[[[CANCEL]]]' then begin
            (*state).curobj = newobj
            plpedit_plot, state
            plpedit_updateinfo,state
         endif
      end

      'Exit' : begin
         if (*state).dirty then begin
            if (*state).autosave then plpedit_filesave,state $
            else if not plpedit_filecheck(state) then return
         endif
         widget_control, event.top, /DESTROY
         return
      end

      'Filter Select': begin
         (*state).curfil = (*(*state).fil_list)[event.index]
         (*state).select = -1L
         widget_control,(*state).flagid,sensitive=0
         plpedit_plot, state
         plpedit_updateinfo,state
      end

      'Mainbase': begin
         info=widget_info((*state).colbaseid,/geometry)
         widget_control,(*state).drawwin,xsize=event.x,ysize=event.y-info.ysize
         plpedit_plot, state
      end

      'Set Flag': begin
         if (*state).fn eq '' or (*state).select lt 0 then return

         if event.value eq 'Bad' then begin
            if (*(*state).bad)[(*state).select] eq 0 then begin
               (*(*state).bad)[(*state).select] = 1
               (*state).dirty = 1
               plpedit_plot, state
            endif
         endif else begin
            if (*(*state).bad)[(*state).select] eq 1 then begin
               (*(*state).bad)[(*state).select] = 0
               (*state).dirty = 1
               plpedit_plot, state
            endif
         endelse

      end

      'Window': begin
         if event.type eq 0 and event.press eq 1 then begin
            if (*state).type eq '001' then begin
               z = where( (*(*state).fil) eq (*state).curfil, count)
               dev=convert_coord((*(*state).time)[z],(*(*state).mag)[z],/data,/to_device)
               d_x = dev[0,*]
               d_y = dev[1,*]
               dist = sqrt( (d_x-event.x)^2 + (d_y-event.y)^2 )
               zz=where(dist eq min(dist))
               zz=zz[0]
               (*state).select = z[zz]
               widget_control, (*state).flagid, SET_VALUE=(*(*state).bad)[(*state).select]
               widget_control,(*state).flagid,sensitive=1
               plpedit_plot, state
               plpedit_updateinfo,state
            endif else if (*state).type eq 'dat' then begin
               dev=convert_coord((*(*state).lon),(*(*state).mag),/data,/to_device)
               d_x = dev[0,*]
               d_y = dev[1,*]
               dist = sqrt( (d_x-event.x)^2 + (d_y-event.y)^2 )
               zz=where(dist eq min(dist))
               zz=zz[0]
               (*state).select = zz
               plpedit_plot, state
               plpedit_updateinfo,state
            endif else if (*state).type eq 'log' then begin
               z = where( (*(*state).fil) eq (*state).curfil and $
                          (*(*state).obj) eq (*state).curobj, count)
               dev=convert_coord((*(*state).time)[z],(*(*state).mag)[z],/data,/to_device)
               d_x = dev[0,*]
               d_y = dev[1,*]
               dist = sqrt( (d_x-event.x)^2 + (d_y-event.y)^2 )
               zz=where(dist eq min(dist))
               zz=zz[0]
               (*state).select = z[zz]
               widget_control, (*state).flagid, SET_VALUE=(*(*state).bad)[(*state).select]
               widget_control,(*state).flagid,sensitive=1
               plpedit_plot, state
               plpedit_updateinfo,state
            endif
         endif
      end

      else: begin
         print,'EVENT NAME: ',event_name
         message, 'Unknown event:', /INFO
         help, event, /STRUCTURE
      end

   endcase

end ; end of event handler

pro plpedit

   if (!d.flags and 256) eq 0 then begin
      print, 'Error. No windowing device. PLPEDIT cannot be started.'
      return
   endif

   setusym,1

   ;Define the main base.
   mainbase = widget_base( TITLE='PLPEDIT: Photometry editor', $
                           /COLUMN, UVALUE=0, MBAR=bar, $
                           /TLB_SIZE_EVENTS )

   menu = CW_PdMenu(bar, /RETURN_NAME, $
                    ['1\File',$
                     '0\Open',$
                     '0\Save',$
                     '0\Reload',$
                     '0\Print',$
                     '0\Autosave Off',$
                     '0\Autosave On',$
                     '0\001 Files',$
                     '0\dat Files',$
                     '0\log Files',$
                     '2\Exit',$
                     '1\Controls',$
                     '0\Scale Good', $
                     '0\Scale All', $
                     '0\Verbose', $
                     '2\Terse'], UVALUE='THE_MENU', /MBAR)

   base = widget_base(mainbase,/column)

   win1 = widget_draw( base, XSIZE=650, YSIZE=300, RETAIN=2, $
                       /BUTTON_EVENTS, UVALUE='Window' )

   colbase      = widget_base(base,/ROW)
   b1 = widget_button(colbase,value=' Exit ',uvalue='Exit')
   filid = widget_droplist(colbase,value=['none'], $
                           uvalue='Filter Select',/dynamic_resize)
   widget_control,filid,sensitive=0
   objbut = widget_button(colbase,value=' Change Object ',uvalue='Change Object')
   widget_control,objbut,sensitive=0
   toggle = cw_bgroup( colbase, ['Good','Bad'], /no_release, /exclusive, $
                       /return_name, /row, uvalue='Set Flag' )
   widget_control,toggle,sensitive=0

   objjd  = widget_label( colbase, value='             ', /align_left, /dynamic_resize )
   objmag = widget_label( colbase, value='        ', /align_left, /dynamic_resize )
   objerr = widget_label( colbase, value='       ', /align_left, /dynamic_resize )

   state = ptr_new({ $

      ; Data and information in the widget
      autosave: 1, $             ; Flag, if set automatically saves file
      bad: ptr_new(), $          ; Bad flags
      curfil: 'V', $             ; Name of current filter being plotted.
      curobj: '', $              ; Name of current object being plotted.
      dirty: 0, $                ; has data in current file been modified?
      err: ptr_new(), $          ; Magnitude error
      exptime: ptr_new(), $      ; Exposure time list (used for log files).
      fil: ptr_new(), $          ; Filter code
      fil_list: ptr_new(), $     ; List of current filters.
      filefilter: '*.001', $     ; File filter
      filename: ptr_new(), $     ; List of file names (used for log files).
      fn: '', $                  ; current file name
      fwhm: ptr_new(), $         ; FWHM (used for log files).
      gain: ptr_new(), $         ; Gain values (used for log files).
      hardcopy: 0, $             ; Flag, hardcopy in progress when true.
      jd: ptr_new(), $           ; Julian date of observations
      jd0: 0.0d0, $              ; 0h UT of on Julian date of first point.
      lon: ptr_new(), $          ; sub-earth longitude (.dat only).
      mag: ptr_new(), $          ; Magnitude
      maxcnt: ptr_new(), $       ; Maximum count (used for log files).
      obj: ptr_new(), $          ; Object names (used for log files).
      rad: ptr_new(), $          ; Object aperture radius (used for log files).
      scaleall: 0, $             ; Flag, if set scales plot on all data
      select: -1L, $             ; Currently selected point, index into full
      serial: ptr_new(), $       ; Serial number (used for log files)
      sky1: ptr_new(), $         ; Inner sky radius (used for log files).
      sky2: ptr_new(), $         ; Outer sky radius (used for log files).
      sky: ptr_new(), $          ; Sky signal (used for log files).
      skyerr: ptr_new(), $       ; Sky signal error (used for log files).
      suffix: '', $              ; File suffix of currently loaded file.
      time: ptr_new(), $         ; UT time of observation
      type: '001', $             ; generic file type string identifier
      verbose: 0, $              ; Flag, if set turns on verbose output from avger
      xpos: ptr_new(), $         ; X position (used for log files).
      ypos: ptr_new(), $         ; Y position (used for log files).

      ; Widget ids
      colbaseid: colbase, $      ; ID of button bar on bottom
      drawwin: win1, $           ; ID of main draw window
      fillistid: filid, $        ; ID of filter drop down list
      flagid: toggle, $          ; ID of bad flag toggle.
      obj_jd_id: objjd, $        ; ID of text label for JD of selected point
      obj_mag_id: objmag, $      ; ID of text label for mag of selected point
      obj_err_id: objerr, $      ; ID of text label for err of selected point
      objsel: objbut, $          ; ID of object selection button.

      mainbase: mainbase $       ; ID of top level base.

      })

   ;Stash the state structure pointer.
   widget_control, mainbase, SET_UVALUE=state

   ;Realize the main base.
   widget_control, mainbase, /REALIZE

   ; Give control to the XMANAGER.
   xmanager, 'plpedit', mainbase, $
             EVENT_HANDLER='plpedit_eve',/NO_BLOCK, $
             GROUP_LEADER=mainbase, CLEANUP='plpedit_cleanup'

end

