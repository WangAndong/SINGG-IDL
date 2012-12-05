;+
; NAME:
;   ccdgain
; PURPOSE: (one line)
;   Extract and plot CCD gain transfer curve from flat field image data.
; DESCRIPTION:
; CATEGORY:
;   CCD data processing
; CALLING SEQUENCE:
;   ccdgain,dataset,start,nframes,calibfile
; INPUTS:
;   root     - Root of the image file name (no path, with period).
;   flatname - Filename of master flat field.
;   biasname - Filename of bias frame.
;   frames   - List of frame id's to process.
; OPTIONAL INPUT PARAMETERS:
; KEYWORD PARAMETERS:
;   CALIBFILE - Calibration startup file.  Default is CALIBPATH/files.cal
;
;   CALIBPATH - Path for calibration files.  Default is PATH/calib
;
;   EXCLUDE - Optional vector of image numbers that should be excluded from
;                average.  Default is to include all frames.
;
;   EXPKEY = String - FITS keyword to read to get exposure time, default = EXPTIME
;
;   PATH    - Path to the raw data, default = ''
;
;   SECTION - 4 element vector which, if provide, defines the region of the
;             array dimensions that are used to scale the mean
;             of the arrays before combining (.  If combined in this
;             manner, the arrays are combined weighted by the means.
;                   [x1,x2,y1,y2]
;             These coordinates apply to the pixel locations AFTER cropping.
;             The default is to use the center 50% of the image but not any
;             bigger than 200x200 subsection at the center.
;
; OUTPUTS:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;     93/04/03 - Initial conversion to procedure, Marc W. Buie, Lowell Obs.
;     96/01/06 - MWB, added support for hardcopy under DOS/Windows
;     97/03/15 - MWB, added use of MARKDATA
;     2004/05/05, MWB, added some documentation and changed the plot
;                        axes to conform to standard practice.  Also, changed
;                        the action of the SECTION keyword to match the
;                        similar action with SCALE keyword in mkflat.pro
;-
pro ccdgain,root,start,nframes,calibfile, $
       EXCLUDE=exclude,FILKEY=filkey,PATH=path,CALIBPATH=calibpath, $
       SECTION=in_section,EXPKEY=expkey

if badpar(root,7,0,caller='CCDGAIN: (root) ') then return
if badpar(start,  [2,3],0,caller='CCDGAIN: (start) '  ) then return
if badpar(nframes,[2,3],0,caller='CCDGAIN: (nframes) ') then return
if badpar(calibfile,[0,7],0,caller='CCDGAIN: (calibfile) ',default='files.cal') then return
if badpar(filkey,[0,7],0,caller='CCDGAIN: (filkey) ',default='FILTER') then return
if badpar(expkey,[0,7],0,caller='CCDGAIN: (expkey) ',default='EXPTIME') then return
if badpar(path,[0,7],0,caller='CCDGAIN: (path) ',default='./') then return
path=addslash(path)
defcalib=path+'calib/'
if badpar(calibpath,[0,7],0,caller='CCDGAIN: (calibpath) ', $
             default=defcalib) then return
if badpar(exclude,[0,2,3],[0,1],caller='CCDGAIN: (exclude) ',default=-1) then return
if badpar(in_section,[0,2,3],1,caller='CCDGAIN: (section) ', $
             default=[-1,-1,-1,-1],npts=nsect) then return
if nsect ne 4 then $
   message,'SECTION must contain four elements'

frames=start+indgen(nframes)
bad=intarr(nframes)
print,'Load frames ',start,' to ',start+nframes-1
for i=0,nframes-1 do begin
   z=where(frames[i] eq exclude,count)
   if count ne 0 then bad[i]=1
endfor
sel=where(bad eq 0,count)
if count eq 0 then $
   message,'Error ** you have excluded all frames, nothing to do.'

; Load calibration information
ldcalib,calibfile,calib,valid,calibpath=calibpath
if not valid then return

filename = root+string(frames[sel[0]],format='(i3.3)')
if exists(path+filename+'.fits') then ft='.fits' else ft=''
if not exists(path+filename+ft) then begin
  print,'Image file ',path+filename+ft,' could not be found.  Aborting.'
  return
endif
image = readfits(path+filename+ft,hdr,/silent)
sz=size(image,/dim)
if min(in_section) lt 0 then begin
   section=[sz[0]/4,sz[0]/4,sz[1]/4,sz[1]/4]
print,section
   section=(section < 100)*[-1,1,-1,1] + $
              [sz[0]/2,sz[0]/2,sz[1]/2,sz[1]/2]
print,section
   section = [ section[0] > 0, $
               section[1] < (sz[0]-1), $
               section[2] > 0, $
               section[3] < (sz[1]-1) ]
print,section
endif else begin
   section=in_section
endelse


sectstr='['+string(section[0])+':'+ $
                          string(section[1])+','+ $
                          string(section[2])+':'+ $
                          string(section[3])+']'
sectstr=strcompress(sectstr,/remove_all)
sectstr='Image section '+sectstr
signal=fltarr(nframes)
variance=fltarr(nframes)
print,' '
print,sectstr
print,' '
print,' filename   mean    avgdev  stddev variance   skew     kurt    nfinal'
for i=0,nframes-1 do begin
   if frames[i] ne -1 then begin
           filename = root+string(frames[i],format='(i3.3)')
           if not exists(path+filename+ft) then begin
              print,'Image file ',path+filename+ft,' could not be found.  Aborting.'
              return
           endif
           image = readfits(path+filename+ft,hdr,/silent)
      filter = strtrim(sxpar(hdr,filkey,filename),2)
      exptime = sxpar(hdr,expkey,filename)

      flatcode = where(filter eq calib.filter)
      flatcode=flatcode[0]
      if flatcode[0] eq -1 then $
         message,'Filter '+filter+' does not have a corresponding flat field' 

           ; Overscan correction.
           if calib.xl ge 0 and calib.xr ge 0 and calib.xl lt calib.xr then $
              os = mean( image[calib.xl:calib.xr,*] ) $
           else $
              os = 0.0

           if n_elements(calib.dark) eq 1 then $
              image = (image[calib.x1:calib.x2,calib.y1:calib.y2] - calib.bias - os) / $
                       calib.flat[*,*,flatcode] $
           else $
              image = (image[calib.x1:calib.x2,calib.y1:calib.y2] - calib.bias - $
                                                        calib.dark*exptime - os ) / $
                                   calib.flat[*,*,flatcode]

           robomean,image[section[0]:section[1],section[2]:section[3]],3.0,0.5, $
                       avg,avgdev,stddev,var,skew,kurt,nfinal
           signal[i] = avg
           variance[i] = var
           print,filename,exptime,avg,stddev,var, $
         format='(a,1x,f6.1,1x,f8.2,1x,f7.2,1x,f11.2)'

   endif
endfor

done=0
print=0
while not done do begin

        coeff=goodpoly(signal[sel],variance[sel],1,3,yfit,newx,newy)
        xlin=minmax(signal[sel])
        lin=coeff[0]+coeff[1]*xlin
        if coeff[0] ge 0.0 then $
           readnoise=sqrt(coeff[0])/coeff[1] $
        else $
           readnoise=coeff[0]
        gain = 1/coeff[1]

   if print then begin
      devsave=!d.name
      pfont=!p.font
      set_plot,'ps'
      device,/landscape,/Helvetica
      !p.font=-1
   endif

        yr=minmax([0,xlin,signal[sel]])
        xr=minmax([0,lin,variance[sel]])
        plot,lin,xlin,ytit='Signal (DN)',xtit='Variance (DN)!u2', $
           title='Gain Transfer Curve for '+root,yr=yr,xr=xr
        setusym,-1
        oplot,variance,signal,psym=8
        setusym,1
        oplot,newy,newx,psym=8
        xyouts,0.15,0.9,'Read-noise '+string(readnoise,format='(f6.1)')+' e-', $
           /normal,charsize=1.3
        xyouts,0.15,0.85,'Gain '+string(gain,format='(f5.2)')+' e-/ADU', $
           /normal,charsize=1.3
        xyouts,0.15,0.80,sectstr,/normal,charsize=1.3
        for i=0,n_elements(frames)-1 do $
           xyouts,variance[i],signal[i],string(frames[i]),align=0.5

   if not print then begin
      oldbad = bad
      markdata,variance,signal,bad, $
         xtitle='Variance (DN)',ytitle='Signal (DN)', $
         ptitle='CCD Gain transfer curve'

      sel = where(bad eq 0,count)
      z2bad = where(bad ne oldbad and bad eq 1, countgonebad)
      z2good = where(bad ne oldbad and bad eq 0, countgonegood)

      if countgonebad  ne 0 then print,'Gone bad : ',frames[z2bad]
      if countgonegood ne 0 then print,'Gone good: ',frames[z2good]

      if countgonebad eq 0 and countgonegood eq 0 then begin
         hc='y'
         read,prompt='hardcopy? (y/n) ',hc
         if hc eq 'y' then $
            print=1 $
         else $
            done=1
      endif
   endif else begin
      done=1
      device,/close
      if !version.os_family eq 'unix' then begin
         cmd='lpr idl.ps'
      endif else if !version.os_family eq 'Windows' then begin
         cmd = 'copy idl.ps lpt1:'
      endif
      spawn,cmd
      set_plot,devsave
      !p.font=pfont
   endelse

endwhile

end
