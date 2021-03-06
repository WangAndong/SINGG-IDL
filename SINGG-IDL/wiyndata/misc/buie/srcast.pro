;+
; NAME:
;  srcast
; PURPOSE:
;  Compute ra,dec for all objects in a source list (see findsrc).
; DESCRIPTION:
;
; CATEGORY:
;  Astrometry
; CALLING SEQUENCE:
;  srcast,file
; INPUTS:
;  file - File(s) to be read and have ra,dec computed.  The file names are
;           expected to end in .src (usually generated by findsrc).
; OPTIONAL INPUT PARAMETERS:
;
; KEYWORD INPUT PARAMETERS:
;
;  DIR - Directory where .src files are to be found and where .srd files
;           will be written.
;
;  OVERWRITE - Flag, if not set this program will skip any files that already
;                 have the .srd output file.  Set this to overwrite and
;                 regenerate those files.
;
; OUTPUTS:
;  A file is written, the name of the file is the same as the input except
;    that the final suffix is changed from .src to .srd.  The output file
;    is a FITS format file which is basically a table of numbers. [nobjs,9]
;    The contents are as follows:
;
;   Row    Contents
;    0     Raw x coordinate of object
;    1     Raw y coordinate of object
;    2     FWHM of object
;    3     Raw instrumental magnitude of object
;    4     Uncertainty on the instrumental magnitude
;    5     RA of object, J2000
;    6     Dec of object, J2000
;    7     signal-to-noise ratio of object
;    8     Standard magnitude for object.
;
;  Rows 0-4 and 7 are carried forward from the .src file with no change.
;  The new columns are computed from the astrometric solution for this frame.
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
;  Written by Marc W. Buie, Lowell Observatory, 1999/03/18
;  2000/01/19, MWB, changed for new version of rdastfc
;  2002/01/07, MWB, added DIR keyword
;  2002/09/03, MWB, changed Str_sep call to strsplit
;  2003/02/21, MWB, added a blank padding character to make sure object name
;                      never abuts the file name in the log file.
;  2005/05/10, MWB, added OVERWRITE keyword
;-
pro srcast,file,DIR=dir,OVERWRITE=overwrite

   self='srcast: '
   if badpar(file,7,[0,1],caller=self+'(file) ',npts=nfiles) then return
   if badpar(dir,[0,7],0,caller=self+'(DIR) ',default='') then return
   if badpar(overwrite,[0,7],0,caller=self+'(DIR) ',default='') then return

   IF not exists('fitcoeff.dat') THEN begin
      print,self+'fitcoeff.dat support file not found, aborting.'
      return
   endif

   fmt='($,a)'

   rdastfc,'fitcoeff.dat',ffn,ftype,xc,yc,cra,cdec,photzp,flagarr,coeffarr,ncoeffs,nlines

   for i=0,nfiles-1 do begin
      pos=strpos(file[i],'.src')
      if pos ne -1 then begin
         imname = strmid(file[i],0,pos) + strmid(file[i],pos+4,999)
         z = where(imname eq ffn,count)
         if count eq 2 then begin
            outname = strmid(file[i],0,pos) + '.srd' + strmid(file[i],pos+4,999)
            if not exists(outname) or overwrite then begin
               print,file[i],format=fmt

               ; Bust up the file name and get the extension
               words=strsplit(imname,'x',/extract)
               if n_elements(words) eq 2 then $
                  exttag = 'x'+words[1] $
               else $
                  exttag = ''

               ; Get the index to the coefficients
               IF ftype[z[0]] eq 'eta' THEN BEGIN
                  floce = z[0]
                  flocx = z[1]
               ENDIF ELSE BEGIN
                  floce = z[1]
                  flocx = z[0]
               ENDELSE

               ; Get the raw positions.
               list=readfits(dir+file[i],hdrsrc,/silent)
               nlist=n_elements(list)/6
               xpos=reform(list[*,0],nlist)
               ypos=reform(list[*,1],nlist)
               fwhm=reform(list[*,2],nlist)
               mag =reform(list[*,3],nlist)
               err =reform(list[*,4],nlist)
               snr =reform(list[*,5],nlist)
               nx  =sxpar(hdrsrc,'XSIZE')
               ny  =sxpar(hdrsrc,'YSIZE')
               renormfac=sqrt(float(nx)^2+float(ny)^2)

               ; Extract the coefficients
               cxi  = coeffarr[flocx,0:ncoeffs[flocx]-1]
               ceta = coeffarr[floce,0:ncoeffs[floce]-1]
               cxi  = cxi[*]
               ceta = ceta[*]
               xiterms  = flagarr[flocx,*]
               etaterms = flagarr[floce,*]
               xiterms  = xiterms[*]
               etaterms = etaterms[*]

               dx=(xpos-xc[flocx])/renormfac
               dy=(ypos-yc[flocx])/renormfac
               zp=photzp[flocx]

               xi = asteval(dx,dy,cxi,xiterms)/3600.0d0*!dpi/180.0d0
               eta= asteval(dx,dy,ceta,etaterms)/3600.0d0*!dpi/180.0d0
               astsn2rd,xi,eta,cra[flocx],cdec[floce],ra,dec

               ; compute limiting magnitude
               object  =sxpar(hdrsrc,'OBJECT')
               airmass =sxpar(hdrsrc,'AIRMASS')
               meanfwhm=sxpar(hdrsrc,'MEANFWHM')
               objrad  =sxpar(hdrsrc,'OBJRAD')
               avgsky  =sxpar(hdrsrc,'SKYLEVEL')
               skysigma=sxpar(hdrsrc,'SKYSIGMA')
               gain    =sxpar(hdrsrc,'GAIN')
               exptime =sxpar(hdrsrc,'EXPTIME')
               maxsig  =sxpar(hdrsrc,'MAXSIG')
               obscura1=sxpar(hdrsrc,'OBSCURA1')
               obscura2=sxpar(hdrsrc,'OBSCURA2')

               gauss2d,2*objrad+5,2*objrad+5,objrad+2,objrad+2,meanfwhm,psf
               basphote,gain,psf*skysigma*2.0,exptime,objrad+2,objrad+2,objrad,0.0,0.0, $
                  /exact,/nolog,/silent,mag=maglimit
               basphote,gain,psf*(maxsig-avgsky),exptime,objrad+2,objrad+2,objrad,0.0,0.0, $
                  /exact,/nolog,/silent,mag=brightlimit

               list=[[xpos],[ypos],[fwhm],[mag],[err],[ra],[dec],[snr],[mag+zp]]
               sxaddpar,hdrsrc,'NAXIS2',9
               sxaddpar,hdrsrc,'PHOTZP',zp,' Photometric zero-point correction (mag)'
               sxaddpar,hdrsrc,'MAGLIMIT',maglimit+zp,' 2 sigma limiting magnitude'
               sxaddpar,hdrsrc,'SATLIMIT',brightlimit+zp,' Saturation limiting magnitude'
               writefits,dir+outname,list,hdrsrc
               print,' fwhm=',strn(meanfwhm,format='(f5.1)'), $
                     ' zp=',strn(zp,format='(f8.3)',length=6), $
                     ' maglimits=',strn(brightlimit+zp,format='(f7.1)'), $
                     ',',strn(maglimit+zp,format='(f7.1)')

               ; send information to log file
               fnlog = 'info'+exttag+'.log'
               tag   = imname
               info  = ' '+strn(object,padtype=1,length=10) + ' ' + $
                       strn(exptime,length=6,format='(f6.1)') + ' ' + $
                       strn(airmass,length=4,format='(f4.2)') + ' ' + $
                       strn(meanfwhm,length=5,format='(f5.2)') + ' ' + $
                       strn(avgsky,length=5,format='(i5)') + ' '+ $
                       strn(skysigma,length=4,format='(i4)') + ' '+ $
                       strn(nlist,length=5,format='(i5)') + ' ' + $
                       strn(obscura1,format='(f5.3)') + ' ' + $
                       strn(obscura2,format='(f5.3)') + ' ' + $
                       strn(zp,format='(f8.3)',length=6) + ' ' + $
                       strn(brightlimit+zp,format='(f8.2)') + ' ' + $
                       strn(maglimit+zp,format='(f8.2)')
               repwrite,fnlog,tag,tag+info
            endif

         endif else begin
            print,count,' entries for ',imname,' found in fitcoeff.dat; skipping.'
         endelse

      endif
      
   endfor

end
