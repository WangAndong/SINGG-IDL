;+
; NAME:
;    hstpsf
; PURPOSE: (one line)
;    Find or create a HST PSF file using Tiny Tim.
; DESCRIPTION:
;    See Tiny Tim User's Manual, Version 6.1 Alpha, for details.
; CATEGORY:
;    Miscellaneous
; CALLING SEQUENCE:
;    hstpsf, x, y, date, filter, bmvnum, psf, xmax, ymax
; INPUTS:
;    x, y    : Chip position.
;    date    : Date of observation (YYMMDD format) (not used by ACS/HRC).
;    filter  : Filter name. May be f439w or f555w. Case-insensitive.
;    bmvnum  : TinyTim Color Index (B-V) number (integer) OR a directory
; name (string) in which a user-specified spectrum file will be found.
; If this argument is a B-V number, it may be:
;                BMVNUM    TYPE       B-V
;                1           O5      -0.34
;                2           O8F     -0.32
;                3           O6      -0.31
;                4           B1V     -0.27
;                5           B3V     -0.21
;                6           B6V     -0.12
;                7           A0V     -0.04
;                8           A5V      0.12
;                9           F6V      0.37
;               10           F8V      0.48
;               11           G2V      0.56
;               12           G5V      0.66
;               13           G8V      0.75
;               14           K4V      0.92
;               15           K7V      1.28
;               16           M1.5V    1.45
;               17           M3V      1.44
; If this argument is a directory name, it must be just a name, not a path.
; The directory must exist just below the instrument directory (ACS/HRC,
; presently). The spectrum file in the directory must have the directory
; name as its root and must end in ".dat' . For example, if the bmvnum
; argument is 'example' the example directory (within the HRC directory)
; must cantain a spectrum file named 'example.dat' . The contents of the
; spectrum file must be in TinyTim Version 6.1 ASCII format (See
; Appendix C in the TinyTim manual).
;
; OPTIONAL INPUT PARAMETERS:
;
; KEYWORD INPUT PARAMETERS:
;    CAMERA   = TinyTim Camera Number. Required. May be:
;               2  for Planetary Camera (WFPC) (f/30.0) (see restrictions) or
;               16 for ACS - High Resolution Channel (f/72)
;    CHIP     = May be 5, 6, 7, or 8 (default: 6) for WFPC1. Not used
;               if camera is 16 (ACS/HRC).
;    FLUSH    = If set, flushes the local cache before begining PSF search.
;    GRID     = Grid spacing for nearest PSF location.  Default=50 pixels.
;    HSTPATH  = The starting directory path for the location of the PSF'S.
;               Used, primarily, for maintenance.
;    PSFSIZE  = Size of requested PSF, in arcseconds (default: 5).
;    SAMPFACT = Sample factor (default: 2). Not used with ACS/HRC.
;    VERBOSE  = If set, displays informational messages.
;    Z4       = The value to use for the fourth Zernike coefficient. Default
;               is 0.0 (scalar).
;
; KEYWORD OUTPUT PARAMETERS:
;    PSFROOTNAME - returns the name of the psfroot (with path, no suffix)
;
; OUTPUTS:
;    psf : The generated PSF array.
;    xmax : Interpolated maximum x-location.
;    ymax : Interpolated maximum y-location.
;
; COMMON BLOCKS:
;    hstpsf_com  : Local. Holds a chosen number of PSF's in memory cache
; (see variable ncache).
; SIDE EFFECTS:
;
; RESTRICTIONS:
;    Support for the Planetary Camera, Chip 6 (P6), is for old PSF files in
; disk cache, that were generated with the old version of TinyTim (2.4).
;    Support for the ACS/HRC and TinyTim Version 6.1 Alpha.
;    Support for the F439W and F555W filters.
;    Requires IDL 5.6 or later.
; PROCEDURE:
;    A nearest grid position is computed from the input chip position using the
; specified, or default, grid spacing.
;    If a PSF has been previously computed at that grid position (matching the
; other input parameters), that file is loaded into the caller's PSF array.
;    Otherwise, a new PSF is created, placed into the appropriate
; sub-directory, and loaded into the caller's PSF array.
;
; MODIFICATION HISTORY:
;    Written by Doug Loucks, Lowell Observatory, August, September, 1993.
;    12/1/93, DWL, Modified to work with Tiny Tim Version 2.4.
;    12/9/93, DWL, Modified to hold one copy of a PSF in common.  If the
; requested PSF is the same as that in common, a disk-access is avoided and
; the memory copy is returned.
;    12/16/93, DWL, Added VERBOSE keyword.  If set, displays informational
; messages.
;    1/12/94, DWL, Added cache for two psf's.
;    1/21/94, DWL, Added cache for n psf's. See local variable ncache.
;    1/21/94, DWL, Added code to compuite and store each psf's interpolated
; maximum. Also, added FLUSH keyword.
;    2003/01/15, DWL, Added keywords CAMERA, CHIP, PSFSIZE, and
; SAMPFACT. These allow continued, but minimal support for calls from
; P6MODEL. The new TinyTim (Version 6.1 Alpha) does not generate the same
; output as the old TinyTim, for the old planetary camera. The
; modifications allow for return of a P6 PSF array, if called from P6MODEL
; and the old PSF file is available in disk cache; new P6 PSF's are
; not generated for calls from P6MODEL.
;    Updated to use TinyTim Version 6.1 Alpha.
;    Added support for the new ACS/HRC HST camera. The new
; HRC code will be used if called from the new routine HRCMODEL (CAMERA=16).
;    Modified the local common block to use a single anonymous
; structure whose tags manage the PSF memory cache.
;   2003/07/23, MWB, cosmetic tweaks, changed message to print and tucked
;     a few more things behind the verbose keyword control.
;   2004/02/11, DWL, added keyword Z4, which specifies a value for the
;     fourth Zernike term.
;   2004/02/17, DWL, changed file handling to put temporary files in the
;     the main psf directory rather than in the current directory.
;   2004/02/18, MWB, changed handling of Z4 and removed all chmod calls.
;-

pro hstpsf, x, y, date, filter, bmvnum, psf, xmax, ymax,$
    CAMERA=camera,$
    CHIP=chip,$
    FLUSH=flush,$
    GRID=grid,$
    HSTPATH=hstpath,$
    PSFROOTNAME=outpsfrootname,$
    PSFSIZE=psfsize,$
    SAMPFACT=sampfact,$
    VERBOSE=verbose,$
    Z4=z4

   common hstpsf_com, local_cache

   self = '% HSTPSF: '

   ; In case an error occurs.
   psf = -1

   if n_params() eq 0 then begin
      print,self,'Usage: hstpsf, x, y, date, filter, bmvnum, psf, xmax, ymax'
      return
   endif

   ; Verify proper input parameter types.

   if badpar(x, [2,3,4,5], 0, caller=self+'(x) ') then return
   if badpar(y, [2,3,4,5], 0, caller=self+'(y) ') then return
   if badpar(date, 7, 0, caller=self+'(date) ' ) then return
   if badpar(filter, 7, 0, caller=self+'(filter) ') then return
   if badpar(bmvnum, [2,7], 0, caller=self+'(bmvnum) ') then return

   ; Input keywords that need to be checked.
   if badpar(camera, 2, 0, caller=self+'(camera) ') then return
   if badpar(chip, [0,2], 0, caller=self+'(chip) ', default=6) then return
   if badpar(flush, [0,2], 0, caller=self+'(flush) ', default=0) then return
   if badpar(grid, [0,2], 0, caller=self+'(grid) ', default=50) then return

   if badpar(hstpath, [0,7], 0, caller=self+'(hstpath) ',$
             default='/gryll/data2/hstpsf/') then return

   if badpar(psfsize, [0,2,3,4,5], 0, caller=self+'(psfsize) ',$
             default=5) then return

   if badpar(sampfact, [0,2], 0, caller=self+'(sampfact) ',$
             default=2) then return

   if badpar(verbose, [0,2], 0, caller=self+'(verbose) ', default=0) then return
   if badpar(z4, [0,4], 0, caller=self+'(Z4) ', default=0.0) then return

   if abs(z4) gt 1.0 then begin
      print, self + 'Z4 must be between -1 and 1'
      return
   endif



   ;******************* Variables which might be edited *********************
   ; Number of local memory slots for cached psf's.
   ncache = 3L
   ;*************************************************************************



   ; Initialize the memory-cache structure, if necessary.

   ; Note that the structure tag for the PSF arrays is set, initially, to an
   ; array of valid pointer variables (which point to undefined variables).
   ; This enhances memory efficiency and eliminates the need to allocate
   ; multi-dimensional PSF arrays with specific dimensions.

   ; The array of pointers remain valid during an IDL session; they are
   ; re-used as needed.

   ; When a PSF array is cached, the next available pointer is de-referenced
   ; and points to the new PSF array. When the PSF arrays are flushed, the
   ; pointers are dereferenced and made to point to long-word scalars. This
   ; releases the array memory associated with the pointers.

   if n_elements(local_cache) eq 0 then begin
      local_cache = {save_indx:0L, save_name:strarr(ncache),$
      save_psf:ptrarr(ncache, /allocate_heap),$
      save_max:fltarr(2,ncache)}
   endif


   ; Check if PSF cache should be flushed.

   if flush then begin
      ; Clear the PSF-cache index variable and name variable.
      local_cache.save_indx = 0
      local_cache.save_name[*] = ''

      ; Release any PSF array memory and set to scalar long words.
      for j=0L, ncache-1 do begin
         *local_cache.save_psf[j] = 0
      endfor
   endif 

   ; First conditional block for camera number.
   if camera eq 2 then begin
      instdir = 'WFPC/'
      cameradir = 'PC6/'
   endif else if camera eq 16 then begin
      instdir = 'ACS/'
      cameradir = 'HRC/'
   endif else begin
      print,self,'Camera Number must be 2 or 16.'
      return
   endelse


   ; Compute the nearest grid position.
   gx = nint(x / grid) * grid
   gy = nint(y / grid) * grid

   ; Need string versions of the nearest chip position to form the PSF filename.
   sx = strtrim(string(gx), 2)
   sy = strtrim(string(gy), 2)


   if verbose then begin
      ; Display the input and the grid positions.
      print,self, 'Input position: ' + string(x, y, format='(2F12.4)')
      print,self, 'Grid  position: ' + string(gx, gy, format='(2I12)')
      print,self, 'Grid spacing: ',grid,format='(a,a,i5)'
   endif


   ; Test the HSTPATH directory.
   result = file_search(hstpath, /mark_directory, /test_directory,$
            /fully_qualify_path)

   if result[0] eq '' then begin
      print,'Error accessing HSTPATH directory ' + hstpath
      return
   endif

   mainpath = result[0]

   ; Force upper case filter name.
   uc_filter = strupcase(filter)

   ; Get type code for the incoming bmvnum argument.
   bmvtype = size(bmvnum, /type)

   ; Need (perhaps) string version of incoming bmvnum argument for sub-directory
   ; name.
   if bmvtype eq 2 then begin
      ; A B-V (integer) table value was passed.
      if bmvnum lt 1 then begin
         print,self, 'Error: bmvnum must be between 1 and 17 inclusive.'
         return
      endif

      if bmvnum gt 17 then begin
         print,self, 'Error: bmvnum must be between 1 and 17 inclusive.'
         return
      endif

      bmvstr = strtrim(string(bmvnum), 2)
   endif else if bmvtype eq 7 then begin
      ; A directory name (string) was passed. This directory is expected to
      ; appear directly below the ACS/HRC directory and is expected to
      ; include a file (see variable spectrum_file) that contains a
      ; user-specified spectrum in TinyTim format (see Appendix C of the
      ; TinyTim Version manual).
      bmvstr = strcompress(bmvnum, /remove_all)

      ; The spectrum file name is the directory name with '.dat' appended.
      spectrum_file = bmvstr + '.dat'
      spectrum_path = mainpath + instdir + cameradir + bmvstr

      ; Test the spectrum path.
      result = file_search(spectrum_path, /mark_directory, /test_directory,$
               /test_read, /fully_qualify_path)

      if result[0] eq '' then begin
         print,self, 'Error accessing spectrum directory ' + spectrum_path
         return
      endif else begin
         ; Append spectrum file name to path.
         spectrum_path = result[0] + spectrum_file

         ; Verify existence of the spectrum file verify that it is readable.
         if not file_test(spectrum_path, /read) then begin
            print,self, 'Error accessing spectrum file ' + spectrum_path
            return
         endif
      endelse
   endif else begin
   endelse

   ; Form the other intermediate directory name strings.
   datedir   = date + '/'
   filterdir = uc_filter + '/'
   bmvdir    = bmvstr + '/'

   ; First conditional block for camera code.
   ; Form the PSF file name and the full path to the PSF file name.
   if camera eq 2 then begin
      path = mainpath + instdir + cameradir + datedir + filterdir + bmvdir
      psfrootname = path + sx + '_' + sy
      psfname = psfrootname + '.fit'
      psmname = psfrootname + '.max'
   endif else if camera eq 16 then begin
      path = mainpath + instdir + cameradir + bmvdir + filterdir

      if abs(z4) ge 0.0005 then begin
         ; Need a sub-directory for PSF's that will be generated with a
         ; non-zero fourth Zernike term.
         z4dir = 'Z4' + ((z4 < 0.0) ? string(fix(z4*1000.0-0.5),$
                 format='(i4.3)') : string('+', fix(z4*1000.0+0.5),$
                 format='(a,i3.3)'))+'/'

         path = path + z4dir
      endif else begin
         z4dir = ''
      endelse

      psfrootname = path + sx + '_' + sy + '_'
      psfname = psfrootname + '00_psf.fits'
      psmname = psfrootname + '00_psf.max'
   endif else begin
   endelse

   outpsfrootname=psfrootname

   ; Perhaps the requested PSF is in memory cache.
   w = where(local_cache.save_name eq psfname, count)

   if count eq 1 then begin
      ; The requested psf is in memory cache.

      if verbose then print,self, $
      'Loading ' + local_cache.save_name[w[0]] + ' from memory cache.'

      psf = *local_cache.save_psf[w[0]]
      xmax = local_cache.save_max[0, w[0]]
      ymax = local_cache.save_max[1, w[0]]
      return
   endif


   ; The requested PSF was not in memory cache. Perhaps it is in disk cache.
   if file_test(psfname, /read) then begin
      ; The requested psf is in disk cache.

      ; Save the PSF path and name into memory cache.
      local_cache.save_name[local_cache.save_indx] = psfname

      if verbose then begin
         print,self,$
         'Loading ' + local_cache.save_name[local_cache.save_indx] +$
         ' from disk cache and placing into memory cache.'
      endif

      ; Read PSF into memory cache.
      *local_cache.save_psf[local_cache.save_indx] =$
      readfits(psfname, /silent)

      ; Put a copy of PSF into the PSF output argument.
      psf = *local_cache.save_psf[local_cache.save_indx]

      ; Retrieve the dimensions of the PSF array.
      psfdim = size(psf, /dimensions)

      if file_test(psmname, /read) then begin
         ; The interpolated maximum save file can be read.
         xmax = 0.0
         ymax = 0.0
         openr, lu, psmname, /get_lun
         readf, lu, xmax, ymax
         free_lun, lu
      endif else begin
         ; Need to compute the interpolated maximum.

         if verbose then print,self,'Computing interpolated maximum...'

         boxm, psf, psfdim[0]/2, psfdim[1]/2, 10, 10, xm, ym
         findmax, xm, ym, psf, xmax, ymax, fm
         openw, lu, psmname, /get_lun
         printf, lu, xmax, ymax
         free_lun, lu
      endelse

      ; Save the PSF maximum location into memory cache.
      local_cache.save_max[*, local_cache.save_indx] = [xmax, ymax]

      ; Bump the memory-cache index.
      local_cache.save_indx = (local_cache.save_indx + 1) mod ncache
      return
   endif


   ; If this point is reached and the camera is 2 (old Planetary Camera), the
   ; requested old PSF was not on disk. In this case, an error condition
   ; is reported and control returns to the caller.

   if camera eq 2 then begin
      print,self,$
      'Error: Requested old (WFPC/PC6) PSF was not in disk cache: '+$
      psfname
      return
   endif

   ; Create the sub-directories, if they don't exist.
   file_mkdir, path

;   ; Set the execute and write permissions for the sub-directories.
;   file_chmod, [mainpath+instdir,$
;      mainpath+instdir+cameradir,$
;      mainpath+instdir+cameradir+bmvdir,$
;      mainpath+instdir+cameradir+bmvdir+filterdir],$
;      /a_execute, /a_write

;   if z4dir ne '' then begin
;      ; Set permissions on the Z4 sub-directory.
;      file_chmod,mainpath+instdir+cameradir+bmvdir+filterdir+z4dir,$
;      /a_execute, /a_write 
;   endif


   ; ACS/HRC.
   ;---------------------------------------------------------------------------
   if camera eq 16 then begin
      ; Form a string array of the tiny1 input responses. It will
      ; be shipped to tiny1 via a spawned bi-directional pipe.

      if bmvtype eq 2 then begin
         spectrum_choice = '1'
         spectrum_item = bmvstr
      endif else if bmvtype eq 7 then begin
         spectrum_choice = '5'
         spectrum_item = spectrum_path
      endif else begin
      endelse

      tiny1input = strtrim([string(camera), sx+' '+sy,$
                   uc_filter, spectrum_choice, spectrum_item,$
                   string(psfsize), psfrootname], 2)

      if verbose then print,self, 'Running tiny1...'

      ; Spawn tiny1 via a bi-directional pipe, and submit the tiny1 responses.
      ; This process runs asynchronously.
      spawn, 'tiny1 '+psfrootname+'.tmp', unit=lun
      printf, lun, tiny1input
      flush, lun

      ; Need to wait for tiny1 to complete, or to exceed approximately ten
      ; seconds of time.
      for j=0, 19 do begin
         if file_test(psfrootname+'.tmp', /noexpand_path) then break
         wait, 0.5
      endfor

      ; Finished with the bi-directional pipe.
      free_lun, lun

      ; Test for tiny1 success, or for failure due to a timeout.
      if not file_test(psfrootname+'.tmp', /noexpand_path) then begin
         print,'file ',psfrootname+'.tmp    z4dir=[',z4dir,']'
         print,'z4 = ',z4,format='(a,1x,f15.8)'
         print,self, 'Error: tiny1 failed.'
         return
      endif


      if z4dir ne '' then begin
         openr, rlun, psfrootname+'.tmp', /get_lun
         openw, wlun, psfrootname+'.par', /get_lun

         iline = ''

         while not eof(rlun) do begin
            readf, rlun, iline
            oline = iline
            sline = strsplit(iline, '#', /extract)

            if n_elements(sline) eq 2 then begin
               if strcmp(' Z4 =', sline[1], 5) then begin
                  oline = string(z4, format='(f7.3)') + '    #' + sline[1]
               endif
            endif

            printf, wlun, oline
         endwhile

         free_lun, rlun, wlun
         file_delete, psfrootname+'.tmp'
      endif else begin
         file_move, psfrootname+'.tmp', psfrootname+'.par'
      endelse


      ; Spawn tiny2 and wait for completion.
      ; This process runs synchronously.
      if verbose then print,self, 'Running tiny2. This may take awhile...'
      spawn, 'tiny2 ' + psfrootname + '.par'

      ; Test for tiny2 success or failure.
      if not file_test(psfname) then begin
         print,self, 'Error: tiny2 failed. PSF not created.'
         return
      endif

      ;  Form an array of the file names created by Tinytim.
      fnames = [psfname, psfrootname+'.par', psfrootname+'.tt3']

;      ; Set permission codes of the files.
;      file_chmod, fnames, /a_write

      ; Save the full path to the PSF in memory cache.
      local_cache.save_name[local_cache.save_indx] = psfname
   endif
   ;---------------------------------------------------------------------------



   ; Load the requested PSF.

   if verbose then begin
      print,self, 'Loading newly-generated PSF ' +$
      local_cache.save_name[local_cache.save_indx] +$
      ' from disk cache and placing into memory cache.'
   endif

   psf = readfits(psfname, /silent)

   psfdim = size(psf, /dimensions)
   *local_cache.save_psf[local_cache.save_indx] = psf

   ; Compute and save the interpolated maximum.
   boxm, psf, psfdim[0]/2, psfdim[1]/2, 10, 10, xm, ym
   findmax, xm, ym, psf, xmax, ymax, fm
   openw, lu, psmname, /get_lun
   printf, lu, xmax, ymax
   free_lun, lu

;   ; Set permissions of the PSF computed-maximum file.
;   file_chmod, psmname, /a_read, /a_write

   ; Save the computed maximum into memory cache.
   local_cache.save_max[*, local_cache.save_indx] = [xmax, ymax]

   ; Bump the memory-cache index.
   local_cache.save_indx = (local_cache.save_indx + 1) mod ncache
end
