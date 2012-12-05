pro assemble_data, fili, outdir
  ;
  ; assemble the data for running a singg/sungg pipeline
  ;
  ; fili   -> input file should have the following columns
  ;           1 HIPASS target name
  ;           2 UV directory
  ;           3 UV mask directory
  ;           4 Optical directory
  ;           5 FUV file name
  ;           6 NUV file name
  ;           7 R (continuum) image name
  ;           8 Halpha image name
  ;           9 R mask file name
  ;           10 Halpha mask file name
  ;           11 UV mask file name
  ; outdir -> root output directory
  ;
  ; G. Meurer 6/2010
  fmti      = '(a,a,a,a,a,a,a,a,a,a,a)'
  prog      = 'ASSEMBLE_DATA: '
  flog      = 'assemble_data.log'
  ;
  ; open log file
  openw, ll, flog, /get_lun
  ;
  ; read input file
  printf, -1, prog+'reading input file: '+fili
  printf, ll, prog+'reading input file: '+fili
  readcol, fili, hname, udir, odir, mdir, filf, filn, filr, filh, fmskr, fmskh, fmskuv, format=fmti
  nobj       = n_elements(hname)
  printf, -1, prog+'will process '+strtrim(string(nobj),2)+' fields (i.e. unique HIPASS targets)'
  printf, ll, prog+'will process '+strtrim(string(nobj),2)+' fields (i.e. unique HIPASS targets)'
  ;
  ; create base outdir if needed
  printf, -1, prog+'checking status of output directory: '+outdir
  printf, ll, prog+'checking status of output directory: '+outdir
  inf        = file_info(outdir)
  if inf.exists and (not inf.directory) then begin 
     printf, -1, prog+'Output directory exists but is not a directory, returning...'
     printf, ll, prog+'Output directory exists but is not a directory, returning...'
     return
  endif 
  if not inf.exists then begin
     printf, -1, prog+'Output directory does not exist, creating : '+outdir
     printf, ll, prog+'Output directory does not exist, creating : '+outdir
     file_mkdir, outdir
  endif else begin
     printf, -1, prog+'directory exists: '+outdir
     printf, ll, prog+'directory exists: '+outdir
  endelse 
  ;
  ; go to base directory
  printf, -1, prog+'Going to directory '+outdir
  printf, ll, prog+'Going to directory '+outdir
  cd, outdir, current=cwd
  ;
  ; loop through objects
  for ii = 0, nobj-1 do begin 
     ;
     ; zap old directory if it exists
     inf      = file_info(hname[ii])
     if inf.exists then begin
        printf, -1, prog+'removing previous version of directory '+hname[ii]
        printf, ll, prog+'removing previous version of directory '+hname[ii]
        file_delete, hname[ii], /allow_nonexistent, /recursive, /verbose
     endif 
     ;
     ; create directory for run, and enter it
     printf, -1, prog+'creating directory: '+hname[ii]
     printf, ll, prog+'creating directory: '+hname[ii]
     file_mkdir, hname[ii]
     printf, -1, prog+'entering directory: '+hname[ii]
     printf, ll, prog+'entering directory: '+hname[ii]
     cd, hname[ii], current=based
     ;
     ; create symbolic link to main fits images
     printf, -1, prog+'making symbolic links'
     printf, ll, prog+'making symbolic links'
     file_link, udir[ii]+filf[ii], '.'
     file_link, udir[ii]+filn[ii], '.'
     file_link, odir[ii]+filr[ii], '.'
     file_link, odir[ii]+filh[ii], '.'
     ;
     ; create symbolic links to mask files
     if fmskr[ii] ne 'NULL' then file_link, odir[ii]+fmskr[ii], '.'
     if fmskh[ii] ne 'NULL' then file_link, odir[ii]+fmskh[ii], '.'
     file_link, mdir[ii]+fmskuv[ii], '.'
     ;
     cd, based
  endfor 
  cd, cwd
end 
