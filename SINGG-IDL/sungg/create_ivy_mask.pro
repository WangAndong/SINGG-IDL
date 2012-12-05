pro create_ivy_mask, frestmask, hdi, mask, fmasko=fmasko
  ;
  ; conjure up a mask from one of Ivy Wong's IDL savesets
  ; 
  ; frestmask -> IDL saveset file(s) made by Ivy when creating masks.
  ; hdi       -> header of the corresponding image.
  ; mask      <- output mask image.
  ; fmasko    -> if set, the fits mask image to write
  ;
  ; If frestmask is a string array then the output mask is the 
  ; logical AND of all the input masks.
  ;
  ; G. Meurer  5/2010
  ; G. Meurer  6/2010 - add logic to combine masks when frestmask is 
  ;                     a string array.
  ;
  ; get image size from header
  nx        = sxpar(hdi, 'naxis1')
  ny        = sxpar(hdi, 'naxis2')
  ;
  ; blank output mask
  mask      = make_array(nx, ny, /int, value=0)
  ;
  ; determine number of inputs
  nm        = n_elements(frestmask)
  ;
  ; loop through save sets
  for ii = 0, nm-1 do begin
     ;
     ; restore save set
     restore, frestmask[ii]
     ;
     ; add to mask
     if min(indexstar) gt -1l  then mask[indexstar] = mask[indexstar] + 1
     if min(indexgal) gt -1l   then mask[indexgal]  = mask[indexgal] + 1
  endfor 
  ;
  ; final mask is where pixels are masked in all input masks
  if nm eq 0 then begin 
     mask = byte(mask) 
  endif else begin 
     pp   = where(mask eq nm, npp)
     mask = 0b*byte(mask)
     if npp gt 0 then mask[pp] = 1b
  endelse 
  ;
  ; **** probably should do something to transform input header to
  ; output round about here.
  ;
  ; write mask file if requested
  if keyword_set(fmasko) then fits_write, fmasko, mask, hdi
end 
