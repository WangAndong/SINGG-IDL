pro create_rpc_mask, fili, frpc, filo
  ;
  ; This program is similar to create_ivy_mask but for the 
  ; case of one save set.
  ;
  ; fili -> template fits file used just as a header template
  ; frpv -> rpc mask saveset
  ; filo -> output file name
  ;
  ; G. Meurer 04/2011
  goodval   = 0b
  badval    = 1b
  ;
  ; read in input image
  fits_read, fili, imi, hdi
  ;
  ; make output image array, set to goodval
  mask      = byte(imi)*0b + goodval
  ;
  ; restore save set
  restore, frpc
  ;
  ; add to mask
  if min(indexstar) gt -1l  then mask[indexstar] = badval
  if min(indexgal) gt -1l   then mask[indexgal]  = badval
  ;
   fits_write, filo, mask, hdi
end 
