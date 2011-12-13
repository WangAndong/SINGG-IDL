pro psend,psfile,noprint=noprint,clobber=clobber
;
if !d.name ne 'SUN' then begin
  device,/close
  if n_elements(psfile) gt 0 then begin
    if keyword_set(clobber) then spawn,'mv -f idl.ps '+psfile $
                            else spawn,'mv idl.ps '+psfile
    if not keyword_set(noprint) then spawn,'lpr '+psfile
  endif else if not keyword_set(noprint) then spawn,'lpr idl.ps'
  device,/portrait
  set_plot,'X'
  setbgfg,!white,!black
endif
;
end
