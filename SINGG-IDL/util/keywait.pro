pro keywait,message,dum
  ;
  ; Prompt and wait for any key to be pressed.
  ;
  ; J. McCann (JHU) late 1990s
  ; G. Meurer (UWA/ICRAR) 10/2011 
  ;    + set on_error, 2 so that if you <ctrl>-c you get back 
  ;      to calling program
  ;    + also returns the key that was struck should the user want it
  On_error,2                            ;Return to caller
  if n_params(0) lt 1 then message='Press any key to continue...'
  if message ne '' then print,message
  dum=get_kbrd(1)
end


