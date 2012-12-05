pro plog, lu, pfx, line
  ;
  ; Print a line to the screen log and the logfile (if opened).
  ;
  ;  lu   -> logical unit of the log file
  ;          if lu < 0 then the line is only written to the screen
  ;  pfx  -> prefix for log line
  ;          usually this will be the name of the program where the
  ;          message comes from
  ;  line -> line to be written to the log file
  ;
  ;  the actual line that is written is the string pfx+line
  ;
  ; G. Meurer 6/2010 ICRAR/UWA
  printf, -1, pfx+line
  if lu ge 0 then printf, lu, pfx+line
end 
