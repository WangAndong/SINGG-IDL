PRO young
; Calculates the H2:HI mass ratio from the sample given.

  filename = !singgdir+"young.txt"

  readcol_new,filename,logmh2,logmhi,COMMENT="#"

  val=sig_pm(logmh2-logmhi)

  PRINT,MEAN(logmh2),MEAN(logmhi),MEAN(logmh2-logmhi),val

END
