PRO seterrmod, rn, gain, drate, exptime, nreads, skylev
   COMMON errmod, varpix
   ;
   ; Estimates the mean variance per pixel, stores in a common block.
   varrn    = nreads*rn*rn
   vardark  = drate*exptime
   varsky   = skylev*gain
   varpix   = (varsky + varrn + vardark)/gain
END 
