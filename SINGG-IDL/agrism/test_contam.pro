PRO test_contam
   filspc   = 'hdfn_g800l_2.SPC.fits'
   luns     = fxposit(filspc,1)
   status   = 0
   i        = 0
   mincont  = make_array(1377,value=-1)
   maxcont  = make_array(1377,value=-1)
   REPEAT BEGIN
      tab = mrdfits(luns, 0, status=status)
      IF status EQ 0 THEN BEGIN
         cont       = tab.contam
         mincont[i] = min(cont)
         maxcont[i] = max(cont)
         print,i,mincont[i],maxcont[i]
         i = i + 1
      ENDIF 
   ENDREP UNTIL status NE 0
   print,min(mincont),max(maxcont)
END
