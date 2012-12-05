PRO mk_sstdb, roc
;
; Make database of SST reserved objects
;
   fili    = 'roc_ed.dat'                   ; input file
   fmt     = '(a,a,a,a,a,a,a,l)'            ; input format
   field   = ['name', 'ra', 'dec', 'aot', 'arrayid', 'tfov', 'pi', 'pid'] ; fields for structure, db
   dstring = ['a', 'd', 'd', 'a', 'a', 'a', 'a', 'i'] ; datatypes for structure
   filo    = 'roc'                          ; output file name prefix
   ;
   readcol, fili, name, ra0, dec0, aot, arrayid, tfov, pi, pid, comment='#', format=fmt
   dimen   = n_elements(name)
   ra      = 15.0d0*sexideg(ra0, delim=':')
   dec     = sexideg(dec0, delim=':')
   ;
   print,'entries = ', n_elements(name)
   print,min(ra),max(ra),min(dec),max(dec)
   print,'Max strlen name = ', max(strlen(name))
   print,'Max strlen aot = ', max(strlen(aot))
   print,'Max strlen arrayid = ', max(strlen(arrayid))
   print,'Max strlen tfov = ', max(strlen(tfov))
   print,'Max strlen pi = ', max(strlen(pi))
   ;
   ; make structure
   create_struct2, roc, 'SST_ROC', field, dstring, dimen=dimen
   ;
   ; populate structure
   roc.name    = name
   roc.ra      = ra
   roc.dec     = dec
   roc.aot     = aot
   roc.arrayid = arrayid
   roc.tfov    = tfov
   roc.pi      = pi
   roc.pid     = pid
   ;
   ; create database
   !PRIV       = 2
   dbcreate, filo, 1, 1, /external
   dbopen, filo, 1
   dbbuildstruct, roc
   dbclose,dummy
END
