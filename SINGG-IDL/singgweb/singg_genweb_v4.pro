PRO singg_genweb_v4
   sdir      = '/data1/acs7/meurer/SINGG/Sample/V04b_dec04/WWW/'
   samproot  = 'sample4_ravsort'
   nhd_samp  = 2
   nhd_ext   = 4
   colobs    = 90
   nmax      = 45
   obslis    = 'observed.lis'
   obsfil    = 'observed.dat'
   shistpl   = 'masshist.png'
   shisttab  = 'masshist.dat'
   obspage   = 'observed.html'
   samptab   = 'index.html'
   title     = 'SINGG: Sample V04b (Dec 2004)'
   subtitle  = ''
   imgdir    = '/data1/acs7/meurer/SINGG/Sample/V04b_dec04/CADC/'
   jpgflag   = 1b
   version   = 4
   ;
   ; get the date in a nice format to append to files
   caldat, systime(/julian), month, day, year
   dstr      = ddmmyy(day, month, year)
   ;
   ; generate file name 
   sampfil   = samproot + '.dat'
   sampnew   = samproot + '.new'
   sampbck   = samproot + '_pre' + dstr + '.dat'
   updatefil = samproot + '_' + dstr + '.dat'
   ;
   ; go to sample directory
   cd,sdir,current=cwd
   ;
   ; save backup copy of sample
   cmd = 'cp '+sampfil+' '+sampbck
   spawn,cmd
   cmd = 'cp '+sampfil+' '+sampnew
   spawn,cmd
   ;
   ; update observed galaxies.
   ;
   print,' Running singg_make_observed '
   singg_make_observed,sampfil,nhd_samp,colobs,obslis,obsfil,updatefil
   print,' Running singg_observed_table '
   singg_observed_table,obsfil,samptab,obspage
   ;
   ; copy updated file to standard name
   cmd = '/bin/cp -f '+updatefil+' '+sampfil
   spawn,cmd
   ;
   ; re-create singg_sample database
   print,' making IDL database '
   mksinggsampdb, usedir=sdir
   ;
   ; tar database files
   cmd = 'tar cvf singg_sample_db.tar singg_sample.db?'
   spawn, cmd
   ;
   ; make mass histograms
   ;
   print,' making mass histograms '
   setplotcolors
   setbgfg,!white,!black

   singg_masshist, updatefil, 0.2, 6.0, 11.5, 0, nmax, pfile=shisttab, version=version
   makepng,shistpl,/color
   ;
   ; Update web pages
   ;
   print,' Running singg_genweb '
   singg_genweb,sampfil,obspage,shistpl,shisttab,title,subtitle,imgdir=imgdir,jpgflag=jpgflag
   ;
   ; go back to where you came from
   cd, cwd
END 



