FUNCTION singg_ddb_fname, entry, option, hpath=hpath, nopath=nopath
   ;
   ; Given an entry number in the singg_derived database, this
   ; function returns the name of a file or files created for QA 
   ; analysis.
   ;
   ; entry   -> entry number in database
   ; option  -> option discribing which file name to return
   ;             0. 'skyprof_i' : sky brightness profile, Hanish name
   ;             1. 'cog_i'     : curve of growth, Hanish name
   ;             2. 'sbprof_i'  : surface brightness prof., Hanish name
   ;             3. 'haps_i'    : Hanish RGB img with aps, Hanish name
   ;             4. 'rgood_i'   : R good pixel RGB image, Hanish name
   ;             5. 'rbad_i'    : R bad  pixel RGB image, Hanish name
   ;             6. 'netgood_i' : Net good pixel RGB image, Hanish name
   ;             7. 'netbad_i'  : Net bad  pixel RGB image, Hanish name
   ;             8. 'skyprof_o' : sky brightness profile, Hanish name
   ;             9. 'cog_o'     : curve of growth, Meurer name
   ;            10. 'sbprof_o'  : surface brightness prof., Meurer name
   ;            11. 'haps_o'    : Hanish RGB img with aps, Meurer name
   ;            12. 'rgood_o'   : R good pixel RGB image, Meurer name
   ;            13. 'rbad_o'    : R bad  pixel RGB image, Meurer name
   ;            14. 'netgood_o' : Net good pixel RGB image, Meurer name
   ;            15. 'netbad_o'  : Net bad  pixel RGB image, Meurer name
   ;            16. 'skyprof_s' : sky brightness profile, stamp name
   ;            17. 'cog_s'     : curve of growth, stamp name
   ;            18. 'sbprof_s'  : surface brightness prof., stamp name
   ;            19. 'haps_s'    : Hanish RGB img with aps, stamp name
   ;            20. 'rgood_s'   : R good pixel RGB image, stamp name
   ;            21. 'rbad_s'    : R bad  pixel RGB image, stamp name
   ;            22. 'netgood_s' : Net good pixel RGB image, stamp name
   ;            23. 'netbad_s'  : Net bad  pixel RGB image, stamp name
   ;            24. 'rssfits'   : R band sky subtracted fits file
   ;            25. 'nbssfits'  : narrow band sky subtracted fits file
   ;            26. 'netssfits' : Net Halpha sky subtracted fits file
   ;            27. 'rplfits'   : R band pl mask file
   ;            28. 'nbplfits'  : narrow band pl mask file 
   ;            29. 'netplfits' : Net Halpha pl mask file 
   ;            30. 'rplfitsgz'   : R band pl mask file (gzipped)
   ;            31. 'nbplfitsgz'  : narrow band pl mask file (gzipped)
   ;            32. 'netplfitsgz' : Net Halpha pl mask file (gzipped)
   ;            33. 'root_png'  : Root name for png files
   ;            34. 'rsecat'    : R band SE catalog
   ;            35. 'nbsecat'   : Narrow band SE catalog
   ;            36. 'netsetcat' : Net Halpha SE catalog
   ;            37. 'calibcat'  : merged & calibrated SE catalog
   ;            38. 'calibplt'  : plot of merged & calibrated SE catalog
   ;            39. 'hispecplt' : HI spectrum jpg plot
   ;            40. 'apsjpg'    : JPG image showing apertures
   ;            41-43. 'sqrt?png' : PNG image square root stretch,
   ;            44-46. 'lin?png'  : PNG image linear stretch
   ;            47-49. 'log?png'  : PNG image log stretch
   ;                              for sqrt?png, lin?png and log?png, "?"
   ;                              stands for 1,3, or 8
   ;            50. 'apmatch_i' : output from apmatch.pro + 
   ;                              apmatch_txtout.pro in original location
   ;            51. 'dbcheck_i' : output from qa_hipassmatch.pro + 
   ;                              dbcheck_txt.pro in original location
   ;            52. 'apmatch_o' : output from apmatch.pro + 
   ;                              apmatch_txtout.pro in SR2QA data dir
   ;            53. 'dbcheck_o' : output from qa_hipassmatch.pro + 
   ;                              dbcheck_txt.pro in SR2QA data dir
   ;            --  'all'       : returns an array indexed as above with
   ;                              all names
   ;            --  'options'   : returns an array of the options listed
   ;                              above, as indexed above.
   ; hpath   -> If set, the start path to Dan Hanish's QA plots.  This
   ;            should look something like '/data1/acs22/hanish/'
   ;            which will be used as the default
   ; nopath  -> if set the file name(s) will be returned without the
   ;            preceding path.  The default is to return file names
   ;            including the full path.
   ;
   ; The singg_derived database needs to be opened for this 
   ; procedure to work
   ;
   ; G. Meurer 07/2007
   ;
   hp      = '/data1/acs22/hanish/'
   optref  = ['skyprof_i', 'cog_i', 'sbprof_i', 'haps_i', 'rgood_i', 'rbad_i', 'netgood_i', 'netbad_i', $
              'skyprof_o', 'cog_o', 'sbprof_o', 'haps_o', 'rgood_o', 'rbad_o', 'netgood_o', 'netbad_o', $
              'skyprof_s', 'cog_s', 'sbprof_s', 'haps_s', 'rgood_s', 'rbad_s', 'netgood_s', 'netbad_s', $
              'rssfits', 'nbssfits', 'netssfits', 'rplfits', 'nbplfits', 'netplfits', 'rplfitsgz', $
              'nbplfitsgz', 'netplfitsgz', 'rootpng', 'rsecat', 'nbsecat', 'netsecat', 'calibcat', $
              'calibplt', 'hispecplt', 'apsjpg', 'sqrt1png', 'sqrt3png', 'sqrt8png', 'lin1png', $ 
              'lin3png', 'lin8png', 'log1png', 'log3png', 'log8png', 'apmatch_i', 'dbcheck_i', $
              'apmatch_o', 'dbcheck_o']
   nf      = n_elements(optref)
   ;
   ; work in lower case
   opt     = strlowcase(strtrim(option,2))
   ;
   ; If option is set to 'options' then just exit returning optref
   IF opt EQ 'options' THEN return, optref
   ;
   ; set various paths
   IF keyword_set(hpath) THEN pathi1 = hpath ELSE pathi1 = hp
   patho1  = '/data2/acs34/meurer/SINGG/SR2QA/Plots/'
   patho2  = '/data2/acs34/meurer/SINGG/SR2QA/Images/'
   patho3  = '/data2/acs34/meurer/SINGG/'
   patho4  = '/data2/acs34/meurer/SINGG/SR2QA/Data/'
   ;
   ; Get quantities needed from database
   dbext, entry, 'name,runid,object,filter_r,filter_n', name, runid, hname, filtr, filtn
   name    = strtrim(name[0],2)
   runid   = strtrim(runid[0],2)
   rstr    = strmid(runid,3)
   hname   = strtrim(hname[0],2)
   filtr   = strtrim(filtr,2)
   filtn   = strtrim(filtn,2)
   ;
   ; set str denoting cont filter type
   frstr    = 'R'
   IF filtr EQ '6850/95' THEN frstr = 'C'
   ;
   ; set string giving short NB filter name, if '6693' kludge to '6696'
   fnstr = strmid(filtn,0,4)
   IF fnstr EQ '6693' THEN fnstr = '6696'
   ;
   ; check if there are observations with other filters
   ; if so special file naming will be needed: sets mflag
   mflag   = 0b
   dbext, -1, 'entry,runid,object', ent, rid, obj
   obj     = strtrim(obj,2)
   rid     = strtrim(rid,2)
   kk      = where(obj EQ hname AND rid EQ runid, nkk)
   IF nkk GT 1 THEN BEGIN 
      dbext, ent[kk], 'filter_r,filter_n', fr, fn
      j1   = sort(fr)
      k1   = uniq(fr[j1])
      j2   = sort(fn)
      k2   = uniq(fn[j2])
      nn   = max([n_elements(k1), n_elements(k2)])
      IF nn GT 1 THEN mflag = 1b
   ENDIF 
   ;
   ; Name kludging 1. : cases which can be solved
   ; by setting mflag
   IF hname EQ 'J1444+01' OR (hname EQ 'J1339-31' AND runid EQ 'Run13') THEN mflag = 1b
   ;
   ; set root file name by whether mflag is set
   IF NOT mflag THEN BEGIN 
      rooti  = hname
      rooti1 = hname
      rooti2 = hname
      rooto  = hname+'_r'+rstr
   ENDIF ELSE BEGIN 
      rooti  = hname+'_'+fnstr
      rooti1 = hname
      rooti2 = hname+'_'+fnstr
      rooto  = hname+'_'+fnstr+frstr+'_r'+rstr
      join   = ''
   ENDELSE 
   ;
   ; set input path
   pathi   = pathi1+runid+'/Proc4/'+hname+'/'
   pathi2  = pathi1+runid+'/Proc3/'+hname+'/'
   pathi3  = patho3+runid+'/Proc4/'
   ;
   ; name kludging 2. : cases best solved by changing rooti & pathi and/or rooto
   ;IF hname EQ 'J1303-17c' THEN BEGIN 
   ;   rooti  = 'J1303-17b'
   ;   pathi  = pathi1+runid+'/Proc4/J1303-17b/'
   ;ENDIF 
   IF hname EQ 'J2215-45b' THEN BEGIN 
      rooti  = 'J2215-45a'
      rooti1 = 'J2215-45a'
      rooti2 = 'J2215-45a'
      pathi  = pathi1+runid+'/Proc4/J2215-45a/'
      pathi2 = pathi1+runid+'/Proc3/J2215-45a/'
   ENDIF
   IF (name EQ 'J1339-31A' OR name EQ 'J1339-31:S1' OR name EQ 'J1339-31:S2' OR name EQ 'J1339-31') THEN BEGIN 
      ;
      ; we save our largest contortions for J1339-31 & J1339-31A
      IF runid EQ 'Run03' THEN BEGIN 
         rooti  = 'J1339-31b'
         rooti1 = 'J1339-31b'
         rooti2 = 'J1339-31b'
         pathi  = pathi1+runid+'/Proc4/J1339-31b/'
         pathi2 = pathi1+runid+'/Proc3/J1339-31b/'
      ENDIF ELSE BEGIN 
         rooti  = 'J1339-31_'+fnstr
         rooti1 = 'J1339-31'
         rooti2 = 'J1339-31_'+fnstr
         pathi  = pathi1+runid+'/Proc4/J1339-31/'
         pathi2 = pathi1+runid+'/Proc3/J1339-31/'
         mflag  = 1b
      ENDELSE 
   ENDIF 
   ;
   kk      = strpos(name,':S')
   IF kk LT 0 THEN sstr = '1' ELSE sstr = strmid(name,kk+2)
   ;
   ; go ahead and make all file names and paths
   file        = make_array(nf, /string, value='')
   path        = make_array(nf, /string, value='')
   path[0:7]   = pathi
   file[0]     = rooti+'_'+frstr+'_sky.jpg'
   file[1]     = rooti+'_'+frstr+'_flux'+sstr+'.jpg'
   file[2]     = rooti+'_'+frstr+'sub_prof'+sstr+'.jpg'
   file[3]     = rooti+'_'+frstr+'_3clr.jpg'
   file[4]     = rooti+'_'+frstr+'_good.jpg'
   file[5]     = rooti+'_'+frstr+'_bad.jpg'
   file[6]     = rooti+'_'+frstr+'_net_good.jpg'
   file[7]     = rooti+'_'+frstr+'_net_bad.jpg'
   path[8:10]  = patho1
   file[8]     = rooto+'_skyprof_1.jpg'
   file[9]     = rooto+'_cog'+sstr+'_1.jpg'
   file[10]    = rooto+'_sbprof'+sstr+'_1.jpg'
   path[11:15] = patho2
   file[11]    = rooto+'_haps2.jpg'
   file[12]    = rooto+'_Cntgood2.jpg'
   file[13]    = rooto+'_Cntbad2.jpg'
   file[14]    = rooto+'_Netgood2.jpg'
   file[15]    = rooto+'_Netbad2.jpg'
   path[16:18] = patho1
   file[16]    = rooto+'_skyprof_4.jpg'
   file[17]    = rooto+'_cog'+sstr+'_4.jpg'
   file[18]    = rooto+'_sbprof'+sstr+'_4.jpg'
   path[19:23] = patho2
   file[19]    = rooto+'_haps8.jpg'
   file[20]    = rooto+'_Cntgood8.jpg'
   file[21]    = rooto+'_Cntbad8.jpg'
   file[22]    = rooto+'_Netgood8.jpg'
   file[23]    = rooto+'_Netbad8.jpg'
   path[24:26] = pathi
   file[24]    = rooti1+'_'+frstr+'_ss.fits'
   file[25]    = rooti1+'_'+fnstr+'_ss.fits'
   file[26]    = rooti2+'_'+frstr+'sub_ss.fits'
   path[27:29] = patho3+runid+'/Proc4/'
   file[27]    = rooti1+'_'+frstr+'.pl.fits'
   file[28]    = rooti1+'_'+fnstr+'.pl.fits'
   file[29]    = rooti2+'_'+frstr+'sub.pl.fits'
   path[30:32] = pathi2
   file[30]    = rooti1+'_'+frstr+'.pl.fits.gz'
   file[31]    = rooti1+'_'+fnstr+'.pl.fits.gz'
   file[32]    = rooti2+'_'+frstr+'sub.pl.fits.gz'
   path[33]    = patho3+runid+'/Proc4/'
   file[33]    = rooto
   path[34:36] = pathi
   file[34]    = rooti1+'_'+frstr+'_sex.cat'
   IF mflag THEN file[34] = rooti1+'_'+frstr+'_ref'+fnstr+'_sex.cat'
   file[35]    = rooti1+'_'+fnstr+'_sex.cat'
   file[36]    = rooti2+'_'+frstr+'sub_sex.cat'
   path[37]    = patho4
   file[37]    = rooti2+'_'+frstr+'sub_ss_r'+rstr+'_catcalib.dat'
   path[38:39] = patho1
   file[38]    = rooti2+'_'+frstr+'sub_ss_r'+rstr+'_cat_dbcheck.jpg'
   file[39]    = hname+'_hipass_spec.jpg'
   path[40:42] = patho2
   file[40]    = rooto+'_aps.jpg'
   file[41]    = rooto+'_sqrt1.png'
   file[42]    = rooto+'_sqrt3.png'
   file[43]    = rooto+'_sqrt8.png'
   file[44]    = rooto+'_lin1.png'
   file[45]    = rooto+'_lin3.png'
   file[46]    = rooto+'_lin8.png'
   file[47]    = rooto+'_log1.png'
   file[48]    = rooto+'_log3.png'
   file[49]    = rooto+'_log8.png'
   path[50:51] = pathi3
   file[50]    = rooti2+'_'+frstr+'sub_ss_r'+rstr+'_apmatch.out'
   file[51]    = rooti2+'_'+frstr+'sub_ss_r'+rstr+'_dbcheck.out'
   path[52:53] = patho4
   file[52]    = rooti2+'_'+frstr+'sub_ss_r'+rstr+'_apmatch.out'
   file[53]    = rooti2+'_'+frstr+'sub_ss_r'+rstr+'_dbcheck.out'
   ;
   ; decide how to create the output & do it
   IF opt EQ 'all' THEN BEGIN 
      IF keyword_set(nopath) THEN out = file ELSE out = path+file
   ENDIF ELSE BEGIN 
      jj       = where(optref EQ opt, njj)
      IF njj EQ 0 THEN BEGIN 
         out   = ''
      ENDIF ELSE BEGIN 
         jj    = jj[0]
         IF keyword_set(nopath) THEN out = file[jj] ELSE out = path[jj]+file[jj]
      ENDELSE 
   ENDELSE 
   ;
   return, out
END 


