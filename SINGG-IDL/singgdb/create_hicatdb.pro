PRO create_hicatdb, version
   ;
   ;  This runs mkhicatdb to make the different versions of 
   ;  the HIcat databases.  As it stands the program can only be run
   ;  once per idl session. The parameter version sets which version 
   ;  the hicatdb to write:
   ;  version = 'FEB04' - February 2004 version. This is the default
   ;  version = 'MAY02' - May 2002 version, with no correction of 
   ;                      radio convention velocities.
   ;  version = 'MAY02CONV' - May 2002 version, with velocities, widths 
   ;                          and integrated fluxes corrected from
   ;                          radio convention to the standard optical
   ;                          convention.
   ;
   ;  G. Meurer 2/04
   ;  G. Meurer 12/04 - adjusted to use new version of mkhicatdb.pro
   ;                    which inserts E(B-V) and flag value from 
   ;                    Schlegel et al. (1998, ApJ, 500, 525)
   ;                    also uses E(B-V) in the sort block for the database.
   ;
   indblk = ['RA','DEC','VEL_50MAX','WIDTH_50MAX','SP','SINT','EBV']
   IF keyword_set(version) THEN vers = strupcase(strtrim(version,2)) $
                           ELSE vers = 'FEB04'
   CASE vers OF 
      'FEB04' : BEGIN 
           flowmod = 'flow_hicat_feb04.simple'
           junkstr = findfile(flowmod, count=nfound)
           IF nfound EQ 0 THEN mkhicatdb, 'params_feb04.dat', 'hicat_feb04_ed.txt', 'hicat_feb04', hicat, indblk=indblk, $
                               vfld1='VEL_50MAX', vfld2='VEL_MOM', /newvers $
                          ELSE mkhicatdb, 'params_feb04.dat', 'hicat_feb04_ed.txt', 'hicat_feb04', hicat, indblk=indblk, $
                               vfld1='VEL_50MAX', vfld2='VEL_MOM', flowmod=flowmod, /newvers 
         END 
      'MAY02' : BEGIN 
           flowmod = 'flow_hicat_may02.simple'
           junkstr = findfile(flowmod, count=nfound)
           IF nfound EQ 0 THEN mkhicatdb, 'hicat_header.dat', 'par_mF_mNB_may02.txt', 'hicat_may02', hicat2, indblk=indblk, $
                               vfld1='VEL_50MAX', vfld2='VEL_MOM', /noconvrt $
                          ELSE mkhicatdb, 'hicat_header.dat', 'par_mF_mNB_may02.txt', 'hicat_may02', hicat2, indblk=indblk, $
                               vfld1='VEL', vfld2='VEL_MOM', flowmod=flowmod, /noconvrt 
         END 
      'MAY02CONV' : BEGIN 
           flowmod = 'flow_hicat_may02_optconv.simple'
           junkstr = findfile(flowmod, count=nfound)
           IF nfound EQ 0 THEN mkhicatdb, 'hicat_header.dat', 'par_mF_mNB_may02.txt', 'hicat_may02_optconv', hicat3, $
                               indblk=indblk, vfld1='VEL', vfld2='VEL_MOM' $
                          ELSE mkhicatdb, 'hicat_header.dat', 'par_mF_mNB_may02.txt', 'hicat_may02_optconv', hicat3, $
                               indblk=indblk, vfld1='VEL', vfld2='VEL_MOM', flowmod=flowmod
         END  
      ELSE : print, 'unknown version : ', vers
      ENDCASE 
   
END 
