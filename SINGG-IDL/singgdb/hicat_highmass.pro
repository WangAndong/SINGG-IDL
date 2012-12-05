pro hicat_highmass
   ;
   ; find high mass galaxies in HICAT 
   ; (for potential use in BETA commissioning)
   ;
   ; G. Meurer 10/2011
   hdb     = 'hicat_feb04'
   lhmlim  = 10.67
   glatlim = 30.0
   hdr     = 'hipass+     cube  Sint   Sp  lg(MHI)  V_HI    W50    W20'
   ;
   dbopen, hdb
   list    = dbfind('logmhi > '+strtrim(string(lhmlim),2))
   list2   = dbfind('glat > '+strtrim(string(glatlim),2), list)
   list3   = dbfind('glat < -'+strtrim(string(glatlim),2), list)
   list    = [list2,list3]
   nl      = n_elements(list)
   dbext, list, 'hipass_name,ra,dec,vel_hi,width_50max,width_20max,sp,sint,cube,logmhi', $
                hname,ra,dec,vhi,w50,w20,sp,sint,cube,lmhi
   ;
   kk      = reverse(sort(sint))
   ;
   printf, -1, 'number of galaxies found: ', nl
   fmto    = '(a11,i5,f6.1,f6.2,f7.2,i7,i7,i7)'
   printf, -1, hdr
   for ii = 0, nl-1 do begin 
      jj   = kk[ii]
      printf,-1,ljust(hname[jj],11),cube[jj],sint[jj],sp[jj],lmhi[jj],vhi[jj],w50[jj],w20[jj], $
                format=fmto
   endfor
end 

