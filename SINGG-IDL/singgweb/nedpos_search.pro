function nedpos_search, rastr, decstr
   ;
   ; return the URL for a ned near position search
   ;
   ; G. Meurer 06/2007
   s1 = 'http://nedwww.ipac.caltech.edu/cgi-bin/nph-objsearch?in_csys=Equatorial&in_equinox=J2000.0&lon='
   s2 = '&lat='
   s3 = '&radius=0.5&search_type=Near+Position+Search&out_csys=Equatorial&out_equinox=J2000.0'
   s4 = '&obj_sort=Distance+to+search+center&of=pre_text&zv_breaker=30000.0&list_limit=5'
   s5 = '&img_stamp=YES&z_constraint=Unconstrained&z_value1=&z_value2=&z_unit=z&ot_include=ANY&nmp_op=ANY'
   cln = '%3A'
   rah = strmid(rastr,0,2)
   ram = strmid(rastr,3,2)
   ras = strmid(rastr,6)
   dcd = strmid(decstr,0,3)
   dcm = strmid(decstr,4,2)
   dcs = strmid(decstr,7)
   url = s1+rah+cln+ram+cln+ras+s2+dcd+cln+dcm+cln+dcs+s3+s4+s5
   return, url
END 
