FUNCTION blem_collapsex, im
   ;
   ; collapse along rows to make 1d column array
   sz  = size(im)
   sp  = make_array(sz[1],/float)
   FOR i = 0, sz[1]-1 DO BEGIN 
      sp[i] = total(im[i,*])
   ENDFOR 
   return, sp
END 

