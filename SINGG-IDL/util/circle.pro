FUNCTION circle, xcenter, ycenter, radius, npoints=npoints
   ;
   ; Returns the coordinates of a closed circle whose center
   ; is at xcenter,ycenter having radius=radius 
   ;
   ; This was made to do the same thing as the Douglas
   ; Fanning version of circle, but using my own 
   ; (straight forward) algorithm.
   ;
   ; xcenter -> X position of center
   ; ycenter -> Y position of center
   ; radius  -> radius of circle
   ; ncircle -> If set, the number of evenly spaced points along the 
   ;            circle. If not set then npoints=100 is 
   ;            adopted.
   ;
   ; returned array: arr[2,npoints] - where arr[0,*] are the X 
   ;                 coordinates and arr[1,*] arr the corresponding Y
   ;                 coordinates.  The points are closed so that 
   ;                 arr[*,0] = arr[*,npoints-1]
   ; 
   ;
   ; Gerhardt Meurer 02/05
   if not keyword_set(npoints) then npoints=100
   ang = 2.0*!pi*findgen(npoints)/float(npoints-1)
   arr = make_array(2,npoints)
   arr[0,*] = xcenter+radius*cos(ang)
   arr[1,*] = ycenter+radius*sin(ang)
   ;stop
   return, arr
end 
