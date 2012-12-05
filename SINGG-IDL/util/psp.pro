pro psp,xsize=xs,ysize=ys,yoffset=yoff,xoffset=xoff,eps=eps

; default settings make 5 by 5 inch plot region
if not keyword_set(xs) then xs=6.125
if not keyword_set(ys) then ys=5.825
if not keyword_set(yoff) then yoff=3.
if not keyword_set(xoff) then xoff=1.2
set_plot,'ps'
device,/portrait,/inches,xs=xs,ys=ys,yo=yoff,xo=xoff,/color
if keyword_set(eps) then device,/encapsulated

end
