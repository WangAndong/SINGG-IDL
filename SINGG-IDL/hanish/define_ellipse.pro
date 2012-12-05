PRO define_ellipse,Ax,Ay,Bx,By,Cx,Cy,Gx,Gy,a,b,theta,Dx,Dy,Ex,Ey,Fx,Fy
; A,B,C,G are inputs from the _ellipse.dat
; a,b,theta, D,E,F are outputs

; Assume the major-axis endpoints are at least oriented on the correct axis and that the physical center, D, is at the intersection of A-B and C-G, AND that point C is on the ellipse
   v1x = (Bx-Ax)/(Cx-Gx)
   v1y = (By-Ay)/(Cy-Gy)
   v2x = (Ax-Gx)/(Cx-Gx)
   v2y = (Ay-Gy)/(Cy-Gy)

; y0 is the ratio along the A-B axis to the center, x0 is along the C-E axis, we only need 1
   y0 = (v2y - v2x)/(v1x - v1y)
   x0 = v1x * y0 + v2x
   Dx = y0*Bx + (1.0 - y0)*Ax
   Dy = y0*By + (1.0 - y0)*Ay

   theta = ATAN((By - Ay) / (Bx - Ax))
   a = SQRT(((Bx - Ax)^2 + (By - Ay)^2))/2
   btemp = SQRT(((Cx - Gx)^2 + (Cy - Gy)^2))/2

   btop = ((Cy-Dy)*COS(theta) - (Cx-Dx)*SIN(theta))
   bbottom = SQRT(1.0 - (((Cx-Dx)*COS(theta) + (Cy-Dy)*SIN(theta))/a)^2)
;   btop = (Cy*COS(theta) - Cx*SIN(theta) - Dy)
;   bbottom = SQRT(1.0 - ((Cx*COS(theta) + Cy*SIN(theta) - Dx)/a)^2)
   b = ABS(btop/bbottom)

   Ex = Dx - b*SIN(theta)
   Ey = Dy + b*COS(theta)
   Fx = Dx + b*SIN(theta)
   Fy = Dy - b*COS(theta)
END