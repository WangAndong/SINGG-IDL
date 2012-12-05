FUNCTION calc_z,x,y,Dx,Dy,theta,a,b
   term1 = (((x-Dx)*COS(theta) + (y-Dy)*SIN(theta))/a)^2
   term2 = (((y-Dy)*COS(theta) - (x-Dx)*SIN(theta))/b)^2
   z = SQRT(term1 + term2)
   RETURN,z
END
