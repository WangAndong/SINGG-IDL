FUNCTION gsym, selection, angle=angle, fill=fill, thick=thick, flop=flop
  ;
  ; selection -> tells which symbol to plot
  ; angle     -> position angle of symbol in degrees measured CW
  ; fill      -> if set (and relevant) the symbol is filled
  ; flop      -> reflect the symbol in y (about the x axis)
  ;
  ; Set symbol with usersym, from a wide range of possibilities
  ;
  ; selection an integer => 0
  ;     0-29    specialized symbols
  ;             for now just use sym.pro
  ;     nni     a regular radially symmetric symbol
  ;             nn specifies number of points or sides
  ;              i specifies type
  ;     i = 0   stick star
  ;     i = 1   polygon
  ;     i = 2   star
  ;     i = 3   pointy star
  ;     i = 4   flower
  ;     i = 5   flowers with petals
  ;     i = 6   swirl
  ;     i = 7   buzz saw 
  ;     i = 8   broken stick star
  ;     i = 9   windmill
  ;
  ; G. Meurer 11/2010
  radian    = 180.0/!pi
  nvmax     = 49        ; maximum number of vertices allowed by usersym
  ;
  sel       = fix(abs(selection))
  IF keyword_set(angle) THEN ang0 = angle ELSE ang0 = 0.0   ; angle offset
  IF sel LE 29 THEN BEGIN 
     result = sym(selection)
  ENDIF ELSE BEGIN 
     result = 8
     ;
     ; parse number of sides and option
     nside  = fix(sel/10)     ; number of sides
     opt    = sel - 10*nside  ; option
     nc     = 10 - 2*fix(nside GE 5)  ; number of points in a circle
     ;
     ; make symbol by stringing "feathers" together; these are 
     ; azimuthally repeating points to connect.  Position of one
     ; feather given by xf, yf.  The position angle of the 
     ; feathers are given by thf, and their corresponding sines 
     ; and cosines are sthf, cthf.
     dth    = 360.0/float(nside)
     thf    = ang0 + findgen(nside)*dth
     sthf   = sin(thf/radian)
     cthf   = cos(thf/radian)
     ;
     ; set coords of defining feather by opt
     CASE opt OF 
        0 : BEGIN 
               ; stick star
               xf     = [0.0, 0.0]
               yf     = [1.0, 0.0]
               closed = 0b
            END
        1 : BEGIN 
               ; polygon
               xf     = [0.0]
               yf     = [1.0]
               closed = 1b
            END
        2 : BEGIN 
               ; star
               ang1   = 0.5*dth/radian
               IF nside GT 4 THEN BEGIN 
                  ang0   = 2.0*!pi*(0.25 - 1.0/float(nside))
                  ang2   = !pi - ang0 - ang1
                  aa     = sqrt(sin(ang0)/(sin(ang1)*sin(ang2)*(1.0/tan(ang0) + 1.0/tan(ang1))))
               ENDIF ELSE BEGIN 
                  aa     = 0.35
               ENDELSE 
               xf     = [0.0, -1.0*aa*sin(ang1)]
               yf     = [1.0, aa*cos(ang1)]
               closed = 1b
            END
        3 : BEGIN 
               ; pointy star
               ang1   = 0.5*dth/radian
               IF nside GT 4 THEN BEGIN 
                  ang0   = 2.0*!pi*(0.25 - 1.0/float(nside))
                  ang2   = !pi - ang0 - ang1
                  aa     = 0.45*sqrt(sin(ang0)/(sin(ang1)*sin(ang2)*(1.0/tan(ang0) + 1.0/tan(ang1))))
               ENDIF ELSE BEGIN 
                  aa     = 0.18
               ENDELSE 
               xf     = [0.0, -1.0*aa*sin(ang1)]
               yf     = [1.0, aa*cos(ang1)]
               closed = 1b
            END
        4 : BEGIN 
               ; flower
               ang1   = 0.5*dth/radian
               aa     = 1.0/(1.0+tan(ang1))
               dx     = aa*tan(ang1)
               nhc    = nc / 2
               dth    = !pi/float(nhc)
               ang    = dth*(findgen(nhc)+1.0)
               xf     = 1.0*dx*cos(ang)
               yf     = aa + dx*sin(ang)
               closed = 1b
            END
        5 : BEGIN 
               ; flower with petals
               ang1   = 0.5*dth/radian
               aa     = 1.0/(1.0+tan(ang1))
               dx     = aa*tan(ang1)
               nhc    = nc / 2
               dth    = !pi/float(nhc)
               ang    = dth*findgen(nhc+1)
               xf     = [1.0*dx*cos(ang), 0.0]
               yf     = [aa + dx*sin(ang), 0.0]
               closed = 1b
            END
        6 : BEGIN 
               ;  swirl: like a stick star where each stick is a half circle
               nhc    = nc / 2
               dth    = !pi/float(nhc)
               ang    = dth*[(findgen(nhc)+1.0), reverse(findgen(nhc))]
               xf     = 0.5 - 0.5*cos(ang)
               yf     = 0.5*sin(ang)
               closed = 0b
            END
        7 : BEGIN 
               ; buzz saw
               IF nside GT 4 THEN BEGIN 
                  ang0   = 2.0*!pi*(0.25 - 1.0/float(nside))
                  ang1   = 0.5*dth/radian
                  ang2   = !pi - ang0 - ang1
                  aa     = sqrt(sin(ang0)/(sin(ang1)*sin(ang2)*(1.0/tan(ang0) + 1.0/tan(ang1))))
               ENDIF ELSE BEGIN 
                  aa     = 0.35
               ENDELSE 
               ang3   = dth/radian
               xf     = [0.0, -1.0*aa*sin(ang3)]
               yf     = [1.0, aa*cos(ang3)]
               closed = 1b
            END
        8 : BEGIN 
               ; broken stick star
               ang1   = 0.5*dth/radian
               s1     = sin(ang1)
               c1     = cos(ang1)
               xf     = [0.0, -1.0*s1, 0.0, 0.0]
               yf     = [c1, c1, c1, 0.0]
               IF nside EQ 4 THEN BEGIN 
                  print, "Fascist!"
                  xf  = [0.0, 0.0]
                  yf  = [1.0, 0.0]
               ENDIF 
               closed = 0b
            END
        9 : BEGIN 
               ; windmill
               ang1   = 0.5*dth/radian
               s1     = sin(ang1)
               c1     = cos(ang1)
               xf     = [0.0, 0.0, -1.0*s1]
               yf     = [0.0, c1, c1]
               closed = 1b
            END
     ENDCASE 
     npf  = n_elements(xf)
     np   = npf*nside + closed
     ;
     ; finish making the symbol, but only if there are not 
     ; too many vertices
     IF np LE nvmax THEN BEGIN 
        xp   = make_array(np, /float, value=0.0)
        yp   = make_array(np, /float, value=0.0)
        ;
        ; string feathers together
        FOR ii = 0, nside-1 DO BEGIN 
           j1        = ii*npf
           j2        = j1+npf-1
           xp[j1:j2] = -1.0*cthf[ii]*xf + sthf[ii]*yf
           yp[j1:j2] = sthf[ii]*xf + cthf[ii]*yf
        ENDFOR 
        IF closed THEN BEGIN 
           xp[np-1]  = xp[0]
           yp[np-1]  = yp[0]
        ENDIF 
        ;
        ; flop the coords if requested
        IF keyword_set(flop) THEN yp = -1.0*yp
        ;
        ; boolean to decide wether to fill in the symbol
        ufill = (keyword_set(fill) AND closed)
        ;
        ; make the symbol
        usersym, xp, yp, fill=ufill, thick=thick
     ENDIF ELSE BEGIN 
        ;
        ; just use a dot
        result = 3
     ENDELSE 
  ENDELSE 
  ;     
  return, result
END 
