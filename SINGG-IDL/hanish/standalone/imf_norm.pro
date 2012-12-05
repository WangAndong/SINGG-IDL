PRO imf_norm,imf=imf,cutoff=cutoff

; finds the normalization factors for a given IMF

  IF NOT KEYWORD_SET(imf) THEN imf = 0
  IF NOT KEYWORD_SET(cutoff) THEN cutoff = 10.0

; m are the mass range limits, and will be one element larger than the
; rest of the arrays.  So, bin N goes from m[N] to m[N+1]
; b is the exponent of dN/dm

  CASE imf OF
    0: BEGIN
; Old Salpeter (1955):
         s = "Salpeter (1955)"
         m = [0.1,100.0]
         b = [-2.35]
       END
; arbitrary mixed IMF from Pagel:
    1: BEGIN
         s = "Pagel"
         m = [0.1,0.5,1.0,3.16,100.0]
         b = [-0.85,-1.85,-3.4,-2.7]
       END
; Old Scalo (1986)
    2: BEGIN
         s = "Scalo (1986)"
         m = [0.15,0.18,0.42,0.62,1.18,3.5,120.0] ; was a pseudo-break at 20.0
         b = [1.6,-1.01,-2.75,-2.08,-3.5,-2.63]
       END
; New Scalo (1998)
    3: BEGIN
         s = "Scalo (1998)"
         m = [0.1,1.0,10.0,100.0]
         b = [-1.2,-2.7,-2.3]
       END
; Old Kroupa (1991)
    4: BEGIN
         s = "Kroupa (1991)"
         m = [0.01,0.08,0.5,20.0,100.0]
         b = [-0.3,-1.3,-2.3,-2.3]
       END
; New Kroupa (2001)
    5: BEGIN
         s = "Kroupa (2001)"
         m = [0.01,0.08,0.5,1.0,100.0]
         b = [-0.3,-1.8,-2.7,-2.3]
       END
; Baldry (2003)
    6: BEGIN
         s = "Baldry (2003)"
         m = [0.1,0.5,120.0]
         b = [-1.5,-2.2]
       END
; SalA (in Baldry 2003)
    7: BEGIN
         s = "Salpeter mod A (from Baldry 2003)"
         m = [0.1,0.5,120.0]
         b = [-1.5,-2.35]
       END
; SalB (in Baldry 2003)
    8: BEGIN
         s = "Salpeter mod B (from Baldry 2003)"
         m = [0.1,1.0,120.0]
         b = [-1.5,-2.35]
       END
    ELSE: BEGIN
            PRINT,"ERROR in imf_norm: invalid IMF ",imf
            RETURN
          END
  ENDCASE

  num_bins = N_ELEMENTS(m)-1
  IF N_ELEMENTS(b) NE num_bins THEN PRINT,"ERROR!  b has too many elements"

; We want to derive two things, a and f.
; a is the coefficient in front of each range.  It'll be defined by
; our normalization, and by the boundaries.
  a = FLTARR(num_bins)

; f is the relative fraction of the MASS each bin contains.
  f = FLTARR(num_bins)

; n is the relative fraction of the NUMBER each bin contains.
  n = FLTARR(num_bins)

; w is a temporary value, the weighting of each bin relative to the
; first.

  w = FLTARR(num_bins)

  FOR ii = 0,(num_bins-1) DO BEGIN

; Now, we start.  First up, use the boundary conditions to walk
; through the normalizations, finding the weightings relative to the
; first bin.

    IF ii EQ 0 THEN BEGIN
      w[ii] = 1.0
    ENDIF ELSE BEGIN
; Match the value with the previous bin
      w[ii] = w[ii-1] * m[ii]^(b[ii-1]-b[ii])
    ENDELSE

; Finally, calculate the values for f and n
    f[ii] = (w[ii]/(b[ii]+2.0)) * (m[ii+1]^(b[ii]+2.0) - m[ii]^(b[ii]+2.0))

; Instead of integrating m dN/dm, it's just integrating dN/dm
    n[ii] = (w[ii]/(b[ii]+1.0)) * (m[ii+1]^(b[ii]+1.0) - m[ii]^(b[ii]+1.0))
  ENDFOR

; Now, normalize it all so that the total of f is 1
  a = w / TOTAL(f)
  f = f / TOTAL(f)
  n = n / TOTAL(n)

  PRINT,"IMF: ",STRTRIM(s,2)
  FOR ii = 0,num_bins-1 DO BEGIN
    PRINT,"(m dN/dm) = ",a[ii]," m^",(b[ii]+1.0),"; ",m[ii]," < m < ",m[ii+1],"; f=",f[ii],"; n=",n[ii], $
          FORMAT='(A,F8.3,A,F6.3,A,F6.2,A,F6.2,A,F6.4,A,F6.4)'
  ENDFOR

; Finally, calculate what fraction of the luminosity comes from
; stars above 10 solar masses

  ind = WHERE(m GT cutoff,count)-1
  IF count EQ 0 THEN BEGIN
    PRINT,"ERROR in imf_norm: cutoff too high ",cutoff
    RETURN
  ENDIF

  atot = 0.0

  FOR ii = 0,count-1 DO BEGIN
    aexp = b[ind[ii]]+2.0
    IF m[ii] GT cutoff THEN BEGIN
      atot = atot + a[ind[ii]]*(m[ind[ii]+1]^aexp - m[ind[ii]]^aexp) / aexp
    ENDIF ELSE BEGIN
      atot = atot + a[ind[ii]]*(m[ind[ii]+1]^aexp - cutoff^aexp) / aexp
    ENDELSE
  ENDFOR

  PRINT,"Fraction above ",FLOAT(cutoff)," solar masses = ",atot,FORMAT='(A,F6.2,A,F7.5)'

END
