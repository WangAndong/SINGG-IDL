PRO update_object,object,galind,numgals

; Renames various object names and renumbers the sources.
; Unlike the previous "update_name" script, this should only be run
; ONCE.  Otherwise, you risk screwing up all sorts of stuff.

  FOR ii = 0,N_ELEMENTS(object)-1 DO BEGIN
    source = STRMID(object[ii],0,8)
    CASE source OF
      'J1339-31': BEGIN
; S1 is A, S2 and S3 are blank
        IF numgals[ii] GE 3 THEN BEGIN
          IF galind[ii] EQ 1 THEN BEGIN
            object[ii] = 'J1339-31A'
            numgals[ii] = 1
          ENDIF ELSE BEGIN
            object[ii] = 'J1339-31'
            galind[ii] = galind[ii]-1
            numgals[ii] = numgals[ii]-1
          ENDELSE
        ENDIF
        END
        'J1234+02': BEGIN
; S1 and S2 are A, S3 is B, S4 is outside both but close to B.  The
; Schmidt data is the only one with B.
        IF numgals[ii] GE 4 THEN BEGIN
          IF galind[ii] LE 2 THEN BEGIN
            object[ii] = 'J1234+02A'
            numgals[ii] = 2
          ENDIF ELSE BEGIN
            object[ii] = 'J1234+02B'
            galind[ii] = galind[ii]-2
            numgals[ii] = numgals[ii]-2
          ENDELSE
        ENDIF
        END
      'J2215-45': BEGIN
; S1 is B, S2 and S3 are A, and S4+ (schmidt) are outside both.
        IF numgals[ii] GE 3 THEN BEGIN
          IF galind[ii] EQ 1 THEN BEGIN
            object[ii] = 'J2215-45b'
            numgals[ii] = 1
          ENDIF ELSE BEGIN
            object[ii] = 'J2215-45a'
            galind[ii] = galind[ii]-1
            numgals[ii] = numgals[ii]-1
          ENDELSE
        ENDIF
        END
;; J0205-55a needs to be split, but not right away.
;        'J0205-55': BEGIN
;        IF numgals[ii] GE 6 THEN BEGIN
;          aarr = [1,3,5]
;          aind = WHERE(aarr EQ galind[ii])
;          barr = [2,6,7,4]
;          bind = WHERE(barr EQ galind[ii],bcount)
;          IF bcount EQ 0 THEN BEGIN
;            object[ii] = 'J0205-55a'
;            galind[ii] = aind[0]+1
;            numgals[ii] = N_ELEMENTS(aarr)
;          ENDIF ELSE BEGIN
;            object[ii] = 'J0205-55b'
;            galind[ii] = bind[0]+1
;            numgals[ii] = N_ELEMENTS(barr)
;          ENDELSE
;        ENDIF
;        END
      ELSE: BEGIN
; Do nothing.
        END
    ENDCASE
  ENDFOR

  RETURN

END
