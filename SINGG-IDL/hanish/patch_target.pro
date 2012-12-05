PRO patch_target,hdr,change=change

; Given a header with the "RUNID", "TARGET", and "TARGTYPE" keywords,
; in the cases where multiple targets were in the frame, use a
; specified one.

  runid = STRTRIM(SXPAR(hdr,'RUNID'),2)
  target = STRTRIM(SXPAR(hdr,'TARGET'),2)
  oldtarget = target
  targtype = STRTRIM(SXPAR(hdr,'TARGTYPE'),2)
  change = 0b

; First, the ones common to several exposures within a run:
  CASE runid OF
    '03': BEGIN
         IF target EQ 'J1339-31' THEN target = 'J1339-31b'
       END
    '04s': BEGIN
         IF target EQ 'J1318-21' THEN target = 'J1320-21'
         IF target EQ 'J1234+02A' THEN target = 'J1234+02B'
         IF target EQ 'J2215-45b' THEN target = 'J2215-45a'
       END
    '10': BEGIN
         IF target EQ 'J1145+02' THEN BEGIN
           target = 'J1144+02x'
           targtype = 'REJECT'
         ENDIF
         IF target EQ 'J2215-45b' THEN target = 'J2215-45a'
       END
    '13': BEGIN
         IF target EQ 'J1144+02x' THEN BEGIN
           target = 'J1145+02'
           targtype = 'SINGG'
         ENDIF
         IF target EQ 'J1339-31b' THEN target = 'J1339-31'
       END
    '17': BEGIN
; This one actually did BOTH narrow-bands.
         IF target EQ 'J1339-31b' THEN target = 'J1339-31'
       END
    ELSE: BEGIN
; Do nothing.
       END
  ENDCASE

  image = STRTRIM(SXPAR(hdr,'FILENAME'),2)
  CASE image OF
    '0406078': BEGIN
                 target = 'J2102-16'
                 targtype = 'SINGG'
               END
    '0406082': BEGIN
                 target = 'J2102-16'
                 targtype = 'SINGG'
               END
    '0604071': BEGIN
                 target = 'J2022-31'
                 targtype = 'SINGG'
               END
    '0604072': BEGIN
                 target = 'J2022-31'
                 targtype = 'SINGG'
               END
    '0604074': BEGIN
                 target = 'J2022-31'
                 targtype = 'SINGG'
               END
    '0604075': BEGIN
                 target = 'J2022-31'
                 targtype = 'SINGG'
               END
    ELSE: BEGIN
; Do nothing
          END
  ENDCASE

  IF STRTRIM(target,2) NE STRTRIM(oldtarget,2) THEN BEGIN
; We changed something, so edit the header.
    change = 1b
    SXADDPAR,hdr,'TARGET',target,'Target name '
    SXADDPAR,hdr,'TARGTYPE',targtype,'Target type '
  ENDIF

  RETURN

END
