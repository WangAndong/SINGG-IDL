; Defines the possible bands and dumps them in a COMMON block named band. This needs to be
; in its own file because it is also called in make_ssoupin.
;
; S. Andrews (ICRAR/UWA) 01/2013

pro ssoup_initvars
    COMMON bands, band, nband, bandnam, bandavail, nbandavail, combo, ncombo
    ; these are the possible bands (I hate you, IDL, for being case-insensitive)
    ; think of this as an enumeration
    band      = { $
      mir_w4 : "W4", $
      mir_w3 : "W3", $
      mir_w2 : "W2", $
      mir_w1 : "W1", $
      halpha : 'HALPHA', $
      r      : 'R', $
      nuv    : 'NUV', $
      fuv    : 'FUV' $
      ;ps_g   : "g", $
      ;ps_r   : "r", $
      ;ps_i   : "i", $
      ;ps_z   : "z", $
      ;ps_y   : "y", $
      ; fir    : "FIR" $
    } 
    nband     = n_tags(band)
    ; HTML friendly
    bandnam   = ["W4", "W3", "W2", "W1", 'H&alpha;', 'R', 'NUV', 'FUV']; $
      ; "g", "r", "i", "z", "y", "FIR"]
    ; the rest of this stuff is defined in ssoup_inputs.pro
end