PRO vlsr, jd, ra, dec, vlsr

;+
; NAME:
;    VLSR
;
; PURPOSE:
;    Calculate the line-of-sight velocity difference between a geocentric
;     observer and a target at RA,DEC in the LSR frame.
;
;    NOTE: 1. Neglects the rotational velocity of Earth and the motion
;              of Earth around the Earth-Moon barycenter.
;          2. Uses heliocentric velocity as the intermediate frame, rather
;              than S.S. barycenter.
;          3. Uses the IAU definition of LSR: 20 km/s toward 18h, +30d (B1900)
;              in the heliocentric frame.
;          4. Appears this routine is accurate to within 0.5 km/s
;
;    REFS: Allen's Astrophysical Quantities, 4th Ed., p. 661
;          Kerr & Lynden-Bell, 1986, MNRAS, 221, 1023
;
; Calling Sequence
;
;    VLSR, jd, ra, dec, vlsr
;
; Arguments
;
; Input
;    JD			Julian Date of interest
;    RA, DEC		RA and DEC of sky position, both in degrees (J2000)
;
; Output
;    VLSR		Line-of-sight projected velocity of LSR
;-

; Constants
pi=3.1415926535d0
d2r=pi/180.d0

; Definition of vLSR, vamp (km/s) toward RA=aaa, DEC=ddd (radians)
vamp=20.0
aaa=18.d0*15.d0*d2r
ddd=30.d0*d2r

; Precess vLSR direction to J2000 epoch
precess, aaa, ddd, 1900.0, 2000.0, /radian

; Direction cosines toward LSR direction
lx=cos(aaa)*cos(ddd)
ly=sin(aaa)*cos(ddd)
lz=sin(ddd)

; RA and DEC of sky position, converted to radians
rr=ra*d2r
dr=dec*d2r

; Direction cosines toward sky position
tx=cos(rr)*cos(dr)
ty=sin(rr)*cos(dr)
tz=sin(dr)

; Line-of-sight projected velocity between LSR frame and heliocentric frame
; Dot product of vLSR vector and sky position unit vector
hel_lsr = vamp*(lx*tx + ly*ty + lz*tz)

; Line-of-sight projected velocity between geocentric frame and helio. frame
; Dot product of vhelio vector and sky position unit vector

baryvel, jd, 2000.0, vhel, vbary

geo_hel = vhel[0]*tx + vhel[1]*ty + vhel[2]*tz

; Total line-of-sight velocity between geocentric frame and LSR frame is
; sum of HEL_LSR and GEO_HEL. Also, LSR velocity is negative of Earth
; velocity, so vlsr = -(geo_hel + hel_lsr)

vlsr = -(geo_hel + hel_lsr)

return
end
