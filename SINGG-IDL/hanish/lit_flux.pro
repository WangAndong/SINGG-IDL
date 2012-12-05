PRO lit_flux
; Recalculate fluxes for galaxies included in our literature table

  apfile = !singgdir+"/apertures.txt"
  readcol_new,apfile,name,aptype,ra,dec,diamin,diamaj,pa_w_lit, $
              FORMAT='(A,X,A,A,A,A,A,F)',COMMENT='#',/SILENT

;  tablefile = "~/data/lit_flux.dat"
;  IF FILE_TEST(tablefile) THEN spawn,"/bin/rm -f "+tablefile
;  OPENW,tableunit,tablefile,/GET_LUN

  FOR ii = 0,N_ELEMENTS(name)-1 DO BEGIN
    CD,"~/data/littemp"
    spawn,"/bin/rm -f -r J*.fits"
;    spawn,"mkdir littemp"

    hipassname = "j"+STRTRIM(STRMID(name[ii],1,7),2)
;    PRINT,"Copying object ",hipassname
    spawn,"cp ~/data/Run0?/"+STRTRIM(hipassname,2)+"/J*Rsub_ss.fits ~/data/littemp"
    spawn,"cp ~/data/Run0?/"+STRTRIM(hipassname,2)+"/J*_Rsub_mask.fits ~/data/littemp"
    spawn,"cp ~/data/Run0?/"+STRTRIM(hipassname,2)+"/J*Rsub.pl.fits ~/data/littemp"
;    spawn,"cp ~/data/Run0?/"+STRTRIM(hipassname,2)+"/*ellipse.dat ~/data/littemp"
;    CD,"littemp"

;    PRINT,"  Reading old ellipse file"
;    spawn,"ls *ellipse.dat",ellipse
; Read the existing ellipse file

;    read_ellipse_file,ellipse[0],numgals,refimage,Dx,Dy,Px,Py,pa, $
;                      a_i,b_i,z_s,z_f,z_c
;    spawn,"more "+ellipse[0]

    spawn,"ls J*Rsub_ss.fits",imlist
    fits_read,imlist[0],img,hd
    sz = SIZE(img)
buffer=0
;    buffer = SXPAR(hd,"BUFFER")
    skysub = STRTRIM(SXPAR(hd,'SKYSUB',count=count),2)
    skylev = 0.0
    IF count GT 0 AND skysub EQ 'T' THEN skylev = SXPAR(hd,'SKYLEV')
    phot = SXPAR(hd,"PHOTFLUX")
    tempra = sexideg(ra[ii])*15.d0
    tempdec = sexideg(dec[ii])
    adxy,hd,tempra,tempdec,cenX,cenY

    del_pa_x = ATAN(SXPAR(hd,"CD1_2")/SXPAR(hd,"CD1_1"))*!radeg
    del_pa_y = ATAN(-SXPAR(hd,"CD2_1")/SXPAR(hd,"CD2_2"))*!radeg
    del_pa = (del_pa_x + del_pa_y)/2

    pa_lit = pa_w_lit[ii] + del_pa
    theta_lit = (pa_lit-90.0)*!dtor

    getrot,hd,rot,cdelt 
    cdelt  = abs(cdelt)*3600.
    as_pix = cdelt[0]
    
    a_lit = FLOAT(STRMID(diamaj[ii],0,STRLEN(diamaj[ii])-1)) / as_pix
    b_lit = FLOAT(STRMID(diamin[ii],0,STRLEN(diamin[ii])-1)) / as_pix

; Set up mask
    spawn,"ls J*mask.fits",masklist
    spawn,"ls J*.pl.fits",pllist
    fits_read,masklist[0],maskimg,junk,/data_only
    fits_read,pllist[0],plimg,junk,/data_only
    mask = maskimg OR (plimg LT 1.5)

    flux=0.d0
    IF STRTRIM(aptype[ii],2) EQ "ellipse" THEN BEGIN
      dist_ellipse,tempmask,[sz[1],sz[2]],cenX,cenY,(a_lit/b_lit),pa_lit
      ellmask = (tempmask LT a_lit)

      flux = TOTAL(DOUBLE(img-skylev)*FLOAT(ellmask AND NOT mask))*phot
    ENDIF ELSE BEGIN
; Rectangular aperture.  Easy.
      minX = MAX([0,LONG(cenX+buffer-a_lit/2.0)])
      maxX = MIN([sz[1]-1,LONG(cenX+buffer+a_lit/2.0)])
      minY = MAX([0,LONG(cenY+buffer-b_lit/2.0)])
      maxY = MIN([sz[2]-1,LONG(cenY+buffer+b_lit/2.0)])

      recmask = BYTARR(sz[1],sz[2])
      recmask[minX:maxX,minY:maxY] = 1b
      flux = TOTAL(DOUBLE(img-skylev)*FLOAT(recmask AND NOT mask))*phot
    ENDELSE

    PRINT,name[ii],ALOG10(flux),cenX,cenY

  ENDFOR

;  CLOSE,tableunit
;  FREE_LUN,tableunit

  RETURN
END
