PRO write_ssfits,filename,img,hd,Dx,Dy,Px,Py,sky,skysigpix,delsky, $
                 header_template,racen,deccen,diamin,diamaj,dr,pa,bxw,photmtrc
; Takes all the stuff you pass in, adds it to the header, then writes the 
; _ss.fits file.

; Add the information to the file's header
  SXADDPAR,hd,"ISOPHX",Dx[0],' Isophote center X [pixels]'
  SXADDPAR,hd,"ISOPHY",Dy[0],' Isophote center Y [pixels]'
  SXADDPAR,hd,"BRIGHTX",Px[0],' Brightness peak X [pixels]'
  SXADDPAR,hd,"BRIGHTY",Py[0],' Brightness peak Y [pixels]'
  SXADDPAR,hd,"SKYLEV",sky[0],' box2box sky level'
  SXADDPAR,hd,"SKYSIG",skysigpix,' pixel-to-pixel sky RMS'
  SXADDPAR,hd,"SKYSIGBX",sky[1],' box-to-box sky RMS'
  SXADDPAR,hd,"SKYSUB",'T',' sky subtraction complete?'

; Now, all the NOAO keywords...
  SXADDPAR,hd,"SKYAPRA",racen[0],' RA of sky aperture [degrees]'
  SXADDPAR,hd,"SKYAPDEC",deccen[0],' Declination of sky aperture [degrees]'
  SXADDPAR,hd,"SKYAPA1",(diamaj*60.0), $
             ' Semi-major axis of inner sky aperture [arcsec]',after='SKYAPDEC'
  SXADDPAR,hd,"SKYAPA2",((diamaj + 2.0*dr)*60.0), $
              ' Semi-major axis of outer sky aperture [arcsec]',after='SKYAPA1'
  SXADDPAR,hd,"SKYAPAB1",(diamaj/diamin), $
              ' Axial ratio (A/B) of inner sky aperture'
  SXADDPAR,hd,"SKYAPAB2",((diamaj + 2.0*dr)/(diamin + 2.0*dr)), $
              ' Axial ratio (A/B) of outer sky aperture'
  SXADDPAR,hd,"SKYAPPA1",pa,' Position angle of inner sky aperture [degrees]'
  SXADDPAR,hd,"SKYAPPA2",pa,' Position angle of outer sky aperture [degrees]'
  SXADDPAR,hd,"SKYBOX",bxw,' Box size used by box2boxbg [pixels]'
; Placeholder for a later step.
  SXADDPAR,hd,'PHOTMTRC',photmtrc,' Observed in photometric conditions (Y/N/P)?'
  SXADDPAR,hd,'DELSKY',delsky,' Sky small-scale structure'

; output = JXXXX-XX_(R/6XXX/Rsub)_ss.fits
; Don't include any path in this.
  slashpos = STRPOS(filename,'/',/REVERSE_SEARCH)
  odir = STRMID(filename,0,slashpos+1)
  output = STRMID(filename,slashpos+1,(STRLEN(filename)-6-slashpos))+'_ss.fits'
  SXADDPAR,hd,"FILENAME",output,' File name'

; Now that we've added all this new stuff to the header, re-template it
  hdr_template,hd,img,header_template,hd_out,/silent
 
  fits_write,odir+output,FLOAT(img), hd_out

END
