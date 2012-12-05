PRO radialpf_image  

; since we want to compare all images to the image with the
; worst seeing (as we don't want to make the image with
; the worst seeing any worse), we will scale the R-band
; and H-alpha image to the NUV image

;summary of constants

;cpscale=0.4344 ; pixel scale of R-band/H-alpha images ; different from image to image
npscale=1.5  ; pix scale of UV images


photflamNUV=2.06e-16 ; photometric conversion factor from cps to flux for NUV
photplamNUV=2315.7 ;\A

photflamFUV=1.40e-15 ;  photometric conversion factor from cps to flux for FUV
photplamFUV=1538.6 ;\A

FWHM_FUV=4.0/1.5
FWHM_NUV=5.6/1.5

mrad      = 0.25             ; match radius in arcmin
mrsun     = 4.51             ; absolute mag of sun in R band
nsigma    = 2.5
snlimit   = 0.0              ; S/N limit for Halpha observations

; plot parameter

aspect=1.1
charsize = 1.7
symsize = 1.0

; directories

singhome='/data5/sungg/meurer/SINGG/'
sunghome='/data2/sungg/iwong/GALEX_DATA/'
maskhome='/data3/sungg/iwong/WORKING/RAD_PROFILES/'
imagehome='/data5/sungg/jhkim/radprof_data/'

gdb       = 'sungg_derived'  ; sungg galex measurement database
ddb       = 'singg_derived'  ; Halpha measurement database

;using explore_uvha part

dbopen, gdb
entfuv = dbfind('filter = fuv')   ; entries of FUV measurements
entnuv = dbfind('filter = nuv')   ; entries of NUV measurements
dbext, entfuv, 'name,hipname,optid,ra,dec,se,entry_singg_derivd,filename,sname', namef,hnamef,optidf,raf,decf,lsef,singgidf,filef,snamef
dbext, entfuv, 'skylev, skyrms', skylevf, skyrmsf
dbext, entnuv, 'name,hipname,optid,ra,dec,se,entry_singg_derivd,filename,sname', namen,hnamen,optidn,ran,decn,lsen,singgidn,filen,snamen
dbext, entnuv, 'skylev, skyrms', skylevn, skyrmsn

dbclose

;
; make arrays to store H-alpha matches
nf      = n_elements(entfuv)      ; number of FUV measurements
nn      = n_elements(entnuv)      ; number of NUV measurements
enthaf  = make_array(nf, /long, value=-1l)  ; will store singg_derived entries with FUV meas.
enthan  = make_array(nn, /long, value=-1l)  ; will store singg_derived entries with NUV meas.
;
; open H-alpha database, get good entries
dbopen, ddb
goodha  = good_derived3(ng, snlimit=snlimit)
print, 'Number of good entries in singg_derived: ', ng

; loop through NUV entries find H-alpha matches
FOR ii = 0, nn-1 DO BEGIN
    ra   = ran[ii]                               ; store right ascension of this NUV entry
    dec  = decn[ii]                              ; store declination of this NUV entry
    list = dbcircled(ra, dec, mrad, dis, goodha) ; find sources within mrad of this NUV entry
    nl   = n_elements(list)                      ; number of matches to this NUV source
    IF nl EQ 1 AND list[0] LT 0 THEN nl = 0      ; Check if no matches and reset nl if need be
    IF nl GE 1 THEN BEGIN
      IF nl EQ 1 THEN BEGIN
         enthan[ii] = list[0]                   ; only one entry store it
      ENDIF ELSE BEGIN
         jj = sort(dis)                         ; sort by projected distance to NUV source
         enthan[ii] = list[jj[0]]               ; store halpha entry
      ENDELSE
    ENDIF
   ENDFOR
   kn = where(enthan GE 0, nkn)                    ; find entries that have NUV matches
   ;
   ; loop through FUV entries find H-alpha matches
   FOR ii = 0, nf-1 DO BEGIN
      ra   = raf[ii]
      dec  = decf[ii]
      list = dbcircled(ra, dec, mrad, dis, goodha)
      nl   = n_elements(list)
      IF nl EQ 1 AND list[0] LT 0 THEN nl = 0
      IF nl GE 1 THEN BEGIN
         IF nl EQ 1 THEN BEGIN
            ;
            ; store matching entry
            enthaf[ii] = list[0]
         ENDIF ELSE BEGIN
            ;
            ; find closest match if there are multiple
            jj = sort(dis)
            enthaf[ii] = list[jj[0]]
         ENDELSE
      ENDIF
   ENDFOR
   kf = where(enthaf GE 0, nkf)
   ;
  print, 'Number of FUV - H-alpha matches: ', nkf
  print, 'Number of NUV - H-alpha matches: ', nkn


match,singgidf[kf],singgidn[kn],gf,gn


   ; save only UV results that match
   name   = namef[kf[gf]]
   hname  = hnamef[kf[gf]]
   optid  = optidf[kf[gf]]
   ra     = raf[kf[gf]]
   dec    = decf[kf[gf]]
   lse    = lsef[kf[gf]]
   entha  = enthaf[kf[gf]]
   filef  = filef[kf[gf]]
   skylevfuv = skylevf[kf[gf]]
   skyrmsfuv = skyrmsf[kf[gf]]

   filen  = filen[kn[gn]]
   skylevnuv = skylevn[kn[gn]]
   skyrmsnuv = skyrmsn[kn[gn]]

   singgid = singgidf[kf[gf]]
   singgid2 = singgidn[kn[gn]]

nn=n_elements(name)

singgid[0]=444

dbext,singgid,'optid,entry_flux_r,entry_flux_s',optname,entryr,entryha

for ii=0,nn-1 do begin
;print,ii,hname[ii],optid[ii],singgid[ii],entryr[ii],entryha[ii]
endfor

dbclose

;reading image of R and cont. subracted images from singg db

dbopen,'singg_flux'
;for ii=0,nn-1 do begin
dbext,entryr,'runid,filename',runidr,fnamer
dbext,entryha,'runid,filename',runidha,fnameha
;endfor

dbclose


;assigning file names for R band image and masks

nn = n_elements(fnamer)
dnamer = make_array(nn,/string,value='')
dnamer = singhome + strmid(runidr,0,5)+'/Proc4/'
imR=strtrim(dnamer+fnamer)
maskR = strtrim(dnamer+strmid(fnamer,0,9)+'mask.fits') ; mask name


;assigning file names for the continuum subtracted and masks

nn = n_elements(fnameha)
dnameha = make_array(nn,/string,value='')
dnameha = singhome + strmid(runidha,0,5)+'/Proc4/'
imha=strtrim(dnameha+fnameha)
maskha = strtrim(dnameha+strmid(fnameha,0,14)+'mask.fits') ; masks


;assigning file names for FUV and masks

nn= n_elements(filef)
gg=strpos(filef, '/', /reverse_search)
hh=strpos(filef,'.',/reverse_search)

dnamef=make_array(nn,/string,value='')
for ii = 0, nn-1 do begin dnamef[ii] = sunghome+strmid(filef[ii],0,gg[ii]+1)
endfor

fnamef = make_array(nn,/string,value='')
maskf = make_array(nn,/string,value='')

for ii = 0, nn-1 do begin fnamef[ii] = strtrim(strmid(filef[ii],gg[ii]+1,hh[ii]-gg[ii]-1),2)+'.fits.gz' ; changed from bgsb.fits.gz due to change of sky bg treatment
maskf[ii] = strtrim(name[ii])+'mask.fuv.fits'
endfor

imfuv=strtrim(dnamef+fnamef)
maskfuv = strtrim(maskhome+maskf) ;masks

;assigning file names for NUV

nn= n_elements(filen)
gg=strpos(filen, '/', /reverse_search)
hh=strpos(filen,'.',/reverse_search)

dnamen=make_array(nn,/string,value='')
for ii = 0, nn-1 do begin dnamen[ii] = sunghome+strmid(filen[ii],0,gg[ii]+1)
endfor
fnamen = make_array(nn,/string,value='')
maskn = make_array(nn,/string,value='')
for ii = 0, nn-1 do begin fnamen[ii] = strtrim(strmid(filen[ii],gg[ii]+1,hh[ii]-gg[ii]-1),2)+'.fits.gz' ; changed from bgsb.fits.gz due to change of sky bg treatment
maskn[ii] = maskf[ii]

endfor

imnuv=strtrim(dnamen+fnamen)
masknuv=strtrim(maskhome+maskn)


openw,1,'imagemaskfile.txt'
for ii=0,nn-1 do begin
printf,1,ii,' ',hname[ii],optname[ii],imR[ii],maskR[ii],imha[ii],maskha[ii],imfuv[ii],maskfuv[ii],imnuv[ii],masknuv[ii]
endfor
close,1


for ii=0, nn-1 do begin

fnf1=file_info(imR[ii])
fnf2=file_info(imha[ii])
fnf3=file_info(imfuv[ii])
fnf4=file_info(imnuv[ii])
fnf5=file_info(maskr[ii])
fnf6=file_info(maskha[ii])
;fnf7=file_info(maskfuv[ii])

fnf_tot=fnf1.exists+fnf2.exists+fnf3.exists+fnf4.exists+fnf5.exists+fnf6.exists

if (fnf_tot eq 6) then begin

;obtaining PSF from R
;seeing = 1.4946313 arcsec
;original pixel size = 0.4344 arcsec/pix
;rebinned pixel size = 1.5 from GALEX
;seeing = 0.996 pix

fits_read,imR[ii],imgR,hdR
fits_read,maskR[ii],mR,hdmR
mR=uint(mR)

FWHM_R=SXPAR(hdR,'SEEING')/1.5
photflamR=SXPAR(hdR,'PHOTFLAM')
photplamR=SXPAR(hdR,'PHOTPLAM')
rpixsize=SXPAR(hdR, 'XPIXSIZE')

;reading image of Ha

fits_read,imha[ii],imgHa,hdHa
fits_read,maskha[ii], mHa,hdmHa
mHa = uint(mHa)

;obtain PSF from Halpha
;seeing = 1.3243071 arcsec
;original pixel size = 0.4344 arcsec/pix
;rebinned pixel size = 1.5 from GALEX
;seeing = 0.8829 pix

FWHM_Ha=SXPAR(hdHa,'SEEING')/1.5
photfluxHa=SXPAR(hdHa,'PHOTFLUX')
photplamHa=SXPAR(hdHa,'FILTER1')
hapixsize=SXPAR(hdHa,'XPIXSIZE')

;reading image of nd

fits_read,imnuv[ii],imgnd,hdnd
;fits_read,masknuv[ii], mnd,hdmnd
;mnd=uint(mnd)

;reading image of fd

fits_read,imfuv[ii],imgfd,hdfd
;fits_read,maskfuv[ii],mfd,hdmfd
;mfd=uint(mfd)

;converting mask values into 0 and 1 only
;tempmnd=mnd
;badpix=where(tempmnd ne 1)
;goodpix=where(tempmnd eq 1)
;mnd[badpix] = 1 
;mnd[goodpix]= 0 

;tempmfd=mfd
;badpix=where(tempmfd ne 1)
;goodpix = where(tempmfd eq 1)
;mfd[badpix] = 1
;mfd[goodpix] = 0

;extracting size of R-band image

siz=size(imgR)
nx=long(siz[1])
ny=long(siz[2])

;constructing array for corner coordinates

cornx=[-0.5,-0.5,nx-0.5,nx-0.5]
corny=[-0.5,ny-0.5,ny-0.5,-0.5]

;converting x & y cood into RA and Dec within R-band image

xyad,hdR,cornx,corny,corna,cornd

;extracting true min & max values

mina=max(corna)
maxa=min(corna)
mind=min(cornd)
maxd=max(cornd)

;converting RA and Dec into x & y based on fd image

adxy,hdfd,mina,mind,trminx,trminy
adxy,hdfd,maxa,maxd,trmaxx,trmaxy

;trimming fd image based on the coods

HEXTRACT,imgfd,hdfd,newimgfd,newhdfd,round(trminx),round(trmaxx),round(trminy),round(trmaxy)
;HEXTRACT,mfd,hdmfd,newmfd,newhdmfd,round(trminx),round(trmaxx),round(trminy),round(trmaxy)

;trimming nd image based on the coods
HEXTRACT,imgnd,hdnd,newimgnd,newhdnd,round(trminx),round(trmaxx),round(trminy),round(trmaxy)
;HEXTRACT,mnd,hdmnd,newmnd,newhdmnd,round(trminx),round(trmaxx),round(trminy),round(trmaxy)

;rebinning and aligning R-band image based on fd image header
HASTROM,imgR,hdR,newhdfd,MISSING=0
imgR = imgR * (npscale/rpixsize)^2
HASTROM, mR, hdmR, newhdfd, MISSING=0

;rebinning and aligning Ha image based on fd image header
HASTROM,imgHa,hdHa,newhdfd,MISSING=0
imgHa = imgHa * (npscale/hapixsize)^2
HASTROM, mHa, hdmHa, newhdfd, MISSING=0


;create convolution kernel for FUV

FWHM_convNF=sqrt(FWHM_NUV^2-FWHM_FUV^2) ;calculating kernal size
NFgauss=psf_gaussian(Npixel=35,FWHM=FWHM_convNF,/normalize) 


;create convolution kernel for R

FWHM_convNR=sqrt(FWHM_NUV^2-FWHM_R^2) ; calculating kernal size
NRgauss=psf_gaussian(Npixel=50,FWHM=FWHM_convNR,/normalize) 

;create convolution kernel for Halpha

FWHM_convNHa=sqrt(FWHM_NUV^2-FWHM_Ha^2) ; calculating kernal size
NHagauss=psf_gaussian(Npixel=50,FWHM=FWHM_convNHa,/normalize)

;convolve FUV

newFUV = convolve(newimgfd,NFgauss)
print,'convolving fuv'
;newmFUV = newmfd
;newmFUV = convolve(newmfd,NFgauss)

;tempmfd=newmFUV
;badpixfd=where(tempmfd gt 0.075)
;goodpixfd = where(tempmfd le 0.075)
;if badpixfd ne -1 then begin
;newmFUV[badpixfd] = 0 
;newFUV[goodpixfd]=1
;endif else begin
;newmFUV = 1 
;newmFUV=uint(newmFUV)

;newmNUV = newmFUV
;newmNUV = convolve(newmnd,0) ; NUV masks are identical w/ FUV masks according to Ivy


;convolve R
newR = convolve(imgR, NRgauss)
print,'convolving R'

;newmR = mR
newmR = convolve(mR, NRgauss)
print,'convolving R mask'
tempmR=newmR
badpixr=where(tempmR gt 0.075)
goodpixr = where(tempmR le 0.075)
newmR[badpixr] = 0
newmR[goodpixr] = 1
newmR=uint(newmR)

;convolve Halpha
newHa=convolve(imgHa,NHagauss)
print,'convolving Ha'

;newmHa = mHa
newmHa = convolve(mHa, NHagauss)
print,'convolving Ha mask'
tempmha=newmHa
badpixha =where(tempmha gt 0.075)
goodpixha  = where(tempmha le 0.075)
newmHa[badpixha] = 0
newmHa[goodpixha] = 1
newmHa=uint(newmHa)

nameR=strtrim(imagehome+hname[ii])+'Rsurf.fits'
nameHa=strtrim(imagehome+hname[ii])+'Hasurf.fits'
nameNUV=strtrim(imagehome+hname[ii])+'NUVsurf.fits'
nameFUV=strtrim(imagehome+hname[ii])+'FUVsurf.fits'
namemR=strtrim(imagehome+hname[ii])+'Rmask.fits'
namemHa=strtrim(imagehome+hname[ii])+'Hamask.fits'
namemNUV=strtrim(imagehome+hname[ii])+'NUVmask.fits'
namemFUV=strtrim(imagehome+hname[ii])+'FUVmask.fits'

fits_write, nameR, newR,hdR
fits_write, nameHa, newHa, hdHa
fits_write, nameFUV, newFUV, newhdfd
fits_write, nameNUV, newimgnd, newhdnd
fits_write, namemR, newmR,hdmR
fits_write, namemHa, newmHa, hdmHa
;fits_write, namemFUV, newmFUV, newhdmfd
;fits_write, namemNUV, newmNUV, newhdmnd

endif


endfor


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; commenting out
; generating surface brightness profile data


;for ii = 0, nn-1 do begin

;input files

;imgr=strtrim(hname[ii])+'Rsurf.fits'
;maskr=strtrim(hname[ii])+'Rmask.fits'

;imgha=strtrim(hname[ii])+'Hasurf.fits'
;maskha=strtrim(hname[ii])+'Hamask.fits'

;imgfuv=strtrim(hname[ii])+'FUVsurf.fits'
;maskfuv=strtrim(hname[ii])+'FUVmask.fits'

;imgnuv=strtrim(hname[ii])+'NUVsurf.fits'
;masknuv=strtrim(hname[ii])+'NUVmask.fits'

;fnf1= file_info(imgr)
;fnf2= file_info(maskr)
;fnf3= file_info(imgha)
;fnf4= file_info(maskha)
;fnf5= file_info(imgfuv)
;fnf6= file_info(maskfuv)
;fnf7= file_info(imgnuv)
;fnf8= file_info(masknuv)
;fnf9= file_info(imR[ii])

;fnf_tot = fnf1.exists + fnf2.exists + fnf3.exists + fnf4.exists + fnf5.exists + fnf6.exists $
;         + fnf7.exists + fnf8.exists + fnf9.exists

;if fnf_tot eq 9 then begin
 
;original files and profile files for aperture information

;origr=imR[ii]

;profr=strtrim(dnamer[ii]+strtrim(strmid(fnamer[ii],0,gg[ii])) + '_brightness.profile')

;profha=strtrim(dnameha[ii]+strtrim(strmid(fnameha[ii],0,gg[ii])) + '_brightness.profile')

; extract parameters from brightness profile
; pa, ar, center, maximum radii

;read_profile_header, profr, tabler
;pixsize=tabler.pixsize
;centerx = tabler.xcenter
;centery = tabler.ycenter
;pa = tabler.pa
;ar = tabler.axerat
;sky = tabler.skylev
;rphot = tabler.flux_scale
;np = 1

;read_profile_header, profha, tableha
;haphot = tableha.flux_scale
;skyha = tableha.skylev
;skyrmsha = tableha.err_sky_box

;pfplt_extractprof,profr,0, sma,fint,df,altngood,altnbad,altsb,esb,drfaw,efintsk,efintcn

;nl = n_elements(sma)
;flag=make_array(nl)


;for kk = 0, nl-1 do begin ; insert here for future expansion

;if kk eq 0 then goto, jump1
;if sma[kk] lt sma[kk-1] then np = np + 1
;flag[kk] = np

;print,np

;jump1:

;endfor ; kk

;maxr=sma[nl-1]
;maxnew=maxr* pixsize/npscale

;converting center location into x & y (should be done earlier)
;converting maximum radius to final pixel scale

;fits_read,imgr,Rimg,Rhd
;fits_read,maskr,Rmask,Rmhd ; reading R mask and converting pixel values 1 as mask, 0 as good
;tempmask=Rmask
;badpix=where(Rmask eq 0)
;goodpix=where(Rmask ne 0)
;Rmask[badpix] = 1
;Rmask[goodpix]= 0

;tempmask=double(tempmask)
;tempimg= Rimg * tempmask

;fits_read,origr, origimg, orighd

;xyad, orighd, centerx, centery, centera, centerd
;adxy, Rhd, centera, centerd, ctx, cty

;read ha, fuv, nuv images and masks

;fits_read, imgha, haimg, hahd
;fits_read, maskha, hamask, hamhd

;badpix=where(hamask eq 0)
;goodpix=where(hamask ne 0)
;hamask[badpix] = 1
;hamask[goodpix]= 0

;fits_read, imgfuv, fuvimg, fuvhd
;fits_read, maskfuv, fuvmask, fuvmhd

;badpix=where(fuvmask eq 0)
;goodpix=where(fuvmask ne 0)
;fuvmask[badpix] = 1
;fuvmask[goodpix]= 0

;fits_read, imgnuv, nuvimg, nuvhd
;fits_read, masknuv, nuvmask, nuvmhd

;badpix=where(nuvmask eq 0)
;goodpix=where(nuvmask ne 0)
;nuvmask[badpix] = 1
;nuvmask[goodpix]= 0

; extract exposure time of UV images
;exptimefuv = SXPAR (fuvhd, 'EXPTIME')
;exptimenuv = SXPAR (nuvhd, 'EXPTIME')
 
;generating radius bin
; to maximum radius w/ prescribed step size increasement

;n_ell=fix(maxnew/npscale)
;print,n_ell
;radbin=make_array(n_ell,/double,value=0)

;for ii=0, n_ell-1 do begin

;radbin[ii]=(ii+1)*(5.6/npscale)

;endfor
;print,testbin

;run calc_growth

;calc_growth, Rimg, Rmask, radbin, ctx, cty, sky, pa, 1, 1/ar, R_Ngood, R_Nbad, R_flux, R_flux_g, R_sb, R_sb_sig
;
;calc_growth, haimg, hamask, radbin, ctx, cty, skyha, pa, 1, 1/ar, ha_Ngood, ha_Nbad, ha_flux, ha_flux_g,ha_sb, ha_sb_sig

;calc_growth, fuvimg, Rmask, radbin, ctx, cty, 0, pa, 1, 1/ar, fuv_Ngood, fuv_Nbad, fuv_flux, fuv_flux_g,fuv_sb, fuv_sb_sig

;calc_growth, nuvimg, Rmask, radbin, ctx, cty, 0, pa, 1, 1/ar, nuv_Ngood, nuv_Nbad, nuv_flux, nuv_flux_g, nuv_sb, nuv_sb_sig

;nr = n_elements(radbin)
;newradbin=make_array(nr,/float, value=0)

;newhasb=make_array(nr,/float, value=0)
;ehasb = make_array(nr,/float, value=0)

;newfuvsb=make_array(nr,/float, value=0)
;efuvsb = make_array(nr,/float, value=0)

;newnuvsb=make_array(nr,/float, value=0)
;enuvsb = make_array(nr,/float, value=0)

;fuvab=make_array(nr,/float, value=0)
;efuvab=make_array(nr,/float, value=0)

;nuvab=make_array(nr,/float, value=0)
;enuvab=make_array(nr,/float, value=0)


;for ii = 0, nr -1 do begin
 
;newradbin[ii]=radbin[ii] * npscale


;newrsb[ii] = R_sb[ii] / npscale^2 * rphot
;ersb[ii] = sqrt((R_sb_sig[ii]/npscale^2*rphot)^2 + (skyrms*rphot/npscale^2)^2)

;newhasb[ii]=ha_sb[ii] / npscale^2 * haphot
;ehasb[ii]= sqrt((cntrat*newrsb[ii])^2+(skyrmsha*haphot/npscale^2)^2)

;;newfuvsb[ii]=fuv_sb[ii] / npscale^2 * fuvphot
;newfuvsb[ii] = (fuv_sb[ii] - skylevfuv[ii]/fuvphot*npscale^2)/npscale^2*fuvphot ; sky subtraction
;;efuvsb[ii] = sqrt((sqrt(fuv_flux[ii]+(skylevfuv[ii]/fuvphot)*(fuv_Ngood[ii]+fuv_Nbad[ii]) $
;              * npscale^2) * fuvphot / (fuv_Ngood[ii]+fuv_Nbad[ii])/ npscale^2 $
;              / sqrt(exptimefuv))^2 + skyrmsfuv[ii]^2)
;efuvsb[ii] = sqrt((sqrt(fuv_flux[ii])*fuvphot/(fuv_Ngood[ii]+fuv_Nbad[ii])/npscale^2 $
;              / sqrt(exptimefuv))^2 + skyrmsfuv[ii]^2)

;;newnuvsb[ii]=nuv_sb[ii] / npscale^2 * nuvphot
;newnuvsb[ii] = (nuv_sb[ii] - skylevnuv[ii]/nuvphot*npscale^2)/npscale^2*nuvphot ; sky subtraction
;enuvsb[ii] = sqrt((sqrt(nuv_flux[ii]+(skylevnuv[ii]/nuvphot)*(nuv_Ngood[ii]+nuv_Nbad[ii]) $
;              * npscale^2) * nuvphot / (nuv_Ngood[ii]+nuv_Nbad[ii])/ npscale^2 $
;              / sqrt(exptimenuv))^2 + skyrmsnuv[ii]^2)
;enuvsb[ii] = sqrt((sqrt(nuv_flux[ii])*nuvphot/(nuv_Ngood[ii]+nuv_Nbad[ii])/npscale^2 $
;              / sqrt(exptimenuv))^2 + skyrmsnuv[ii]^2)

;fuvab[ii] = -2.5 * alog10(fuv_sb[ii]/npscale^2) + 18.82
;fuvab[ii] = -2.5 * alog10 ((fuv_sb[ii] - skylevfuv[ii]/fuvphot*npscale^2)/npscale^2) + 18.82
;efuvab[ii] = 2.5 * efuvsb[ii] / newfuvsb[ii] / alog(10)

;nuvab[ii] = -2.5 * alog10(nuv_sb[ii]/npscale^2) + 20.08
;nuvab[ii] = -2.5 * alog10 ((nuv_sb[ii] - skylevnuv[ii]/nuvphot*npscale^2)/npscale^2) + 20.08 
;enuvab[ii] = 2.5 * enuvsb[ii] / newnuvsb[ii] / alog(10)

;endfor

;openw,1,strtrim(hipname[ii]+'.surfprof.txt')
;printf, 1, '#####################################################'+hipname[ii]
;printf, 1, '      radius        S(Halpha)        e_S(Halpha)     S(FUV)             e_S(FUV)           S(NUV)             e_S(NUV)           AB(FUV)        e_AB(FUV)         AB(NUV)          e_AB(NUV)'
;for ii = 0, nr -1 do begin
;printf, 1, newradbin[ii],newhasb[ii], ehasb[ii], newfuvsb[ii], efuvsb[ii], newnuvsb[ii], $
;           enuvsb[ii],fuvab[ii], efuvab[ii], nuvab[ii], enuvab[ii],$
;           format='(f17, e17,e17,e17,e17,e17,e17,f17,f17,f17,f17)'

;endfor
;close,1

;hardfile = strtrim(hipname[ii]+'.surfprof.eps')
;plot surface brightness and compare


;   IF keyword_set(hardfile) THEN BEGIN
;      xs    = 8.0
;      ys    = xs/(2.*aspect)
;      yoff  = 8.0
;      xoff  = 3.0
;      thick = 2
;      set_plot,'ps',/copy, /interpolate
;      IF strpos(strlowcase(hardfile), '.eps') GT 0 THEN $
;         device,/inches,xs=xs,ys=ys,yo=yoff,xo=xoff,/color,/encapsulated ELSE $
;         device,/inches,xs=xs,ys=ys,yo=yoff,xo=xoff,/color
;      charsize = 0.6*charsize
;      symsize  = 0.6*symsize
;   ENDIF ELSE BEGIN
;      wxsize   = 1200
;      wysize   = fix(float(wxsize/(2.0*aspect)))       
;      charsize = charsize*wxsize/800.0
;      symsize  = symsize*wxsize/800.0       
;      thick    = 2
;      window, 0, xsize=wxsize, ysize=wysize
;   ENDELSE
;   setplotcolors
   ;
   ; plot surface brightnss results
;   !p.noerase = 0
;   multiplot, [1,3]
;   !p.multi   = [1, 3, 1]  
   

;   plot, newradbin, alog10(newhasb), color=!black
;   oplot, newradbin, alog10(newfuvsb), color=!blue
;   oplot, newradbin, alog10(newnuvsb), color=!red
;   plot, llumrf[jj1], lewf[jj1], xrange=lrrange, yrange=lewrangef, xstyle=1, ystyle=1, psym=sym(1), $
;         xtitle=xtitle, ytitle=ytitlef, charsize=charsize, symsize=symsize, $
;         thick=thick,xthick=thick, ythick=thick, charthick=thick
;   xx         = lrrange
;   yy         = aa1[2] + bb1[2]*xx
;   oplot, xx, yy, color=!black, thick=thick+1
;   yy         = yy - nsigma*sigy1[2]
;   oplot, xx, yy, color=!black, linestyle=2, thick=thick
;   yy         = yy + 2.0*nsigma*sigy1[2]
;   oplot, xx, yy, color=!black, linestyle=2, thick=thick
;   oploterror_old, llumrf[jj1], lewf[jj1], emabsrf[jj1], elewf[jj1], /nohat, errthick=errthick, errcolor=!black, psym=sym(1)
;   oplot, llumrf, lewf, symsize=symsize, color=!black, psym=sym(1)
;   xlab       = lrrange[0] + 0.05*(lrrange[1] - lrrange[0])
;   ylab       = lewrangef[0] + 0.9*(lewrangef[1] - lewrangef[0])
;   xyouts, xlab, ylab, '!3 a', alignment=0.5, charsize=charsize, charthick=thick


;   !p.multi = [1,3,2]
;   multiplot
;   plot, newradbin, alog10(newhasb/newfuvsb), color=!black

;   !p.multi = [1,3,3]
;   multiplot
;   plot, newradbin, fuvab - nuvab , color=!black
;snap_jpg, 'testprof.jpg'

;If keyword_set(hardfile) then BEGIN
; psend, hardfile, /noprint, /clobber
;ENDIF

;endif ; fnf_tot

;endfor

END
