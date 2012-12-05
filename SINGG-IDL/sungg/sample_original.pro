PRO sample

; since we want to compare all images to the image with the
; worst seeing (as we don't want to make the image with
; the worst seeing any worse), we will scale the R-band
; and H-alpha image to the NUV image

;summary of constants
cpscale=0.4344 ; pixel scale of R-band/H-alpha images
npscale=1.5  ; pix scale of UV images

scaling=cpscale/npscale;

photflamNUV=2.06e-16
photplamNUV=2315.7 ;\A

photflamFUV=1.40e-15
photplamFUV=1538.6 ;\A

FWHM_FUV=4.0/1.5
FWHM_NUV=5.6/1.5


singhome='/data5/sungg/meurer/SINGG/'
sunghome='/data2/sungg/iwong/GALEX_DATA/'

;Gerhardt, edit the following four lines for sample fits files

imR='/data5/sungg/meurer/SINGG/Run06/Proc4/J0240-08_R_ss.fits'
imha='/data5/sungg/meurer/SINGG/Run06/Proc4/J0240-08_Rsub_ss.fits'
imnuv='/data2/sungg/iwong/GALEX_DATA/ARCHIVE/MISDR1_18475_0455/MISDR1_18475_0455-nd-intbgsub.fits.gz'
imfuv='/data2/sungg/iwong/GALEX_DATA/ARCHIVE/MISDR1_18475_0455/MISDR1_18475_0455-fd-intbgsub.fits.gz'
name='j0240-08'

;imR='/data5/sungg/meurer/SINGG/Run12/Proc4/J0140-05_R_ss.fits'
;imha='/data5/sungg/meurer/SINGG/Run12/Proc4/J0140-05_Rsub_ss.fits'
;imnuv='/data2/sungg/iwong/GALEX_DATA/RELEASE5/21046-GI1_009012_HPJ0140m05/01-main/05-try/GI1_009012_HPJ0140m05-nd-intbgsub.fits.gz'
;imfuv='/data2/sungg/iwong/GALEX_DATA/RELEASE5/21046-GI1_009012_HPJ0140m05/01-main/05-try/GI1_009012_HPJ0140m05-fd-intbgsub.fits.gz'
;name='j0140-05'

;obtaining PSF from R
;seeing = 1.4946313 arcsec
;original pixel size = 0.4344 arcsec/pix
;rebinned pixel size = 1.5 from GALEX
;seeing = 0.996 pix

fits_read,imR,imgR,hdR

FWHM_R=SXPAR(hdR,'SEEING')/1.5
photflamR=SXPAR(hdR,'PHOTFLAM')
photplamR=SXPAR(hdR,'PHOTPLAM')

;reading image of Ha

fits_read,imha,imgHa,hdHa

;obtain PSF from Halpha
;seeing = 1.3243071 arcsec
;original pixel size = 0.4344 arcsec/pix
;rebinned pixel size = 1.5 from GALEX
;seeing = 0.8829 pix

FWHM_Ha=SXPAR(hdHa,'SEEING')/1.5
photfluxHa=SXPAR(hdHa,'PHOTFLUX')
photplamHa=SXPAR(hdHa,'FILTER1')

;reading image of nd

fits_read,imnuv,imgnd,hdnd

;reading image of fd

fits_read,imfuv,imgfd,hdfd

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

;trimming nd image based on the coods
HEXTRACT,imgnd,hdnd,newimgnd,newhdnd,round(trminx),round(trmaxx),round(trminy),round(trmaxy)

;rebinning and aligning R-band image based on fd image header
HASTROM,imgR,hdR,newhdfd,MISSING=0

;rebinning and aligning Ha image based on fd image header
HASTROM,imgHa,hdHa,newhdfd,MISSING=0

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

newFUV=convolve(newimgfd,NFgauss)

;convolve R
newR=convolve(imgR,NRgauss)

;convolve Halpha
newHa=convolve(imgHa,NHagauss)

beta=-1 ; 


;for imageset Ha, NUV, and FUV

im1=newha
tmp=size(im1,/dimensions)
nx=tmp(0)
ny=tmp(1)
RGBim1=fltarr(nx,ny,3)
RGBim1[*,*,0]=im1
RGBim1[*,*,1]=newimgnd
RGBim1[*,*,2]=newFUV

maxR=ssqrt(1e-17)
maxHa=10e-9
maxNUV=maxR*(photplamNUV/photplamR)^beta
maxFUV=maxR*(photplamFUV/photplamR)^beta

minR=ssqrt(photflamR*0.0)
minHa=minR*(photplamHa/photplamR)^beta
minNUV=minR*(photplamNUV/photplamR)^beta
minFUV=minR*(photplamFUV/photplamR)^beta

RGBimr=ssqrt(photfluxHa*RGBim1[*,*,0])
RGBimg=ssqrt(photflamNUV*RGBim1[*,*,1])
RGBimb=ssqrt(photflamFUV*RGBim1[*,*,2])

img1r=bytscl(RGBimr,min=minHa,max=maxHa)
img1g=bytscl(RGBimg,min=minNUV,max=maxNUX)
img1b=bytscl(RGBimb,min=minFUV,max=maxFUV)

RGBim=make_array(nx,ny,3,/byte)
RGBim[*,*,0]=img1r
RGBim[*,*,1]=img1g
RGBim[*,*,2]=img1b

xsize=nx
ysize=ny

window,0,/pixmap,xsize=xsize,ysize=ysize
tv,RGBim,true=3

im=tvrd(true=3)

name5=strtrim(name)+'_hnf_1.jpg'
write_jpeg,name5,im,TRUE=3,quality=100

maxR=ssqrt(2.5e-18)
maxHa=5e-9
maxNUV=maxR*(photplamNUV/photplamR)^beta
maxFUV=maxR*(photplamFUV/photplamR)^beta

minR=ssqrt(photflamR*0.0)
minHa=minR*(photplamNUV/photplamR)^beta
minNUV=minR*(photplamNUV/photplamR)^beta
minFUV=minR*(photplamFUV/photplamR)^beta

img2r=bytscl(RGBimr,min=minHa,max=maxHa)
img2g=bytscl(RGBimg,min=minNUV,max=maxNUV)
img2b=bytscl(RGBimb,min=minFUV,max=maxFUV)

RGBim=make_array(nx,ny,3,/byte)
RGBim[*,*,0]=img2r
RGBim[*,*,1]=img2g
RGBim[*,*,2]=img2b


xsize=nx
ysize=ny

window,0,/pixmap,xsize=xsize,ysize=ysize
tv,RGBim,true=3

im=tvrd(true=3)

name6=strtrim(name)+'_hnf_2.jpg'
write_jpeg,name6,im,TRUE=3,quality=100


END
