PRO playmask2_ngc3887

; since we want to compare all images to the image with the
; worst seeing (as we don't want to make the image with
; the worst seeing any worse), we will scale the R-band
; and H-alpha image to the NUV image

; discovered that HASTROM, HEXTRACT, HREBIN & XYXY from
; idlastro libraries could make my life a lot easier

fits_read,"J1147-16_R_ss.fits",img,hd

cpscale=0.4344 ; pixel scale of R-band/H-alpha images
npscale=1.5  ; pix scale of UV images

scaling=cpscale/npscale;
HREBIN,img,hd,OUT=[2348*scaling,2348*scaling]


siz=size(img)
nx=long(siz[1])
ny=long(siz[2])

cornx=[-0.5,-0.5,nx-0.5,nx-0.5]
corny=[-0.5,ny-0.5,ny-0.5,-0.5]

xyad,hd,cornx,corny,corna,cornd


mina=max(corna)
maxa=min(corna)
mind=min(cornd)
maxd=max(cornd)

print,mina
print,maxa
print,mind
print,maxd




fits_read,"GI1_009058_NGC3887_0001-fd-int.fits",refimg,refhd

;exposure=SXPAR(refhd,'EXPTIME')
;refimg=refimg*exposure


; must trim uv image before doing comparison otherwise 
; the r-band image will have a black mask surrounding it!

; note that XYAD & ADXY have been used interatively to determine
; the edges of the r-band image and then translated to the relevant
; sides on the uv image

adxy,refhd,mina,mind,trminx,trminy
adxy,refhd,maxa,maxd,trmaxx,trmaxy


HEXTRACT,refimg,refhd,newrefimg,newrefhd,trminx,trmaxx,trminy,trmaxy

fits_write,"ngc3887-fd-trimeed.fits",newrefimg,newrefhd

HASTROM,img,hd,newrefhd,MISSING=0

fits_write,"ngc3887-R_scaled.fits",img,hd

END
