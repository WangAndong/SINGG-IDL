PRO surfprofgen_test,hardfile=hardfile

;physical parameter


npscale=1.5  ; pix scale of UV images

fuvphot = 1.4e-15 ; photometric conversion factor from cps to flux for FUV
nuvphot = 2.06e-16 ; photometric conversion factor from cps to flux for NUV

;plotting parameter

aspect=1.1
charsize = 1.7
symsize = 1.0

ng = 2 ; number of galaxies to test

ecntrat = make_array(ng, /float, value=0)
skylevfuv = make_array(ng, /float,  value=0)
skylevnuv = make_array(ng, /float, value=0)
skyrmsfuv = make_array(ng, /float, value=0)
skyrmsnuv = make_array(ng, /float, value=0)
exptimefuv = make_array(ng, /float, value=0)
exptimenuv = make_array(ng, /float, value=0)
skyrmsr = make_array(ng, /float, value=0)
skyrmsha = make_array(ng, /float, value=0)

imgr = make_array(ng, /string, value='')
maskr= make_array(ng, /string, value='')
imgha = make_array(ng, /string, value='')
maskha = make_array(ng, /string, value='')
imgfuv = make_array(ng, /string, value='')
imgnuv = make_array(ng, /string, value='')
gname=  make_array(ng, /string, value=' ')

origr = make_array(ng, /string, value='')
profr = make_array(ng, /string, value='')
profha = make_array(ng, /string, value='')
proffuv = make_array(ng, /string, value='')
surfprofname = make_array(ng, /string, value='')

;for J2257-41
;names

gname[0] = 'J2257-41'
imgr[0]='J2257-41Rsurf.fits'
maskr[0]='J2257-41Rmask.fits'
imgha[0]='J2257-41Hasurf.fits'
maskha[0]='J2257-41Hamask.fits'
imgfuv[0]='J2257-41FUVsurf.fits'
imgnuv[0]='J2257-41NUVsurf.fits'

;constants
ecntrat[0] = 0.00228092 ; error in count ratio for continuum subtraction 
skylevfuv[0] = 3.814e-14
skylevnuv[0] = 7.41e-14
skyrmsfuv[0] =  2.131e-20
skyrmsnuv[0] =  2.113e-20
skyrmsr[0] = 0.0178
skyrmsha[0] = 0.000661024

exptimefuv[0] = 2380.0
exptimenuv[0] = 2380.0

;temporary comparison files and profile files for aperture information
origr[0]='/data5/sungg/meurer/SINGG/Run06/Proc4/J2257-41_R_ss.fits'
profr[0]='/data5/sungg/meurer/SINGG/Run06/Proc4/J2257-41_R_ss_brightness.profile'
profha[0]='/data5/sungg/meurer/SINGG/Run06/Proc4/J2257-41_Rsub_ss_brightness.profile'
proffuv[0]='/data3/sungg/iwong/WORKING/RAD_PROFILES/NGC7424.fuv.rpc.dat'

; second example J0317-41

gname[1] = 'J0317-41'
imgr[1]='J0317-41Rsurf.fits'
maskr[1]='J0317-41Rmask.fits'
imgha[1]='J0317-41Hasurf.fits'
maskha[1]='J0317-41Hamask.fits'
imgfuv[1]='J0317-41FUVsurf.fits'
imgnuv[1]='J0317-41NUVsurf.fits'

ecntrat[1] = 0.00217704 ; error in count ratio for continuum subtraction
skylevfuv[1] = 5.199e-14
skylevnuv[1] = 5.867e-14
skyrmsfuv[1] =  3.895e-20
skyrmsnuv[1] =  5.917e-20
skyrmsr[1] = 0.218502
skyrmsha[1] = 0.00750027

exptimefuv[1] = 1675.0
exptimenuv[1] = 1675.0

;temporary comparison files and profile files for aperture information
origr[1]='/data5/sungg/meurer/SINGG/Run06/Proc4/J0317-41_R_ss.fits'
profr[1]='/data5/sungg/meurer/SINGG/Run06/Proc4/J0317-41_R_ss_brightness.profile'
profha[1]='/data5/sungg/meurer/SINGG/Run06/Proc4/J0317-41_Rsub_ss_brightness.profile'
proffuv[1]= '/data3/sungg/iwong/WORKING/RAD_PROFILES/NGA_NGC1291.fuv.rpc.dat'

for jj = 0, 0 do begin

; extract parameters from brightness profile
; pa, ar, center, maximum radii

read_profile_header, profr[jj], tabler
pixsize=tabler.pixsize
centerx = tabler.xcenter
centery = tabler.ycenter
pa = tabler.pa
ar = tabler.axerat

np = 1
rphot = tabler.flux_scale
sky_R = skyrmsr[jj] * rphot / pixsize^2 ; cgs

read_profile_header, profha[jj], tableha
haphot = tableha.flux_scale

restore, proffuv[jj]
npixbguv = npixbkgd

sky_ha = skyrmsha[jj] * haphot / pixsize^2 ; cgs

sky_fuv = skylevfuv[jj] / fuvphot /npixbguv; dn
sky_nuv = skylevnuv[jj] / nuvphot /npixbguv; dn

print, sky_fuv, sky_nuv

;stop

pfplt_extractprof,profr[jj],0, sma,fint,df,altngood,altnbad,altsb,esb,drfaw,efintsk,efintcn

nl = n_elements(sma)
flag=make_array(nl)

for kk = 0, nl-1 do begin ; insert here for future expansion

if kk eq 0 then goto, jump1
if sma[kk] lt sma[kk-1] then np = np + 1
flag[kk] = np

;print,np

jump1:

endfor ; kk

maxr=sma[nl-1]
maxnew=maxr* pixsize/npscale

;converting center location into x & y (should be done earlier)
;converting maximum radius to final pixel scale
fits_read,imgr[jj],Rimg,Rhd

;fits_read,maskha, hamask,hamhd
fits_read,origr[jj], origimg, orighd

xyad, orighd, centerx, centery, centera, centerd
adxy, Rhd, centera, centerd, ctx, cty

;print,pixsize,centerx,centery, pa, ar, maxr,maxnew, ctx, cty

;read ha, fuv, nuv images

; reading R and Ha masks, combining them and converting pixel values 1 as mask, 0 as good, 
fits_read, imgha[jj], haimg, hahd

fits_read,maskr[jj],Rmask,Rmhd ; reading R mask and converting pixel values 1 as mask, 0 as good

fits_read, maskha[jj], hamask, hamhd

tempmask= Rmask + hamask

masksize = size(tempmask)
mx = long(masksize[1])
my = long(masksize[2])

mastermask=make_array(mx, my, /float)

badpix=where(tempmask eq 0)
goodpix=where(tempmask ne 0)
mastermask[badpix] = 1
mastermask[goodpix]= 0

; reading UV images

fits_read, imgfuv[jj], fuvimg, fuvhd
fits_read, imgnuv[jj], nuvimg, nuvhd

;generating radius bin
; to maximum radius w/ prescribed step size increasement

n_ell=fix(maxnew/npscale)
;print,n_ell
radbin=make_array(n_ell,/double,value=0)

for ii=0, n_ell-1 do begin

radbin[ii]=(ii+1)*(5.6/npscale)

endfor
;print,testbin


;run calc_growth

calc_growth, Rimg, mastermask, radbin, ctx, cty, 0, pa, 1, 1/ar, R_Ngood, R_Nbad, R_flux, R_flux_g, R_sb, R_sb_sig

calc_growth, haimg, mastermask, radbin, ctx, cty, 0, pa, 1, 1/ar, ha_Ngood, ha_Nbad, ha_flux, ha_flux_g,ha_sb, ha_sb_sig

calc_growth, fuvimg, mastermask, radbin, ctx, cty, 0, pa, 1, 1/ar, fuv_Ngood, fuv_Nbad, fuv_flux, fuv_flux_g,fuv_sb, fuv_sb_sig

calc_growth, nuvimg, mastermask, radbin, ctx, cty, 0, pa, 1, 1/ar, nuv_Ngood, nuv_Nbad, nuv_flux, nuv_flux_g, nuv_sb, nuv_sb_sig

nn = n_elements(radbin)
newradbin=make_array(nn,/float, value=0)

newrsb=make_array(nn,/float, value=0)
ersb = make_array(nn,/float, value=0)

newhasb=make_array(nn,/float, value=0)
ehasb = make_array(nn,/float, value=0)
altehasb=  make_array(nn,/float, value=0)

newfuvsb=make_array(nn,/float, value=0)
efuvsb = make_array(nn,/float, value=0)

newnuvsb=make_array(nn,/float, value=0)
enuvsb = make_array(nn,/float, value=0)

fuvab=make_array(nn,/float, value=0)
efuvab=make_array(nn,/float, value=0)

nuvab=make_array(nn,/float, value=0)
enuvab=make_array(nn,/float, value=0)

fsfr=make_array(nn,/float, value=0)
hasfr=make_array(nn,/float, value=0)

for ii = 0, nn -1 do begin
 
newradbin[ii] = radbin[ii] * npscale

;newrsb[ii] = R_sb[ii] / npscale^2 * rphot
newrsb[ii] = R_flux_g[ii] /R_Ngood[ii] * rphot / npscale ^2

;ersb[ii] = sqrt((R_sb_sig[ii]/npscale^2*rphot)^2 + (skyrms*rphot/npscale^2)^2) 
ersb[ii] = sqrt((R_sb_sig[ii] * rphot / (npscale^2))^2 + (sky_R)^2)


;newhasb[ii]=ha_sb[ii] / npscale^2 * haphot
newhasb[ii] = ha_flux_g[ii] /ha_Ngood[ii] /npscale^2 * haphot
 
ehasb[ii]= sqrt((ecntrat[jj]*R_flux_g[ii]*haphot/ha_Ngood[ii]/(npscale^2))^2 + sky_ha^2)

;altehasb[ii]= sqrt((ecntrat*newrsb[ii]/haphot*rphot)^2 + skyrmsha^2)

newfuvsb[ii]= (fuv_flux_g[ii]-fuv_Ngood[ii]*sky_fuv)/fuv_Ngood[ii] / npscale^2 * fuvphot

efuvsb[ii] = sqrt((sqrt(fuv_flux_g[ii])$
             / sqrt(exptimefuv[jj]) / fuv_Ngood[ii] / (npscale^2) * fuvphot )^2 $
             +skyrmsfuv[jj]^2)

newnuvsb[ii]= (nuv_flux_g[ii]-nuv_Ngood[ii]*sky_nuv) / nuv_Ngood[ii] / npscale^2 * nuvphot

enuvsb[ii] = sqrt((sqrt(nuv_flux_g[ii]) $
             / sqrt(exptimenuv[jj]) / nuv_Ngood[ii] / (npscale^2) * nuvphot ) ^2 $
             +skyrmsnuv[jj]^2)
 
fuvab[ii] = -2.5 * alog10(newfuvsb[ii]/fuvphot) + 18.82
efuvab[ii] = 2.5 * efuvsb[ii] / newfuvsb[ii] / alog(10)

nuvab[ii] = -2.5 * alog10(newnuvsb[ii]/nuvphot) + 20.08
enuvab[ii] = 2.5 * enuvsb[ii] / newnuvsb[ii] / alog(10)

endfor
;stop

surfprofname[jj]= strtrim(gname[jj]+'surfprof.txt')

openw,1,surfprofname[jj]
printf, 1, format='("#test ", a10)', gname[jj]

for ii = 0, nn -1 do begin
printf, 1, newradbin[ii],newrsb[ii], ersb[ii], newhasb[ii], ehasb[ii], $
           newfuvsb[ii], efuvsb[ii], newnuvsb[ii], enuvsb[ii],$
           fuvab[ii], efuvab[ii], nuvab[ii], enuvab[ii], altehasb[ii],$
           format='(f17, e17, e17, e17,e17,e17,e17,e17,e17,f17,f17,f17,f17,f17)'

endfor
close,1

endfor ; jj
;plot surface brightness and compare


END
