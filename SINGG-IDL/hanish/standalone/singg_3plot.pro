PRO singg_3plot ; ,obj,runid

obj = 'J1337-29'
runid = 'Run03'

; Makes three plots showing the combined images.

  p4dir = STRTRIM(runid,2)+'/Proc4/'+STRTRIM(obj,2)+'/'
  
  spawn,'ls '+p4dir+'J*_?_ss.fits',Rlist
  Rfile = Rlist[0]
  spawn,'ls '+p4dir+'J*_6???_ss.fits',Nlist
  Nfile = Nlist[0]
  spawn,'ls '+p4dir+'J*_?sub_ss.fits',Slist
  Sfile = Slist[0]
  spawn,'ls '+p4dir+'J*_ellipse.dat',elllist
  ellfile = elllist[0]
  read_ellipse_file,ellfile,n_ellipses,refimage,Dx,Dy, $
                    Px,Py,pa,a_i,b_i,z_s,z_f,z_c
  a = a_i*z_s
  b = b_i*z_s

  fits_read,Rfile,Rimg,Rhd
  fits_read,Nfile,Nimg,Nhd
  fits_read,Sfile,Simg,Shd

  Rskysig = SXPAR(Rhd,'SKYSIG')
  Nskysig = SXPAR(Nhd,'SKYSIG')
  Sskysig = SXPAR(Shd,'SKYSIG')

  getrot,Shd,rot,cdelt 
  cdelt  = abs(cdelt)*3600.
  as_pix = cdelt[0]

  buffer = SXPAR(Shd,'BUFFER')
  Px = Px + buffer
  Py = Py + buffer
  Dx = Dx + buffer
  Dy = Dy + buffer
  theta = (pa-90.0)*!dtor
  sz = SIZE(Simg)

  im1 = sz[1]-2*buffer
  im2 = sz[2]-2*buffer

  output = DBLARR(3*im1,im2)

  output[0:im1-1,*] = (Rimg[buffer:sz[1]-buffer-1,buffer:sz[2]-buffer-1]/Rskysig)
  output[im1:2*im1-1,*] = (Nimg[buffer:sz[1]-buffer-1,buffer:sz[2]-buffer-1]/Nskysig)
  output[2*im1:3*im1-1,*] = (Simg[buffer:sz[1]-buffer-1,buffer:sz[2]-buffer-1]/Sskysig)

  plotsig = [0.0,10.0]

  set_plot,'PS'
  setplotcolors
;  setbgfg,!white,!black
  xs = 9.0
  ys = 3.0
  DEVICE,/portrait,/inches,xs=xs,ys=ys,yo=yoff,xo=xoff, $
          bits_per_pixel=4,/encapsulated
  
  tvscale,SSQRT(output),POSITION=[0.0,0.0,1.0,1.0], $
          MAXVALUE=SSQRT(plotsig[1]),MINVALUE=SSQRT(plotsig[0]), $
          BOTTOM=!ngrays-1,TOP=0
;;          BOTTOM=0,TOP=!ngrays-1

  bar = 1.0/3.0

  PLOTS,[bar,bar],[0.0,1.0],!black,/NORMAL,THICK=2.0
  PLOTS,2.0*[bar,bar],[0.0,1.0],!black,/NORMAL,THICK=2.0
  PLOTS,[0.0,0.0,1.0,1.0,0.0],[0.0,1.0,1.0,0.0,0.0],!black,/NORMAL,THICK=3.0
;  PLOTS,[0.001,0.001,0.999,0.999,0.001],[0.002,0.998,0.998,0.002,0.002],!black,/NORMAL,THICK=3.0

  XYOUTS,[0.025,bar+0.025,2.0*bar+0.025],0.9,['(a)','(b)','(c)'],COLOR=0,CHARSIZE=1.5,CHARTHICK=2.0,ALIGNMENT=0.5,/NORMAL
; Should be color=!black, but in 4bit this fails for some reason and
; draws the letters in white.

  psend,'/home/hanish/thesis/m83.eps',/noprint,/clobber

spawn,'display ~/thesis/m83.eps &'

  RETURN
END
