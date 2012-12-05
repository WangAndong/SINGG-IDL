PRO test_shunt
   ; 
   fgrism    = 'grism_edgemsk.fits'
   fsquash   = 'grism_rebin.fits'
   fdirect   = 'sn_epoch1_zi_drz.fits'
   nbin      = 25
   xbin      = nbin
   dxrange   = [-100, 80]
   dyrbn     = 11
   dyspec    =  5
   dxcrcut   = 31
   dxstamp   = 125
   dystamp   = 125
;   xsq       = 30.965   
;   ysq       = 1994.938
   xsq       =  115.563
   ysq       =  1268.89  
   zrange_gr = [-100.000, 4000.00]
   zrange_sq = [-10000.0, 100000.0]
   zrange_d  = [-0.005, 0.050]
   id        = 999
   ;
   ; open fits images
   fits_read, fgrism, im_grism, hdrg
   fits_read, fsquash, im_squash, hdrs
   fits_read, fdirect, im_direct, hdrd
   ;
   ; extract 2d cutouts and 1d collapsed arrays
   shunt_extract, im_grism, im_squash, nbin, xsq, ysq, dxrange, dyrbn, dyspec, dxcrcut, dxstamp, dystamp, $
                   gr_ribbon, gr_spec, gr_crcut, sq_stamp, lim_ribbon, lim_spec, lim_crcut, lim_stamp
   ;
   ; plot extractions
   shunt_plot, id, xsq, ysq, gr_ribbon, gr_spec, gr_crcut, sq_stamp, $
               lim_ribbon, lim_spec, lim_crcut, lim_stamp, $
               zrange_gr, zrange_sq
   im = tvrd(true=3)
   WRITE_JPEG,'test.jpg',im,TRUE=3,QUALITY=100
   hardfile = 'test.ps'
   shunt_plot, id, xsq, ysq, gr_ribbon, gr_spec, gr_crcut, sq_stamp, $
               lim_ribbon, lim_spec, lim_crcut, lim_stamp, $
               zrange_gr, zrange_sq, hardfile=hardfile
   ;
   ; classify
   cl = shunt_classify()
   ;
   ; test direct image extraction
   xoffset      = -65
   xdirect      = make_array(1, /float, value=(xsq*float(xbin) + 0.5*(xbin + 1) + xoffset))
   ydirect      = make_array(1, /float, value=(ysq))
   bx_directx   = make_array(1, 4, /float, value=0.0)
   bx_directy   = make_array(1, 4, /float, value=0.0)
   bx_directx[0,0] = xdirect[0] - 50.0
   bx_directx[0,1] = xdirect[0] + 50.0
   bx_directx[0,2] = xdirect[0] + 50.0
   bx_directx[0,3] = xdirect[0] - 50.0
   bx_directy[0,0] = ydirect[0] - 6.0
   bx_directy[0,1] = ydirect[0] - 6.0
   bx_directy[0,2] = ydirect[0] + 6.0
   bx_directy[0,3] = ydirect[0] + 6.0
   xyad, hdrg, (xdirect - 1.0), (ydirect - 1.0), rad, decd
   x0         = bx_directx[*,0] - 1.0
   x1         = bx_directx[*,1] - 1.0
   x2         = bx_directx[*,2] - 1.0
   x3         = bx_directx[*,3] - 1.0
   y0         = bx_directy[*,0] - 1.0
   y1         = bx_directy[*,1] - 1.0
   y2         = bx_directy[*,2] - 1.0
   y3         = bx_directy[*,3] - 1.0
   xyad, hdrg, x0, y0, ra0, dec0
   xyad, hdrg, x1, y1, ra1, dec1
   xyad, hdrg, x2, y2, ra2, dec2
   xyad, hdrg, x3, y3, ra3, dec3
   bx_ra      = [[ra0], [ra1], [ra2], [ra3]]
   bx_dec     = [[dec0], [dec1], [dec2], [dec3]]
   ;
   ra         = rad[0]
   dec        = decd[0]
   bxra       = bx_ra[0,*]
   bxdec      = bx_dec[0,*]
   shunt_direct_stamp, im_direct, hdrd, dxstamp, dystamp, zrange_d, id, ra, dec, bxra, bxdec
   shunt_direct_stamp, im_direct, hdrd, dxstamp, dystamp, zrange_d, id, ra, dec, bxra, bxdec, hardfile='plook.ps'
   ;
   stop
END 
