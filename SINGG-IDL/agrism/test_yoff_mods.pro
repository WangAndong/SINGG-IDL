PRO test_yoff_mods
   ;
   ; test different models for determining the y offset of the 1st 
   ; order.
   ;
   fmtblem1   = '(i,f,f,f,f,f,f,f,f,i,i,i,f,f,f,f,a)'
   fmtblem2   = '(x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,f,f,f,a,f,f,f,f,f,f,f,f,a)'
   dxmed      = 13
   abcdef     = [-111.527183299, 1.002591233, -0.002904541, -1.372370755, 0.000483156, 0.999925759]
   dxbeam     = [-10, 150]
   dydx_a_1   = [-4.5065e-4, -3.52335e-6, 2.1394e-6, 2.17438e-10, -7.81162e-10, 4.49999e-11]
   yoff_a     = [-0.765986, 7.31305e-5, 1.24362e-4, -9.86897e-8, 2.01405e-7, 2.8342e-8]
   filb       = '/home/meurer/text/ACS/Grism/Figures/hdfn_blem.out'
   xref       = 2183.5
   yref       = 2180.5
   ;
   ; invert matrix
   cx     = -1.0*abcdef[0]
   cy     = -1.0*abcdef[3]
   amat   = [[abcdef[1], [abcdef[2]]], [abcdef[4], abcdef[5]]]
   tmat   = float(invert(amat, status, /double))
   ;
   ; read blem results
   readcol,filb,idg,ximg,yimg,ydb0,magb,ab,bb,thb,fwhmb,flagb1,flagb2,flagb3,pksngfb,pksndf,ctsb,skyb,qcutb,format=fmtblem1
   readcol,filb,apeakb,ashiftb,aw50b,qccb,cpeakb,cshiftb,cw50b,lamb,cprmsb,ximb,yimb,fluxb,csexid,format=fmtblem2
   ;
   ; good sources hdfn_blem
   qcutb = strtrim(qcutb,2)
   qccb  = strtrim(qccb,2)
   g = where(qcutb EQ '0' AND (qccb EQ '0' OR qccb EQ '1'), ng)
   idg = temporary(idg[g])
   ximg = temporary(ximg[g])
   yimg = temporary(yimg[g])
   ydb0 = temporary(ydb0[g])
   magb = temporary(magb[g])
   ab = temporary(ab[g])
   bb = temporary(bb[g])
   thb = temporary(thb[g])
   fwhmb = temporary(fwhmb[g])
   flagb1 = temporary(flagb1[g])
   flagb2 = temporary(flagb2[g])
   flagb3 = temporary(flagb3[g])
   ctsb = temporary(ctsb[g])
   skyb = temporary(skyb[g])
   qcutb = temporary(qcutb[g])
   apeakb = temporary(apeakb[g])
   ashiftb = temporary(ashiftb[g])
   aw50b = temporary(aw50b[g])
   qccb = temporary(qccb[g])
   cpeakb = temporary(cpeakb[g])
   cshiftb = temporary(cshiftb[g])
   cw50b = temporary(cw50b[g])
   lamb = temporary(lamb[g])
   cprmsb = temporary(cprmsb[g])
   ximb = temporary(ximb[g])
   yimb = temporary(yimb[g])
   fluxb = temporary(fluxb[g])
   csexid = temporary(csexid[g])
   ;
   ; calculate offset using blind_emfind algorithm
   i0     = fix(ximg - dxbeam[1] - 0.5*dxmed + 0.5)
   i1     = fix(ximg - dxbeam[0] + 0.5*dxmed + 0.5)
   xz     = 0.5*(i1 + i0) + abcdef[0]
   i0     = i0 - 1
   i1     = i1 - 1
   xdb    = (xz + cx)*tmat[0] + (yimg + cy)*tmat[1]
   ydb    = (xz + cx)*tmat[2] + (yimg + cy)*tmat[3]
   dyb    = yimg - ydb
   dyb0   = yimg - ydb0
   ;
   ; calculate offset using aXe config file
   xda      = ximg + 0.5*float(dxbeam[0] + dxbeam[1])
   yda      = yimg - yoff_a[0]
   dydx_a1  = eval_axe_poly(dydx_a_1, xda, yda, xref=xref, yref=yref)
   yoff_aa  = eval_axe_poly(yoff_a, xda, yda, xref=xref, yref=yref)
   dydx     = 0.0 + 0.5*float(dxbeam[0] + dxbeam[1])*dydx_a1
   dya      = yoff_aa+dydx
   yda      = yimg - dya
   ;
   forprint, idg,ximg,yimg,dyb0,dyb,dya,ydb,yda,yda-ydb
END 
