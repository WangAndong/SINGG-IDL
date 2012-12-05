PRO calc_skysig,STEPSIZE=stepsize

; Picks a point outside the object apertures.
; Creates a "fake" aperture.  Compares box-to-box brightness inside
; the aperture with b2b from annulus.
; Stores the results, sorted by run, filter, and size.

  bxw = 35

  infile = !singgdir+"/skysig.in"
  readcol_new,infile,gname,cenX,cenY,minrad,maxrad, $
              COMMENT='#',FORMAT='(A,F,F,F,F)',/SILENT
;  gname = update_name(gname)

  apind = WHERE(STRMID(gname,0,1) EQ 'J',num_aps)
  runind = WHERE(STRMID(gname,0,3) EQ 'Run')
  num_runs = N_ELEMENTS(runind)-1

  runlist = STRMID(gname[runind[0:num_runs-1]],0,5)

  outfile = !singgdir+"/skysig.out"
  IF FILE_TEST(outfile) THEN BEGIN
    readcol_new,outfile,oname,orf,onf,orad,osdisp,ordisp,onin,onout,oSo,oSs,oSi,oRo,oRs,oRi, $
                COMMENT='#',FORMAT='(A,A,A,F,F,F,F,F,F,F,F,F,F,F)',/SILENT
    num_old = N_ELEMENTS(oname)
  ENDIF ELSE BEGIN
    num_old = 0
  ENDELSE
  OPENW,unit,outfile,/GET_LUN
  PRINTF,unit,"# Name           Rfilt Nfilt        Rad    Sdisp  Rdisp  Nin Nout  Sskyout   Ssigout   Sskyin    Rskyout   Rsigout   Rskyin",FORMAT='(A)'

  headerdb = "proc3_header"
  dbopen,headerdb,0
  dbext,-1,"RUNID,TARGET,IMTYPE,FILENAME,MASKFILE,FILTNAME,SKYSIGBX,BUFFER", $
            runid,object,imtype,filename,maskfile,filtname,skysigbx,buff
  dbclose,dummy
;  object = update_name(object)
  runid = 'Run'+STRTRIM(runid,2)

  Slist = WHERE(STRTRIM(imtype,2) EQ 'net',Scount)

  rfilt = STRARR(Scount)+"R"
  rfilt[WHERE(STRPOS(filename[Slist],'_Csub_ss') GE 0)] = "C"

  setplotcolors

  maxdist = 1000.0
  IF MAX(maxrad) GT maxdist THEN BEGIN
    PRINT,'YOU SCREWED UP!!! Check the maxdist.'
    RETURN
  ENDIF

  IF NOT KEYWORD_SET(stepsize) THEN stepsize = 50.0
  num_dists = FIX(maxdist/stepsize)

  num_combos = 0
  max_combos = 200
  Rf = STRARR(max_combos)
  Nf = STRARR(max_combos)
  run = STRARR(max_combos)
  Fcount = INTARR(max_combos)
  Cind = INTARR(Scount)

  Rd = FLTARR(num_dists,max_combos)
  Sd = FLTARR(num_dists,max_combos)
  Nd = INTARR(num_dists,max_combos)
  Sdisp = FLTARR(num_aps,num_dists,max_combos)
  Rdisp = FLTARR(num_aps,num_dists,max_combos)
  rad = (FINDGEN(num_dists)+1.0)*stepsize

  dummy = FLTARR(2)
  setplotcolors
  PLOT,dummy,dummy,XRANGE=[0,900],YRANGE=[-2.0,2.0],COLOR=!black

  FOR irun = 0,num_runs-1 DO BEGIN
    PRINT,'Starting '+runlist[irun]
    FOR ii = runind[irun]+1,runind[irun+1]-1 DO BEGIN
      PRINT,'  ',gname[ii]
; Match within Slist
      index = WHERE(STRTRIM(object[Slist],2) EQ STRTRIM(gname[ii],2) AND $
                    STRTRIM(runid[Slist],2) EQ STRTRIM(runlist[irun],2),count)

      IF count EQ 0 THEN BEGIN
        PRINT,'ERROR in calc_skysig: no match to object/run ',gname[ii],runlist[irun]
        RETURN
      ENDIF

      FOR kk = 0,count-1 DO BEGIN
; For multi-filter or multi-run galaxies, we want to do each combo separately.
        kind = index[kk]

        ind = WHERE(Rf EQ STRTRIM(rfilt[kind],2) AND $
                    Nf EQ STRTRIM(filtname[Slist[kind]],2) AND $
                    run EQ STRTRIM(runid[Slist[kind]],2),count)
        IF count EQ 0 THEN BEGIN
          Rf[num_combos] = STRTRIM(rfilt[kind],2)
          Nf[num_combos] = STRTRIM(filtname[Slist[kind]],2)
          run[num_combos] = STRTRIM(runid[Slist[kind]],2)
          ind = num_combos
          num_combos = num_combos + 1
          IF num_combos GT max_combos THEN BEGIN
            PRINT,'ERROR in calc_skysig: max_combos too small'
            RETURN
          ENDIF
        ENDIF
        Cind[kind] = ind[0]
        Fcount[ind[0]] = Fcount[ind[0]] + 1

        oldskip = 0b
        oldcount = 0
; Check to see if we can skip this object/filter combo entirely, to
; save time on .fits reading.
        IF num_old GT 0 THEN BEGIN
          oldind = WHERE(STRTRIM(oname,2) EQ STRTRIM(gname[ii],2) AND $
                   STRTRIM(orf,2) EQ STRTRIM(rfilt[kind],2) AND $
                   STRTRIM(onf,2) EQ STRTRIM(filtname[Slist[kind]],2), oldcount)
          IF oldcount GT 0 THEN BEGIN
            IF MIN(orad[oldind]) LE (minrad[ii]+1.0) AND $
               MAX(orad[oldind]) GE (maxrad[ii]-1.0) THEN oldskip = 1b
          ENDIF
        ENDIF

        IF NOT oldskip THEN BEGIN
          buffer = buff[Slist[kind]]
          id = STRTRIM(object[Slist[kind]],2)
          indir = STRTRIM(runid[Slist[kind]],2)+"/Proc3/"+id+"/"
          path = STRTRIM(runid[Slist[kind]],2)+"/Proc4/"+id+"/"

          ellfile = STRTRIM(path+id+"_ellipse.dat",2)
          read_ellipse_file,ellfile,n_ellipses,refimage,Dx,Dy,Px,Py,pa, $
                            a_i,b_i,z_s,z_f,z_c
          a = a_i * z_s
          b = b_i * z_s
          Px = Px + buffer
          Py = Py + buffer
          Dx = Dx + buffer
          Dy = Dy + buffer
          theta = (pa-90.0)*!dtor

          Stemp = STRTRIM(filename[Slist[kind]],2)
          Sfile = path+Stemp
          Smaskfile = STRTRIM(STRMID(Sfile,0,STRLEN(Sfile)-8),2)+"_mask.fits"
          Splfile = indir+STRTRIM(STRMID(Stemp,0,STRLEN(Stemp)-8),2)+".pl.fits.gz"
          fits_read,Sfile,Simg,Shd
          fits_read,Smaskfile,mask,junk,/data_only
          Splimg = readfits(Splfile,/SILENT)
          Smask = mask OR (Splimg LT 0.5)
          mysky,Simg,Simsky,Simsig,mask=Smask,/silent

          Rlist = WHERE(STRTRIM(object,2) EQ STRTRIM(object[Slist[kind]]) AND $
                        STRTRIM(imtype,2) EQ 'cont',Rcount)
          IF Rcount GT 1 THEN Rind = WHERE(STRPOS(filename[Rlist],rfilt[kind]+'_ss') GE 0) $
                         ELSE Rind = 0
          Rtemp = STRTRIM(filename[Rlist[Rind[0]]],2)
          Rfile = path+Rtemp
          Rmaskfile = STRTRIM(STRMID(Rfile,0,STRLEN(Rfile)-10),2)+"_mask.fits"
          Rplfile = indir+STRTRIM(STRMID(Rtemp,0,STRLEN(Rtemp)-8),2)+".pl.fits.gz"
          fits_read,Rfile,Rimg,Rhd
          fits_read,Rmaskfile,mask,junk,/data_only
          Rplimg = readfits(Rplfile,/SILENT)
          Rmask = mask OR (Rplimg LT 1.5)
          mysky,Rimg,Rimsky,Rimsig,mask=Rmask,/silent

          getrot,Rhd,rot,cdelt
          cdelt = ABS(cdelt)*60.
          am_pix = cdelt[0] ; arcmin per pixel
          sz = SIZE(Rimg)

; Pick central point
          xyad,Rhd,cenX[ii],cenY[ii],racen,deccen

          mask_in = BYTARR(sz[1],sz[2]) + 1b
          mask_out = BYTARR(sz[1],sz[2]) + 1b

          apmask = BYTARR(sz[1],sz[2])
          FOR xx = buffer,sz[1]-1-buffer DO BEGIN
            FOR yy = buffer,sz[2]-1-buffer DO BEGIN
              zap = calc_z(xx,yy,Dx,Dy,theta,a,b)
              IF MIN(zap) LT 1.2 THEN apmask[xx,yy] = 1b
            ENDFOR
          ENDFOR
        ENDIF

        FOR jj = FIX(minrad[ii]/stepsize)-1,FIX(maxrad[ii]/stepsize)-1 DO BEGIN

; check to see if this combination has already been done
          oldcount2 = 0
          IF oldcount GT 0 THEN BEGIN
            ind2 = WHERE(ABS(orad[oldind]/rad[jj]-1.0) LT 0.01, oldcount2)
          ENDIF

          IF oldcount2 EQ 0 THEN BEGIN
            AreaZ = MAX([250.0*bxw^2/(!pi*rad[jj]^2), 1.0])

            FOR xx = buffer,sz[1]-1-buffer DO BEGIN
              FOR yy = buffer,sz[2]-1-buffer DO BEGIN
                z = SQRT(FLOAT(xx - cenX[ii])^2 + FLOAT(yy - cenY[ii])^2) / rad[jj]
                IF z LT SQRT(AreaZ+1.0) AND z GT 1.0 THEN mask_out[xx,yy] = 0b
                IF z LT 1.0 THEN mask_in[xx,yy] = 0b
              ENDFOR
            ENDFOR

            IF rad[jj] GT bxw*2.0 THEN BEGIN
              Sskyin = box_sky(Simg,(Smask OR mask_in OR apmask),[Simsky,Simsig],bxw,Sboxin,rejfrac=0.10)
              Rskyin = box_sky(Rimg,(Rmask OR mask_in OR apmask),[Rimsky,Rimsig],bxw,Rboxin,rejfrac=0.10)
              nin = N_ELEMENTS(Rboxin[0,*])
            ENDIF ELSE BEGIN
              Spos = WHERE(NOT (Smask OR mask_in OR apmask))
              grm_avsigclip,Simg[Spos],3.0,100,mode,sigma,ngood,nbad,niter
              Sskyin = [mode,sigma]
              Rpos = WHERE(NOT (Rmask OR mask_in OR apmask))
              grm_avsigclip,Rimg[Rpos],3.0,100,mode,sigma,ngood,nbad,niter
              Rskyin = [mode,sigma]
              nin = 1
            ENDELSE

            Sskyout = box_sky(Simg,(Smask OR mask_out OR apmask),[Simsky,Simsig],bxw,Sboxout,rejfrac=0.10)
            Sdisp[Nd[jj,Cind[kind]],jj,Cind[kind]] = (Sskyout[0]-Sskyin[0])/Sskyout[1]

            Rskyout = box_sky(Rimg,(Rmask OR mask_out OR apmask),[Rimsky,Rimsig],bxw,Rboxout,rejfrac=0.10)
            Rdisp[Nd[jj,Cind[kind]],jj,Cind[kind]] = (Rskyout[0]-Rskyin[0])/Rskyout[1]

            nout = N_ELEMENTS(Rboxout[0,*])
          ENDIF ELSE BEGIN
; This one's already been done.
;;print,"read from file",orad[oldind[ind2[0]]]
            Sdisp[Nd[jj,Cind[kind]],jj,Cind[kind]] = osdisp[oldind[ind2[0]]]
            Rdisp[Nd[jj,Cind[kind]],jj,Cind[kind]] = ordisp[oldind[ind2[0]]]
            nin = onin[oldind[ind2[0]]]
            nout = onout[oldind[ind2[0]]]
            Sskyout = [oSo[oldind[ind2[0]]],oSs[oldind[ind2[0]]]]
            Sskyin = [oSi[oldind[ind2[0]]],oSs[oldind[ind2[0]]]]
            Rskyout = [oRo[oldind[ind2[0]]],oRs[oldind[ind2[0]]]]
            Rskyin = [oRi[oldind[ind2[0]]],oRs[oldind[ind2[0]]]]
          ENDELSE

;; was SYM(Cind[kind]+1)
          OPLOT,rad[jj]*[1.0,1.0],Sdisp[Nd[jj,Cind[kind]],jj,Cind[kind]]*[1.0,1.0], $
                COLOR=!black,PSYM=SYM(1)
          OPLOT,rad[jj]*[1.0,1.0],Rdisp[Nd[jj,Cind[kind]],jj,Cind[kind]]*[1.0,1.0], $
                COLOR=!ddgray,PSYM=SYM(1)

          Sd[jj,Cind[kind]] = Sd[jj,Cind[kind]] + $
                Sdisp[Nd[jj,Cind[kind]],jj,Cind[kind]]
          Rd[jj,Cind[kind]] = Rd[jj,Cind[kind]] + $
                Rdisp[Nd[jj,Cind[kind]],jj,Cind[kind]]

          PRINT,object[Slist[kind]],rfilt[kind], $
                filtname[Slist[kind]],rad[jj], $
                Sdisp[Nd[jj,Cind[kind]],jj,Cind[kind]], $
                Rdisp[Nd[jj,Cind[kind]],jj,Cind[kind]],nin,nout, $
                Sskyout[0],Sskyout[1],Sskyin[0],Rskyout[0],Rskyout[1],Rskyin[0],$
                FORMAT='(A12,2(A10," "),F7.1," ",2(F6.3," "),2(I4," "),6(F9.6," "))'
          PRINTF,unit,object[Slist[kind]],rfilt[kind], $
                 filtname[Slist[kind]],rad[jj], $
                 Sdisp[Nd[jj,Cind[kind]],jj,Cind[kind]], $
                 Rdisp[Nd[jj,Cind[kind]],jj,Cind[kind]],nin,nout, $
                 Sskyout[0],Sskyout[1],Sskyin[0],Rskyout[0],Rskyout[1],Rskyin[0],$
                 FORMAT='(A12,2(A10," "),F7.1," ",2(F6.3," "),2(I4," "),6(F9.6," "))'
          IF FINITE(Sd[jj,Cind[kind]]) AND FINITE(Rd[jj,Cind[kind]]) THEN BEGIN
            Nd[jj,Cind[kind]] = Nd[jj,Cind[kind]] + 1
            Sd[jj,Cind[kind]] = Sd[jj,Cind[kind]] + $
                  Sdisp[Nd[jj,Cind[kind]],jj,Cind[kind]]
            Rd[jj,Cind[kind]] = Rd[jj,Cind[kind]] + $
                  Rdisp[Nd[jj,Cind[kind]],jj,Cind[kind]]
          ENDIF ELSE BEGIN
          ENDELSE
        ENDFOR
      ENDFOR
    ENDFOR
  ENDFOR

  PRINT,"---------------------------------------"
  PRINTF,unit,"#---------------------------------------",FORMAT='(A)'
  PRINTF,unit,"# run  Rfilt Nfilt   count",FORMAT='(A)'
  PRINTF,unit,"#   dist  Smean  Rmean  Ssig   Rsig",FORMAT='(A)'

  FOR ii = 0,num_combos-1 DO BEGIN
    PRINT,run[ii]," ",Rf[ii]," ",Nf[ii]," ",Fcount[ii]
    PRINTF,unit,run[ii],Rf[ii],Nf[ii],Fcount[ii],FORMAT='(A6," ",A1," ",A10," ",I3)'
    ind = WHERE(Nd[*,ii] GT 0)
;; was ii+1 for the symbol
    OPLOT,rad[ind],(Sd[ind,ii]/Nd[ind,ii]),COLOR=!black,PSYM=-SYM(1)
    PRINT,"S: ",MEAN(Sd[ind,ii]/Nd[ind,ii]),STDDEV(Sd[ind,ii]/Nd[ind,ii])
    OPLOT,rad[ind],(Rd[ind,ii]/Nd[ind,ii]),COLOR=!ddgray,PSYM=-SYM(1)
    PRINT,"R: ",MEAN(Rd[ind,ii]/Nd[ind,ii]),STDDEV(Rd[ind,ii]/Nd[ind,ii])
    FOR jj = 0,num_dists-1 DO BEGIN
      IF Nd[jj,ii] GT 0 THEN BEGIN
        Sdev = sig_pm(Sdisp[0:Nd[jj,ii]-1,jj,ii])
        Rdev = sig_pm(Rdisp[0:Nd[jj,ii]-1,jj,ii])
        ERRPLOT,rad[jj],(Sd[jj,ii]/Nd[jj,ii])-Sdev[1],(Sd[jj,ii]/Nd[jj,ii])+Sdev[0],COLOR=!black
        ERRPLOT,rad[jj]+1,(Rd[jj,ii]/Nd[jj,ii])-Rdev[1],(Rd[jj,ii]/Nd[jj,ii])+Rdev[0],COLOR=!ddgray
        PRINTF,unit,rad[jj],MEAN(Sd[jj,ii]/Nd[jj,ii]),MEAN(Rd[jj,ii]/Nd[jj,ii]), $
               MEAN(Sdev),MEAN(Rdev),FORMAT='("  ",F6.1," ",4(F6.3," "))'
      ENDIF
    ENDFOR
  ENDFOR

  CLOSE,unit
  FREE_LUN,unit

END
