PRO blem_multigray, buf, window, expand, bstmp_gr, bstmp_gf, brbnl_dr, brbnl_df, $
                    brbns_gr, brbns_gf, brbns_dr, brbns_df, outpng=outpng, wpos=wpos
   ; 
   ; Makes multiple grayscale plot for blind_emfind
   ;
   ; buf      -> Buffer size in pixels around each panel
   ; window   -> window # to plot to.
   ; expand   -> integer expansion factor.
   ; bstmp_gr -> Byte scaled stamp of raw grism image
   ; bstmp_gf -> Byte scaled stamp of filtered grism image
   ; brbnl_dr -> Byte scaled large ribbon of raw direct image
   ; brbnl_df -> Byte scaled large ribbon of filtered direct image
   ; brbns_gr -> Byte scaled small ribbon of raw grism image
   ; brbns_gf -> Byte scaled small ribbon of filtered grism image
   ; brbns_dr -> Byte scaled small ribbon of raw direct image
   ; brbns_df -> Byte scaled small ribbon of filtered direct image
   ; outpng   -> If set, this is the name of the png file to write.
   ; wpos     -> If set, then the position of the window
   ;
   ; calculate the size of the images to display
   iexp   = fix(max([expand, 1.0]))
   szstgr = size(bstmp_gr)*iexp
   szstgf = size(bstmp_gf)*iexp
   szrldr = size(brbnl_dr)*iexp
   szrldf = size(brbnl_df)*iexp
   szrsdr = size(brbns_dr)*iexp
   szrsdf = size(brbns_df)*iexp
   szrsgr = size(brbns_gr)*iexp
   szrsgf = size(brbns_gf)*iexp
   bb     = iexp*buf
   k1     = 3*bb+szstgr[1]+szstgf[1]
   k2     = 2*bb+max([szrldr[1],szrldf[1],szrsdr[1],szrsdf[1],szrsgr[1],szrsgf[1]])
   xwin   = max([k1,k2])
   ywin   = 8*bb+szstgr[2]+szrldr[2]+szrldf[2]+szrsdr[2]+szrsdf[2]+szrsgr[2]+szrsgf[2]
   IF keyword_set(wpos) THEN window, window, xsize=xwin, ysize=ywin, xpos=wpos[0], ypos=wpos[1] $
                        ELSE window, window, xsize=xwin, ysize=ywin
   ;
   ; create the postage stamps to display
   bstmpgr = rebin(bstmp_gr,szstgr[1],szstgr[2],/sample) 
   bstmpgf = rebin(bstmp_gf,szstgf[1],szstgf[2],/sample) 
   brbnldr = rebin(brbnl_dr,szrldr[1],szrldr[2],/sample) 
   brbnldf = rebin(brbnl_df,szrldf[1],szrldf[2],/sample) 
   brbnsdr = rebin(brbns_dr,szrsdr[1],szrsdr[2],/sample) 
   brbnsdf = rebin(brbns_df,szrsdf[1],szrsdf[2],/sample) 
   brbnsgr = rebin(brbns_gr,szrsgr[1],szrsgr[2],/sample) 
   brbnsgf = rebin(brbns_gf,szrsgf[1],szrsgf[2],/sample) 
   ;
   ; raw grism image
   i      = bb - 1
   j      = ywin - bb - szstgr[2]
   tv,bstmpgr, i, j
   ;
   ; filtered grism image
   i      = i + szstgr[1] + bb
   tv,bstmpgf, i, j
   ;
   ; Large raw direct ribbon
   i      = bb - 1
   j      = j - szrldr[2] - bb
   tv,brbnldr, i, j
   ;
   ; Large filtered direct ribbon
   j      = j - szrldf[2] - bb
   tv,brbnldf, i, j
   ;
   ; Small raw grism ribbon
   j      = j - szrsgr[2] - bb
   tv,brbnsgr, i, j
   ;
   ; Small filtered grism ribbon
   j      = j - szrsgf[2] - bb
   tv,brbnsgf, i, j
   
   ; Small raw direct ribbon
   j      = j - szrsdr[2] - bb
   tv,brbnsdr, i, j
   ;
   ; Small filtered direct ribbon
   j      = j - szrsdf[2] - bb
   tv,brbnsdf, i, j
   ;
   ; output png file
   IF keyword_set(outpng) THEN makepng,outpng,/color
END 

