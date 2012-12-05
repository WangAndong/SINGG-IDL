FUNCTION box_sky,img,mask,firstguess,bxw,boxdata,REJFRAC=rejfrac

;; Switched from box2boxbg2 to 3.

  skyres = FLTARR(2) ; Just in case.
  nsig_init = 3.0
  nsig_fin = 5.0
  boxdata = FLTARR(3,1)
  IF NOT KEYWORD_SET(rejfrac) THEN rejfrac = 0.01
; Note: if you're passing through a mask that chops out large sections
; of the image for sky, or that has a grown mask, you'll want to set
; rejfrac much, much higher.  It'll still mask individual bad pixels
; within box2boxbg3, so don't panic.

  box2boxbg3,img,bxw,nsigma=nsig_init,/use_sky,/nodisplay,$
             mask=mask,results=skyres,startguess=[firstguess[0],firstguess[1]],$
             num_boxes=oldnum,reject_frac=rejfrac,/silent

  sigdiff = 1.0
  skydiff = 1.0
  num_ratio = 1.0
  WHILE (sigdiff GT 0.01 AND skydiff GT 0.001 AND num_ratio GT 0.5) DO BEGIN 
    oldsky = skyres
    box2boxbg3,img,bxw,nsigma=nsig_fin,/use_sky,/nodisplay,$
               mask=mask,results=skyres,startguess=[skyres[0],skyres[1]],$
               boxinfo=boxdata,num_boxes=num_boxes,reject_frac=rejfrac,/silent
    sigdiff = ABS((oldsky[0] - skyres[0])/oldsky[1])
    skydiff = ABS((oldsky[0] - skyres[0])/oldsky[0])
    num_ratio = FLOAT(num_boxes)/FLOAT(oldnum)
  ENDWHILE

  RETURN,skyres
END
