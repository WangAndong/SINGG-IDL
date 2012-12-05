FUNCTION calc_seeing,starfile,imsize,shift,NSIG=nsig
; INPUTS
;   starfile       whatever.fits.stars
;   imsize[2]      2D array of image dimensions.  By default, 2048+2*buffer
;   shift          Size of the edge to ignore (edge+buffer)
; OUTPUT
;   (function)     Seeing, in pixels

  IF NOT FILE_TEST(starfile) THEN BEGIN
    PRINT,"ERROR in calc_seeing: file does not exist ",starfile
    RETURN,0.0
  ENDIF

  readcol_new,starfile,refnumber,X,Y,Mag,flag,$
              A,B,R,FWHM,$
              FORMAT='(I,F,F,F,A,F,F,F,F,F)',COMMENT="#",/SILENT

; Size tolerance, to rule out CRs and galaxies
  min_size = 0.7
  max_size = 5.0
; Ratio tolerance, to rule out galaxies or streaks or such
  max_R = 1.5

  IF NOT KEYWORD_SET(nsig) THEN nsig = 3.0
  maxiter = 100

  good = WHERE(flag EQ '000' $
           AND R LT max_R $
           AND A LT max_size AND B GT min_size $
           AND X GT shift $
           AND X LT ((imsize[0]-1)-shift) $
           AND Y GT shift $
           AND Y LT ((imsize[1]-1)-shift) $
           AND FWHM GT 0.0,ref_count)

  IF ref_count LT 2 THEN BEGIN
    PRINT,"ERROR in calc_seeing: not enough extracted objects ",ref_count,starfile
    RETURN,0.0
  ENDIF

  grm_avsigclip,FWHM(good),nsig,maxiter,$
                seeing,seesig,ngood,nbad,niter

  RETURN,seeing
END
