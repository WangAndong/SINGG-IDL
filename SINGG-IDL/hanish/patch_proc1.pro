FUNCTION patch_proc1,hdr
; Run within housekeeper, AFTER hk_filout has been run (which sets the
; file name), but BEFORE hk_checkhdr.

  imfile = STRTRIM(SXPAR(hdr,'FILENAME'),2) ; objRRNNEEE
  runid = STRTRIM(SXPAR(hdr,'RUNID',count=count),2)
  imnum = STRMID(imfile,3,7) ; RRNNEEE (Run, Night, Exposure)
  night = LONG(STRMID(imfile,5,2)) ; 2-digit night number

; PART ONE: patch those errors that applied to all images within a night.

; Run 4, Night 1 was missing ZD.
  IF runid EQ '04' AND night EQ 1 THEN BEGIN
    test = SXPAR(hdr,'ZD',count=count)
    IF count EQ 0 THEN hk_zdair,-1,hdr,success
  ENDIF

; Run 4, Nights 5 & 6 (Schmidt data) were missing all of the filter2
; information, FNAME1, and DATE-OBS.
  IF runid EQ '04s' THEN BEGIN
    test = SXPAR(hdr,'FNAME1',count=count)
    IF count EQ 0 THEN BEGIN
; In all of the R-band images, the FNAME1 line reads:
; COMMENT |INSTRUMENT     seqstar FNAME1  = 'R CTIO Schmidt 4x4" / Full name of f 
; (note the unbalanced parentheses.)
      SXADDPAR,hdr,'FNAME1','R CTIO Schmidt 4x4',' Full name of filter in wheel one',AFTER='FILTER1'
    ENDIF
    SXADDPAR,hdr,'FILTER2','N/A',' Filter in wheel two',AFTER='FNAME1'
    SXADDPAR,hdr,'FNAME2','N/A',' Full name of filter in wheel two',AFTER='FILTER2'

; Flip the sign on HA
    ha = SXPAR(hdr,'HA',count=count)
    IF count EQ 0 THEN ha = '00:00:00.0'
    IF STRMID(ha,0,1) EQ '-' THEN ha = STRMID(ha,1,STRLEN(ha)-1) $
                             ELSE ha = '-'+STRTRIM(ha,2)
    SXADDPAR,hdr,'HA',ha,' hour angle (H:M:S)',AFTER='ZD'

; Patch UT
    ut = SXPAR(hdr,'UT',count=count)
    IF count NE 1 THEN BEGIN
      sxdelpar,hdr,'UT'
; There are two UT entries in there.  The script read the right one,
; but we want to make sure that's the ONLY one.
      SXADDPAR,hdr,'UT',ut,' UT of TCS coords',AFTER='DATE-OBS'
    ENDIF

    test = SXPAR(hdr,'DATE-OBS',count=count)
    IF count NE 1 THEN BEGIN
; Not only are there multiple entries, but neither is actually correct.
      sxdelpar,hdr,'DATE-OBS'
      IF STRMID(imnum,3,1) EQ '5' THEN day=17 ELSE day=18
      IF STRMID(ut,0,1) EQ '2' THEN day=day-1 ; the start of the run had UT's of 23something
      dateobs = '2001-05-'+STRTRIM(STRING(day),2)+'T'+STRTRIM(ut,2)
      IF STRPOS(ut,'.') LT 0 THEN dateobs = dateobs+'.0'
; There are two DATE-OBS entries in there.  The script reads the last one.
      SXADDPAR,hdr,'DATE-OBS',dateobs,' Date of observation start',AFTER='EXPTIME'
    ENDIF

  ENDIF

; Run 6 had the wrong year on DATE-OBS most of the time.
  IF runid EQ '06' THEN BEGIN
    dateobs = SXPAR(hdr,'DATE-OBS')
    IF STRMID(dateobs,0,4) EQ '2000' THEN BEGIN
      newdate = '2001'+STRMID(dateobs,4,STRLEN(dateobs)-4)
      SXADDPAR,hdr,'DATE-OBS',newdate,' Date of observation start'
    ENDIF
  ENDIF

; PART TWO: patch those objects where multiple objects were in the frame.
;; can't run at this point, because TARGET isn't set until AFTER this point.
;;  patch_target,hdr

; PART THREE: for the rest, patch single images.
  CASE imnum OF
; Run01
    '0102080': BEGIN
        SXADDPAR,hdr,'RA','01:38:01.4',' right ascension (telescope)',AFTER='DCS_TIME'
        SXADDPAR,hdr,'DEC','-04:59:53.4',' declination (telescope)',AFTER='RA'
        SXADDPAR,hdr,'EPOCH',2000.0,' epoch of RA & DEC',AFTER='DEC'
        SXADDPAR,hdr,'ZD',31.0539,' zenith distance (degrees)',AFTER='EPOCH'
        SXADDPAR,hdr,'HA','01:16:08.9',' hour angle (H:M:S)',AFTER='ZD'
        SXADDPAR,hdr,'ST','02:54:12.6',' sidereal time',AFTER='HA'
        SXADDPAR,hdr,'AIRMASS',1.168,' airmass',AFTER='ST'
      END
; Run02
    '0203107': BEGIN
        SXADDPAR,hdr,'XPIXSIZE',0.432,' Pixel size in X (arcsec/pix)',AFTER='WAT2_001'
        SXADDPAR,hdr,'YPIXSIZE',0.432,' Pixel size in Y (arcsec/pix)',AFTER='XPIXSIZE'
      END
    '0204110': BEGIN
        SXADDPAR,hdr,'XPIXSIZE',0.432,' Pixel size in X (arcsec/pix)',AFTER='WAT2_001'
        SXADDPAR,hdr,'YPIXSIZE',0.432,' Pixel size in Y (arcsec/pix)',AFTER='XPIXSIZE'
      END
    '0204105': SXADDPAR,hdr,'RA','10:51:24',' right ascension (telescope)'
    '0204106': SXADDPAR,hdr,'RA','10:51:24',' right ascension (telescope)'
    '0204107': SXADDPAR,hdr,'RA','10:51:24',' right ascension (telescope)'
    '0204108': SXADDPAR,hdr,'RA','10:51:24',' right ascension (telescope)'
    '0204109': SXADDPAR,hdr,'RA','10:51:24',' right ascension (telescope)'
    '0205070': BEGIN
        SXADDPAR,hdr,'RA','02:23:54.58',' right ascension (telescope)',AFTER='DCS_TIME'
        SXADDPAR,hdr,'DEC','-04:37:50.5',' declination (telescope)',AFTER='RA'
        SXADDPAR,hdr,'EPOCH',2000.0,' epoch of RA & DEC',AFTER='DEC'
        SXADDPAR,hdr,'ZD',33.525,' zenith distance (degrees)',AFTER='EPOCH'
        SXADDPAR,hdr,'HA','01:27:02.6',' hour angle (H:M:S)',AFTER='ZD'
        SXADDPAR,hdr,'ST','03:51:00.0',' sidereal time',AFTER='HA'
        SXADDPAR,hdr,'AIRMASS',1.1996,' airmass',AFTER='ST'

        SXADDPAR,hdr,'OBJECT','J0223-04',' Name of the object observed',AFTER='IRAF-TLM'
        SXADDPAR,hdr,'EXPTIME',600.0,' Exposure time in secs',AFTER='AIRMASS'
        SXADDPAR,hdr,'DATE-OBS','2000-12-31T01:54:59.4',' Date of observation start',AFTER='EXPTIME'

        SXADDPAR,hdr,'XPIXSIZE',0.432,' Pixel size in X (arcsec/pix)',AFTER='WAT2_001'
        SXADDPAR,hdr,'YPIXSIZE',0.432,' Pixel size in Y (arcsec/pix)',AFTER='XPIXSIZE'
      END
; Run03
; Run04
; Run04s
    '0405036': SXADDPAR,hdr,'DEC','-17:25:11',' declination (telescope)'
    '0405037': SXADDPAR,hdr,'DEC','-17:25:11',' declination (telescope)'
    '0405038': SXADDPAR,hdr,'DEC','-17:25:11',' declination (telescope)'
    '0405039': SXADDPAR,hdr,'DEC','-17:25:11',' declination (telescope)'
    '0405040': SXADDPAR,hdr,'DEC','-17:25:11',' declination (telescope)'
    '0405041': SXADDPAR,hdr,'DEC','-17:25:11',' declination (telescope)'
    '0406051': SXADDPAR,hdr,'DEC','-17:57:05',' declination (telescope)'
    '0406052': SXADDPAR,hdr,'DEC','-17:57:05',' declination (telescope)'
    '0406053': SXADDPAR,hdr,'DEC','-17:57:05',' declination (telescope)'
    '0406054': SXADDPAR,hdr,'DEC','-17:57:05',' declination (telescope)'
    '0406055': SXADDPAR,hdr,'DEC','-17:57:05',' declination (telescope)'
    '0406056': SXADDPAR,hdr,'DEC','-17:57:05',' declination (telescope)'
    '0406059': SXADDPAR,hdr,'DEC','-20:40:41',' declination (telescope)'
    '0406060': SXADDPAR,hdr,'DEC','-20:40:41',' declination (telescope)'
    '0406061': SXADDPAR,hdr,'DEC','-20:40:41',' declination (telescope)'
    '0406062': SXADDPAR,hdr,'DEC','-20:40:41',' declination (telescope)'
    '0406063': SXADDPAR,hdr,'DEC','-20:40:41',' declination (telescope)'
    '0406064': SXADDPAR,hdr,'DEC','-20:40:41',' declination (telescope)'
    '0406095': BEGIN
        SXADDPAR,hdr,'DATE-OBS','2001-05-18T09:00:21.0',' Date of observation start'
        SXADDPAR,hdr,'UT','09:00:21',' UT of TCS coords'

        SXADDPAR,hdr,'RA','22:15:47',' right ascension (telescope)',AFTER='DCS_TIME'
        SXADDPAR,hdr,'DEC','-45:47:03',' declination (telescope)',AFTER='RA'
        SXADDPAR,hdr,'EPOCH',1950.0,' epoch of RA & DEC',AFTER='DEC'
        SXADDPAR,hdr,'ZD',31.1,' zenith distance (degrees)',AFTER='EPOCH'
        SXADDPAR,hdr,'HA','-02:14:17',' hour angle (H:M:S)',AFTER='ZD'
        SXADDPAR,hdr,'ST','20:01:30',' sidereal time',AFTER='HA'
        SXADDPAR,hdr,'AIRMASS',1.17,' airmass',AFTER='ST'
      END
; Run05
    '0501033': SXADDPAR,hdr,'RA','11:52:49',' right ascension (telescope)'
    '0501034': SXADDPAR,hdr,'RA','11:52:49',' right ascension (telescope)'
; Run06
    '0604071': SXADDPAR,hdr,'DEC','-31:28:53',' declination (telescope)'
    '0604072': SXADDPAR,hdr,'DEC','-31:28:53',' declination (telescope)'
    '0604074': SXADDPAR,hdr,'DEC','-31:28:53',' declination (telescope)'
    '0604075': SXADDPAR,hdr,'DEC','-31:28:53',' declination (telescope)'
; Run07
    '0701062': BEGIN
        SXADDPAR,hdr,'RA','21:36:20.86',' right ascension (telescope)',AFTER='DCS_TIME'
        SXADDPAR,hdr,'DEC','-54:34:16.8',' declination (telescope)',AFTER='RA'
        SXADDPAR,hdr,'EPOCH',2000.0,' epoch of RA & DEC',AFTER='DEC'
        SXADDPAR,hdr,'ZD',26.396,' zenith distance (degrees)',AFTER='EPOCH'
        SXADDPAR,hdr,'HA','00:55:01.1',' hour angle (H:M:S)',AFTER='ZD'
        SXADDPAR,hdr,'ST','22:31:22.0',' sidereal time',AFTER='HA'
        SXADDPAR,hdr,'AIRMASS',1.11639,' airmass',AFTER='ST'

        SXADDPAR,hdr,'FILTER1','r',' Filter in wheel one',AFTER='INSTRUME'
        SXADDPAR,hdr,'FNAME1','R 4x4 4M tel',' Full name of filter in wheel1',AFTER='FILTER1'
        SXADDPAR,hdr,'FILTER2','dia',' Filter in wheel two',AFTER='FNAME1'
        SXADDPAR,hdr,'FNAME2','diaphragm',' Full name of filter in wheel2',AFTER='FILTER2'
        SXADDPAR,hdr,'FILTERS','r dia',' Filter positions',AFTER='FNAME2'

        SXADDPAR,hdr,'EXPTIME',120.0,' Exposure time in secs',AFTER='AIRMASS'
        SXADDPAR,hdr,'OBJECT','J2136-54',' Name of the object observed',AFTER='IRAF-TLM'

        SXADDPAR,hdr,'DATE-OBS','2001-10-25T01:01:01.096',' Date of observation start',AFTER='EXPTIME'

        SXADDPAR,hdr,'XPIXSIZE',0.432,' Pixel size in X (arcsec/pix)',AFTER='WAT2_001'
        SXADDPAR,hdr,'YPIXSIZE',0.432,' Pixel size in Y (arcsec/pix)',AFTER='XPIXSIZE'
      END
; Run08
    '0801104': BEGIN
        SXADDPAR,hdr,'FILTER1','r',' Filter in wheel one',AFTER='AIRMASS'
        SXADDPAR,hdr,'FNAME1','R 4x4 4M tel',' Full name of filter in wheel1',AFTER='FILTER1'
        SXADDPAR,hdr,'FILTER2','dia',' Filter in wheel two',AFTER='FNAME1'
        SXADDPAR,hdr,'FNAME2','diaphragm',' Full name of filter in wheel2',AFTER='FILTER2'
        SXADDPAR,hdr,'FILTERS','r dia',' Filter positions',AFTER='FNAME2'

        SXADDPAR,hdr,'XPIXSIZE',0.432,' Pixel size in X (arcsec/pix)',AFTER='WAT2_001'
        SXADDPAR,hdr,'YPIXSIZE',0.432,' Pixel size in Y (arcsec/pix)',AFTER='XPIXSIZE'
      END
    '0803104': BEGIN
        SXADDPAR,hdr,'RA','11:57:48',' right ascension (telescope)'
        SXADDPAR,hdr,'DEC','-15:44:31',' declination (telescope)'
      END
    '0803105': BEGIN
        SXADDPAR,hdr,'RA','11:57:48',' right ascension (telescope)'
        SXADDPAR,hdr,'DEC','-15:44:31',' declination (telescope)'
      END
    '0803106': BEGIN
        SXADDPAR,hdr,'RA','11:57:48',' right ascension (telescope)'
        SXADDPAR,hdr,'DEC','-15:44:31',' declination (telescope)'
      END
; Run09
; Run10
    '1002091': BEGIN
        SXADDPAR,hdr,'RA','14:44:42.87',' right ascension (telescope)',AFTER='DCS_TIME'
        SXADDPAR,hdr,'DEC',' 1:55:59.80',' declination (telescope)',AFTER='RA'
        SXADDPAR,hdr,'EPOCH',2000.0,' epoch of RA & DEC',AFTER='DEC'
        SXADDPAR,hdr,'ZD',33.122587,' zenith distance (degrees)',AFTER='EPOCH'
        SXADDPAR,hdr,'HA','0:29:08.23',' hour angle (H:M:S)',AFTER='ZD'
        SXADDPAR,hdr,'ST','15:13:51.10',' sidereal time',AFTER='HA'
        SXADDPAR,hdr,'AIRMASS',1.19395,' airmass',AFTER='ST'
      END
; Run11
    '1101110': BEGIN
        SXADDPAR,hdr,'FILTER1','6595',' Filter in wheel one',AFTER='AIRMASS'
        SXADDPAR,hdr,'FNAME1','6595/36 SINGG new',' Full name of filter in wheel1',AFTER='FILTER1'
        SXADDPAR,hdr,'FILTER2','dia',' Filter in wheel two',AFTER='FNAME1'
        SXADDPAR,hdr,'FNAME2','Diaphragm',' Full name of filter in wheel2',AFTER='FILTER2'
        SXADDPAR,hdr,'FILTERS','6595 dia',' Filter positions',AFTER='FNAME2'

        SXADDPAR,hdr,'XPIXSIZE',0.432,' Pixel size in X (arcsec/pix)',AFTER='WAT2_001'
        SXADDPAR,hdr,'YPIXSIZE',0.432,' Pixel size in Y (arcsec/pix)',AFTER='XPIXSIZE'
      END
; Run12
    '1203049': BEGIN
        SXADDPAR,hdr,'FILTER1','6709',' Filter in wheel one',AFTER='AIRMASS'
        SXADDPAR,hdr,'FNAME1','6709/71',' Full name of filter in wheel1',AFTER='FILTER1'
        SXADDPAR,hdr,'FILTER2','dia',' Filter in wheel two',AFTER='FNAME1'
        SXADDPAR,hdr,'FNAME2','diaphragm',' Full name of filter in wheel2',AFTER='FILTER2'
        SXADDPAR,hdr,'FILTERS','6709 dia',' Filter positions',AFTER='FNAME2'

        SXADDPAR,hdr,'XPIXSIZE',0.432,' Pixel size in X (arcsec/pix)',AFTER='WAT2_001'
        SXADDPAR,hdr,'YPIXSIZE',0.432,' Pixel size in Y (arcsec/pix)',AFTER='XPIXSIZE'
      END
; Run13
    '1301083': BEGIN
        SXADDPAR,hdr,'RA','15:39:00.13',' right ascension (telescope)',AFTER='DCS_TIME'
        SXADDPAR,hdr,'DEC','-28:35:33.1',' declination (telescope)',AFTER='RA'
        SXADDPAR,hdr,'EPOCH',2000.0,' epoch of RA & DEC',AFTER='DEC'
        SXADDPAR,hdr,'ZD',33.671510,' zenith distance (degrees)',AFTER='EPOCH'
        SXADDPAR,hdr,'HA','-02:35:20.9',' hour angle (H:M:S)',AFTER='ZD'
        SXADDPAR,hdr,'ST','13:03:49.1',' sidereal time',AFTER='HA'
        SXADDPAR,hdr,'AIRMASS',1.1966400,' airmass',AFTER='ST'
      END
    '1307060': SXADDPAR,hdr,'RA','02:18:07',' right ascension (telescope)'
    '1307061': SXADDPAR,hdr,'RA','02:18:07',' right ascension (telescope)'
    '1307062': SXADDPAR,hdr,'RA','02:18:07',' right ascension (telescope)'
    '1307063': SXADDPAR,hdr,'RA','02:18:07',' right ascension (telescope)'
; Run15
; Run17
    '1704090': BEGIN
        SXADDPAR,hdr,'RA','12:34:42',' right ascension (telescope)',AFTER='DCS_TIME'
        SXADDPAR,hdr,'DEC','02:13:13',' declination (telescope)',AFTER='RA'
        SXADDPAR,hdr,'EPOCH',2000.0,' epoch of RA & DEC',AFTER='DEC'
        SXADDPAR,hdr,'ZD',32.51,' zenith distance (degrees)',AFTER='EPOCH'
        SXADDPAR,hdr,'HA','-0:13:51',' hour angle (H:M:S)',AFTER='ZD'
        SXADDPAR,hdr,'ST','12:20:51.7',' sidereal time',AFTER='HA'
        SXADDPAR,hdr,'AIRMASS',1.185,' airmass',AFTER='ST'
      END
    '1704091': BEGIN
        SXADDPAR,hdr,'RA','12:34:38',' right ascension (telescope)',AFTER='DCS_TIME'
        SXADDPAR,hdr,'DEC','02:12:21',' declination (telescope)',AFTER='RA'
        SXADDPAR,hdr,'EPOCH',2000.0,' epoch of RA & DEC',AFTER='DEC'
        SXADDPAR,hdr,'ZD',32.38,' zenith distance (degrees)',AFTER='EPOCH'
        SXADDPAR,hdr,'HA','-0:05:39',' hour angle (H:M:S)',AFTER='ZD'
        SXADDPAR,hdr,'ST','12:28:58.7',' sidereal time',AFTER='HA'
        SXADDPAR,hdr,'AIRMASS',1.183,' airmass',AFTER='ST'
      END
    '1704092': BEGIN
        SXADDPAR,hdr,'RA','12:34:36',' right ascension (telescope)',AFTER='DCS_TIME'
        SXADDPAR,hdr,'DEC','02:11:54',' declination (telescope)',AFTER='RA'
        SXADDPAR,hdr,'EPOCH',2000.0,' epoch of RA & DEC',AFTER='DEC'
        SXADDPAR,hdr,'ZD',32.37,' zenith distance (degrees)',AFTER='EPOCH'
        SXADDPAR,hdr,'HA','0:01:07',' hour angle (H:M:S)',AFTER='ZD'
        SXADDPAR,hdr,'ST','12:35:42,7',' sidereal time',AFTER='HA'
        SXADDPAR,hdr,'AIRMASS',1.183,' airmass',AFTER='ST'
      END
    '1704093': BEGIN
        SXADDPAR,hdr,'RA','12:34:37',' right ascension (telescope)',AFTER='DCS_TIME'
        SXADDPAR,hdr,'DEC','02:12:02',' declination (telescope)',AFTER='RA'
        SXADDPAR,hdr,'EPOCH',2000.0,' epoch of RA & DEC',AFTER='DEC'
        SXADDPAR,hdr,'ZD',32.45,' zenith distance (degrees)',AFTER='EPOCH'
        SXADDPAR,hdr,'HA','0:07:40',' hour angle (H:M:S)',AFTER='ZD'
        SXADDPAR,hdr,'ST','12:42:16.7',' sidereal time',AFTER='HA'
        SXADDPAR,hdr,'AIRMASS',1.185,' airmass',AFTER='ST'
      END
    '1704094': BEGIN
        SXADDPAR,hdr,'RA','12:34:27',' right ascension (telescope)',AFTER='DCS_TIME'
        SXADDPAR,hdr,'DEC','02:12:02',' declination (telescope)',AFTER='RA'
        SXADDPAR,hdr,'EPOCH',2000.0,' epoch of RA & DEC',AFTER='DEC'
        SXADDPAR,hdr,'ZD',32.6,' zenith distance (degrees)',AFTER='EPOCH'
        SXADDPAR,hdr,'HA','0:15:19',' hour angle (H:M:S)',AFTER='ZD'
        SXADDPAR,hdr,'ST','12:49:55.7',' sidereal time',AFTER='HA'
        SXADDPAR,hdr,'AIRMASS',1.2,' airmass',AFTER='ST'
      END
    '1704095': BEGIN
        SXADDPAR,hdr,'RA','12:34:36',' right ascension (telescope)',AFTER='DCS_TIME'
        SXADDPAR,hdr,'DEC','02:11:50',' declination (telescope)',AFTER='RA'
        SXADDPAR,hdr,'EPOCH',2000.0,' epoch of RA & DEC',AFTER='DEC'
        SXADDPAR,hdr,'ZD',34.32,' zenith distance (degrees)',AFTER='EPOCH'
        SXADDPAR,hdr,'HA','0:38:22',' hour angle (H:M:S)',AFTER='ZD'
        SXADDPAR,hdr,'ST','13:12:57.7',' sidereal time',AFTER='HA'
        SXADDPAR,hdr,'AIRMASS',1.213,' airmass',AFTER='ST'
      END
    '1704096': BEGIN
        SXADDPAR,hdr,'RA','12:34:38',' right ascension (telescope)',AFTER='DCS_TIME'
        SXADDPAR,hdr,'DEC','02:13:52',' declination (telescope)',AFTER='RA'
        SXADDPAR,hdr,'EPOCH',2000.0,' epoch of RA & DEC',AFTER='DEC'
        SXADDPAR,hdr,'ZD',55.59,' zenith distance (degrees)',AFTER='EPOCH'
        SXADDPAR,hdr,'HA','03:59:11',' hour angle (H:M:S)',AFTER='ZD'
        SXADDPAR,hdr,'ST','15:33:48.7',' sidereal time',AFTER='HA'
        SXADDPAR,hdr,'AIRMASS',1.784,' airmass',AFTER='ST'
      END
    '1707065': BEGIN
; Fix a few invalid entries first.
;; NOTE: Some of the other bad-TCS images might have the same problem.
        SXADDPAR,hdr,'DATE-OBS','2004-03-29T00:07:00.0',' Date of observation start'
        SXADDPAR,hdr,'UTSHUT','00:07:0.0',' UT of shutter open'
        SXADDPAR,hdr,'UT','23:42:00.0',' UT of TCS coords'

        SXADDPAR,hdr,'RA','06:42:30.90',' right ascension (telescope)',AFTER='DCS_TIME'
        SXADDPAR,hdr,'DEC','-45:09:45.3',' declination (telescope)',AFTER='RA'
        SXADDPAR,hdr,'EPOCH',2000.0,' epoch of RA & DEC',AFTER='DEC'
        SXADDPAR,hdr,'ZD',17.62,' zenith distance (degrees)',AFTER='EPOCH'
        SXADDPAR,hdr,'HA','00:46:59.5',' hour angle (H:M:S)',AFTER='ZD'
        SXADDPAR,hdr,'ST','07:29:32.4',' sidereal time',AFTER='HA'
        SXADDPAR,hdr,'AIRMASS',1.065,' airmass',AFTER='ST'
      END
    ELSE: BEGIN
; Not one of our problem images.
      END
  ENDCASE

  RETURN,hdr
END
