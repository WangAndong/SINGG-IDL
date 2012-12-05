pro medhdr,filelist,nsig,iterations

PRINT,'Inputs in the form of filelist(in quotes),number of sigma,number of iterations' 

;pro medhdr
;filelist=''
;READ,filelist,PROMPT='Enter name of file list'
;READ,nsig,PROMPT='Enter number of sigma'
;READ,iterations,PROMPT='Enter number of iterations'



;runs on a list of files (such as filelist in /aspen/usr2/furst) rejecting
;outliers deviating by more than nsig*sigma.  If more than 1 iterations is set,
;it will define a new sigma after the first set of outliers are rejected 
;and will run again.

;the purpose of this program is to provide HIIphot.pro with the value of 
;PSFfwhm_pix which is related to the median value of the FWHM values of the
;appropriate *.stars files.  It also provides the distance to the galaxy
;in Mpc for HIIphot.

READCOL,filelist,ha0fits,rssfits,rsubssfits,csubssfits,ha0stars,rstars, $
rintstars,contstars,contintstars,FORMAT='A,A,A,A,A,A,A,A,A',SKIPLINE=4

OPENW,1,FILEPATH('filelist.out',ROOT_DIR='/cibola/data1/singg/furst/progtest')



PRINTF,1,'The initial conditions include the number of sigma' 
PRINTF,1,'used as a rejection criteria in 1 and the number of iterations run by the program in 2' 
;these value are the same for all files
PRINTF,1,''  
PRINTF,1,'The results are begun with the file name followed by the trunction value (1)'
PRINTF,1,'the original (pre-outlier rejection)'
PRINTF,1,'sigma in column 2, median(3) and mean(4), followed by the number of'
PRINTF,1,'rejected outliers in 5 and finally the final sigma(6), median(7) and mean(8).'

PRINTF,1,''
PRINTF,1,FORMAT='("conditions:"5X,F4.2,5X,I1)',nsig,iterations




;#############################################################################




FOR i1=0,N_ELEMENTS(ha0fits)-1 DO BEGIN

fitsfile=ha0fits(i1)
file=ha0stars(i1)
PRINT,file,fitsfile

;define the inputs for medhdrval, which will actually determine the median.
;the "file" is the .stars file from which the FWHM information is taken
;while the fitsfile contains the header to which it will be written.

;this section deals with the ha0 files.

;########



medhdrval,file,fitsfile,nsig,iterations



ENDFOR 

;medhdrval runs on the inputs taken from teh file list.  input description above.
;#####################################################################




FOR j1=0,N_ELEMENTS(rssfits)-1 DO BEGIN
IF rssfits(j1) EQ 'none' AND rstars(j1) EQ 'none' THEN BEGIN
	PRINT,'no file'
ENDIF ELSE BEGIN

	IF rssfits(j1) EQ 'none' THEN BEGIN
		PRINT,'no file'
	ENDIF ELSE BEGIN
		fitsfile=rssfits(j1)
	ENDELSE
	IF rintstars(j1) EQ 'none' THEN BEGIN
		file=rstars(j1)
	ENDIF ELSE BEGIN
		file=rintstars(j1)
	ENDELSE

;the rintss.fits.stars file is prefered over the rss.fits.stars file.  If the rint file exists,
;it will be used.  Otherwise, the r will be used.  

;###########


medhdrval,file,fitsfile,nsig,iterations




;##########


ENDELSE
ENDFOR






;#####################################################################






FOR k1=0,N_ELEMENTS(rsubssfits)-1 DO BEGIN



IF rsubssfits(k1) EQ 'none' THEN BEGIN
	PRINT,'no fits file'
ENDIF ELSE BEGIN
	fitsfile=rsubssfits(k1)
	IF ha0stars(k1) EQ 'none' THEN BEGIN
		PRINT,'no ha0stars file'
	ENDIF ELSE BEGIN
		ha0stars1=ha0stars(k1)
		READCOL,ha0stars1,a,b,c,d,e,f,g,h,FWHM_imha0,aa,format='',/SILENT
		medianha0=MEDIAN(FWHM_imha0)
	ENDELSE

	IF rintstars(k1) EQ 'none' THEN BEGIN
		IF rstars(k1) EQ 'none' THEN BEGIN
			PRINT,'no rint or r file'
		ENDIF ELSE BEGIN
			rstars1=rstars(k1)
			READCOL,rstars1,a,b,c,d,e,f,g,h,FWHM_imr,aa,format='',/SILENT
			medianr=MEDIAN(FWHM_imr)
			medianrint=0
		ENDELSE
	ENDIF ELSE BEGIN
		rintstars1=rintstars(k1)
		READCOL,rintstars1,a,b,c,d,e,f,g,h,FWHM_imrint,aa,format='',/SILENT
		medianrint=MEDIAN(FWHM_imrint)
		medianr=0
	ENDELSE



	IF medianha0 GT (medianr OR medianrint) THEN BEGIN
		file=ha0stars(k1)



medhdrval,file,fitsfile,nsig,iterations

	ENDIF ELSE BEGIN

		IF medianrint GT 0 THEN BEGIN

			file=rintstars(k1)
medhdrval,file,fitsfile,nsig,iterations


		ENDIF ELSE BEGIN 
			IF medianr GT 0 THEN BEGIN

				file=rstars(k1)
medhdrval,file,fitsfile,nsig,iterations

			ENDIF ELSE BEGIN
				PRINT,'no r or rint file'
			ENDELSE
		ENDELSE

	ENDELSE
ENDELSE


ENDFOR


;puts whichever has a greater value, the median of the ha0.stars or the median of the r.stars 
;or rint.stars, is put into the rsubss.fits header. 
;################################################################







FOR w1=0,N_ELEMENTS(csubssfits)-1 DO BEGIN

READCOL,filelist,ha0fits,rssfits,rsubssfits,csubssfits,ha0stars,rstars, $
rintstars,contstars,contintstars,FORMAT='A,A,A,A,A,A,A,A,A',/SILENT,SKIPLINE=4

IF csubssfits(w1) EQ 'none' THEN BEGIN
	PRINT,'no fits file'
ENDIF ELSE BEGIN
	fitsfile=csubssfits(w1)
	IF ha0stars(w1) EQ 'none' THEN BEGIN
		PRINT,'no ha0stars file'
	ENDIF ELSE BEGIN
		ha0stars1=ha0stars(w1)
		READCOL,ha0stars1,a,b,c,d,e,f,g,h,FWHM_imha0,aa,format='',/SILENT
		medianha0=MEDIAN(FWHM_imha0)
	ENDELSE

	IF contintstars(w1) EQ 'none' THEN BEGIN
		IF contstars(w1) EQ 'none' THEN BEGIN
			PRINT,'no contint or cont file'
		ENDIF ELSE BEGIN
			contstars1=contstars(w1)
			READCOL,contstars1,a,b,c,d,e,f,g,h,FWHM_imcont,aa,format='',/SILENT
			mediancont=MEDIAN(FWHM_imcont)
			mediancontint=0
		ENDELSE
	ENDIF ELSE BEGIN
		contintstars1=contintstars(w1)
		READCOL,contintstars1,a,b,c,d,e,f,g,h,FWHM_imcontint,aa,format='',/SILENT
		mediancontint=MEDIAN(FWHM_imcontint)
		mediancont=0
	ENDELSE



	
	IF medianha0 GT (mediancont OR mediancontint) THEN BEGIN
		file=ha0stars(w1)

medhdrval,file,fitsfile,nsig,iterations

	ENDIF ELSE BEGIN

		IF mediancontint GT 0 THEN BEGIN

			file=contintstars(w1)
medhdrval,file,fitsfile,nsig,iterations


		ENDIF ELSE BEGIN 
			IF mediancont GT 0 THEN BEGIN

				file=contstars(w1)
medhdrval,file,fitsfile,nsig,iterations

			ENDIF ELSE BEGIN
				PRINT,'no cont or contint file'
			ENDELSE
		ENDELSE

	ENDELSE
ENDELSE


ENDFOR



CLOSE,1

;uses the same criteria for csub as rsub (see previous comment)
;#############################################

READCOL,'SAMPLE.lst',prefix,b,c,d,e,f,g,h,i,distance,FORMAT='A,A,A,A,A,A,A,A,A,F',/SILENT
FOR c1=0,N_ELEMENTS(rsubssfits)-1 DO BEGIN
FOR d1=0,N_ELEMENTS(prefix)-1 DO BEGIN
IF rsubssfits(c1) EQ 'none' THEN BEGIN
;	PRINT,'no rsubss file'
ENDIF ELSE BEGIN
	hdrtest=STRMID(rsubssfits(c1),0,5)
	disttest=STRMID(prefix(d1),0,5)
;	PRINT,hdrtest,disttest
	IF hdrtest EQ disttest THEN BEGIN
		dist=distance(d1)
		disthdr=rsubssfits(c1)
		header=STRING(REPLICATE(32B,80))
		readfitsfile=READFITS(disthdr,header)
		FXADDPAR,header,"DIST_MPC",dist,"Distance to object (mpc)"
		WRITEFITS,disthdr,readfitsfile,header
		;opens the fits file and adds the "DIST_MPC" value to the header
	ENDIF
ENDELSE	 

;adds the distance in Mpc to the rsubss file from this SAMPLE.lst file provided

IF csubssfits(c1) EQ 'none' THEN BEGIN
;	PRINT,'no csubss file'
ENDIF ELSE BEGIN
	hdrtest1=STRMID(csubssfits(c1),0,5)
	disttest1=STRMID(prefix(d1),0,5)
;	PRINT,hdrtest1,disttest1
	IF hdrtest1 EQ disttest1 THEN BEGIN
		dist1=distance(d1)
		disthdr1=csubssfits(c1)
		header1=STRING(REPLICATE(32B,80))
		readfitsfile1=READFITS(disthdr1,header1)
		FXADDPAR,header1,"DIST_MPC",dist1,"Distance to object (mpc)"
		WRITEFITS,disthdr1,readfitsfile1,header1
		;opens the fits file and adds the "DIST_MPC" value to the header
	ENDIF
ENDELSE

;does the same for the csub

ENDFOR 
ENDFOR	
END