PRO plot_mhistcat, cat, mcol, zeropoint, mlim, dm=dm, hardfile=hardfile
   ;
   ; Plot mag histogram of a catalog 
   ;
   qq         = "'"
   filtemp    = '_pcat.cat'
   xtitle     = '!3 Mag '
   ytitle     = '!3 Number '
   title      = '!3 '+cat
   IF NOT keyword_set(dm) THEN dm = 0.1
   IF keyword_set(hardfile) THEN BEGIN 
      psp
   ENDIF 
   ;
   ; awk out magnitude column and add zeropoint
   file_delete, filtemp, /allow_nonexistent, /quiet
   mcolstr    = strtrim(string(mcol),2)+'+'+strtrim(string(zeropoint),2)
   awkcmd     = 'awk '+qq+'substr($1,1,1) != "#" {printf "%s \n", $'+mcolstr+'}'+qq+' '+cat+' > '+filtemp
   print, awkcmd
   spawn, awkcmd
   ;
   readcol, filtemp, mag
   nmag       = histogram(mag, min=min(mlim), max=max(mlim), binsize=dm)
   mhist      = min(mlim) + dm*(findgen((max(mlim)-min(mlim))/dm)+ 1.0)
   plot, mhist, nmag, xrange=mlim, xstyle=1, psym=10, $
    xtitle=xtitle, ytitle=ytitle, title=title
   IF keyword_set(hardfile) THEN BEGIN 
      psend, hardfile, /noprint, /clobber
   ENDIF 
END 
