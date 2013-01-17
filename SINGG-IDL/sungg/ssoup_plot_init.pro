pro ssoup_plot_finish, fjpg, feps, wxsize, epilepsy=epilepsy
  ;
  ; Finishes a plot, outputting it to PS, JPEG and possibly the screen.
  ; 
  ; fjpg -> jpg file name
  ; feps -> eps file name
  ; wxsize -> horizontal size of jpg. The vertical size of the JPG is determined by the 
  ; postscript bounding box.
  ; epilepsy -> display plot on screen
  ; 
  ; S. Andrews (ICRAR/UWA) 01/2013

    ; finish PS
    device,/close
    set_plot,'X',/copy
    setplotcolors
    setbgfg,!white,!black
    
    ; write JPG (requires ghostscript)
    ; I hate you, IDL, for making it near impossible to use the Z buffer
    ; This magic incantation crops the PS such that it doesn't have much white space around it
    ; http://stackoverflow.com/questions/12484353/how-to-crop-a-section-of-a-pdf-file-to-png-using-ghostscript/12485020
    spawn,"gs -q -sDEVICE=bbox -dBATCH -dNOPAUSE -dLastPage=1 " + feps + " 2>&1 | grep %%BoundingBox > temp.txt"
    openr,52,"temp.txt",error=err
    if err ne 0 then plog,-1,"SSOUP_PLOT_FINISH:","Failed to create JPG"
    strtemp=""
    while not eof(52) do readf,52,strtemp
    crap = ""
    reads,strtemp,crap,x1,y1,x2,y2,format='(A14, 4(x, I0))'
    x2 = fix(x2)
    wysize = 1L*fix(y2)*wxsize/x2
    res = 72L*wxsize/x2 ; Short ints? Who uses those any more?
    ; We need to increase right/top margins a little.
    wxsize = fix(wxsize*1.05)
    wysize = fix(wysize*1.05)
    spawn,"gs -sDEVICE=jpeg -o " + fjpg + " -dJPEGQ=100 -r" + numstr(res) + " -g" + numstr(wxsize) $
        + "x" + numstr(wysize) + " -c '<</Install {0 0 translate}>> setpagedevice' -dLastPage=1 -f " + feps
    free_lun,52
    
    ; display JPG on screen
    if keyword_set(epilepsy) then begin
        read_jpeg,fjpg,im,true=1 
        window,0,xsize=wxsize,ysize=wysize
        tv,im,true=1
    endif
end

pro ssoup_plot_init, feps, xs, ys, xoff, yoff
  ;
  ; Starts a plot.
  ;
  ; feps -> eps file name
  ; xs -> eps plot parameter
  ; ys -> eps plot parameter
  ; xoff -> eps plot parameter
  ; yoff -> eps plot parameter
 
    ; write to PS
    set_plot,'ps',/copy, /interpolate
    IF strpos(strlowcase(feps), '.eps') GT 0 THEN $
       device,/inches,file=feps,xs=xs,ys=ys,yo=yoff,xo=xoff,/color,/encapsulated $
    ELSE $
       device,/inches,file=feps,xs=xs,ys=ys,yo=yoff,xo=xoff,/color
    setplotcolors
    setbgfg,!white,!black
end