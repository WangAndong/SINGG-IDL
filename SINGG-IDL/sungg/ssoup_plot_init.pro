pro ssoup_plot_finish, fjpg, feps, wxsize, wysize, epilepsy=epilepsy
  ;
  ; Finishes a plot, outputting it to PS, JPEG and possibly the screen.
  ; 
  ; fjpg -> jpg file name
  ; feps -> eps file name
  ; wxsize -> horizontal size of jpg
  ; wysize -> vertical size of jpg
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
    spawn,"gs -sDEVICE=jpeg -o " + fjpg + " -dJPEGQ=100 -g" + numstr(wxsize) + "x" + numstr(wysize) + " " + feps
    
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