  ; fjpg     -> jpg file name
  ; feps     -> eps file name
  ; xs       -> eps plot parameter
  ; ys       -> eps plot parameter
  ; xoff     -> eps plot parameter
  ; yoff     -> eps plot parameter
  ; wxsize   -> horizontal size of jpg
  ; wysize   -> vertical size of jpg
  ; epilepsy -> display plot on screen
  
pro ssoup_plot_finish, fjpg, feps, xs, ys, xoff, yoff, wxsize, wysize, epilepsy=epilepsy
    
    ; grab Z buffer
    im = tvrd(true=3)
    
    ; write EPS
    set_plot,'ps',/copy, /interpolate
    erase
    IF strpos(strlowcase(feps), '.eps') GT 0 THEN $
        device,/inches,file=feps,xs=xs,ys=ys,yo=yoff,xo=xoff,/color,/encapsulated $
        ELSE $
        device,/inches,file=feps,xs=xs,ys=ys,yo=yoff,xo=xoff,/color
    tv,im,true=3
    device,/close
    
    ; write JPG
    write_jpeg,fjpg,im,TRUE=3,QUALITY=100

    ; display Z buffer on screen
    ; P.S. only works on REAL operating systems
    set_plot, "X", /copy, /interpolate
    if keyword_set(epilepsy) then begin
        window,0,xsize=wxsize,ysize=wysize
        tv,im,true=3
    endif
    
    ; reset plot device
    COMMON deviceprevious, thisdevice
    set_plot,thisdevice
        
end

  ; wxsize   -> horizontal size of jpg
  ; wysize   -> vertical size of jpg
pro ssoup_plot_init, wxsize, wysize
    ; restore this on end
    COMMON deviceprevious, thisdevice
    thisDevice = !D.Name
    ; swap to Z buffer
    set_plot,'Z',/copy, /interpolate
    erase
    device,set_resolution=[wxsize,wysize],set_pixel_depth=24,decomposed=1
end