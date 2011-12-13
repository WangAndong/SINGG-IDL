pro mkprimary, fitsname, newfitsname, exten

; Copy an extension image to a primary header

fits_read,fitsname,im,h,exten=exten, /no_pdu
 fxaddpar, header, 'SIMPLE', 'T',' Written by IDL:  '+ systime()
 sxdelpar,header,['XTENSION','EXTNAME','EXTVER','EXTLEVEL']
fits_write,newfitsname,im,h         ;Write to primary file

return
end
