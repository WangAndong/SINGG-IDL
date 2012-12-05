pro test, ofid
  imgr = 'imgr'
  imgha = 'imgha'
  imgn  = 'imgn'
  imgf  = 'imgf'
  hdr   = 'hdr'
  hdha  = 'hdha'
  hdn   = 'hdn'
  hdf   = 'hdf'
  case ofid of 
     0: begin
          oimg = imgr
          ohd  = hdr
        end
     1: begin
          oimg = imgha
          ohd  = hdha
        end
     2: begin
          oimg = imgn
          ohd  = hdn
        end
     3: begin
          oimg = imgf
          ohd  = hdf
        end
     else: begin
          oimg = imgr
          ohd  = hdr
        end
  endcase
print, oimg
print, ohd
end 


