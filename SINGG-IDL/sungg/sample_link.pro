FUNCTION sample_link, obj
  ;
  ; returns URL for the singg_sample web page for object obj 
  ;
  ; G. Meurer (UWA/ICRAR) 07/2010
  burl      = 'http://sungg.pha.jhu.edu/protected/'
  IF obj NE '' THEN BEGIN 
     url    = burl+'/SINGG/Sample/WWW/'+obj+'.html'
  ENDIF ELSE BEGIN
     url    = burl+'singg_sample.html'
  ENDELSE 
  return, url
END 
