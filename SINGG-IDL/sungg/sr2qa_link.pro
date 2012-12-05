FUNCTION sr2qa_link, obj
  ;
  ; return the URL of the SR2QA web page for obj
  ;
  ; G. Meurer  (UWA/ICRAR)  07/2010
  burl      = 'http://sungg.pha.jhu.edu/protected/SINGG/SR2QA/'
  IF obj NE '' THEN BEGIN 
     url    = burl+obj+'/'+obj+'_sr2qa.html'
  ENDIF ELSE BEGIN
     url    = burl+'index.html'
  ENDELSE 
  return, url
END 
