FUNCTION resolve_symlink, path  
  ;
  ; taken from IDL help for file_readlink
  savepath = path      ; Remember last successful translation  
  WHILE (path NE '') DO BEGIN  
    path = FILE_READLINK(path, /ALLOW_NONEXISTENT, $ 
      /ALLOW_NONSYMLINK)  
    IF (path NE '') THEN BEGIN  
      ; If returned path is not absolute, use it to replace the  
      ; last path segment of the previous path.  
      IF (STRMID(path, 0, 1) NE '/') THEN BEGIN  
        last = STRPOS(savepath, '/', /REVERSE_SEARCH)  
        IF (last NE -1) THEN path = STRMID(savepath, 0, last) $  
          + '/' + path  
      ENDIF  
      savepath = path  
    ENDIF  
  ENDWHILE  
  
  ; FILE_EXPAND_PATH removes redundant things like /./ from   
  ; the result.  
  RETURN, FILE_EXPAND_PATH(savepath)  
  
END
