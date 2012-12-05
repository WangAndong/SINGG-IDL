function numstr, num
  ;
  ; convert a number to a simople trimmed string
  ;
  ; G. Meurer 6/2010
  ;
  str       = strtrim(string(num),2)
  return, str
end 
