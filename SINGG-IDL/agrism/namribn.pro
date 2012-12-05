FUNCTION namribn, pribn, id
   type = '.jpg'
   ;type = '.png'
   name = pribn + strtrim(string(id),2) + type
   return, name
END 

