FUNCTION namspec, pspec, id
   name = pspec + strtrim(string(id),2) + '.png'
   return, name
END 

