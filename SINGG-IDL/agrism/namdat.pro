FUNCTION namdat, prefix, id
   name = prefix + strtrim(string(id),2) + '.dat'
   return, name
END 

