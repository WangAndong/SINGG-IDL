FUNCTION namjpg, prefix, id
   name = prefix + strtrim(string(id),2) + '.jpg'
   return, name
END 

