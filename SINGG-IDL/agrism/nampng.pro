FUNCTION nampng, prefix, id
   name = prefix + strtrim(string(id),2) + '.png'
   return, name
END 

