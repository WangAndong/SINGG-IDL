FUNCTION namps, prefix, id
   name = prefix + strtrim(string(id),2) + '.ps'
   return, name
END 

