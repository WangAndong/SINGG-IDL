FUNCTION nampng, prefix, id
   name = prefix + strtrim(string(id),2) + '.png'
   return, name
END 

FUNCTION namdat, prefix, id
   name = prefix + strtrim(string(id),2) + '.dat'
   return, name
END 

FUNCTION namstmp, pstmp, id
   name = pstmp + strtrim(string(id),2) + '.png'
   return, name
END 

FUNCTION namribn, pribn, id
   name = pribn + strtrim(string(id),2) + '.png'
   return, name
END 

FUNCTION namspec, pspec, id
   name = pspec + strtrim(string(id),2) + '.png'
   return, name
END 

