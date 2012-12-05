FUNCTION namstmp, pstmp, id
   name = pstmp + strtrim(string(id),2) + '.png'
   return, name
END 

