PRO shunt_class_key, unit
   ;
   ; print html classification key to table
   ;
   ; G. Meurer 09/2005
   printf, unit, '<p><TABLE border=1 cellpadding=3>'
   printf, unit, '<tr bgcolor="#a8a8a8" valign=middle><th>SHUNT spectra eye classification scheme</th></tr>'
   printf, unit, '<tr bgcolor="#c0eec0" valign=middle><td>S : this really looks like a Supernova (to me)!</td></tr>'
   printf, unit, '<tr bgcolor="#c0c0ee" valign=middle><td>A : Absorption spectrum, neither (obviously) K nor M star </td></tr>'
   printf, unit, '<tr bgcolor="#eec0c0" valign=middle><td>M : obvious M star </td></tr>'
   printf, unit, '<tr bgcolor="#eec0c0" valign=middle><td>K : obvious K star </td></tr>'
   printf, unit, '<tr bgcolor="#eec0ee" valign=middle><td>B : Break spectrum</td></tr>'
   printf, unit, '<tr bgcolor="#c0eeee" valign=middle><td>E : Emission line spectrum </td></tr>'
   printf, unit, '<tr bgcolor="#eeeeee" valign=middle><td>O : Other order (not first order)</td></tr>'
   printf, unit, '<tr bgcolor="#eeeec0" valign=middle><td>U : Unclear, flat or featureless spectrum</td></tr>'
   printf, unit, '<tr bgcolor="#ffffd0" valign=middle><td>D : below s/n or brightness cut</td></tr>'
   printf, unit, '<tr bgcolor="#eeeeee" valign=middle><td>O : Other order (not first order)</td></tr>'
   printf, unit, '</table></p>'
END 
