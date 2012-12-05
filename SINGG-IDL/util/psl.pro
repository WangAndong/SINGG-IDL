pro psl,dummy,eps=eps

set_plot,'ps'
device,/landscape
if keyword_set(eps) then device,/encapsulated

end
