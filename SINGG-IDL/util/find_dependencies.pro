pro find_dependencies
  fili  = 'dependencies_list.txt'
  filo  = 'dependencies.out'
  wd    = '~/IDL/Pro/Work/'
  readcol, fili, prog, format='(a)'
  np = n_elements(prog)
  cd, wd, current=cwd
  spawn, 'touch '+filo
  for ii = 0, np-1 do begin
     print,prog[ii]
     cmd = 'find . | grep -i "/'+prog[ii]+'.pro" | grep -v ".pro-" | grep -v ".pro~" >> '+filo
     spawn,cmd
  endfor 
  spawn,'mv -f '+filo+' '+cwd
  cd,cwd
end 
