;+
; NAME:
;  openmysql
; PURPOSE:
;  Open a mySQL database for operations via a pipe.
; DESCRIPTION:
; CATEGORY:
;  Database
; CALLING SEQUENCE:
;  openmysql,lun,dbname
; INPUTS:
; OPTIONAL INPUT PARAMETERS:
;  dbname - Name of database to open at start (default=no database).
; KEYWORD INPUT PARAMETERS:
; OUTPUTS:
;  lun - the logical unit of the pipe (use free_lun to close).
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
;  Assumes that your .my.cnf file points to the correct mysql server and that
;    it includes the login information.  Also, the command 'mysql' must appear
;    in your default path.
; PROCEDURE:
; MODIFICATION HISTORY:
;  Written by Marc W. Buie, Lowell Observatory, 2002/01/09
;-
pro openmysql,lun,dbname

   if badpar(lun,[0,2,3],0,caller='openmysql (lun) ') then return
   if badpar(dbname,[0,7],0,caller='openmysql (dbname) ',default='') then return

   cmd = 'mysql -B -q -n '+dbname
   spawn,cmd,unit=lun

end
