;+
; NAME:
;  mysqldocscan
; PURPOSE:
;  Scan a database and report discrepancies in the documentation table.
; DESCRIPTION:
;  This routine uses a standard table named "doc" in a database and compares
;  its entries with the tables and fields actually defined in the database.
;  If inconsistencies are found, they are displayed on standard output.
; CATEGORY:
;  Database
; CALLING SEQUENCE:
;  mysqldocscan,dblun
; INPUTS:
;  dblun - the logical unit of the pipe (opened by openmysql).
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
; OUTPUTS:
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  Written by Doug Loucks, Lowell Observatory, 2005/10/08
;-

pro mysqldocscan, dblun
   if badpar(dblun,[2,3],0,caller='mysqldocscan (dblun) ') then return

   tab = string(byte(9))

   ; Retrieve all of the table names and field names defined in the doc table.
   cmd = 'select tablename,field,source from doc;'

   mysqlquery,dblun,cmd,tablename,field,source, format='(a,a,a)'

   ; Extract a list of unique table names.
   u_tablename = tablename[uniq(tablename,sort(tablename))]

   ; Retrieve a list of the tables that are actually defined in the database.
   cmd = 'show tables;'
   mysqlcmd,dblun,cmd,actual_tables,num_actual_tables

   if num_actual_tables lt 2 then begin
      print, 'No tables defined for database.'
      return
   endif

   print, 'Documentation scan results:'

   ; Check for documentation of tables that no longer exist in the database.

   for j=0L,n_elements(u_tablename)-1L do begin
      if u_tablename[j] eq 'NULL' then continue
      w0 = where(actual_tables eq u_tablename[j], w0count)

      if w0count eq 0L then begin
         print, '  Documented table ' + u_tablename[j] +$
            ' not found in database.'
      endif
   endfor



   ; Go through the actual tables. Check if each table is documented. If
   ; not, print a message. Otherwise, go through each actual field and
   ; check if each field is documented. If not, print a message.

   for j=1,num_actual_tables-1 do begin
      ; Don't care about the doc table.
      if actual_tables[j] eq 'doc' then continue

      ; Get the documented fields for this table.
      w1 = where(tablename eq actual_tables[j], w1count)

      if w1count gt 0L then begin
         ; There is at least one entry for this table.
         doc_fields = field[w1]
         doc_sources = source[w1]
         w2 = where(doc_sources eq 'xxHIDExx', w2count)
         if w2count gt 0L then continue
      endif else begin
         ; There is no documentation for this table.
         print, '  Table ' + actual_tables[j] + ' not documented.'
         continue
      endelse

      ; Get a list of the fields defined for this table.
      cmd = 'describe ' + actual_tables[j] + ';'
      mysqlcmd,dblun,cmd,actual_fields,num_actual_fields
      if num_actual_fields le 1 then continue

      ; Go through the list of fields and check if each field is documented.

      actual_field_names = strarr(num_actual_fields-1)

      for k=1,num_actual_fields-1 do begin
         tmp_str = strsplit(actual_fields[k],tab,/extract)
         actual_field_names[k-1] = tmp_str[0]
         w2 = where(doc_fields eq tmp_str[0], w2count)

         if w2count eq 0L then begin
            print, '  Field ' + tmp_str[0] + ' in table ' +$
               actual_tables[j] + ' not documented.'
         endif

      endfor

      ; Locate documented fields for the current table that are not in
      ; the table in the database.

      for m=0L,n_elements(doc_fields)-1L do begin
         if doc_fields[m] eq 'NULL' then continue
         w3 = where(actual_field_names eq doc_fields[m], w3count)

         if w3count eq 0L then begin
            print, '  Documented field ' + doc_fields[m] +$
               ' not found in table ' + actual_tables[j] + '.'
         endif
      endfor

   endfor
end
