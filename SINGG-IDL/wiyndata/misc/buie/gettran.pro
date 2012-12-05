;+
; NAME:
;  gettran
; PURPOSE:
;  Find and return transformation solution for a given night and instrument
; DESCRIPTION:
; CATEGORY:
;  Photometry
; CALLING SEQUENCE:
;  gettran,inst,date,filter,color1,color2,tran,transig,jdref
; INPUTS:
;  inst - Instrument code
;  date - YYMMDD string of date of observation
;  filter - Landolt filter code 01234 is UBVRI
;  color1 - filter code for first color
;  color2 - filter code for second color (uses color index of C1-C2)
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
; OUTPUTS:
;  tran    - Transformation coefficients
;  transig - uncertainties
;  jdref   - Time reference point for extinction
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
; 96/10/17 - Written by Marc W. Buie, Lowell Observatory
; 97/2/6, MWB, added time dependent extinction term
; 2004/02/09, MWB, changed path to transf files.
;-
pro gettran,inst,date,filter,color1,color2,tran,transig,jdref

   if badpar(inst,  7,      0,caller='GETTRAN: (inst) ') then return
   if badpar(date,  7,      0,caller='GETTRAN: (date) ') then return
   if badpar(filter,[1,2,3],0,caller='GETTRAN: (filter) ') then return
   if badpar(color1,[1,2,3],0,caller='GETTRAN: (color1) ') then return
   if badpar(color2,[1,2,3],0,caller='GETTRAN: (color2) ') then return

   sfnames=['U','B','V','R','I']
   ifnames=['u','b','v','r','i']

   blanks='          '
   tagdate=date+blanks
   taginst=inst+blanks
   tagdate=strmid(tagdate,0,6)
   taginst=strmid(taginst,0,10)
   tag=taginst+' '+tagdate

   fil = ifnames[filter]
   c1  = ifnames[color1]
   c2  = ifnames[color2]

   histname='/frakir/raid/buie/Reduced/transf_'+fil+'.'+c1+'m'+c2

   if not exists(histname) then begin
      print,'GETTRAN: ERROR! Transformation file ',histname,' not found'
      return
   endif

   str1=''
   str2=''
   openr,lun,histname,/get_lun
   while(not eof(lun) and str1 ne tag) do begin
      readf,lun,str1,str2,format='(a17,a90)'
   endwhile
   free_lun,lun

   if str1 ne tag then begin
      print,'GETTRAN: ERROR! ',sfnames[filter], $
         ', (',sfnames[color1],'-',sfnames[color2], $
         ') transformation for [',tag,'] not found'
      return
   endif
   
   r1 = 0.0d0
   reads,str2,v1,e1,v2,e2,v3,e3,v4,e4,v5,e5,r1, $
      format='(5(1x,f7.4,1x,f6.4),1x,d13.5)'

   tran    = [v1,v2,v3,v4,v5]
   transig = [e1,e2,e3,e4,e5]
   jdref   = r1

end
