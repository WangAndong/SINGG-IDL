FUNCTION blem_selpeak, np, selmeth=selmeth
   ;
   ; select cross correlation peak for blind_emfind.
   ; If there are (np) two or less peaks, selection is automatic.
   ; np = 1 means that only the auto-correlation peak is
   ; valid, and it is then selected.  np = 2 means that there is
   ; one cross correlation peak as well as the auto correlation;
   ; cross correlation peak is then selected.  If np > 2 then
   ; the user selects the peak.
   ; 
   ; np      -> number of peaks to choose from
   ; selmeth <- the method of selection is returned to
   ;            this optional keyword as either "auto" or "hand".
   ; blem_selpeak <- returned as the chosen peak
   ;
   ; G. Meurer  12/2002 - originally written
   ;            06/2004 - added automatic selection for np < 3, 
   ;                      selmeth, and documentation.
   IF np LT 1 THEN message, 'Non valid value of np in blem_selpeak'
   selmeth = 'auto'
   CASE np OF 
      1 :   sel = 0
      2 :   sel = 1
      ELSE: BEGIN 
         REPEAT BEGIN 
            print, ' '
            print, 'To redo peak fitting type an integer => ', np
            print, 'To redo cut plots type an integer <= -1'
            print, 'Otherwise enter an integer from 0 to ', np-1
            read,  'Enter peak corresponding to grism detection : ', sel
            sel = fix(sel)
            ;goodsel = sel LE np-1
            goodsel = 1b
         ENDREP UNTIL goodsel
         sel = max([sel, -1])
         selmeth = 'hand'
      END 
   ENDCASE 
   IF selmeth EQ 'auto' THEN print, 'Peak '+strtrim(string(sel),2)+' automatically selected'
   return, sel
END 

