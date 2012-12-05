PRO singg_photcal,runname
; Simple wrapper program for fc_photcal.  This must be run in the STD directory
; for each run.
; INPUT
;  runname             Name of run, i.e. "Run03"

  filstdm  = 'std_measure.dat'     ; output from std_measure

  filstde  = 'std_measure_ed.dat'  ; Output from fc_photcal, which
                                   ; is in the same format filstdm, but
                                   ; contains edited photometry -
                                   ; the edits consists of flags.

;; Replace this with an actual database read.
  filfilts = !singgdir+'filtfile.dat'       ; filter data base
   
  prefix   = STRTRIM(runname,2)+'_all' ; Prefix for output files.  These 
				       ; include:
				       ; <prefix>.par : output parameters
 				       ; <prefix>.html : web page
				       ; <prefix>_<filter>.dat : data for a 
				       ;                       given filter.
				       ; <prefix>_<filter>.png : plot for
				       ;                       the filter.

  username = 'G.R. Meurer'         ; username - used in webpage.

  email    = 'meurer@pha.jhu.edu'  ; email - used in webpage

; Run it once in interactive mode:
  fc_photcal, filstdm, filstde, filfilts, prefix, username, email=email, /interac
 
; Then, as cleanup, run it once in non-interactive mode:
  fc_photcal, filstde, 'junk.out', filfilts, prefix, username, email=email

END
