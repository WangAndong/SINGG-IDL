;+
; NAME:
;    qinput
; PURPOSE: (one line)
;    Prompt user for input from a widget.
; DESCRIPTION:
;
; CATEGORY:
;    Widgets
; CALLING SEQUENCE:
;    result = qinput( [keywords] )
; INPUTS:
;
; OPTIONAL INPUT PARAMETERS:
;
; KEYWORD PARAMETERS:
;    DEFAULT  = String. Default value.
;    FLOATING = Set this keyword to accept a floating-point value.
;    GROUP_LEADER = Group leader id. If present, qinput runs as a modal
;                   application. Otherwise, it runs as a non-modal application.
;    PROMPT   = Character string to be displayed as a prompt.
;    INTEGER  = Set this keyword to accept an integer value.
;    LONG     = Set this keyword to accept a longword integer value.
;    STRING   = Set this keyword to accept a string value.
;    TITLE    = Title for the widget.
; OUTPUTS:
;    result = the returned value.
; COMMON BLOCKS:
;
; SIDE EFFECTS:
;    Suspends other widget event generation.  A user response is required.
; RESTRICTIONS:
;
; PROCEDURE:
;
; MODIFICATION HISTORY:
;    Written by Doug Loucks, Lowell Observatory, July 27, 1994.
;    2002/03/06, Marc W. Buie, fixed bug in handling keyword type switches,
;                  added DEFAULT keyword.
;
;    2004/04/12, Doug Loucks, Cleaned up event handling. Added code that uses
; pointer variables. Added a 'Cancel' button.
;-


; -----------------------------------------------------------------------------
; Procedure qinput_event
; Default event handler. This event handler receives events from the
; CW_FIELD compound widget.
; -----------------------------------------------------------------------------
pro qinput_event, event
   compile_opt hidden

   ; Get the pointer to the result variable.
   widget_control, event.top, get_uvalue=pvalue

   ; Store the value of the text widget.
   *pvalue = event.value

   widget_control, event.top, /DESTROY
end


; -----------------------------------------------------------------------------
; Procedure qinput_cancel_event
; Event handler for the 'Cancel' button.
; -----------------------------------------------------------------------------
pro qinput_cancel_event, event
   compile_opt hidden

   ; Get the pointer to the result variable.
   widget_control, event.top, get_uvalue=pvalue

   ; Store a null string into the result variable.
   *pvalue = ''

   widget_control, event.top, /DESTROY
end

; ------------------------------------------------------------------------------
; Function qinput
; ------------------------------------------------------------------------------
function qinput, PROMPT=prompt, FLOATING=floating, INTEGER=integer,$
   GROUP_LEADER=group_leader,$
   LONG=long, STRING=string, title=title, DEFAULT=default

   if not keyword_set(prompt) then prompt='Input field:'

   if not keyword_set(title) then title='Input Request:'
   if not keyword_set(default) then default=''

   ; Define the top-level-base. It will be modal and will be positioned over
   ; the widget specified as the group leader.


   if keyword_set(group_leader) then begin
      tlb = widget_base(TITLE=title, COLUMN=1, /FLOATING,$
         GROUP_LEADER=group_leader, /MODAL)
   endif else begin
      tlb = widget_base(TITLE=title, COLUMN=1)
   endelse

   ; Add a CW_FIELD compound widget.
   dummy = cw_field(tlb,$
      FLOATING=floating,$
      INTEGER=integer,$
      LONG=long,$
      STRING=string,$
      /RETURN_EVENTS,$
      TITLE=prompt,$
      VALUE=default)

   ; Add a 'Cancel' button.
   dummy = widget_button(tlb, VALUE='Cancel', EVENT_PRO='qinput_cancel_event')

   ; The user's entry value will be stored in a heap variable, via a pointer.
   presult = ptr_new(/ALLOCATE_HEAP)

   widget_control, tlb, /REALIZE
   widget_control, tlb, SET_UVALUE=presult

   xmanager, 'qinput', tlb

   ; Get the value to be returned to the caller.
   if ptr_valid(presult) then begin
      rvalue = *presult
   endif else begin
      rvalue = ''
   endelse

   ; Free the pointer variable.
   ptr_free, presult

   return, rvalue
end
