;+
; NAME:
;    viewtext
; PURPOSE: (one line)
;    View a string, or string array, of text in a scrollable text widget.
; DESCRIPTION:
;
; CATEGORY:
;    Widgets
; CALLING SEQUENCE:
;    viewtext, text [, KEYWORDS]
; INPUTS:
;    text : String (scalar, 1-D or 2-D array) of text to be displayed.
; OPTIONAL INPUT PARAMETERS:
;
; KEYWORD PARAMETERS:
;    EXIT   = Label for exit button.  Default is 'Dismiss'.
;    FONT   = Font to use for the text.  Default is '8x13'.
;    GROUP  = Group Leader.
;    TITLE  = Title of widget.
;    XSIZE  = Width of text.  Default is 80 characters.
;    YSIZE  = Length of text.  Default is 40 lines.
; OUTPUTS:
;
; COMMON BLOCKS:
;
; SIDE EFFECTS:
;
; RESTRICTIONS:
;
; PROCEDURE:
;
; MODIFICATION HISTORY:
;    Written by Doug Loucks, Lowell Observatory, May, 1993.
;    1/7/94, DWL, Added Hardcopy button.
;    8/25/94, DWL, Minor mods.
;    96/07/02, MWB, changed YSIZE default to 40 lines.
;-

; ------------------------------------------------------------------------------
; Procedure viewtext_eve
; event handler for the View Header procedure.
; ------------------------------------------------------------------------------
PRO viewtext_eve, event
WIDGET_CONTROL, event.id, /HOURGLASS

stash = WIDGET_INFO( event.handler, /CHILD )
WIDGET_CONTROL, stash, GET_UVALUE=state, /NO_COPY

CASE event.id OF
   state.dismissid : BEGIN
      WIDGET_CONTROL, event.handler, SET_UVALUE=state, /NO_COPY
      WIDGET_CONTROL, event.top, /DESTROY
      RETURN
   END

   state.hardcpyid : BEGIN
      WIDGET_CONTROL, state.textid, GET_VALUE=text
      GET_LUN, lu
      OPENW, lu, 'header.lis'
      FOR j=0, N_ELEMENTS( text )-1 DO BEGIN
         PRINTF, lu, text[j]
      ENDFOR
      CLOSE, lu
      FREE_LUN, lu
      SPAWN, 'lpr -r header.lis'
   END
ENDCASE

WIDGET_CONTROL, stash, SET_UVALUE=state, /NO_COPY
END


; ------------------------------------------------------------------------------
; Procedure viewtext
; Displays text in a scrollable text widget.
; ------------------------------------------------------------------------------
PRO viewtext, text, EXIT=in_exit, FONT=in_font, GROUP=in_group, MODAL=in_modal,$
TITLE=in_title, XSIZE=in_xsize, YSIZE=in_ysize

; Check for required parameter.
IF N_PARAMS() NE 1 THEN BEGIN
   MESSAGE, 'viewtext, text [,keywords]', /INFO
   RETURN
ENDIF

; Required parameter must be a 1-D or 2-D string array.
IF badpar( text, 7, [0,1,2], CALLER='% VIEWTEXT: ' ) THEN RETURN

; Set the keyword parameters.
IF KEYWORD_SET( in_exit  ) THEN exitbutton=in_exit ELSE exitbutton='Dismiss'
IF KEYWORD_SET( in_font  ) THEN font=in_font ELSE font='8x13'
IF KEYWORD_SET( in_modal ) THEN modal=in_modal ELSE modal=0
IF KEYWORD_SET( in_title ) THEN title=in_title ELSE title='View Text'
IF KEYWORD_SET( in_xsize ) THEN xsize=in_xsize ELSE xsize=80
IF KEYWORD_SET( in_ysize ) THEN ysize=in_ysize ELSE ysize=40

; Create the main base and set the group leader.
mainbase = WIDGET_BASE( TITLE=title, /COLUMN )
IF KEYWORD_SET( in_group ) THEN group=in_group ELSE group=mainbase

state = {dismissid:0L, hardcpyid:0L, textid:0L}

; Create the exit button.
state.dismissid = WIDGET_BUTTON( mainbase, VALUE=exitbutton )
; Create the hardcopy button.
state.hardcpyid = WIDGET_BUTTON( mainbase, VALUE='Hardcopy' )

; Create the text widget.
state.textid = WIDGET_TEXT( mainbase, VALUE=text, XSIZE=xsize, YSIZE=ysize, $
               /SCROLL, FONT=font )

WIDGET_CONTROL, mainbase, /REALIZE

;Stash the state.
stash = WIDGET_INFO( mainbase, /CHILD )
WIDGET_CONTROL, stash, SET_UVALUE=state, /NO_COPY

XMANAGER, 'viewtext', mainbase, $
          EVENT_HANDLER='viewtext_eve', $
          GROUP_LEADER=group, $
          MODAL=modal
END
