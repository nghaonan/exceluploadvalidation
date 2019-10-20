report z_upload.
*&---------------------------------------------------------------------*
*&  Program Name       : Z_UPLOAD
*&  Program description: This program demo how to upload excel file from
*&                       server(AL11) and local pc.
*&---------------------------------------------------------------------*
class lcl_upload definition deferred.
*Data declaration
types: begin of ty_file,
         mandt   type mandt,
         vbeln   type vbeln,
         vkorg   type vkorg,
         erdat   type erdat,
         erzet   type erzet,
         netwr   type netwr_ak,
         awahr   type awahr_ak,
         mahza   type mahza,
         stwae   type stwae,
         pendays type pdays,
       end of ty_file.
types: begin of ty_message,
         row       type i,
         fieldname type lvc_fname,
         message   type string,
       end of ty_message,
       tt_message type standard table of ty_message with empty key.
data: lv_filepath type dxlpath,
      go_upload   type ref to lcl_upload,
      gt_file     type standard table of ty_file,
      dref_err    type ref to data,
      gt_message  type tt_message.
field-symbols: <gfs_t_error> type table.

*Class Defination
class lcl_upload definition inheriting from zcl_upload final.
  protected section.
    methods: validate_row redefinition.
  private section.
    methods: validate_mandt importing value(im_value) type mandt
                            exporting ex_error        type char1.
    methods: validate_vkorg importing value(im_value) type vkorg
                            exporting ex_error        type char1.
endclass.
class lcl_upload implementation.
  method validate_row.
    data: ls_row_content type ty_file,
          lv_error       type char1,
          ls_message     type ty_message,
          lt_message     type tt_message.
*****************************EXPLANATION*****************************
*This part will perform 2 kind of validation
*1. Format validation
*2. Data accuracy validation
*
*There are parameters passed from caller method 'VALIDATE'
*5 parameters here:
* - IM_ROW_CONTENT -> Excel content which is in string format
* - IM_ROW_NO      -> Row number
* - EX_ROW_CONTENT -> Excel content which is in excel file(custom) format
* - EX_ERROR       -> FLAG X if error, it will be copied to error structure
* - CH_MESSAGE     -> Add custom message
*****************************EXPLANATION*****************************

*There are two methods to do validation.
*First method -> Get the field one by one and compare,
*                it is tedious but there could be some
*                scenario that some fields depend on
*                each other in validation

*Second method -> Do validation in a do loop to read
*                 field one by one
*
*Comment out one of the method, to see the output
*
******************************************
* Start of first method
******************************************
    assign component 'MANDT' of structure im_row_content to field-symbol(<mandt>).
    if sy-subrc = 0.
*Type validation
      check_type( exporting im_value = <mandt>                 " Input Value
                  importing ex_error = lv_error                " Error Indicator.
                  changing  ch_value = ls_row_content-mandt ). " Output Value
      if lv_error <> abap_true.
*Data accuracy(Put your own logic here ).
        validate_mandt( exporting im_value = ls_row_content-mandt
                        importing ex_error = lv_error ).
      endif.
*Error message here(example)
      if lv_error = abap_true.
        ls_message-row = im_row.
        ls_message-fieldname = 'MANDT'.
        ls_message-message = 'Invalid Client Number' && | | && <mandt>.
        append ls_message to lt_message.
        clear: ls_message.
        ex_error = abap_true.
      endif.
      clear: lv_error.
    endif.
*****Repeat the same thing for other fields......

*Return value to method VALIDATE
    ex_row_content = ls_row_content.
    ch_message = lt_message.

******************************************
* End of first method
******************************************

*--------------------------------------------------------------------*

*******************************************
** Start of second method
*******************************************
*    data: lv_type type char1,
*          lt_comp type standard table of abap_componentdescr with key name,
*          ls_comp type abap_componentdescr,
*          lo_stru type ref to cl_abap_structdescr.
*
*    lo_stru ?= cl_abap_typedescr=>describe_by_data( im_row_content ).
*    lt_comp = lo_stru->get_components( ).
*    do.
*      assign component sy-index of structure im_row_content to field-symbol(<a>).
*      if sy-subrc = 0.
*        assign component sy-index of structure ex_row_content to field-symbol(<b>).
*        read table lt_comp into ls_comp index sy-index.
*        if sy-subrc = 0.
**Type validation
*          check_type( exporting im_value = <a>      " Input Value
*                      importing ex_error = lv_error " Error Indicator.
*                      changing  ch_value = <b> ).   " Output Value
*          if lv_error <> abap_true.
**Data accuracy(Put your own logic here ).
**Pass the value to another validate method
*            if sy-subrc = 0.
*              case ls_comp-name.
*                when 'MANDT'.
*                  validate_mandt( exporting im_value = <b>
*                                  importing ex_error = lv_error ).
*                when 'VKORG'.
*                  validate_vkorg( exporting im_value = <b>
*                                  importing ex_error = lv_error ).
**Put other fields here...
*                  "when..
*                  "when..
*                when others.
*                  "Put your own logic
*              endcase.
*            endif.
*          endif.
*          if lv_error = abap_true.
**Error message here(example)
*            ls_message-row = im_row.
*            ls_message-fieldname = ls_comp-name.
*            case ls_comp-name.
*              when 'MANDT'.
*                ls_message-message = 'Invalid Client Number' && | | && <b>.
*              when 'VKORG'.
*                ls_message-message = 'Invalid sales org' && | | && <b>.
*            endcase.
*
*            append ls_message to lt_message.
*            clear: ls_message.
*            ex_error = abap_true.
*          endif.
*        else.
*          exit.
*        endif.
*      else.
*        exit.
*      endif.
*      clear: lv_error, ls_comp.
*    enddo.
**Return value to method VALIDATE
*    ex_row_content = ls_row_content.
*    ch_message = lt_message.
*******************************************
** End of second method
*******************************************
  endmethod.

  method validate_mandt.
*Own logic..
*Example
    if im_value <> 100."client 100
      ex_error = abap_true.
    endif.
  endmethod.
  method validate_vkorg.
*Own logic
*Example
    if im_value > 0.
      ex_error = abap_true.
    endif.
  endmethod.
endclass.

parameters: p_serv radiobutton group rb1 default 'X' user-command cm1,
            p_locl radiobutton group rb1,
            p_path type dxlpath OBLIGATORY.

at selection-screen on value-request for p_path.
  if p_serv = abap_true.
    p_path = go_upload->get_file_serv( ).
  elseif p_locl = abap_true.
    p_path = go_upload->get_file_pc( ).
  endif.

initialization.
  create object: go_upload.

start-of-selection.
  if p_serv = abap_true.
    perform read_from_server.
  elseif p_locl = abap_true.
    perform read_from_local.
  endif.

end-of-selection.

form read_from_server.
*Upload from AL11
  go_upload->set_constructor(
    exporting
      im_filepath = p_path      " Local file for upload/download
      im_skip_row = 1           " Skip row
  ).

  go_upload->upload_excel_server(
    changing
      ch_success          = gt_file          " To store successful loaded record.
      ch_error            = dref_err         " To store failed record.
      ch_message          = gt_message       " To store custom message.
    exceptions
      empty_filepath      = 1                " Empty Filepath.
      file_not_found      = 2                " File not found.
      excel_content_error = 3                " Excel Content Error
      no_active_worksheet = 4                " No active worksheet
      no_excel_content    = 5                " No content found in excel.
      others              = 6 ).
  if sy-subrc <> 0.
*Put own logic to cater exception
  endif.

**Display successful loaded data
  if gt_file is not initial.
    cl_demo_output=>display( gt_file ).
  endif.

*Display error data
  assign dref_err->* to <gfs_t_error>.
  if sy-subrc = 0.
    cl_demo_output=>display( <gfs_t_error> ).
  endif.

*Display message
  if gt_message is not initial.
    cl_demo_output=>display( gt_message ).
  endif.
endform.

form read_from_local.
*Upload from local pc
  go_upload->set_constructor(
    exporting
      im_filepath = p_path      " Local file for upload/download
      im_skip_row = 1           " Skip row
  ).
  go_upload->upload_excel_pc(
    changing
      ch_success              = gt_file                 " To store successful loaded record.
      ch_error                = dref_err                " To store failed record.
      ch_message              = gt_message              " To store custom message.
    exceptions
      empty_filepath          = 1
      file_open_error         = 2
      file_read_error         = 3
      no_batch                = 4
      gui_refuse_filetransfer = 5
      invalid_type            = 6
      no_authority            = 7
      unknown_error           = 8
      bad_data_format         = 9
      header_not_allowed      = 10
      separator_not_allowed   = 11
      header_too_long         = 12
      unknown_dp_error        = 13
      access_denied           = 14
      dp_out_of_memory        = 15
      disk_full               = 16
      dp_timeout              = 17
      not_supported_by_gui    = 18
      error_no_gui            = 19
      excel_content_error     = 20
      no_active_worksheet     = 21
      no_excel_content        = 22
      others                  = 23 ).
  if sy-subrc <> 0.
    message id sy-msgid type sy-msgty number sy-msgno
      with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  endif.
**Display successful loaded data
  if gt_file is not initial.
    cl_demo_output=>display( gt_file ).
  endif.

**Display error data
  assign dref_err->* to <gfs_t_error>.
  if sy-subrc = 0.
    cl_demo_output=>display( <gfs_t_error> ).
  endif.

*Display message
  if gt_message is not initial.
    cl_demo_output=>display( gt_message ).
  endif.
endform.
