class ZCL_UPLOAD definition
  public
  create public .

public section.

  types:
    begin of ty_message,
        row       type i,
        fieldname type lvc_fname,
        message   type string,
      end of ty_message .
  types:
    tt_message type standard table of ty_message with empty key .

  methods CONSTRUCTOR
    importing
      value(IM_FILEPATH) type DXLPATH optional
      value(IM_SKIP_ROW) type I optional .
  methods SET_CONSTRUCTOR
    importing
      value(IM_FILEPATH) type DXLPATH
      value(IM_SKIP_ROW) type I .
  methods UPLOAD_EXCEL_SERVER
    changing
      !CH_SUCCESS type TABLE
      !CH_ERROR type ref to DATA
      !CH_MESSAGE type TT_MESSAGE optional
    exceptions
      EMPTY_FILEPATH
      FILE_NOT_FOUND
      EXCEL_CONTENT_ERROR
      NO_ACTIVE_WORKSHEET
      NO_EXCEL_CONTENT .
  methods UPLOAD_EXCEL_PC
    changing
      !CH_SUCCESS type TABLE
      !CH_ERROR type ref to DATA
      !CH_MESSAGE type TT_MESSAGE optional
    exceptions
      EMPTY_FILEPATH
      FILE_OPEN_ERROR
      FILE_READ_ERROR
      NO_BATCH
      GUI_REFUSE_FILETRANSFER
      INVALID_TYPE
      NO_AUTHORITY
      UNKNOWN_ERROR
      BAD_DATA_FORMAT
      HEADER_NOT_ALLOWED
      SEPARATOR_NOT_ALLOWED
      HEADER_TOO_LONG
      UNKNOWN_DP_ERROR
      ACCESS_DENIED
      DP_OUT_OF_MEMORY
      DISK_FULL
      DP_TIMEOUT
      NOT_SUPPORTED_BY_GUI
      ERROR_NO_GUI
      EXCEL_CONTENT_ERROR
      NO_ACTIVE_WORKSHEET
      NO_EXCEL_CONTENT .
  methods GET_FILE_PC
    returning
      value(RE_FILEPATH) type DXLPATH .
  methods GET_FILE_SERV
    importing
      !IM_DIRECTORY type DXLPATH default '.'
    returning
      value(RE_FILEPATH) type DXLPATH .
  protected section.

    methods validate_row
      importing
        value(im_row_content) type any
        value(im_row)         type i
      exporting
        !ex_row_content       type any
        !ex_error             type char1
      changing
        !ch_message           type tt_message optional .
    methods check_type
      importing
        value(im_value) type any
      exporting
        !ex_error       type char1
      changing
        !ch_value       type any .
    methods check_type_p
      importing
        value(im_value) type any
      exporting
        !ex_error       type char1
      changing
        !ch_value       type any .
    methods check_type_i
      importing
        value(im_value) type any
      exporting
        !ex_error       type char1
      changing
        !ch_value       type any .
    methods check_type_c
      importing
        value(im_value) type any
      exporting
        !ex_error       type char1
      changing
        !ch_value       type any .
    methods check_type_f
      importing
        value(im_value) type any
      exporting
        !ex_error       type char1
      changing
        !ch_value       type any .
    methods check_type_n
      importing
        value(im_value) type any
      exporting
        !ex_error       type char1
      changing
        !ch_value       type any .
    methods check_type_d
      importing
        value(im_value) type any
      exporting
        !ex_error       type char1
      changing
        !ch_value       type any .
    methods check_type_t
      importing
        value(im_value) type any
      exporting
        !ex_error       type char1
      changing
        !ch_value       type any .
    methods check_type_other
      importing
        value(im_value) type any
      exporting
        !ex_error       type char1
      changing
        !ch_value       type any .
private section.

  data FILEPATH type DXLPATH .
  data SKIP_ROW type I .

  methods VALIDATE
    importing
      !IM_DATA type ref to DATA
    changing
      !CH_SUCCESS type TABLE
      !CH_ERROR type ref to DATA
      !CH_MESSAGE type TT_MESSAGE optional .
  methods READ_EXCEL_SERVER
    exporting
      !EX_SOLIX type SOLIX_TAB
    exceptions
      FILE_NOT_FOUND .
  methods READ_EXCEL_PC
    exporting
      !EX_SOLIX type SOLIX_TAB
      !EX_SUBRC type SYST_SUBRC .
  methods TO_XSTRING
    importing
      !IM_SOLIX type SOLIX_TAB
    exporting
      !EX_XSTRING type XSTRING
    exceptions
      EMPTY_XSTRING
      EMPTY_SOLIX .
  methods EXTRACT_EXCEL
    importing
      !IM_XDOCUMENT type XSTRING
    changing
      !CH_DATA type ref to DATA
    exceptions
      EXCEL_CONTENT_ERROR
      NO_ACTIVE_WORKSHEET
      NO_EXCEL_CONTENT .
  methods CREATE_STRUCTURE
    importing
      !IM_FILE_STRUC type ref to DATA
    changing
      !CH_STR_TABLE type ref to DATA
      !CH_STR_STRUC type ref to DATA .
ENDCLASS.



CLASS ZCL_UPLOAD IMPLEMENTATION.


  method check_type.
*&---------------------------------------------------------------------*
*  Method Name       : CHECK_TYPE
*  Method Visibility : Protected
*  Method Desc       : Check type of the data. Then check if the value
*                      matches with the type. This will cover all the
*                      data types. The following check_type_* can be
*                      redefined with own logic.
*&---------------------------------------------------------------------*
    data: lv_type  type char1,
          lv_error type char1.
    describe field ch_value type lv_type.
*Different checking for each type
*Each check method can be redefined in caller program
    case lv_type.
      when 'P'.
        check_type_p( exporting im_value = im_value
                      importing ex_error = ex_error
                      changing  ch_value = ch_value ).
      when 'I'.
        check_type_i( exporting im_value = im_value
                      importing ex_error = ex_error
                      changing  ch_value = ch_value ).
      when 'C'.
        check_type_c( exporting im_value = im_value
                      importing ex_error = ex_error
                      changing  ch_value = ch_value ).
      when 'F'.
        check_type_f( exporting im_value = im_value
                      importing ex_error = ex_error
                      changing  ch_value = ch_value ).
      when 'N'.
        check_type_n( exporting im_value = im_value
                      importing ex_error = ex_error
                      changing  ch_value = ch_value ).
      when 'D'.
        check_type_d( exporting im_value = im_value
                      importing ex_error = ex_error
                      changing  ch_value = ch_value ).
      when 'T'.
        check_type_t( exporting im_value = im_value
                      importing ex_error = ex_error
                      changing  ch_value = ch_value ).
      when others.
        check_type_other( exporting im_value = im_value
                          importing ex_error = ex_error
                          changing  ch_value = ch_value ).
    endcase.
  endmethod.


  method check_type_c.
*&---------------------------------------------------------------------*
*  Method Name       : CHECK_TYPE_C
*  Method Visibility : Protected
*  Method Desc       : Check type C.
*&---------------------------------------------------------------------*
    ch_value = im_value.
  endmethod.


  method check_type_d.
*&---------------------------------------------------------------------*
*  Method Name       : CHECK_TYPE_D
*  Method Visibility : Protected
*  Method Desc       : Check type D.
*&---------------------------------------------------------------------*
    data: lv_external type string,
          lv_internal type datum.
    if im_value is not initial.
      lv_external = im_value.
      call function 'CONVERT_DATE_TO_INTERNAL'
        exporting
          date_external            = lv_external
        importing
          date_internal            = lv_internal
        exceptions
          date_external_is_invalid = 1
          others                   = 2.
      if sy-subrc <> 0.
        ex_error = abap_true.
      else.
        ch_value = lv_internal.
      endif.
    endif.
  endmethod.


  method check_type_f.
*&---------------------------------------------------------------------*
*  Method Name       : CHECK_TYPE_F
*  Method Visibility : Protected
*  Method Desc       : Check type F.
*&---------------------------------------------------------------------*
    try.
        ch_value = im_value.
      catch cx_sy_conversion_overflow "If value is not in range
            cx_sy_conversion_no_number. "If value is not numeric
        ex_error = abap_true.
    endtry.
  endmethod.


  method check_type_i.
*&---------------------------------------------------------------------*
*  Method Name       : CHECK_TYPE_I
*  Method Visibility : Protected
*  Method Desc       : Check type I.
*                      Type I accept number ranged from
*                      -2,147,483,648 to +2,147,483,647
*&---------------------------------------------------------------------*
    data:lv_value type string.
    lv_value = im_value.
    replace all occurrences of ',' in lv_value with ''.
    try.
        ch_value = lv_value.
      catch cx_sy_conversion_overflow "If value is not in range
            cx_sy_conversion_no_number. "If value is not numeric
        ex_error = abap_true.
    endtry.
  endmethod.


  method check_type_n.
*&---------------------------------------------------------------------*
*  Method Name       : CHECK_TYPE_N
*  Method Visibility : Protected
*  Method Desc       : Check type N. It does not accept decimal places
*&---------------------------------------------------------------------*
    data: lv_value type string.
    lv_value = im_value.
    replace all occurrences of ',' in lv_value with ''.
    try.
        ch_value = lv_value.
      catch cx_sy_conversion_overflow "If value is not in range
            cx_sy_conversion_no_number. "If value is not numeric
        ex_error = abap_true.
    endtry.
  endmethod.


  method check_type_other.
*&---------------------------------------------------------------------*
*  Method Name       : CHECK_TYPE_OTHER
*  Method Visibility : Protected
*  Method Desc       : Check other type
*&---------------------------------------------------------------------*
    ch_value = im_value.
  endmethod.


  method check_type_p.
*&---------------------------------------------------------------------*
*  Method Name       : CHECK_TYPE_P
*  Method Visibility : Protected
*  Method Desc       : Check type P. Convert to float then to PACK
*&---------------------------------------------------------------------*
    data: lv_float type float,
          lv_value type string.
    lv_value = im_value.
    replace all occurrences of ',' in lv_value with ''.
    try.
        lv_float = lv_value.
        ch_value = lv_float.
      catch cx_sy_conversion_no_number "If value is not numeric
            cx_sy_conversion_overflow. "If value is not in range
        ex_error = abap_true.
    endtry.
  endmethod.


  method check_type_t.
*&---------------------------------------------------------------------*
*  Method Name       : CHECK_TYPE_T
*  Method Visibility : Protected
*  Method Desc       : Check type T.
*&---------------------------------------------------------------------*
    data: lv_value type string.
    lv_value = im_value.
    replace all occurrences of ':' in lv_value with ''.
    if lv_value co '0123456789'.
      ch_value = lv_value.
    else.
      ex_error = abap_true.
    endif.
  endmethod.


  method constructor.
*&---------------------------------------------------------------------*
*  Method Name       : CONSTRUCTOR
*  Method Visibility : Public
*  Method Desc       : This is the constructor of the class. When the
*                      instance is created, the parameters has to be set
*                      with value
*                      filepath - the directory of the file
*                      worksheet_index - which sheet to read in excel
*                      file
*                      skip_row - skip header row
*&---------------------------------------------------------------------*
    filepath        = im_filepath.
    skip_row        = im_skip_row.
  endmethod.


  method create_structure.
*&---------------------------------------------------------------------*
*  Method Name       : CREATE_STRUCTURE
*  Method Visibility : Private
*  Method Desc       : Create string type structure
*&---------------------------------------------------------------------*
    data:lt_comp         type standard table of abap_componentdescr with key name,
         lo_stru         type ref to cl_abap_structdescr,
         ls_comp         type abap_componentdescr,
         ls_fieldcatalog type lvc_s_fcat,
         lt_fieldcatalog type lvc_t_fcat,
         lt_data         type ref to data.

    field-symbols: <lfs_struc>       type any,
                   <lfs_data>        type ref to data,
                   <lfs_table>       type table,
                   <lfs_file_struc>  type any,
                   <lfs_str_table>   type any,
                   <lfs_t_str_table> type table.


    if im_file_struc is bound.
      assign im_file_struc->* to <lfs_file_struc>.
      if <lfs_file_struc> is assigned.
        lo_stru ?= cl_abap_typedescr=>describe_by_data( <lfs_file_struc> ).
        lt_comp = lo_stru->get_components( ).
        loop at lt_comp into ls_comp.
          ls_fieldcatalog-fieldname = ls_comp-name.
          ls_fieldcatalog-inttype = 'g'. "String type
          append ls_fieldcatalog to lt_fieldcatalog.
          clear: ls_fieldcatalog.
        endloop.
        if lt_fieldcatalog is not initial.
          assign ch_str_table to <lfs_str_table>.
          call method cl_alv_table_create=>create_dynamic_table
            exporting
              it_fieldcatalog           = lt_fieldcatalog
            importing
              ep_table                  = <lfs_str_table>
            exceptions
              generate_subpool_dir_full = 1
              others                    = 2.
          if sy-subrc = 0.
*Return a table & structure with string
            if <lfs_str_table> is bound.
              ch_str_table = <lfs_str_table>.
              assign <lfs_str_table>->* to <lfs_t_str_table>.
              create data ch_str_struc like line of <lfs_t_str_table>.
            endif.
          endif.
        endif.
      endif.
    endif.
  endmethod.


  method extract_excel.
*&---------------------------------------------------------------------*
*  Method Name       : EXTRACT_EXCEL
*  Method Visibility : Private
*  Method Desc       : Extract data from excel xstring
*&---------------------------------------------------------------------*
    constants : lc_filename type string value 'DEFAULT'.

    data : lt_worksheets type table of string,
           l_excel       type ref to cl_fdt_xl_spreadsheet,
           l_excel_core  type ref to cx_fdt_excel_core,
           l_data        type ref to data,
           l_dref        type ref to data,
           lv_msg        type string,
           lv_ws_name    type string,
           ls_bapiret2   type bapiret2.
    try.
* Get excel content
        create object l_excel
          exporting
            document_name = lc_filename
            xdocument     = im_xdocument
            mime_type     = 'xls'.
      catch cx_fdt_excel_core into l_excel_core.
        raise excel_content_error.
    endtry.

* Call method to get list of worksheets in the .xlsx file
    l_excel->if_fdt_doc_spreadsheet~get_worksheet_names( importing worksheet_names = lt_worksheets ).

* Condition to check whether .xlsx file has any active worksheets
* Always default to read first worksheet in excel
* This worksheet index can be passed from caller prog.
    read table lt_worksheets index 1 into lv_ws_name.
    if sy-subrc <> 0.
      "Error
      raise no_active_worksheet.
    endif.
* Get reference of .xlsx file contents in the active worksheet
    ch_data = l_excel->if_fdt_doc_spreadsheet~get_itab_from_worksheet( lv_ws_name ).
    if ch_data is not bound.
      raise no_excel_content.
    endif.
  endmethod.


  method get_file_pc.
*&---------------------------------------------------------------------*
*  Method Name       : GET_FILE_PC
*  Method Visibility : Public
*  Method Desc       : This method is for caller to choose a file on
*                      local PC.
*&---------------------------------------------------------------------*

    data: lv_path       type dxfields-longpath,
          lv_abend_flag type dxfields-abendflag.

* Value help for Filename
    call function 'F4_DXFILENAME_TOPRECURSION'
      exporting
        i_location_flag = 'P'  "c_win
        i_server        = '?'
        i_path          = '/'
        filemask        = '(Excel Files *.xls;*.xlsx)|Excel Files *.xls;*.xlsx'
        fileoperation   = 'R'
      importing
        o_path          = lv_path
        abend_flag      = lv_abend_flag
      exceptions
        rfc_error       = 1
        others          = 2.

    if sy-subrc <> 0.
      message id      sy-msgid
              type    'S'
              number  sy-msgno
              with    sy-msgv1 display like sy-msgty.
    endif.

    if not lv_abend_flag is initial.
      clear lv_path.
    endif.
    re_filepath = lv_path.
  endmethod.


  method get_file_serv.
    call function '/SAPDMC/LSM_F4_SERVER_FILE'
      exporting
        directory        = im_directory
        filemask         = '*.xlsx'
      importing
        serverfile       = RE_filepath
      exceptions
        canceled_by_user = 1
        others           = 2.
  endmethod.


  method read_excel_pc.
*&---------------------------------------------------------------------*
*  Method Name       : READ_EXCEL_PC
*  Method Visibility : Private
*  Method Desc       : Read excel file from local pc.
*&---------------------------------------------------------------------*
    data: lv_filename type string,
          lt_solix    type solix_tab.
    lv_filename = filepath.
    call method cl_gui_frontend_services=>gui_upload
      exporting
        filename                = lv_filename
        filetype                = 'BIN'
      changing
        data_tab                = lt_solix
      exceptions
        file_open_error         = 1
        file_read_error         = 2
        no_batch                = 3
        gui_refuse_filetransfer = 4
        invalid_type            = 5
        no_authority            = 6
        unknown_error           = 7
        bad_data_format         = 8
        header_not_allowed      = 9
        separator_not_allowed   = 10
        header_too_long         = 11
        unknown_dp_error        = 12
        access_denied           = 13
        dp_out_of_memory        = 14
        disk_full               = 15
        dp_timeout              = 16
        not_supported_by_gui    = 17
        error_no_gui            = 18
        others                  = 19.
    ex_subrc = sy-subrc.
    ex_solix = lt_solix.
  endmethod.


  method read_excel_server.
*&---------------------------------------------------------------------*
*  Method Name       : READ_EXCEL_SERVER
*  Method Visibility : Private
*  Method Desc       : Read excel file from server
*&---------------------------------------------------------------------*
    data: lt_bapiret2 type bapiret2_t,
          lv_solix    type solix.
    open dataset filepath for input in binary mode.
    if sy-subrc <> 0.
      raise file_not_found.
      return.
    endif.
    do.
      read dataset filepath into lv_solix.
      if sy-subrc <> 0.
        exit.
      else.
        append lv_solix to ex_solix.
      endif.
    enddo.
    close dataset filepath.
  endmethod.


  method set_constructor.
*&---------------------------------------------------------------------*
*  Method Name       : SET_CONSTRUCTOR
*  Method Visibility : Public
*  Method Desc       : This is to set the value if the constructor did
*                      not set
*                      filepath - the directory of the file
*                      skip_row - skip header row
*&---------------------------------------------------------------------*
    filepath        = im_filepath.
    skip_row        = im_skip_row.
  endmethod.


  method to_xstring.
*&---------------------------------------------------------------------*
*  Method Name       : TO_XSTRING
*  Method Visibility : Private
*  Method Desc       : Convert to XString format
*&---------------------------------------------------------------------*
    if im_solix is initial.
      "Should not pass an empty itab
      raise empty_solix.
    endif.
    call method cl_bcs_convert=>solix_to_xstring
      exporting
        it_solix   = im_solix
      receiving
        ev_xstring = ex_xstring.
    if ex_xstring is initial.
      raise empty_xstring.
    endif.
  endmethod.


  method upload_excel_pc.
*&---------------------------------------------------------------------*
*  Method Name       : UPLOAD_EXCEL_PC
*  Method Visibility : Public
*  Method Desc       : This method is for caller to upload excel file
*                      from local PC
*&---------------------------------------------------------------------*
    data : lt_solix    type solix_tab,
           lv_xstring  type xstring,
           lv_filename type string,
           lt_data     type ref to data,
           lv_subrc    type syst_subrc.

*Check filepath
    if filepath is initial.
      raise empty_filepath.
    endif.
*Read excel
    read_excel_pc(
      importing
        ex_solix = lt_solix                 " GBT: SOLIX as Table Type
        ex_subrc = lv_subrc                 " ABAP System Field: Return Code of ABAP Statements
    ).
    if lv_subrc <> 0.
      case lv_subrc.
        when 1.
          raise file_open_error.
        when 2.
          raise file_read_error.
        when 3.
          raise no_batch.
        when 4.
          raise gui_refuse_filetransfer.
        when 5.
          raise invalid_type.
        when 6.
          raise no_authority.
        when 7.
          raise unknown_error.
        when 8.
          raise bad_data_format.
        when 9.
          raise header_not_allowed.
        when 10.
          raise separator_not_allowed.
        when 11.
          raise header_too_long.
        when 12.
          raise unknown_dp_error.
        when 13.
          raise access_denied.
        when 14.
          raise dp_out_of_memory.
        when 15.
          raise disk_full.
        when 16.
          raise dp_timeout.
        when 17.
          raise not_supported_by_gui.
        when 18.
          raise error_no_gui.
        when others.
      endcase.
    endif.

*Convert to XString
    to_xstring( exporting im_solix   = lt_solix
                    importing ex_xstring = lv_xstring ).
*Extract Excel
    extract_excel( exporting  im_xdocument        = lv_xstring
                      changing   ch_data             = lt_data
                      exceptions excel_content_error = 1
                                 no_active_worksheet = 2
                                 no_excel_content    = 3 ).
    if sy-subrc = 1.
      raise excel_content_error.
    elseif sy-subrc = 2.
      raise no_active_worksheet.
    elseif sy-subrc = 3.
      raise no_excel_content.
    endif.

*Validation
    validate( exporting im_data    = lt_data
              changing  ch_success = ch_success
                        ch_error   = ch_error
                        ch_message = ch_message
    ).
  endmethod.


  method upload_excel_server.
*&---------------------------------------------------------------------*
*  Method Name       : UPLOAD_EXCEL_SERVER
*  Method Visibility : Public
*  Method Desc       : This method is for caller to upload excel file
*                      from AL11.
*&---------------------------------------------------------------------*
    data: lt_bapiret2 type bapiret2_t,
          lt_solix    type solix_tab,
          lv_solix    type solix,
          lv_xstring  type xstring,
          lt_data     type ref to data.
*Validation
*Check filepath
    if filepath is initial.
      raise empty_filepath.
    endif.
*Read excel
    read_excel_server( importing ex_solix = lt_solix
                       exceptions file_not_found = 1 ).
    if sy-subrc = 1.
      raise file_not_found.
    endif.
*Convert to XString
    to_xstring( exporting im_solix   = lt_solix
                importing ex_xstring = lv_xstring ).
*Extract excel
    extract_excel( exporting  im_xdocument        = lv_xstring
                   changing   ch_data             = lt_data
                   exceptions excel_content_error = 1
                              no_active_worksheet = 2
                              no_excel_content    = 3 ).
    if sy-subrc = 1.
      raise excel_content_error.
    elseif sy-subrc = 2.
      raise no_active_worksheet.
    elseif sy-subrc = 3.
      raise no_excel_content.
    endif.

*Validation
    validate( exporting im_data    = lt_data
              changing  ch_success = ch_success
                        ch_error   = ch_error
                        ch_message = ch_message ).

  endmethod.


  method validate.
*&---------------------------------------------------------------------*
*  Method Name       : VALIDATE
*  Method Visibility : Private
*  Method Desc       : Validate all the row in the excel file. In this
*                      method, it receives the excel content in
*                      IM_DATA, then it gets successful itab(CH_SUCCESS)
*                      in custom structure, and CH_MESSAGE is optional.
*                      The idea is, use the structure in CH_SUCCESS,
*                      to create another itab with string type and
*                      store excel content which has error.
*                      If the excel content passes validation, then
*                      it will append to CH_SUCCESS, else CH_ERROR.
*&---------------------------------------------------------------------*
    data: dref_file_struc  type ref to data,
          lt_comp          type standard table of abap_componentdescr with key name,
          lo_stru          type ref to cl_abap_structdescr,
          ls_comp          type abap_componentdescr,
          lv_error         type char1,
          lv_row_no        type syst_tabix,  "Row number
          dref_t_str       type ref to data, "Table with string
          dref_s_str       type ref to data, "Structure with string
          dref_s_err       type ref to data, "Error structure with string,
          dref_s_succ      type ref to data, "Success structure with string
          dref_succ        type ref to data, "Success structue
          lt_message       type tt_message,  "Message
          lt_final_message type tt_message.  "Final message

    field-symbols: <lfs_t_file_data> type table,
                   <lfs_s_file_data> type any,
                   <lfs_s_err>       type any,
                   <lfs_s_succ>      type any,
                   <lfs_t_err>       type table,
                   <lfs_s_str>       type any.
*Get the structure & table of the input itab
    create data dref_file_struc like line of ch_success.
    create_structure(
      exporting
        im_file_struc = dref_file_struc " File Structure.
      changing
        ch_str_table  = dref_t_str      " Table with string.
        ch_str_struc  = dref_s_str      " Data structure with string.
    ).
    assign im_data->* to <lfs_t_file_data>.
    if <lfs_t_file_data> is assigned.
*Create data object and reference to field symbol
      create data dref_s_err  like dref_s_str.
      create data dref_s_succ like line of ch_success.
      assign dref_s_err->*    to <lfs_s_err>.
      assign dref_s_succ->*   to <lfs_s_succ>.
      assign dref_s_str->*    to <lfs_s_str>.
      create data ch_error    like table of <lfs_s_str>.
      assign ch_error->*      to <lfs_t_err>.
*Loop the data and do validation
      loop at <lfs_t_file_data> assigning <lfs_s_file_data>.
        lv_row_no = sy-tabix.
        if lv_row_no <= skip_row.
          continue.
        endif.
        do.
          assign component sy-index of structure <lfs_s_file_data> to field-symbol(<a>).
          if sy-subrc = 0.
            assign component sy-index of structure <lfs_s_str> to field-symbol(<b>).
            if sy-subrc = 0.
              <b> = <a>.
            else.
              exit.
            endif.
          else.
            exit.
          endif.
        enddo.
        unassign: <a>, <b>.
*Pass the string structure to do custom validation
        validate_row(
          exporting
            im_row_content = <lfs_s_str>       " Row content to be validated.
            im_row         = lv_row_no         " Row number
          importing
            ex_row_content = <lfs_s_succ>      " Excel content which is in excel file format
            ex_error       = lv_error          " Error Indicator
          changing
            ch_message     = lt_message ).     " Error structure
        if lv_error = abap_true.
          append <lfs_s_file_data> to <lfs_t_err>.
        else.
          append <lfs_s_succ> to ch_success.
        endif.
        if lt_message is not initial.
          append lines of lt_message to ch_message.
        endif.
        clear: lv_error, lt_message.
        clear: <lfs_s_str>.
      endloop.
    endif.
  endmethod.


  method validate_row.
*&---------------------------------------------------------------------*
*  Method Name       : VALIDATE_ROW
*  Method Visibility : Protected
*  Method Desc       : Validate each content row. This method can be
*                      redefined to put custom logic.
*&---------------------------------------------------------------------*
    data: lv_type type char1.
    do.
      assign component sy-index of structure im_row_content to field-symbol(<a>).
      if sy-subrc = 0.
        assign component sy-index of structure ex_row_content to field-symbol(<b>).
        if sy-subrc = 0.
          check_type(
            exporting
              im_value = <a>
            changing
              ch_value = <b> ).
        else.
          exit.
        endif.
      else.
        exit.
      endif.
    enddo.
  endmethod.
ENDCLASS.
