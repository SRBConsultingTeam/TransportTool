REPORT zsrb_transporttool.

** See https://www.srb.at

*******************************************************************************************************************************
** The MIT License (MIT)
**
** Copyright 2022 SRB Consulting Team GmbH
**
** Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation
** files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy,
** modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the
** Software is furnished to do so, subject to the following conditions:
**
** The above copyright notice and this permission notice shall be included in all copies or substantial portions of the
** Software.
**
** THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE
** WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
** COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,
** ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*******************************************************************************************************************************

*******************************************************************************************************************************
**
** Steps to restrict usage of the report
**  - create a new include named `zsrb_transporttool_exit`,
**  - in the include, implement the a class named `lcl_authorization_helper_exit`,
**  - in the class, implement the interface `lif_srb_transporttool_auth`,
**  - in method `lif_srb_transporttool_auth~is_authorized` implement your custom authorizaiton check.
**
*******************************************************************************************************************************

*******************************************************************************************************************************
**
** Steps to include additional files in zip archive
**  - create a new include named `zsrb_transporttool_exit`,
**  - in the include, implement the a class named `LCL_FILE_HELPER_EXIT`,
**  - in the class, implement the interface `lif_srb_transporttool_files`,
**  - in method `lif_srb_transporttool_files~add_custom_files` additional files can be added.
**
*******************************************************************************************************************************

SELECTION-SCREEN BEGIN OF BLOCK b0 WITH FRAME TITLE TEXT-000.
  PARAMETERS p_trans TYPE trkorr OBLIGATORY.
  PARAMETERS p_desc TYPE as4text.
  PARAMETERS p_summar TYPE abap_bool DEFAULT abap_true AS CHECKBOX.
SELECTION-SCREEN END OF BLOCK b0.

INTERFACE lif_srb_transporttool_auth DEFERRED.
INTERFACE lif_srb_transporttool_files DEFERRED.
CLASS lcl_authorization_helper DEFINITION DEFERRED.
CLASS lcl_transport_helper DEFINITION DEFERRED.
CLASS lcl_processor DEFINITION DEFERRED.

INTERFACE lif_srb_transporttool_auth.
  CONSTANTS: BEGIN OF authorization,
               export_transport TYPE string VALUE 'EXP_TRANS',
             END OF authorization.

  METHODS is_authorized
    IMPORTING requested_authorization TYPE string
    RETURNING VALUE(authorized)       TYPE abap_bool.
ENDINTERFACE.

INTERFACE lif_srb_transporttool_files.
  METHODS add_custom_files
    IMPORTING zip_helper       TYPE REF TO cl_abap_zip
              transport_helper TYPE REF TO lcl_transport_helper.
ENDINTERFACE.

CLASS lcl_authorization_helper DEFINITION.
  PUBLIC SECTION.
    METHODS is_authorized
      IMPORTING requested_authorization TYPE string
      RETURNING VALUE(authorized)       TYPE abap_bool.
ENDCLASS.

CLASS lcl_authorization_helper IMPLEMENTATION.
  METHOD is_authorized.
    DATA custom_authorization_helper TYPE REF TO lif_srb_transporttool_auth.
    TRY.
        CREATE OBJECT custom_authorization_helper TYPE ('LCL_AUTHORIZATION_HELPER_EXIT').
        authorized = custom_authorization_helper->is_authorized( requested_authorization ).

      CATCH cx_sy_create_object_error.
        authorized = abap_true.
    ENDTRY.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_files_helper DEFINITION.
  PUBLIC SECTION.
    METHODS add_custom_files
      IMPORTING zip_helper       TYPE REF TO cl_abap_zip
                transport_helper TYPE REF TO lcl_transport_helper.
ENDCLASS.

CLASS lcl_files_helper IMPLEMENTATION.
  METHOD add_custom_files.
    DATA custom_files_helper TYPE REF TO lif_srb_transporttool_files.

    TRY.
        CREATE OBJECT custom_files_helper TYPE ('LCL_FILE_HELPER_EXIT').
        custom_files_helper->add_custom_files(
            zip_helper       = zip_helper
            transport_helper = transport_helper
        ).

      CATCH cx_sy_create_object_error ##no_handler.
    ENDTRY.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_transport_helper DEFINITION.
  PUBLIC SECTION.
    TYPES: transport_directories_tt TYPE STANDARD TABLE OF stmsttefi WITH EMPTY KEY.

    CLASS-METHODS: retrieve_transport_desc_s
      IMPORTING transport                    TYPE trkorr
      RETURNING VALUE(transport_description) TYPE as4text.

    METHODS: constructor
      IMPORTING transport TYPE trkorr.

    METHODS: get_transport_directories
      RETURNING VALUE(transport_directories) TYPE transport_directories_tt.

    METHODS: get_data_directory
      RETURNING VALUE(data_directory) TYPE string.

    METHODS: get_cofiles_directory
      RETURNING VALUE(cofiles_directory) TYPE string.

    METHODS: get_datafile_path
      RETURNING VALUE(datafile_path) TYPE string.

    METHODS: get_cofile_path
      RETURNING VALUE(cofiles_path) TYPE string.

    METHODS: get_datafile_filename
      RETURNING VALUE(datafile_filename) TYPE string.

    METHODS: get_cofile_filename
      RETURNING VALUE(cofile_filename) TYPE string.

    METHODS: get_transport_documentation
      RETURNING VALUE(transport_documentation) TYPE string.

    METHODS: get_transport_desc
      RETURNING VALUE(transport_description) TYPE string.

    METHODS: get_transport
      RETURNING VALUE(transport) TYPE trkorr.

  PRIVATE SECTION.
    DATA transport_directories TYPE transport_directories_tt.
    DATA transport TYPE trkorr.
ENDCLASS.

CLASS lcl_transport_helper IMPLEMENTATION.

  METHOD retrieve_transport_desc_s.

    DATA header TYPE trwbo_request_header.

    header-trkorr = transport.

    CALL FUNCTION 'TRINT_READ_REQUEST_HEADER'
      EXPORTING
        iv_read_e07t   = 'X'
      CHANGING
        cs_request     = header
      EXCEPTIONS
        empty_trkorr   = 1
        not_exist_e070 = 2
        OTHERS         = 3.

    IF sy-subrc <> 0.
*   MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*     WITH SY-MSGV1
    ENDIF.

    transport_description = header-as4text.

  ENDMETHOD.

  METHOD constructor.
    me->transport = transport.

    SELECT SINGLE @abap_true
      FROM e070
     WHERE trkorr = @transport
     INTO @DATA(transport_exists).

    IF ( transport_exists <> abap_true ).
      MESSAGE e807(tr)  WITH transport sy-sysid.
    ENDIF.
  ENDMETHOD.

  METHOD get_cofiles_directory.
    DATA(temp_directories) = get_transport_directories( ).

    cofiles_directory = temp_directories[ dir = 'cofiles' ]-path.
  ENDMETHOD.

  METHOD get_data_directory.
    DATA(temp_directories) = get_transport_directories( ).

    data_directory = temp_directories[ dir = 'data' ]-path.
  ENDMETHOD.

  METHOD get_transport_directories.
    IF ( me->transport_directories IS INITIAL ).
      CALL FUNCTION 'TMS_TP_CHECK_TRANS_DIR'
        TABLES
          tt_tefi              = me->transport_directories
        EXCEPTIONS
          permission_denied    = 1
          get_dir_names_failed = 2
          build_path_failed    = 3
          OTHERS               = 4.

      IF sy-subrc <> 0.
*       MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      ENDIF.

    ENDIF.

    transport_directories = me->transport_directories.
  ENDMETHOD.

  METHOD get_cofile_path.
    cofiles_path = get_cofiles_directory( ) && cl_fs_path=>separator && get_cofile_filename( ).
  ENDMETHOD.

  METHOD get_datafile_path.
    datafile_path = get_data_directory( ) && cl_fs_path=>separator && get_datafile_filename( ).
  ENDMETHOD.

  METHOD get_cofile_filename.
    cofile_filename = substring( val = transport off = 3 ) && '.' && substring( val = transport len = 3 ).
  ENDMETHOD.

  METHOD get_datafile_filename.
    datafile_filename = 'R' && substring( val = transport off = 4 ) && '.' && substring( val = transport len = 3 ).
  ENDMETHOD.

  METHOD get_transport_documentation.
    DATA documentation_tab TYPE tline_t.

    CALL FUNCTION 'TRINT_DOCU_INTERFACE'
      EXPORTING
        iv_object = transport
*       iv_action = 'R'
*       iv_modify_appending = 'X'
      TABLES
        tt_line   = documentation_tab
      EXCEPTIONS
        error     = 1
        OTHERS    = 2.
    IF sy-subrc <> 0.
*     MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*       WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.

    CALL FUNCTION 'IDMX_DI_TLINE_INTO_STRING'
      EXPORTING
        it_tline       = documentation_tab
      IMPORTING
        ev_text_string = transport_documentation.

  ENDMETHOD.

  METHOD get_transport_desc.
    transport_description = lcl_transport_helper=>retrieve_transport_desc_s( transport ).
  ENDMETHOD.

  METHOD get_transport.
    transport = me->transport.
  ENDMETHOD.

ENDCLASS.

CLASS lcl_processor DEFINITION.
  PUBLIC SECTION.
    TYPES: BEGIN OF configuration_t,
             data_filename   TYPE string,
             cofile_filename TYPE string,
           END OF configuration_t.

    METHODS constructor
      IMPORTING generate_summary_file TYPE abap_bool
                transport_helper      TYPE REF TO lcl_transport_helper
                authorization_helper  TYPE REF TO lcl_authorization_helper
                files_helper          TYPE REF TO lcl_files_helper.

    METHODS main.

    METHODS add_file_to_zip
      IMPORTING
        filepath TYPE string
        filename TYPE string.

    METHODS add_textfile_to_zip
      IMPORTING
        textcontent TYPE string
        filename    TYPE string.

    METHODS download_zip_file.

    METHODS get_zip_file_name
      RETURNING VALUE(zip_file_name) TYPE string.

    METHODS generate_config_file
      IMPORTING data_filename      TYPE string
                cofile_filename    TYPE string
      RETURNING VALUE(config_file) TYPE string.

  PRIVATE SECTION.
    DATA transport_helper TYPE REF TO lcl_transport_helper.
    DATA authorization_helper TYPE REF TO lcl_authorization_helper.
    DATA files_helper TYPE REF TO lcl_files_helper.
    DATA zip_helper TYPE REF TO cl_abap_zip.
    DATA generate_summary_file TYPE abap_bool.
ENDCLASS.

CLASS lcl_processor IMPLEMENTATION.

  METHOD constructor.
    me->transport_helper = transport_helper.
    me->authorization_helper = authorization_helper.
    me->files_helper = files_helper.
    me->generate_summary_file = generate_summary_file.
  ENDMETHOD.

  METHOD main.
    IF ( NOT authorization_helper->is_authorized( lif_srb_transporttool_auth=>authorization-export_transport ) ).
      MESSAGE 'Not authorized to export transport' TYPE if_drf_const=>msg_type_error.
    ENDIF.

    zip_helper = NEW cl_abap_zip( ).

    add_file_to_zip(
        filepath = transport_helper->get_datafile_path( )
        filename = transport_helper->get_datafile_filename( )
    ).

    add_file_to_zip(
        filepath = transport_helper->get_cofile_path( )
        filename = transport_helper->get_cofile_filename( )
    ).

    add_textfile_to_zip(
        textcontent = generate_config_file( cofile_filename = transport_helper->get_cofile_filename( )   data_filename = transport_helper->get_datafile_filename( ) )
        filename    = '.config.json'
    ).

    files_helper->add_custom_files(
        zip_helper       = zip_helper
        transport_helper = transport_helper
    ).

    IF ( generate_summary_file = abap_true ).

      DATA(description) = transport_helper->get_transport_desc( ).
      DATA(documentation) = transport_helper->get_transport_documentation( ).

      DATA(file_content) = `# ` && description && cl_abap_char_utilities=>newline && documentation.

      add_textfile_to_zip(
          textcontent = file_content
          filename    = 'info.md'
      ).

    ENDIF.


    download_zip_file( ).

  ENDMETHOD.

  METHOD add_file_to_zip.
    DATA datafile_content TYPE xstring.

    OPEN DATASET filepath FOR INPUT IN BINARY MODE.
    IF ( sy-subrc <> 0 ).
      MESSAGE e866(tr) WITH filepath.
    ENDIF.

    READ DATASET filepath INTO datafile_content.
    CLOSE DATASET filepath.

    zip_helper->add(
        name           = filename
        content        = datafile_content
    ).
  ENDMETHOD.

  METHOD add_textfile_to_zip.
    DATA xtextcontent TYPE xstring.

    CALL FUNCTION 'SCMS_STRING_TO_XSTRING'
      EXPORTING
        text   = textcontent
*       mimetype = space
*       encoding =
      IMPORTING
        buffer = xtextcontent
      EXCEPTIONS
        failed = 1
        OTHERS = 2.
    IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*   WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.

    zip_helper->add(
        name           = filename
        content        = xtextcontent
*        compress_level = 6
    ).

  ENDMETHOD.

  METHOD download_zip_file.
    DATA length TYPE i.
    DATA zip_content_table TYPE w3mimetabtype.
    DATA targetfilename TYPE string.
    DATA fullpath TYPE string.
    DATA path TYPE string.

    DATA(zipcontent) = zip_helper->save( ).


    CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
      EXPORTING
        buffer        = zipcontent
      IMPORTING
        output_length = length
      TABLES
        binary_tab    = zip_content_table.

    CALL METHOD cl_gui_frontend_services=>file_save_dialog
      EXPORTING
        default_extension = '.zip'
        default_file_name = get_zip_file_name( )
      CHANGING
        filename          = targetfilename
        path              = path
        fullpath          = fullpath.

    IF ( fullpath IS INITIAL ).
      RETURN.
    ENDIF.

    cl_gui_frontend_services=>gui_download(
      EXPORTING
        bin_filesize              = length
        filename                  = fullpath
        filetype                  = 'BIN'
      CHANGING
        data_tab                  = zip_content_table
      EXCEPTIONS
        file_write_error          = 1
        no_batch                  = 2
        gui_refuse_filetransfer   = 3
        invalid_type              = 4
        no_authority              = 5
        unknown_error             = 6
        header_not_allowed        = 7
        separator_not_allowed     = 8
        filesize_not_allowed      = 9
        header_too_long           = 10
        dp_error_create           = 11
        dp_error_send             = 12
        dp_error_write            = 13
        unknown_dp_error          = 14
        access_denied             = 15
        dp_out_of_memory          = 16
        disk_full                 = 17
        dp_timeout                = 18
        file_not_found            = 19
        dataprovider_exception    = 20
        control_flush_error       = 21
        not_supported_by_gui      = 22
        error_no_gui              = 23
        OTHERS                    = 24
    ).
    IF sy-subrc <> 0.
*   MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*     WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.


  ENDMETHOD.

  METHOD get_zip_file_name.
    zip_file_name = |Transport_{ transport_helper->get_transport( ) }_{ sy-datum }.zip|.
  ENDMETHOD.

  METHOD generate_config_file.
    DATA(config) = VALUE configuration_t(
        data_filename   = data_filename
        cofile_filename = cofile_filename
    ).

    config_file = /ui2/cl_json=>serialize( config ).
  ENDMETHOD.

ENDCLASS.


INCLUDE zsrb_transporttool_exit IF FOUND.

AT SELECTION-SCREEN OUTPUT.

  LOOP AT SCREEN.
    IF screen-name = 'P_DESC'.
      screen-input = '0'.
      MODIFY SCREEN.
      EXIT.
    ENDIF.
  ENDLOOP.

AT SELECTION-SCREEN ON p_trans.
  p_desc = lcl_transport_helper=>retrieve_transport_desc_s( p_trans ).

  DATA(dynpfields) = VALUE dynpread_t( ( fieldname = 'P_DESC' fieldvalue = p_desc ) ).

  CALL FUNCTION 'DYNP_VALUES_UPDATE'
    EXPORTING
      dyname               = sy-repid
      dynumb               = sy-dynnr
    TABLES
      dynpfields           = dynpfields
    EXCEPTIONS
      invalid_abapworkarea = 1
      invalid_dynprofield  = 2
      invalid_dynproname   = 3
      invalid_dynpronummer = 4
      invalid_request      = 5
      no_fielddescription  = 6
      undefind_error       = 7
      OTHERS               = 8.
  IF sy-subrc <> 0.
*     MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*       WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.


AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_trans.

  DATA: es_selected_request TYPE  trwbo_request_header.

  CALL FUNCTION 'TR_F4_REQUESTS'
    EXPORTING
      iv_username             = sy-uname         " User name
*     iv_trkorr_pattern       =                  " Generic template for request number
*     iv_trfunctions          =                  " Request types (string from TRFUNCTIONs)
      iv_trstatus             = 'R'                 " Request status (string from TRSTATUSes)
*     iv_from_date            =
*     iv_to_date              =
*     iv_client               =                  " Source client
*     iv_project              =
      iv_title                = 'Transport wählen'                 " Text for title line
      iv_via_selection_screen = abap_false
      iv_complete_requests    = 'X'
*     it_exclude_requests     =                  " Table of Request Numbers
    IMPORTING
      ev_selected_request     = p_trans                 " Selected request
      es_selected_request     = es_selected_request.

  IF ( p_trans IS NOT INITIAL ).
    p_desc = es_selected_request-as4text.

    DATA(dynpfields) = VALUE dynpread_t( ( fieldname = 'P_DESC' fieldvalue = p_desc ) ).


    CALL FUNCTION 'DYNP_VALUES_UPDATE'
      EXPORTING
        dyname               = sy-repid
        dynumb               = sy-dynnr
      TABLES
        dynpfields           = dynpfields
      EXCEPTIONS
        invalid_abapworkarea = 1
        invalid_dynprofield  = 2
        invalid_dynproname   = 3
        invalid_dynpronummer = 4
        invalid_request      = 5
        no_fielddescription  = 6
        undefind_error       = 7
        OTHERS               = 8.
    IF sy-subrc <> 0.
*     MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*       WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.
  ENDIF.

START-OF-SELECTION.


  DATA(processor) = NEW lcl_processor(
    generate_summary_file = p_summar
    transport_helper      = NEW lcl_transport_helper( p_trans )
    authorization_helper = NEW lcl_authorization_helper( )
    files_helper = NEW lcl_files_helper( )
  ).

  processor->main( ).
