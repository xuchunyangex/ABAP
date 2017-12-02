*&---------------------------------------------------------------------*
*& Report  Z_COMMENTED_CODE_FINDER
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT z_commented_code_finder MESSAGE-ID eu.

TYPE-POOLS:
  slis,
  sscr.

TABLES:
  seoclasstx,
  tadir,
  tlibt,
  d020s,
  trdir.

TYPES: BEGIN OF st_match_lines,
        start_line TYPE i,
        end_line   TYPE i,
       END OF st_match_lines.
TYPES: BEGIN OF st_abaptxt,
        line TYPE sy-tabix,
        source TYPE abaptxt255,
        comment  TYPE xflag,
        keyword  TYPE xflag,
        header   TYPE xflag,
        end_with_dot TYPE xflag,
        code_block TYPE xflag,
        commented_code TYPE xflag,
       END OF st_abaptxt.

TYPES: tt_abaptxt TYPE TABLE OF st_abaptxt.

CLASS:
  lcl_source_scan DEFINITION DEFERRED.

DATA:
  lo_sscan   TYPE REF TO lcl_source_scan,
  lv_sstring TYPE text255,
  lv_appl    TYPE taplt-appl.

SELECTION-SCREEN

                  BEGIN OF BLOCK a01 WITH FRAME TITLE a01.
SELECT-OPTIONS   nmspace     FOR trdir-name.
PARAMETERS      shwerr      TYPE xfeld AS CHECKBOX DEFAULT 'X'.
PARAMETERS      erronly      TYPE xfeld AS CHECKBOX DEFAULT 'X'.
SELECT-OPTIONS      users      FOR sy-uname.
SELECTION-SCREEN: END OF BLOCK a01,

                  BEGIN OF BLOCK a10 WITH FRAME TITLE a10.
SELECT-OPTIONS:   repname  FOR trdir-name MEMORY ID rs_scan_repid,
                  dynnr    FOR d020s-dnum,
                  subc     FOR trdir-subc,
                  appl     FOR lv_appl,
                  cnam     FOR trdir-cnam MATCHCODE OBJECT user_addr,
                  unam     FOR trdir-unam MATCHCODE OBJECT user_addr.
SELECTION-SCREEN: END OF BLOCK a10,
                  BEGIN OF BLOCK a11 WITH FRAME TITLE a11.
SELECT-OPTIONS    devclass FOR tadir-devclass OBLIGATORY.
SELECTION-SCREEN: END OF BLOCK a11,
                  BEGIN OF BLOCK a12 WITH FRAME TITLE a12.
SELECT-OPTIONS:   funcgrp  FOR tlibt-area.
SELECTION-SCREEN: END OF BLOCK a12,
                  BEGIN OF BLOCK a13 WITH FRAME TITLE a13.
SELECT-OPTIONS:   p_class  FOR seoclasstx-clsname .
SELECTION-SCREEN: END OF BLOCK a13,
                  BEGIN OF BLOCK a20 WITH FRAME TITLE a20.
PARAMETERS:       plusminu(2) TYPE n DEFAULT 2,
                  inclu       TYPE xfeld AS CHECKBOX DEFAULT 'X'.
SELECTION-SCREEN: END OF BLOCK a20,
                  BEGIN OF BLOCK a30 WITH FRAME TITLE a30 .
PARAMETERS:       rb_code  RADIOBUTTON GROUP r10 DEFAULT 'X',
                  rb_dyn   RADIOBUTTON GROUP r10,
                  rb_all   RADIOBUTTON GROUP r10,
                  p_vers   TYPE xfeld NO-DISPLAY ."AS CHECKBOX .
SELECTION-SCREEN: END OF BLOCK a30.

*----------------------------------------------------------------------*
*       CLASS lcx_scan_exceptions DEFINITION
*----------------------------------------------------------------------*
*       Exceptions for source scanning
*----------------------------------------------------------------------*
CLASS lcx_scan_exceptions DEFINITION INHERITING FROM cx_static_check FINAL.
ENDCLASS.                    "lcx_scan_exceptions DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_source_scan DEFINITION
*----------------------------------------------------------------------*
*       ABAP source scanner
*----------------------------------------------------------------------*
CLASS lcl_source_scan DEFINITION FINAL.
  PUBLIC SECTION.
    METHODS:
      constructor,

      f4_class
        CHANGING
          cv_class_name TYPE clike,

      f4_function_group
        IMPORTING
          iv_group_name TYPE clike,

      f4_repname
        CHANGING
          cv_repname TYPE clike,

      pbo,

      start.

  PROTECTED SECTION.
    TYPES:
      BEGIN OF ty_dynpro,
        repname LIKE d020s-prog,
        dynnr  LIKE d020s-dnum,
      END OF ty_dynpro.

    TYPES:
      BEGIN OF ty_ls_objname,
        report TYPE sy-repid,
        dynnr  TYPE sy-dynnr,
      END OF ty_ls_objname.

    DATA:
      go_alv        TYPE REF TO cl_salv_hierseq_table,
      gv_hit_count  TYPE i,
      gv_sstring    TYPE string,
      gv_dynp_found TYPE xfeld,
      gv_vers_found TYPE xfeld,
      gt_dynpro     TYPE STANDARD TABLE OF ty_dynpro,
      gt_object     TYPE STANDARD TABLE OF tadir-obj_name,
      gt_vrsd       TYPE HASHED TABLE OF vrsd
                      WITH UNIQUE KEY objname versno,
      gt_source TYPE tt_abaptxt,
      gv_report TYPE syrepid,
      gv_dynpro TYPE sydynnr,

      BEGIN OF gs_alv_header,
        repname TYPE tadir-obj_name,
        repdecr TYPE text200,
        dynnr   TYPE sy-dynnr,
        expand  TYPE xfeld,
        versno  TYPE vrsd-versno,
      END OF gs_alv_header,

      gt_alv_header LIKE STANDARD TABLE OF gs_alv_header,

      BEGIN OF gs_alv_item,
        repname    TYPE sy-repid,
        dynnr      TYPE sy-dynnr,
        versno     TYPE vrsd-versno,
        line_no    TYPE sy-tabix,
        text       TYPE text255,
        hit        TYPE xfeld,
        cell_color TYPE lvc_t_scol,
        color      TYPE xfeld,

      END OF gs_alv_item,

      gt_alv_item LIKE STANDARD TABLE OF gs_alv_item.

    CONSTANTS:
      gc_x TYPE xfeld VALUE 'X'.

    METHODS:
      add_to_hitlist
        IMPORTING
          iv_report      TYPE clike
          iv_dynpro      TYPE clike OPTIONAL
          iv_source_line TYPE clike
          iv_tabix       TYPE sy-tabix
          iv_hit         TYPE xfeld
          iv_versno      TYPE vrsd-versno,

      call_abap_editor
        IMPORTING
          is_alv_item LIKE gs_alv_item,

      call_screen_painter
        IMPORTING
          is_alv_item LIKE gs_alv_item,

      display,

      display_abap_version
        IMPORTING
          is_alv_item LIKE gs_alv_item,

      display_screen_painter_version
        IMPORTING
          is_alv_item LIKE gs_alv_item,

      display_version_management
        IMPORTING
          is_alv_header LIKE gs_alv_header,

      get_alv_instance,
      get_dynpro_flow_logic
        IMPORTING
          iv_report TYPE clike
          iv_dynpro TYPE clike
        RETURNING VALUE(rt_dflow) LIKE gt_source,

      get_hit_set
        IMPORTING
          iv_report      TYPE clike
          iv_dynpro      TYPE clike OPTIONAL
          it_abap        TYPE tt_abaptxt
          iv_from        TYPE sy-tabix
          iv_to          TYPE sy-tabix
          iv_versno      TYPE vrsd-versno
        EXPORTING
          ev_addition    TYPE abap_bool,

      get_version_numbers
        IMPORTING
          iv_report TYPE clike
          iv_dynpro TYPE clike OPTIONAL
        RETURNING VALUE(rt_vrsd) LIKE gt_vrsd,

      get_source_code
        IMPORTING
          iv_report TYPE clike
        RETURNING VALUE(rt_source) LIKE gt_source,

      get_source_details
        CHANGING ct_source LIKE gt_source,

        check_if_code
        IMPORTING
          iv_source_line   TYPE clike
        RETURNING VALUE(rv_is_source) TYPE abap_bool,

      get_dynpros,
      get_source_names,

      get_source_by_version
        IMPORTING
          iv_report TYPE clike
          iv_dynpro TYPE clike OPTIONAL
          iv_versno TYPE vrsd-versno
        RETURNING VALUE(rt_abap) LIKE gt_source,

      get_report_names,
      get_function_names,
      get_class_names,
      get_interface_names,
      get_includes,

      search_abap_source   RAISING lcx_scan_exceptions,
      search_dynpro_source RAISING lcx_scan_exceptions,

      search_source        RAISING lcx_scan_exceptions,

      set_alv_attributes,

      on_link_click
          FOR EVENT link_click OF cl_salv_events_hierseq
            IMPORTING
                sender
                level
                row
                column.

ENDCLASS.                    "lcl_source_scan DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_source_scan IMPLEMENTATION
*----------------------------------------------------------------------*
*       ABAP source scanner
*----------------------------------------------------------------------*
CLASS lcl_source_scan IMPLEMENTATION.
  METHOD display_screen_painter_version.
    DATA:
      lv_object_name TYPE versobjnam,
      ls_infolna     TYPE vrsinfolna,
      ls_infolnb     TYPE vrsinfolnb,
      ls_vrsd        LIKE LINE OF gt_vrsd,
      ls_object_name TYPE ty_ls_objname.

    ls_object_name-report = is_alv_item-repname.
    ls_object_name-dynnr  = is_alv_item-dynnr.
    lv_object_name        = ls_object_name.

    READ TABLE gt_vrsd WITH TABLE KEY objname = lv_object_name
                                      versno  = is_alv_item-versno
                                      INTO ls_vrsd.

    CHECK sy-subrc IS INITIAL.

    ls_infolna = lv_object_name.
    MOVE-CORRESPONDING ls_vrsd TO ls_infolnb.

    CALL FUNCTION 'RS_SCRP_SHOW_VERS'
      EXPORTING
        infolna = ls_infolna
        infolnb = ls_infolnb
        objname = lv_object_name
        versno  = is_alv_item-versno
      EXCEPTIONS
        OTHERS  = 0.

  ENDMETHOD.                    "display_screen_painter_version

  METHOD display_abap_version.
    DATA:
      lt_trdir       TYPE STANDARD TABLE OF trdir,
      lv_object_name TYPE versobjnam,
      lv_title       TYPE sy-title,
      lt_abap        TYPE abaptxt255_tab.

    lv_object_name = is_alv_item-repname.

*   Display report version
    CALL FUNCTION 'SVRS_GET_REPS_FROM_OBJECT'
      EXPORTING
        object_name                  = lv_object_name
        object_type                  = 'REPS'
        versno                       = is_alv_item-versno
        iv_no_release_transformation = gc_x
      TABLES
        repos_tab                    = lt_abap
        trdir_tab                    = lt_trdir
      EXCEPTIONS
        no_version                   = 1
        OTHERS                       = 2.

    CHECK sy-subrc IS INITIAL.

    CONCATENATE 'Programm:'(004)
                is_alv_item-repname
                'Version'(005)
                 is_alv_item-versno
                 INTO lv_title SEPARATED BY space.

    EDITOR-CALL FOR lt_abap TITLE lv_title DISPLAY-MODE.

  ENDMETHOD.                    "display_abap_version

  METHOD call_screen_painter.
    CALL FUNCTION 'RS_SCRP'
      EXPORTING
        abl_line    = is_alv_item-line_no
        dynnr       = is_alv_item-dynnr
        progname    = is_alv_item-repname
        wanted_mode = 'SHOW'
      EXCEPTIONS
        OTHERS      = 0.

  ENDMETHOD.                    "call_screen_painter

  METHOD get_source_code.
    DATA: lt_source TYPE abaptxt255_tab,
          lv_index  TYPE sy-tabix.

    FIELD-SYMBOLS: <ls_source> LIKE LINE OF lt_source,
                   <ls_source_str> LIKE LINE OF gt_source.
    READ REPORT iv_report INTO lt_source.

    LOOP AT lt_source ASSIGNING <ls_source>.
      lv_index = sy-tabix.
      APPEND INITIAL LINE TO rt_source ASSIGNING <ls_source_str>.
      <ls_source_str>-line = lv_index.
      <ls_source_str>-source = <ls_source>.


    ENDLOOP.


  ENDMETHOD.                    "get_source_details


  METHOD get_source_details.

    DATA: lv_header_started    TYPE abap_bool,
          lv_head_ln_count     TYPE sy-tabix,
          lv_header_strt_indx  TYPE sy-tabix,
          lv_header_end_indx   TYPE sy-tabix,
          lv_index             TYPE sy-tabix,
          lv_last_char_len     TYPE i,
          lv_last_char         TYPE c,
          lv_string            TYPE string,
          lv_contn_code        TYPE abap_bool.

    FIELD-SYMBOLS: <ls_source> LIKE LINE OF ct_source.

    LOOP AT ct_source ASSIGNING <ls_source>.
      IF <ls_source>-source(1) = '*'.
        lv_string = <ls_source>-source.

        <ls_source>-comment = abap_true.
        IF check_if_code( <ls_source>-source ) = abap_true.
          <ls_source>-keyword = abap_true.
        ENDIF.


        lv_string = <ls_source>-source.
        SPLIT lv_string AT '"' INTO lv_string lv_last_char.
        CONDENSE lv_string.

        lv_last_char_len = strlen( lv_string ) - 1.

        IF lv_last_char_len <= 1.
        ELSE.
          lv_last_char = lv_string+lv_last_char_len.
          IF  ( lv_last_char = '.' OR lv_last_char = ',' ).
            <ls_source>-end_with_dot = abap_true.
            <ls_source>-commented_code = abap_true.
          ENDIF.
        ENDIF.
      ENDIF.

    ENDLOOP.


    LOOP AT gt_source ASSIGNING <ls_source>.
      lv_index = sy-tabix.
      IF <ls_source>-line < 30."Check for header.
        lv_string = <ls_source>-source.
        CONDENSE lv_string.
        IF <ls_source>-comment = abap_true..
          IF lv_header_started IS INITIAL
             AND lv_string CO '*-'.
            lv_header_started  = abap_true.
            lv_head_ln_count = 1.
            lv_header_strt_indx = lv_index.
          ELSEIF lv_header_started = abap_true
                 AND lv_string CO '*-'.
            lv_head_ln_count = lv_head_ln_count + 1.
            IF lv_head_ln_count = 6.
              lv_header_end_indx = lv_index.
              EXIT.
            ENDIF.
          ENDIF.
        ELSE.
          CLEAR:lv_header_started,
                lv_head_ln_count,
                lv_header_strt_indx,
                lv_header_end_indx.
          IF lv_header_started  = abap_true.
            EXIT.
          ENDIF.
        ENDIF.
      ELSE.
        EXIT.
      ENDIF.
    ENDLOOP.

    IF lv_header_started  = abap_true AND
       lv_header_strt_indx IS NOT INITIAL AND
       lv_header_end_indx IS NOT INITIAL.
      LOOP AT gt_source ASSIGNING <ls_source> FROM lv_header_strt_indx TO lv_header_end_indx.
        <ls_source>-header = abap_true.
        CLEAR <ls_source>-commented_code.

      ENDLOOP.
    ENDIF.
    CLEAR lv_contn_code.


    LOOP AT gt_source ASSIGNING <ls_source>.
      lv_index = sy-tabix.
      IF <ls_source>-comment = abap_true.

        lv_string = <ls_source>-source.
        CONDENSE lv_string.
        REPLACE ALL OCCURRENCES OF '*' IN lv_string WITH ''.
        TRANSLATE lv_string TO UPPER CASE.
        IF lv_contn_code = abap_true.
          <ls_source>-code_block = abap_true.
          <ls_source>-commented_code = abap_true.
        ENDIF.
        IF lv_string CP 'CALL FUNCTION*' OR
           lv_string CP 'SELECT*' OR
           lv_string CP 'CALL METHOD*'.
          IF <ls_source>-end_with_dot <> abap_true.
            <ls_source>-code_block = abap_true.
            <ls_source>-commented_code = abap_true.
            lv_contn_code = abap_true.
          ENDIF.
        ELSEIF lv_contn_code = abap_true AND
               <ls_source>-end_with_dot = abap_true.
          CLEAR lv_contn_code.
        ENDIF.
      ELSEIF lv_contn_code = abap_true.
        CLEAR lv_contn_code.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.                    "get_source_details

  METHOD call_abap_editor.
    CALL FUNCTION 'EDITOR_PROGRAM'
      EXPORTING
        appid   = 'PG'
        display = gc_x
        program = is_alv_item-repname
        line    = is_alv_item-line_no
        topline = is_alv_item-line_no
      EXCEPTIONS
        OTHERS  = 0.

  ENDMETHOD.                    "call_abap_editor


  METHOD check_if_code.

    DATA: lt_tokens TYPE TABLE OF stokex,
          lt_sstmnt TYPE TABLE OF sstmnt,
          lt_levels TYPE TABLE OF slevel,
          lv_source_line TYPE text255.


    FIELD-SYMBOLS: <ls_sstmnt> LIKE LINE OF lt_sstmnt.

    lv_source_line = iv_source_line.
    REPLACE ALL OCCURRENCES OF '*' IN lv_source_line WITH ''.

    SCAN ABAP-SOURCE lv_source_line
                              TOKENS INTO lt_tokens
                              STATEMENTS INTO lt_sstmnt
                              WITH ANALYSIS
                              LEVELS INTO lt_levels.

    READ TABLE lt_sstmnt ASSIGNING <ls_sstmnt> INDEX 1.
    IF sy-subrc = 0.
      IF <ls_sstmnt>-type = 'K'.
        rv_is_source = abap_true.
      ENDIF.
    ENDIF.

  ENDMETHOD.                    "check_if_code


  METHOD display_version_management.
    IF is_alv_header-dynnr IS INITIAL.
*     call version management for programs
      CALL FUNCTION 'RS_PROGRAM_VERSIONS'
        EXPORTING
          progname = is_alv_header-repname
        EXCEPTIONS
          OTHERS   = 0.
    ELSE.
      CALL FUNCTION 'RS_SCRP_VERSION'
        EXPORTING
          dynnr     = is_alv_header-dynnr
          progname  = is_alv_header-repname
          no_update = gc_x.
    ENDIF.
  ENDMETHOD.                    "display_version_management

  METHOD constructor.
    DATA:
      ls_restrict    TYPE sscr_restrict,
      ls_opt_list    TYPE sscr_opt_list,
      ls_association TYPE sscr_ass.

    ls_opt_list-name       = 'RESTRICT'.
    ls_opt_list-options-cp = gc_x.
    ls_opt_list-options-eq = gc_x.

    APPEND ls_opt_list TO ls_restrict-opt_list_tab.

    ls_association-kind    = 'S'.
    ls_association-name    = 'SSTRING'.
    ls_association-sg_main = 'I'.
    ls_association-op_main = ls_association-op_addy = 'RESTRICT'.

    APPEND ls_association TO ls_restrict-ass_tab.

    CALL FUNCTION 'SELECT_OPTIONS_RESTRICT'
      EXPORTING
        program     = sy-repid
        restriction = ls_restrict
      EXCEPTIONS
        OTHERS      = 0.

  ENDMETHOD.                    "constructor

  METHOD get_dynpro_flow_logic.
    DATA: ls_dhead  TYPE d020s,
          lt_dfield TYPE STANDARD TABLE OF d021s,
          lt_dflow  TYPE STANDARD TABLE OF d022s,
          lt_dmatch TYPE STANDARD TABLE OF d023s,

          lv_index  TYPE sy-tabix,

          BEGIN OF ls_dynp_id,
            prog TYPE d020s-prog,
            dnum TYPE d020s-dnum,
         END OF ls_dynp_id.

    FIELD-SYMBOLS: <ls_dflow> LIKE LINE OF lt_dflow,
                   <ls_source_str> LIKE LINE OF gt_source.


    ls_dynp_id-prog = iv_report.
    ls_dynp_id-dnum = iv_dynpro.

    IMPORT DYNPRO ls_dhead lt_dfield lt_dflow lt_dmatch ID ls_dynp_id.
    LOOP AT lt_dflow ASSIGNING <ls_dflow>.
      lv_index = sy-tabix.
      APPEND INITIAL LINE TO rt_dflow ASSIGNING <ls_source_str>.
      <ls_source_str>-line = lv_index.
      <ls_source_str>-source = <ls_dflow>-line.
    ENDLOOP.
  ENDMETHOD.                    "get_dynpro_flow_logic

  METHOD on_link_click.
    DATA:
      ls_alv_header  LIKE LINE OF gt_alv_header,
      ls_alv_item    LIKE LINE OF gt_alv_item.

    CASE level.
      WHEN '1'.
        READ TABLE gt_alv_header INDEX row INTO ls_alv_header.
        CHECK sy-subrc IS INITIAL.

        display_version_management( ls_alv_header ).

      WHEN '2'.
        READ TABLE gt_alv_item INDEX row INTO ls_alv_item.
        CHECK sy-subrc IS INITIAL.

        IF ls_alv_item-dynnr IS INITIAL.
          IF ls_alv_item-versno IS INITIAL.
            call_abap_editor( ls_alv_item ).
          ELSE.
            display_abap_version( ls_alv_item ).
          ENDIF.

          SET PARAMETER ID 'RID' FIELD sy-repid.
        ELSE.
*         Call screen painter
          IF ls_alv_item-versno IS INITIAL.
            call_screen_painter( ls_alv_item ).
          ELSE.
            display_screen_painter_version( ls_alv_item ).
          ENDIF.
        ENDIF.
    ENDCASE.
  ENDMETHOD.                    "on_link_click

  METHOD set_alv_attributes.
    DATA:
      lo_layout    TYPE REF TO cl_salv_layout,
      lo_events    TYPE REF TO cl_salv_events_hierseq,
      lo_functions TYPE REF TO cl_salv_functions_list,
      lo_level     TYPE REF TO cl_salv_hierseq_level,
      lo_column    TYPE REF TO cl_salv_column_hierseq,
      lo_columns   TYPE REF TO cl_salv_columns_hierseq,
      lt_columns   TYPE salv_t_column_ref,
      ls_columns   LIKE LINE OF lt_columns,
      lo_settings  TYPE REF TO cl_salv_display_settings,
      lv_title     TYPE lvc_title,
      lv_hits      TYPE lvc_title,
      ls_color     TYPE lvc_s_colo,
      ls_layout    TYPE salv_s_layout_key,
      lt_functions TYPE salv_t_ui_func.

*   Layout
    ls_layout-report = sy-repid.
    ls_layout-handle = 'SCAN'.

    lo_layout = go_alv->get_layout( ).
    lo_layout->set_key( ls_layout ).
    lo_layout->set_save_restriction( ).

*   Function keys/buttons
    lo_functions = go_alv->get_functions( ).
    lo_functions->set_all( gc_x ).

*   exclude the following functions (column paging buttons)
    lt_functions = lo_functions->get_functions( ).

*   Display settings
    lo_settings = go_alv->get_display_settings( ).

*   Title
    lv_hits = gv_hit_count.
    SHIFT lv_hits LEFT DELETING LEADING space.

    CONCATENATE lv_hits
                'Treffer'(001)
                INTO lv_hits SEPARATED BY space.

    lv_title = 'Source Scan für String:'(002).

    CONCATENATE lv_title
                gv_sstring
                INTO lv_title SEPARATED BY space.

    CONCATENATE lv_title
                lv_hits
                INTO lv_title SEPARATED BY ' - '.

    lo_settings->set_list_header( lv_title ).

*   Event handling
    lo_events = go_alv->get_event( ).
    SET HANDLER on_link_click FOR lo_events.

*   Field catalog
    TRY.
*       Field catalog/columns - header table
        lo_columns  = go_alv->get_columns( '1' ).
        lt_columns = lo_columns->get( ).

        TRY.
            lo_columns->set_expand_column( 'EXPAND' ).

            lo_level = go_alv->get_level( '1' ).
            lo_level->set_items_expanded( gc_x ).

          CATCH cx_salv_data_error.
        ENDTRY.

        LOOP AT lt_columns INTO ls_columns.
          CASE ls_columns-columnname.
            WHEN 'EXPAND'.
              ls_columns-r_column->set_technical( ).

            WHEN 'DYNNR'.
              IF gv_dynp_found IS INITIAL.
                ls_columns-r_column->set_technical( ).
              ELSE.
                ls_columns-r_column->set_output_length( '15' ).
              ENDIF.

            WHEN 'VERSNO'.
              IF gv_vers_found IS INITIAL.
                ls_columns-r_column->set_technical( ).
              ELSE.
                ls_columns-r_column->set_leading_zero( gc_x ).
                ls_columns-r_column->set_output_length( '15' ).
                TRY.
                    lo_column ?= ls_columns-r_column.
                    lo_column->set_cell_type( if_salv_c_cell_type=>hotspot ).
                  CATCH cx_sy_move_cast_error.
                ENDTRY.
              ENDIF.
          ENDCASE.
        ENDLOOP.

*       Field catalog/columns - item table
        lo_columns = go_alv->get_columns( '2' ).

        TRY.
            lo_columns->set_color_column( 'CELL_COLOR' ).
          CATCH cx_salv_data_error.
        ENDTRY.

        lt_columns = lo_columns->get( ).

        LOOP AT lt_columns INTO ls_columns.
          CASE ls_columns-columnname.
            WHEN 'REPNAME'.
              ls_columns-r_column->set_technical( ).
            WHEN 'REPDECR'.
              ls_columns-r_column->set_technical( ).

            WHEN 'DYNNR'.
              ls_columns-r_column->set_technical( ).

            WHEN 'VERSNO'.
              ls_columns-r_column->set_technical( ).

            WHEN 'CELL_COLOR'.
              ls_columns-r_column->set_technical( ).

            WHEN 'HIT'.
              ls_columns-r_column->set_technical( ).

            WHEN 'LINE_NO'.
              ls_color-col = '4'.
              TRY.
                  lo_column ?= ls_columns-r_column.
                  lo_column->set_color( ls_color ).
                  lo_column->set_leading_zero( gc_x ).
                CATCH cx_sy_move_cast_error.
              ENDTRY.

            WHEN 'TEXT'.
              TRY.
                  lo_column ?= ls_columns-r_column.
                  lo_column->set_cell_type( if_salv_c_cell_type=>hotspot ).
                CATCH cx_sy_move_cast_error.
              ENDTRY.

          ENDCASE.
        ENDLOOP.
      CATCH cx_salv_not_found.
    ENDTRY.

  ENDMETHOD.                    "set_alv_attributes

  METHOD get_alv_instance.
    DATA:
      lt_alv_bind TYPE salv_t_hierseq_binding,
      ls_alv_bind LIKE LINE OF lt_alv_bind.

    ls_alv_bind-master = ls_alv_bind-slave = 'REPNAME'."'REPDECR'."
    APPEND ls_alv_bind TO lt_alv_bind.

    ls_alv_bind-master = ls_alv_bind-slave = 'DYNNR'.
    APPEND ls_alv_bind TO lt_alv_bind.

    ls_alv_bind-master = ls_alv_bind-slave = 'VERSNO'.
    APPEND ls_alv_bind TO lt_alv_bind.

    TRY.
        CALL METHOD cl_salv_hierseq_table=>factory
          EXPORTING
            t_binding_level1_level2 = lt_alv_bind
          IMPORTING
            r_hierseq               = go_alv
          CHANGING
            t_table_level1          = gt_alv_header
            t_table_level2          = gt_alv_item.

      CATCH cx_salv_data_error.
      CATCH cx_salv_not_found.
    ENDTRY.

  ENDMETHOD.                    "get_alv_instance

  METHOD f4_repname.
    CALL FUNCTION 'REPOSITORY_INFO_SYSTEM_F4'
      EXPORTING
        object_type          = 'PROG'
        object_name          = cv_repname
        suppress_selection   = 'X'
      IMPORTING
        object_name_selected = cv_repname
      EXCEPTIONS
        cancel               = 0.
  ENDMETHOD.                                                "f4_repname

  METHOD f4_function_group.
    DATA:
      lv_fname TYPE dynfnam.

    lv_fname = iv_group_name.

    CALL FUNCTION 'RS_HELP_HANDLING'
      EXPORTING
        dynpfield                 = lv_fname
        dynpname                  = sy-dynnr
        object                    = 'FG  '
        progname                  = sy-repid
        suppress_selection_screen = 'X'.

  ENDMETHOD.                    "f4_function_group

  METHOD f4_class.
    CALL FUNCTION 'F4_DD_ALLTYPES'
      EXPORTING
        object               = cv_class_name
        suppress_selection   = gc_x
        display_only         = space
        only_types_for_clifs = gc_x
      IMPORTING
        result               = cv_class_name.
  ENDMETHOD.                                                "f4_class

  METHOD display.

    DATA text TYPE c LENGTH 150.

    IF gv_hit_count IS INITIAL.
      MESSAGE s326 WITH gv_sstring.
      RETURN.
    ENDIF.

    IF sy-batch IS INITIAL.
      text = |DISPLAY { gv_hit_count } HITS...|.
      CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
        EXPORTING
          text = text.
    ENDIF.

    SORT gt_alv_item BY repname dynnr versno line_no hit DESCENDING.
    DELETE ADJACENT DUPLICATES FROM gt_alv_item COMPARING repname dynnr versno line_no.

    get_alv_instance( ).
    CHECK go_alv IS NOT INITIAL.

    set_alv_attributes( ).

    go_alv->display( ).

  ENDMETHOD.                    "display

  METHOD add_to_hitlist.
    DATA:
      ls_col LIKE LINE OF gs_alv_item-cell_color.

    gs_alv_item-repname = iv_report.

    gs_alv_item-dynnr   = iv_dynpro.
    gs_alv_item-line_no = iv_tabix.
    gs_alv_item-versno  = iv_versno.
    gs_alv_item-text    = iv_source_line.
*    CONCATENATE gs_alv_item-repname '_1' INTO gs_alv_item-repdecr.

    IF iv_hit IS NOT INITIAL.
      gs_alv_item-hit = gc_x.
      ADD 1 TO gv_hit_count.
      ls_col-fname     = 'TEXT'.
      ls_col-color-col = iv_hit."'5'.
      APPEND ls_col TO gs_alv_item-cell_color.
    ENDIF.

    APPEND gs_alv_item TO gt_alv_item.

    CLEAR gs_alv_item.
  ENDMETHOD.                    "add_to_hitlist

  METHOD get_hit_set.
    DATA: lv_end     TYPE i,
          lv_start   TYPE i,
          lv_xtabix  TYPE sy-tabix,
          lv_hitline TYPE xfeld,
          lt_hit_item LIKE STANDARD TABLE OF gs_alv_item,
          lv_dcode_found    TYPE abap_bool.


    FIELD-SYMBOLS:
      <lv_abap> LIKE LINE OF it_abap,
      <ls_hit_item> LIKE LINE OF lt_hit_item.

    lv_start = iv_from - plusminu .
    lv_end   = iv_to   + plusminu.
    CLEAR ev_addition.
    IF lv_start < 1.
      lv_start = 1.
    ENDIF.

    WHILE lv_start <= lv_end.
      READ TABLE it_abap ASSIGNING <lv_abap> INDEX lv_start.
      IF sy-subrc IS NOT INITIAL.
        EXIT.
      ENDIF.

      lv_xtabix = <lv_abap>-line.

      IF <lv_abap>-commented_code = abap_true.
        lv_hitline = 6.
        lv_dcode_found = abap_true.
      ELSEIF <lv_abap>-keyword = abap_true.
        lv_hitline = 2.
      ELSEIF <lv_abap>-comment = abap_true.
        lv_hitline = 4.
      ELSE.
        lv_hitline = 0.
      ENDIF.

      APPEND INITIAL LINE TO lt_hit_item ASSIGNING <ls_hit_item>.
      <ls_hit_item>-repname = iv_report.
      <ls_hit_item>-dynnr   = iv_dynpro.
      <ls_hit_item>-line_no = lv_xtabix.
      <ls_hit_item>-versno  = iv_versno.
      <ls_hit_item>-text    = <lv_abap>-source.
      <ls_hit_item>-color    = lv_hitline.

      ADD 1 TO lv_start.

    ENDWHILE.

    IF shwerr IS INITIAL OR ( shwerr = abap_true AND lv_dcode_found = abap_true ).
      LOOP AT lt_hit_item ASSIGNING <ls_hit_item>.
        CALL METHOD add_to_hitlist
          EXPORTING
            iv_report      = <ls_hit_item>-repname
            iv_dynpro      = <ls_hit_item>-dynnr
            iv_source_line = <ls_hit_item>-text
            iv_tabix       = <ls_hit_item>-line_no
            iv_hit         = <ls_hit_item>-color
            iv_versno      = <ls_hit_item>-versno.
        ev_addition = abap_true.
      ENDLOOP.

    ENDIF.
  ENDMETHOD.                    "get_hit_set

  METHOD get_source_by_version.
    DATA:
      lv_object_name TYPE versobjnam,
      ls_object_name TYPE ty_ls_objname,
      lt_trdir       TYPE STANDARD TABLE OF trdir,
      lt_d022s       TYPE STANDARD TABLE OF d022s,
      lv_index  TYPE sy-tabix.

    FIELD-SYMBOLS: <ls_d022s> LIKE LINE OF lt_d022s,
                   <ls_source_str> LIKE LINE OF gt_source.

    IF iv_dynpro IS INITIAL.
      lv_object_name = iv_report.

      CALL FUNCTION 'SVRS_GET_REPS_FROM_OBJECT'
        EXPORTING
          object_name                  = lv_object_name
          object_type                  = 'REPS'
          versno                       = iv_versno
          iv_no_release_transformation = 'X'
        TABLES
          repos_tab                    = rt_abap
          trdir_tab                    = lt_trdir
        EXCEPTIONS
          no_version                   = 1
          OTHERS                       = 2.
    ELSE.
      ls_object_name-report = iv_report.
      ls_object_name-dynnr  = iv_dynpro.

      lv_object_name = ls_object_name.

      CALL FUNCTION 'SVRS_GET_VERSION_DYNP_40'
        EXPORTING
          object_name = lv_object_name
          versno      = iv_versno
        TABLES
          d022s_tab   = lt_d022s
        EXCEPTIONS
          OTHERS      = 1.

      CHECK sy-subrc IS INITIAL AND lt_d022s IS NOT INITIAL.
      LOOP AT lt_d022s ASSIGNING <ls_d022s>.
        lv_index = sy-tabix.
        APPEND INITIAL LINE TO gt_source ASSIGNING <ls_source_str>.
        <ls_source_str>-line = lv_index.
        <ls_source_str>-source = <ls_d022s>.

      ENDLOOP.
    ENDIF.
  ENDMETHOD.                    "get_source_by_version

  METHOD get_version_numbers.
    DATA:
      ls_objname TYPE ty_ls_objname,
      lv_objtype TYPE vrsd-objtype,
      lv_objname TYPE versobjnam,
      lv_versno  TYPE versno,
      lt_vrsn    TYPE STANDARD TABLE OF vrsn,
      lt_vrsd    TYPE STANDARD TABLE OF vrsd.

    ls_objname-report = iv_report.
    ls_objname-dynnr  = iv_dynpro.
    lv_objname        = ls_objname.

    IF iv_dynpro IS INITIAL.
      lv_objtype = 'REPS'.
    ELSE.
      lv_objtype = 'DYNP'.
    ENDIF.

    CALL FUNCTION 'SVRS_GET_VERSION_DIRECTORY_46'
      EXPORTING
        objname      = lv_objname
        objtype      = lv_objtype
      TABLES
        lversno_list = lt_vrsn
        version_list = lt_vrsd
      EXCEPTIONS
        OTHERS       = 1.

    CHECK sy-subrc IS INITIAL .

    SORT lt_vrsd BY objname versno.
    DELETE ADJACENT DUPLICATES FROM lt_vrsd COMPARING objname versno.

    rt_vrsd = lt_vrsd.

    DELETE TABLE rt_vrsd WITH TABLE KEY objname = lv_objname
                                        versno  = lv_versno.

    SORT rt_vrsd.

    CHECK iv_dynpro IS NOT INITIAL.
*   For dynpros we need to save the version information for the version display
*   this is not required for source code
    INSERT LINES OF rt_vrsd INTO TABLE gt_vrsd.

  ENDMETHOD.                    "get_version_Numbers

  METHOD search_abap_source.
    DATA:
      percentage     TYPE i,
      old_percentage TYPE i VALUE -1,
      text           TYPE c LENGTH 150.

    LOOP AT gt_object INTO gv_report.

      IF sy-batch IS INITIAL.
        percentage = sy-tabix * 100 / lines( gt_object ).
        text = |SEARCH ABAP SOURCES ({ sy-tabix }/{ lines( gt_object ) })...|.

        IF old_percentage <> percentage.
          CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
            EXPORTING
              percentage = percentage
              text       = text.
          old_percentage = percentage.
        ENDIF.
      ENDIF.
      gt_source = get_source_code( iv_report = gv_report ).


      search_source( ).

    ENDLOOP.

    FREE gt_object.

  ENDMETHOD.                    "search_abap_source


**************************************
*    CustDev: SEARCH SOURCE
**************************************

  METHOD search_source.
    DATA:
      lt_source_vers      TYPE tt_abaptxt,
      lv_string_found     TYPE xfeld,
      lt_vrsd             TYPE STANDARD TABLE OF vrsd,
      ls_vrsd             LIKE LINE OF lt_vrsd,
      lv_number           TYPE i,
      lv_index            TYPE i,
      lt_results          TYPE TABLE OF st_match_lines,
      lt_final            TYPE TABLE OF st_match_lines,
      ls_result           LIKE LINE OF lt_results,
      lv_low              TYPE i,
      lv_prev             TYPE i,
      lv_last_char_len    TYPE i,
      lv_last_char        TYPE c,
      lv_addition         TYPE abap_bool,
      lv_object_added     TYPE abap_bool,
      lv_string           TYPE string,
      ls_address TYPE bapiaddr3,
      lt_return  TYPE TABLE OF bapiret2.




    FIELD-SYMBOLS: <ls_source> LIKE LINE OF gt_source,
                   <ls_result1> LIKE LINE OF lt_results,
                   <ls_result2> LIKE LINE OF lt_results.


    IF p_vers IS INITIAL.
      lv_number = 1.
    ELSE.
      lt_vrsd = get_version_numbers( iv_report = gv_report
                                     iv_dynpro = gv_dynpro ).
      lv_number = lines( lt_vrsd ) + 1.
    ENDIF.

    DO lv_number TIMES.
      CLEAR lv_string_found.

      IF sy-index = 1.
        CLEAR ls_vrsd.
      ELSE.
        lv_index = sy-index - 1.
        READ TABLE lt_vrsd INDEX lv_index INTO ls_vrsd.
        CHECK sy-subrc IS INITIAL.

        lt_source_vers = get_source_by_version( iv_report = gv_report
                                                iv_dynpro = gv_dynpro
                                                iv_versno = ls_vrsd-versno ).

        IF lt_source_vers IS NOT INITIAL.
          gt_source = lt_source_vers.
        ELSE.
          CONTINUE.
        ENDIF.
      ENDIF.

      REFRESH lt_results.


      CALL METHOD get_source_details
        CHANGING
          ct_source = gt_source.

      IF erronly = abap_true.
        DELETE gt_source WHERE commented_code <> abap_true.
      ENDIF.


      LOOP AT gt_source ASSIGNING <ls_source>.
        lv_index = sy-tabix.
        IF <ls_source>-comment = abap_true AND <ls_source>-header <> abap_true.
          APPEND INITIAL LINE TO lt_results ASSIGNING <ls_result1>.
          <ls_result1>-start_line = lv_index.
        ENDIF.
      ENDLOOP.

      LOOP AT lt_results ASSIGNING <ls_result1>.
        lv_index = sy-tabix.
        IF lv_low IS INITIAL.
          lv_low = lv_prev = <ls_result1>-start_line.
          CONTINUE.
        ELSE.
          IF <ls_result1>-start_line = lv_prev + 1.
            lv_prev = <ls_result1>-start_line.
            CONTINUE.
          ELSE.
            APPEND INITIAL LINE TO lt_final ASSIGNING <ls_result2>.
            <ls_result2>-start_line = lv_low.
            <ls_result2>-end_line = lv_prev.
            lv_low = lv_prev = <ls_result1>-start_line.
          ENDIF.
        ENDIF.
      ENDLOOP.

      IF lv_low IS NOT INITIAL AND lv_prev IS NOT INITIAL.
        APPEND INITIAL LINE TO lt_final ASSIGNING <ls_result2>.
        <ls_result2>-start_line = lv_low.
        <ls_result2>-end_line = lv_prev.
      ENDIF.

      CHECK lt_final IS NOT INITIAL.


      SORT lt_final BY start_line end_line.
      DELETE ADJACENT DUPLICATES FROM lt_final COMPARING start_line end_line.
      lv_object_added = abap_false.
      LOOP AT lt_final INTO <ls_result1>.

        CALL METHOD get_hit_set
          EXPORTING
            iv_report   = gv_report
            iv_dynpro   = gv_dynpro
            it_abap     = gt_source
            iv_from     = <ls_result1>-start_line
            iv_to       = <ls_result1>-end_line
            iv_versno   = ls_vrsd-versno
          IMPORTING
            ev_addition = lv_addition.
        IF lv_addition = abap_true.
          lv_object_added = abap_true.
        ENDIF.
      ENDLOOP.
      IF lv_object_added = abap_true.
*       Add ALV header entry
        CLEAR gs_alv_header.
        gs_alv_header-repname = gv_report.
        gs_alv_header-dynnr   = gv_dynpro.
        gs_alv_header-versno  = ls_vrsd-versno.
        CLEAR: ls_vrsd, ls_address.

        SELECT SINGLE
          unam AS author
          udat AS datum
          INTO CORRESPONDING FIELDS OF ls_vrsd
          FROM  reposrc
          WHERE progname = gv_report.
        IF users[] IS NOT INITIAL.
          IF ls_vrsd-author NOT IN users.
            CONTINUE.
          ENDIF.
        ENDIF.

        CALL FUNCTION 'BAPI_USER_GET_DETAIL'
          EXPORTING
            username = ls_vrsd-author
          IMPORTING
            address  = ls_address
          TABLES
            return   = lt_return.

        CONCATENATE '('  ls_vrsd-author ':' ls_address-firstname ' ' ls_address-lastname ')' INTO  gs_alv_header-repdecr SEPARATED BY space.

        APPEND gs_alv_header TO gt_alv_header.

        IF gv_dynpro IS NOT INITIAL.
          gv_dynp_found = gc_x.
        ENDIF.

        IF ls_vrsd-versno IS NOT INITIAL.
          gv_vers_found = gc_x.
        ENDIF.
      ENDIF.

    ENDDO.

  ENDMETHOD.                    "search_source

  METHOD search_dynpro_source.

    DATA ls_dynpro LIKE LINE OF gt_dynpro.

    IF sy-batch IS INITIAL.
      CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
        EXPORTING
          text = 'SEARCH DYNPRO SOURCES...'.
    ENDIF.

    LOOP AT gt_dynpro INTO ls_dynpro.
      REFRESH gt_source.

      gv_report = ls_dynpro-repname.
      gv_dynpro = ls_dynpro-dynnr.

      gt_source = get_dynpro_flow_logic( iv_report =  ls_dynpro-repname
                                         iv_dynpro  = ls_dynpro-dynnr ).

      CHECK gt_source IS NOT INITIAL.

      search_source( ).

    ENDLOOP.

  ENDMETHOD.                    "search_dynpro_source

  METHOD get_dynpros.
    CHECK gt_object IS NOT INITIAL.

    SELECT prog dnum INTO TABLE gt_dynpro
      FROM d020s FOR ALL ENTRIES IN gt_object
      WHERE prog = gt_object-table_line
      AND   dnum IN dynnr.
  ENDMETHOD.                    "get_dynpros

  METHOD get_includes.
    DATA:
     lt_inc     TYPE STANDARD TABLE OF tadir-obj_name,
     lt_inc_tmp LIKE lt_inc,
     lv_program TYPE sy-repid,
     lv_old     TYPE xfeld.

    FIELD-SYMBOLS:
      <lv_obj> TYPE tadir-obj_name.

    CHECK inclu IS NOT INITIAL.

    LOOP AT gt_object ASSIGNING <lv_obj>
      WHERE table_line(2) <> 'CL'.    "for classes we already have the includes

      REFRESH lt_inc_tmp.

      IF lv_old IS NOT INITIAL.
        CALL FUNCTION 'GET_INCLUDES'
          EXPORTING
            progname = <lv_obj>
          TABLES
            incltab  = lt_inc_tmp.

      ELSE.
        lv_program = <lv_obj>.

        CALL FUNCTION 'RS_GET_ALL_INCLUDES'
          EXPORTING
            program      = lv_program
          TABLES
            includetab   = lt_inc_tmp
          EXCEPTIONS
            not_existent = 1
            no_program   = 2
            OTHERS       = 3.

        CHECK sy-subrc IS INITIAL.
      ENDIF.

      APPEND LINES OF lt_inc_tmp TO lt_inc.
    ENDLOOP.

    SORT lt_inc.
    DELETE ADJACENT DUPLICATES FROM lt_inc.

    APPEND LINES OF lt_inc TO gt_object.

  ENDMETHOD.                    "get_includes

  METHOD get_report_names.
    SELECT obj_name INTO TABLE gt_object
      FROM tadir
      WHERE pgmid  = 'R3TR'
      AND   object = 'PROG'
      AND   devclass IN devclass.                       "#EC CI_GENBUFF
  ENDMETHOD.                    "get_report_names

  METHOD get_function_names.
    DATA:
      lt_obj     TYPE STANDARD TABLE OF tadir-obj_name,
      lv_obj     TYPE tadir-obj_name,
      lv_fgroup  TYPE rs38l-area,
      lv_program TYPE progname.

    FIELD-SYMBOLS:
      <lv_obj> LIKE LINE OF lt_obj.

    SELECT obj_name INTO TABLE lt_obj
      FROM tadir
      WHERE pgmid  = 'R3TR'
      AND   object = 'FUGR'
      AND   devclass IN devclass
      AND   obj_name IN funcgrp.                        "#EC CI_GENBUFF

    LOOP AT lt_obj ASSIGNING <lv_obj>.
      lv_fgroup = <lv_obj>.
      CLEAR lv_program.

      CALL FUNCTION 'FUNCTION_INCLUDE_CONCATENATE'
        CHANGING
          program       = lv_program
          complete_area = lv_fgroup
        EXCEPTIONS
          OTHERS        = 1.

      CHECK sy-subrc IS INITIAL AND lv_program IS NOT INITIAL.

      lv_obj = lv_program.
      APPEND lv_obj TO gt_object.
    ENDLOOP.
  ENDMETHOD.                    "get_function_names

  METHOD get_class_names.
    DATA lt_obj TYPE STANDARD TABLE OF tadir-obj_name.
    DATA ls_obj TYPE tadir-obj_name.

    SELECT obj_name INTO TABLE lt_obj
      FROM tadir
      WHERE pgmid  = 'R3TR'
      AND   object = 'CLAS'
      AND   devclass IN devclass
      AND   obj_name IN p_class.                        "#EC CI_GENBUFF

    LOOP AT lt_obj INTO ls_obj.
      APPEND cl_oo_classname_service=>get_classpool_name( |{ ls_obj }| ) TO gt_object.
    ENDLOOP.

  ENDMETHOD.                    "get_class_names

  METHOD get_interface_names.
    DATA lt_obj TYPE STANDARD TABLE OF tadir-obj_name.
    DATA ls_obj TYPE tadir-obj_name.

    SELECT obj_name INTO TABLE lt_obj
      FROM tadir
      WHERE pgmid  = 'R3TR'
      AND   object = 'INTF'
      AND   devclass IN devclass
      AND   obj_name IN p_class.                        "#EC CI_GENBUFF

    LOOP AT lt_obj INTO ls_obj.
      APPEND cl_oo_classname_service=>get_interfacepool_name( |{ ls_obj }| ) TO gt_object.
    ENDLOOP.

  ENDMETHOD.                    "get_interface_names

  METHOD get_source_names.
    IF repname[] IS NOT INITIAL OR
       cnam[]    IS NOT INITIAL OR
       unam[]    IS NOT INITIAL OR
       subc[]    IS NOT INITIAL OR
       appl[]    IS NOT INITIAL.

      SELECT name APPENDING TABLE gt_object
        FROM trdir
        WHERE name IN repname
        AND   cnam IN cnam
        AND   unam IN unam
        AND   subc IN subc
        AND   appl IN appl.
    ENDIF.

    IF devclass[] IS NOT INITIAL.
      get_report_names( ).
      get_function_names( ).
      get_class_names( ).
      get_interface_names( ).
    ENDIF.

    IF funcgrp[] IS NOT INITIAL.
      get_function_names( ).
    ENDIF.

    IF p_class[] IS NOT INITIAL.
      get_class_names( ).
      get_interface_names( ).
    ENDIF.

    IF rb_code IS INITIAL.
      get_dynpros( ).
    ENDIF.

  ENDMETHOD.                    "get_source_names

  METHOD start.


    IF sy-batch IS INITIAL.
      CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
        EXPORTING
          text = 'GET INCLUDES...'.
    ENDIF.

    get_source_names( ).
    get_includes( ).
*    DELETE gt_object WHERE table_line  NP 'ZCL_IM_EIMP_SS_CDP_ALG_BADI===CM002*'." or
**                           table_line  NP 'ZCL_IM_EIMP_SS_CDP_APPLICATIONCM003*' or
**                           table_line  NP 'ZCL_SS_CDP_CONFIG_UTIL========CM00W*'.




    IF nmspace[] IS NOT INITIAL.
      DELETE gt_object WHERE table_line  IN nmspace.
*    delete gt_object WHERE TABLE_LINE cp '*CP'.
    ENDIF.

    IF rb_dyn IS INITIAL.
      TRY.
          search_abap_source( ).
        CATCH lcx_scan_exceptions.
          RETURN.
      ENDTRY.
    ENDIF.

    IF rb_code IS INITIAL.
      TRY.
          search_dynpro_source( ).
        CATCH lcx_scan_exceptions.
          RETURN.
      ENDTRY.
    ENDIF.

    display( ).
  ENDMETHOD.                    "start

  METHOD pbo.
    DATA ls_screen TYPE screen.

    LOOP AT screen INTO ls_screen.
      CHECK ls_screen-group1 = 'DSP'.
      ls_screen-input = '0'.
      MODIFY screen FROM ls_screen.
    ENDLOOP.
  ENDMETHOD.                    "pbo
ENDCLASS.                    "lcl_source_scan IMPLEMENTATION

INITIALIZATION.
  CREATE OBJECT lo_sscan.

  a01 = 'Object Selection'(a01).
  a10 = 'Report/Dynpro Selektion'(a10).
  a11 = 'Paket Selektion'(a11).
  a12 = 'Funktionsgruppen Selektion'(a12).
  a13 = 'Klassen Selektion'(a13).
  a20 = 'Suchkriterien'(a20).
  a30 = 'Suchbereich'(a30).

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_class-low.
  lo_sscan->f4_class( CHANGING cv_class_name = p_class-low ).

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_class-high.
  lo_sscan->f4_class( CHANGING cv_class_name = p_class-high ).

AT SELECTION-SCREEN ON VALUE-REQUEST FOR funcgrp-low.
  lo_sscan->f4_function_group( 'FUNCGRP-LOW' ).

AT SELECTION-SCREEN ON VALUE-REQUEST FOR funcgrp-high.
  lo_sscan->f4_function_group( 'FUNCGRP-HIGH' ).

AT SELECTION-SCREEN ON VALUE-REQUEST FOR repname-low.
  lo_sscan->f4_repname( CHANGING cv_repname = repname-low ).

AT SELECTION-SCREEN ON VALUE-REQUEST FOR repname-high.
  lo_sscan->f4_repname( CHANGING cv_repname = repname-high ).

AT SELECTION-SCREEN OUTPUT.
  lo_sscan->pbo( ).

START-OF-SELECTION.
  lo_sscan->start( ).