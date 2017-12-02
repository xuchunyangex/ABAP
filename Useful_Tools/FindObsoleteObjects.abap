*&---------------------------------------------------------------------*
*& Report  ZOBSOLETEOBJECTS
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  yobsoleteobjects_new.

INCLUDE YOBSOLETEOBJECTS_TOP_NEW.
*INCLUDE zobsoleteobjects_top_new.
INCLUDE YOBSOLETEOBJECTS_SEL_NEW.
*INCLUDE zobsoleteobjects_sel_new.
INCLUDE YOBSOLETEOBJECTS_EVTS_NEW.
*INCLUDE zobsoleteobjects_evts_new.
INCLUDE YOBSOLETEOBJECTS_LOGIC_NEW.
*INCLUDE zobsoleteobjects_logic_new.
INCLUDE YOBSOLETEOBJECTS_DISP_NEW.
*INCLUDE zobsoleteobjects_disp_new.

*&---------------------------------------------------------------------*
*&  Include           ZOBSOLETEOBJECTS_TOP
*&---------------------------------------------------------------------*

TABLES sscrfields.

TYPES:BEGIN OF ty_result,
        object_name        TYPE tadir-obj_name,
        parent_object_name TYPE char40,
        object_type        TYPE char20,
        changed_on         TYPE aedat,
        changed_by         TYPE char12,
      END OF ty_result,

      BEGIN OF ty_class_name,
         clsname TYPE seoclsname,
      END OF ty_class_name,

      BEGIN OF ty_clsobjname,
         name TYPE eu_lname,
      END OF ty_clsobjname.

DATA:gv_ucomm             TYPE sscrfields-ucomm,
     gv_package_name      TYPE devclass,
     gv_prev_package_name TYPE devclass,

     gt_fm                  TYPE TABLE OF ty_result,
     gt_include             TYPE TABLE OF ty_result,
     gt_class_method        TYPE TABLE OF ty_result,
     gt_class_attributes    TYPE TABLE OF ty_result,
     gt_class_constants     TYPE TABLE OF ty_result,
     gt_interface_constants TYPE TABLE OF ty_result,
     gt_int_method          TYPE TABLE OF ty_result,
     gt_domain              TYPE TABLE OF ty_result,
     gt_data_element        TYPE TABLE OF ty_result,
     gt_table_type          TYPE TABLE OF ty_result,
     gt_tab_stru            TYPE TABLE OF ty_result,
     gt_view                TYPE TABLE OF ty_result,

      gt_result        TYPE TABLE OF ty_result.


*&---------------------------------------------------------------------*
*&  Include           ZOBSOLETEOBJECTS_SEL
*&---------------------------------------------------------------------*

SELECTION-SCREEN: BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
PARAMETERS:   p_pckg    TYPE devclass.
SELECTION-SCREEN: END OF BLOCK b1.

SELECTION-SCREEN: BEGIN OF BLOCK b2 WITH FRAME TITLE text-002.
PARAMETERS:   p_func    AS CHECKBOX MODIF ID all DEFAULT 'X',
              p_incl    AS CHECKBOX MODIF ID all,
              p_clsmet  AS CHECKBOX MODIF ID all,
              p_intmet  AS CHECKBOX MODIF ID all,
              p_intcst  AS CHECKBOX MODIF ID all,
              p_clsatt  AS CHECKBOX MODIF ID all,
*              p_clscst  AS CHECKBOX MODIF ID all,
              p_ddic    AS CHECKBOX USER-COMMAND ddic MODIF ID all.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 7(20) FOR FIELD p_doma.
SELECTION-SCREEN POSITION 5.
PARAMETERS    p_doma    AS CHECKBOX MODIF ID all .
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 7(20) FOR FIELD p_dele.
SELECTION-SCREEN POSITION 5.
PARAMETERS    p_dele    AS CHECKBOX MODIF ID all.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 7(20) FOR FIELD p_ttyp.
SELECTION-SCREEN POSITION 5.
PARAMETERS    p_ttyp    AS CHECKBOX MODIF ID all.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 7(20) FOR FIELD p_tab_st.
SELECTION-SCREEN POSITION 5.
PARAMETERS    p_tab_st  AS CHECKBOX MODIF ID all.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 7(20) FOR FIELD p_view.
SELECTION-SCREEN POSITION 5.
PARAMETERS    p_view    AS CHECKBOX MODIF ID all.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN: END OF BLOCK b2.

SELECTION-SCREEN: BEGIN OF BLOCK b3.
PARAMETERS: p_all     AS CHECKBOX USER-COMMAND all.
SELECTION-SCREEN: END OF BLOCK b3.

*&---------------------------------------------------------------------*
*&  Include           ZOBSOLETEOBJECTS_EVTS
*&---------------------------------------------------------------------*

AT SELECTION-SCREEN.
  gv_ucomm = sscrfields-ucomm.
  IF p_pckg IS INITIAL.
*--> raise error message.
  ENDIF.

  IF p_func IS INITIAL AND p_incl IS INITIAL AND p_clsmet IS INITIAL AND p_intmet IS INITIAL AND p_ddic IS INITIAL
     AND p_doma IS INITIAL AND p_dele IS INITIAL AND p_ttyp IS INITIAL AND p_tab_st IS INITIAL AND p_view IS  INITIAL.
*--> raise error message.
  ENDIF.

  gv_package_name = p_pckg.

AT SELECTION-SCREEN OUTPUT.
  CASE gv_ucomm.

    WHEN 'DDIC'.
      IF p_ddic   = abap_true.
        p_doma    = abap_true.
        p_dele    = abap_true.
        p_ttyp    = abap_true.
        p_tab_st  = abap_true.
        p_view    = abap_true.
      ELSE.
        p_doma    = space.
        p_dele    = space.
        p_ttyp    = space.
        p_tab_st  = space.
        p_view    = space.
      ENDIF.

    WHEN 'ALL'.
      IF p_all = abap_true.
        p_func    = abap_true.
        p_incl    = abap_true.
        p_clsmet  = abap_true.
        p_intmet  = abap_true.
        p_ddic    = abap_true.
        p_doma    = abap_true.
        p_dele    = abap_true.
        p_ttyp    = abap_true.
        p_tab_st  = abap_true.
        p_view    = abap_true.

        LOOP AT SCREEN.
          IF screen-group1 = 'ALL'.
            screen-input = 0.
            MODIFY SCREEN.
          ENDIF.
        ENDLOOP.
      ELSE.
        p_func    = space.
        p_incl    = space.
        p_clsmet  = space.
        p_intmet  = space.
        p_ddic    = space.
        p_doma    = space.
        p_dele    = space.
        p_ttyp    = space.
        p_tab_st  = space.
        p_view    = space.
        LOOP AT SCREEN.
          IF screen-group1 = 'ALL'.
            screen-input = 1.
            MODIFY SCREEN.
          ENDIF.
        ENDLOOP.
      ENDIF.

  ENDCASE.

START-OF-SELECTION.

  PERFORM get_data.
  PERFORM display_data.

*----------------------------------------------------------------------*
***INCLUDE ZOBSOLETEOBJECTS_LOGIC .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  GET_DATA
*&---------------------------------------------------------------------*
*text
*----------------------------------------------------------------------*
*-->  p1        text
*<--  p2        text
*----------------------------------------------------------------------*
FORM get_data .

  IF gv_prev_package_name = gv_package_name.
    CLEAR gt_result.
  ELSE.
    gv_prev_package_name = gv_package_name.
    CLEAR: gt_result, gt_fm, gt_class_method, gt_int_method, gt_include, gt_domain, gt_data_element, gt_tab_stru, gt_table_type, gt_view.
  ENDIF.

  IF p_func = abap_true.
    PERFORM get_fm.
    INSERT LINES OF gt_fm INTO TABLE gt_result.
  ENDIF.

  IF p_incl = abap_true.
    PERFORM get_includes.
    INSERT LINES OF gt_include INTO TABLE gt_result.
  ENDIF.

  IF p_clsmet = abap_true.
    PERFORM get_class_meth.
    INSERT LINES OF gt_class_method INTO TABLE gt_result.
  ENDIF.

*** Class attributes
  IF p_clsatt = abap_true.
    PERFORM get_class_attributes.
    INSERT LINES OF gt_class_attributes INTO TABLE gt_result.
  ENDIF.

*** Interface Constants
  IF p_intcst = abap_true.
    PERFORM get_interface_constants.
    INSERT LINES OF gt_interface_constants INTO TABLE gt_result.
  ENDIF.


  IF p_intmet = abap_true.
    PERFORM get_intf_meth.
    INSERT LINES OF gt_int_method INTO TABLE gt_result.
  ENDIF.

  IF p_doma = abap_true.
    PERFORM get_domain.
    INSERT LINES OF gt_domain INTO TABLE gt_result.
  ENDIF.

  IF p_dele = abap_true.
    PERFORM get_data_elements.
    INSERT LINES OF gt_data_element INTO TABLE gt_result.
  ENDIF.

  IF p_tab_st = abap_true.
    PERFORM get_table.
    INSERT LINES OF gt_tab_stru INTO TABLE gt_result.
  ENDIF.

  IF p_view = abap_true.
    PERFORM get_view.
    INSERT LINES OF gt_view INTO TABLE gt_result.
  ENDIF.

  IF p_ttyp = abap_true.
    PERFORM get_tabletype.
    INSERT LINES OF gt_table_type INTO TABLE gt_result.
  ENDIF.

ENDFORM.                    " GET_DATA
*&---------------------------------------------------------------------*
*&      Form  GET_FM
*&---------------------------------------------------------------------*
*text
*----------------------------------------------------------------------*
*-->  p1        text
*<--  p2        text
*----------------------------------------------------------------------*
FORM get_fm .

  DATA:lv_str1            TYPE string,
       lv_str2            TYPE string,
       lv_str3            TYPE string,
       lv_pname           TYPE devclass,
       lv_name            TYPE sobj_name,

       ls_fdir            TYPE tfdir,
       ls_result          TYPE ty_result,
       ls_fm              TYPE trdir,

       lt_fdir            TYPE TABLE OF tfdir,
       lt_package_content TYPE TABLE OF tadir,
       lt_fm              TYPE akb_except_type.

  FIELD-SYMBOLS: <fs_package_content>     TYPE tadir.

  IF gt_fm IS NOT INITIAL.
    RETURN.
  ENDIF.

  SELECT * FROM tadir INTO TABLE lt_package_content WHERE devclass = gv_package_name AND object = 'FUGR'.
  IF sy-subrc = 0.
    LOOP AT lt_package_content ASSIGNING <fs_package_content>.
      lv_pname = <fs_package_content>-obj_name.
      SEARCH lv_pname FOR '/'.
      IF sy-subrc = 0.
        SPLIT lv_pname AT '/' INTO lv_str1 lv_str2 lv_str3.
        CLEAR lv_pname.
        CONCATENATE lv_str1 '/' lv_str2 '/SAPL' lv_str3 INTO lv_pname.
        <fs_package_content>-obj_name = lv_pname.
      ELSE.
        CONCATENATE 'SAPL' lv_pname INTO lv_pname.
        <fs_package_content>-obj_name = lv_pname.
      ENDIF.
    ENDLOOP.

    SELECT * FROM tfdir INTO TABLE lt_fdir FOR ALL ENTRIES IN lt_package_content WHERE pname = lt_package_content-obj_name.
    IF sy-subrc = 0.
      LOOP AT lt_fdir INTO ls_fdir.
*        lv_name = ls_fdir-funcname.
*        CALL FUNCTION 'YAKB_WHERE_USED_LIST'
*          EXPORTING
*            obj_type   = 'FUNC'
*            obj_name   = lv_name
*          IMPORTING
*            references = lt_fm.

        CALL FUNCTION 'CRMOST_WHERE_USED_LIST'  "note
          EXPORTING
            obj_type   = 'FUNC'
            obj_name   = lv_name
          IMPORTING
            references = lt_fm.


        IF lt_fm IS INITIAL.
          CLEAR: lv_str1, lv_str2, lv_str3.
          ls_result-object_name        = ls_fdir-funcname.
          ls_result-parent_object_name = ls_fdir-pname.
          SPLIT ls_fdir-pname AT 'SAP' INTO lv_str1 lv_str2 lv_str3.
          CONCATENATE lv_str1 lv_str2 lv_str3 'U' ls_fdir-include INTO lv_str1.
          SELECT SINGLE * FROM trdir INTO ls_fm WHERE name = lv_str1.
          ls_result-changed_by  = ls_fm-unam.
          ls_result-changed_on  = ls_fm-udat.
          ls_result-object_type = 'Function module'.
          INSERT ls_result INTO TABLE gt_fm.
        ENDIF.
      ENDLOOP.
    ENDIF.

  ENDIF.

ENDFORM.                    " GET_FM

*&---------------------------------------------------------------------*
*&      Form  GET_INCLUDES
*&---------------------------------------------------------------------*
*text
*----------------------------------------------------------------------*
*-->  p1        text
*<--  p2        text
*----------------------------------------------------------------------*
FORM get_includes .
  DATA: lv_name            TYPE sobj_name,

        ls_package_content TYPE tadir,
        ls_include         TYPE trdir,
        ls_result          TYPE ty_result,

        lt_include         TYPE TABLE OF trdir,
        lt_package_content TYPE TABLE OF tadir,
        lt_result          TYPE akb_except_type.

  IF gt_include IS NOT INITIAL.
    RETURN.
  ENDIF.

  SELECT * FROM tadir INTO TABLE lt_package_content WHERE devclass = gv_package_name AND object = 'PROG'.
  IF sy-subrc = 0.
    SELECT * FROM trdir INTO TABLE lt_include FOR ALL ENTRIES IN lt_package_content WHERE name = lt_package_content-obj_name AND subc = 'I'.
    LOOP AT lt_include INTO ls_include.
      lv_name = ls_include-name.
*      CALL FUNCTION 'AKB_WHERE_USED_LIST'
*        EXPORTING
*          obj_type   = 'INCL'
*          obj_name   = lv_name
*        IMPORTING
*          references = lt_result.

      CALL FUNCTION 'CRMOST_WHERE_USED_LIST'  "note
        EXPORTING
          obj_type   = 'INCL'
          obj_name   = lv_name
        IMPORTING
          references = lt_result.

      IF lt_result IS INITIAL.
        ls_result-object_name = ls_include-name.
        ls_result-object_type = 'Include Program'.
        ls_result-changed_on  = ls_include-udat.
        ls_result-changed_by  = ls_include-unam.
        INSERT ls_result INTO TABLE gt_include.
      ENDIF.
    ENDLOOP.
  ELSE.
* WRITE: 'No includes found'.
  ENDIF.

ENDFORM.                    " GET_INCLUDES

*&---------------------------------------------------------------------*
*&      Form  GET_CLASS_METH
*&---------------------------------------------------------------------*
*text
*----------------------------------------------------------------------*
*-->  p1        text
*<--  p2        text
*----------------------------------------------------------------------*
FORM get_class_meth.
  DATA:lv_object_name     TYPE char4,
       lv_class_name      TYPE seoclsname,
       lv_method_name     TYPE seocmpname,

       ls_package_content TYPE tadir,
       ls_class_name      TYPE ty_class_name,
       ls_class_methods   TYPE vseomethod,
       ls_clsobjname      TYPE ty_clsobjname,
       ls_wbcrossgt       TYPE wbcrossgt,
       ls_result          TYPE ty_result,

       lt_class_name      TYPE TABLE OF ty_class_name,
       lt_package_content TYPE TABLE OF tadir,
       lt_class_methods   TYPE TABLE OF vseomethod,
       lt_clsobjname      TYPE TABLE OF ty_clsobjname,
       lt_wbcrossgt       TYPE TABLE OF wbcrossgt,
       lt_result          TYPE akb_except_type.

  lv_object_name = 'CLAS'.
  IF gt_class_method IS NOT INITIAL.
    RETURN.
  ENDIF.

  SELECT * FROM tadir INTO TABLE lt_package_content WHERE devclass = gv_package_name AND object = lv_object_name .
  IF sy-subrc = 0.
    LOOP AT lt_package_content INTO ls_package_content.
      ls_class_name-clsname = ls_package_content-obj_name.
      INSERT ls_class_name INTO TABLE lt_class_name.
    ENDLOOP.

*   ==> Get all the class methods
    SELECT * FROM vseomethod INTO TABLE lt_class_methods FOR ALL ENTRIES IN lt_class_name WHERE clsname  = lt_class_name-clsname
                                                                                          AND refclsname = space
                                                                                          AND refcmpname = space.
    IF sy-subrc = 0.
*     ==> Delete constructor/destructor methods
      DELETE lt_class_methods WHERE mtdtype = '2' OR mtdtype = '3'.

*     ==> Get class method name in the required format
      LOOP AT lt_class_methods INTO ls_class_methods.
        CLEAR ls_clsobjname.
        CONCATENATE ls_class_methods-clsname '\ME:' ls_class_methods-cmpname INTO ls_clsobjname-name.
        INSERT ls_clsobjname INTO TABLE lt_clsobjname.
      ENDLOOP.

*     ==> Do where-use on all the methods
      IF lt_clsobjname IS NOT INITIAL.
        SELECT * FROM wbcrossgt INTO TABLE lt_wbcrossgt FOR ALL ENTRIES IN lt_clsobjname WHERE name =  lt_clsobjname-name.
        DELETE ADJACENT DUPLICATES FROM lt_wbcrossgt COMPARING name.
      ENDIF.

*     ==> Get class method and name. Delete the methods which have been used
      LOOP AT lt_wbcrossgt INTO ls_wbcrossgt.
        SPLIT ls_wbcrossgt-name AT '\ME:' INTO lv_class_name lv_method_name.
        DELETE lt_class_methods WHERE clsname = lv_class_name AND cmpname = lv_method_name.
      ENDLOOP.

      LOOP AT lt_class_methods INTO ls_class_methods.
        ls_result-object_name        = ls_class_methods-cmpname.
        ls_result-parent_object_name = ls_class_methods-clsname.
        ls_result-changed_by         = ls_class_methods-changedby.
        ls_result-changed_on         = ls_class_methods-changedon.

        IF lv_object_name = 'CLAS'.
          ls_result-object_type = 'Class Method'.
          INSERT ls_result INTO TABLE gt_class_method.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDIF.

ENDFORM.                    " GET_CLASS_METH


*&---------------------------------------------------------------------*
*&      Form  get_intf_meth
*&---------------------------------------------------------------------*
*text
*----------------------------------------------------------------------*
FORM get_intf_meth.

  DATA:lv_object_name     TYPE char4,
       lv_class_name      TYPE seoclsname,
       lv_method_name     TYPE seocmpname,

       ls_package_content TYPE tadir,
       ls_class_name      TYPE ty_class_name,
       ls_class_methods   TYPE vseomethod,
       ls_clsobjname      TYPE ty_clsobjname,
       ls_wbcrossgt       TYPE wbcrossgt,
       ls_result          TYPE ty_result,

       lt_class_name      TYPE TABLE OF ty_class_name,
       lt_package_content TYPE TABLE OF tadir,
       lt_class_methods   TYPE TABLE OF vseomethod,
       lt_clsobjname      TYPE TABLE OF ty_clsobjname,
       lt_wbcrossgt       TYPE TABLE OF wbcrossgt,
       lt_result          TYPE akb_except_type.

  lv_object_name = 'INTF'.
  IF gt_int_method IS NOT INITIAL.
    RETURN.
  ENDIF.

  SELECT * FROM tadir INTO TABLE lt_package_content WHERE devclass = gv_package_name AND object = lv_object_name .
  IF sy-subrc = 0.
    LOOP AT lt_package_content INTO ls_package_content.
      ls_class_name-clsname = ls_package_content-obj_name.
      INSERT ls_class_name INTO TABLE lt_class_name.
    ENDLOOP.

*   ==> Get all the class methods
    SELECT * FROM vseomethod INTO TABLE lt_class_methods FOR ALL ENTRIES IN lt_class_name WHERE clsname  = lt_class_name-clsname
                                                                                          AND refclsname = space
                                                                                          AND refcmpname = space.
    IF sy-subrc = 0.
*     ==> Delete constructor/destructor methods
      DELETE lt_class_methods WHERE mtdtype = '2' OR mtdtype = '3'.

*     ==> Get class method name in the required format
      LOOP AT lt_class_methods INTO ls_class_methods.
        CLEAR ls_clsobjname.
        CONCATENATE ls_class_methods-clsname '\ME:' ls_class_methods-cmpname INTO ls_clsobjname-name.
        INSERT ls_clsobjname INTO TABLE lt_clsobjname.
      ENDLOOP.

*     ==> Do where-use on all the methods
      IF lt_clsobjname IS NOT INITIAL.
        SELECT * FROM wbcrossgt INTO TABLE lt_wbcrossgt FOR ALL ENTRIES IN lt_clsobjname WHERE name =  lt_clsobjname-name.
        DELETE ADJACENT DUPLICATES FROM lt_wbcrossgt COMPARING name.
      ENDIF.

*     ==> Get class method and name. Delete the methods which have been used
      LOOP AT lt_wbcrossgt INTO ls_wbcrossgt.
        SPLIT ls_wbcrossgt-name AT '\ME:' INTO lv_class_name lv_method_name.
        DELETE lt_class_methods WHERE clsname = lv_class_name AND cmpname = lv_method_name.
      ENDLOOP.

      LOOP AT lt_class_methods INTO ls_class_methods.
        ls_result-object_name        = ls_class_methods-cmpname.
        ls_result-parent_object_name = ls_class_methods-clsname.
        ls_result-changed_by         = ls_class_methods-changedby.
        ls_result-changed_on         = ls_class_methods-changedon.

        ls_result-object_type = 'Interface Method'.
        INSERT ls_result INTO TABLE gt_int_method.
      ENDLOOP.
    ENDIF.
  ENDIF.

ENDFORM.                    "get_intf_meth

*&---------------------------------------------------------------------*
*&      Form  get_domain
*&---------------------------------------------------------------------*
*text
*----------------------------------------------------------------------*
FORM get_domain .
  DATA:lv_name            TYPE sobj_name,
       lv_objecttype      TYPE seu_obj,

       ls_package_content TYPE tadir,
       ls_result          TYPE ty_result,
       ls_doma            TYPE dd01l,

       lt_domain          TYPE TABLE OF trdir,
       lt_package_content TYPE TABLE OF tadir,
       lt_objects         TYPE TABLE OF rsfind.

  IF gt_domain IS NOT INITIAL.
    RETURN.
  ENDIF.

  SELECT * FROM tadir INTO TABLE lt_package_content WHERE devclass = gv_package_name AND object = 'DOMA'.
  IF sy-subrc = 0.

    LOOP AT lt_package_content INTO ls_package_content.
      REFRESH lt_objects.
      lv_name = ls_package_content-obj_name.
      APPEND lv_name TO lt_objects.
      lv_objecttype = 'DOMA'.

      CALL FUNCTION 'RS_EU_CROSSREF'
        EXPORTING
          i_find_obj_cls               = lv_objecttype
          i_answer                     = ' '
          no_dialog                    = 'X'
          expand_source_in_batch_mode  = ' '
          expand_source_in_online_mode = ' '
        TABLES
          i_findstrings                = lt_objects
        EXCEPTIONS
          not_executed                 = 1
          not_found                    = 2
          illegal_object               = 3
          no_cross_for_this_object     = 4
          batch                        = 5
          batchjob_error               = 6
          wrong_type                   = 7
          object_not_exist             = 8
          OTHERS                       = 9.
      IF sy-subrc <> 0 AND sy-subrc <> 8.
        ls_result-object_name = lv_name.
        ls_result-object_type = 'Domain'.
        SELECT SINGLE * FROM dd01l INTO ls_doma WHERE domname = lv_name.
        IF sy-subrc = 0.
          ls_result-changed_by = ls_doma-as4user.
          ls_result-changed_on = ls_doma-as4date.
        ENDIF.
        INSERT ls_result INTO TABLE gt_domain.
      ENDIF.
    ENDLOOP.
  ENDIF.

ENDFORM.                    " GET_DOMAIN

*&---------------------------------------------------------------------*
*&      Form  GET_DATA_ELEMENTS
*&---------------------------------------------------------------------*
*text
*----------------------------------------------------------------------*
*-->  p1        text
*<--  p2        text
*----------------------------------------------------------------------*
FORM get_data_elements .
  DATA:lv_name            TYPE sobj_name,
       lv_objecttype      TYPE seu_obj,

       ls_package_content TYPE tadir,
       ls_result          TYPE ty_result,
       ls_dele            TYPE dd04l,

       lt_domain          TYPE TABLE OF trdir,
       lt_package_content TYPE TABLE OF tadir,
       lt_objects         TYPE TABLE OF rsfind.

  IF gt_data_element IS NOT INITIAL.
    RETURN.
  ENDIF.

  SELECT * FROM tadir INTO TABLE lt_package_content WHERE devclass = gv_package_name AND object = 'DTEL'.
  IF sy-subrc = 0.

    LOOP AT lt_package_content INTO ls_package_content.
      REFRESH lt_objects.
      lv_name = ls_package_content-obj_name.
      APPEND lv_name TO lt_objects.
      lv_objecttype = 'DTEL'.

      CALL FUNCTION 'RS_EU_CROSSREF'
        EXPORTING
          i_find_obj_cls               = lv_objecttype
          i_answer                     = ' '
          no_dialog                    = 'X'
          expand_source_in_batch_mode  = ' '
          expand_source_in_online_mode = ' '
        TABLES
          i_findstrings                = lt_objects
        EXCEPTIONS
          not_executed                 = 1
          not_found                    = 2
          illegal_object               = 3
          no_cross_for_this_object     = 4
          batch                        = 5
          batchjob_error               = 6
          wrong_type                   = 7
          object_not_exist             = 8
          OTHERS                       = 9.
      IF sy-subrc <> 0 AND sy-subrc <> 8.
        ls_result-object_name = lv_name.
        ls_result-object_type = 'Data Element'.
        SELECT SINGLE * FROM dd04l INTO ls_dele WHERE rollname = lv_name.
        IF sy-subrc = 0.
          ls_result-changed_by = ls_dele-as4user.
          ls_result-changed_on = ls_dele-as4date.
        ENDIF.
        INSERT ls_result INTO TABLE gt_data_element.
      ENDIF.
    ENDLOOP.
  ENDIF.

ENDFORM.                    " GET_DELEMENTS

*&---------------------------------------------------------------------*
*&      Form  GET_TABLE
*&---------------------------------------------------------------------*
*text
*----------------------------------------------------------------------*
*-->  p1        text
*<--  p2        text
*----------------------------------------------------------------------*
FORM get_table .

*  TYPES: BEGIN OF ty_grid,
*          name TYPE strukname,
*         END OF ty_grid.

  DATA:lv_name            TYPE sobj_name,
       lv_objecttype      TYPE seu_obj,

       ls_package_content TYPE tadir,
       ls_result          TYPE ty_result,
       ls_tab             TYPE dd02l,

       lt_domain          TYPE TABLE OF trdir,
       lt_package_content TYPE TABLE OF tadir,
       lt_objects         TYPE TABLE OF rsfind.
*          lt_grid               TYPE TABLE OF /castp/t_grid,
*          lt_grid_name          TYPE TABLE OF ty_grid,
*          lwa_grid              TYPE /castp/t_grid,
*          lwa_grid_name         TYPE ty_grid.

  IF gt_tab_stru IS NOT INITIAL.
    RETURN.
  ENDIF.

  SELECT * FROM tadir INTO TABLE lt_package_content WHERE devclass = gv_package_name AND object = 'TABL'.
  IF sy-subrc = 0.
    LOOP AT lt_package_content INTO ls_package_content.
      REFRESH lt_objects.
      lv_name = ls_package_content-obj_name.
      APPEND lv_name TO lt_objects.
      lv_objecttype = 'TABL'.

      CALL FUNCTION 'RS_EU_CROSSREF'
        EXPORTING
          i_find_obj_cls               = lv_objecttype
          i_answer                     = ' '
          no_dialog                    = 'X'
          expand_source_in_batch_mode  = ' '
          expand_source_in_online_mode = ' '
        TABLES
          i_findstrings                = lt_objects
        EXCEPTIONS
          not_executed                 = 1
          not_found                    = 2
          illegal_object               = 3
          no_cross_for_this_object     = 4
          batch                        = 5
          batchjob_error               = 6
          wrong_type                   = 7
          object_not_exist             = 8
          OTHERS                       = 9.
      IF sy-subrc <> 0 AND sy-subrc <> 8.
        ls_result-object_name = lv_name.
        ls_result-object_type = 'Table/Structure'.
        SELECT SINGLE * FROM dd02l INTO ls_tab WHERE tabname = lv_name.
        IF sy-subrc = 0.
          ls_result-changed_by = ls_tab-as4user.
          ls_result-changed_on = ls_tab-as4date.
        ENDIF.
        INSERT ls_result INTO TABLE gt_tab_stru.
*     lwa_grid_name-name = lv_name.
*     INSERT lwa_grid_name INTO TABLE lt_grid_name.
      ENDIF.
    ENDLOOP.
* SELECT * FROM /castp/t_grid INTO TABLE lt_grid FOR ALL ENTRIES IN lt_grid_name WHERE struct_name = lt_grid_name-name.
* IF sy-subrc = 0.
* LOOP AT lt_grid INTO lwa_grid.
* lv_name = lwa_grid-struct_name.
* READ TABLE t_no TRANSPORTING NO FIELDS WITH KEY name = lv_name.
* IF sy-subrc = 0.
* DELETE t_no INDEX sy-tabix.
* ENDIF.
* ENDLOOP.
* ENDIF.
  ENDIF.
ENDFORM.                    " GET_TABLE
*&---------------------------------------------------------------------*
*&      Form  GET_VIEW
*&---------------------------------------------------------------------*
*text
*----------------------------------------------------------------------*
*-->  p1        text
*<--  p2        text
*----------------------------------------------------------------------*
FORM get_view .
  DATA:lv_name            TYPE sobj_name,
       lv_objecttype      TYPE seu_obj,

       ls_package_content TYPE tadir,
       ls_result          TYPE ty_result,
       ls_view            TYPE dd02l,

       lt_domain          TYPE TABLE OF trdir,
       lt_package_content TYPE TABLE OF tadir,
       lt_objects         TYPE TABLE OF rsfind.

  IF gt_view IS NOT INITIAL.
    RETURN.
  ENDIF.

  SELECT * FROM tadir INTO TABLE lt_package_content WHERE devclass = gv_package_name AND object = 'VIEW'.
  IF sy-subrc = 0.
    LOOP AT lt_package_content INTO ls_package_content.
      REFRESH lt_objects.
      lv_name = ls_package_content-obj_name.
      APPEND lv_name TO lt_objects.
      lv_objecttype = 'VIEW'.

      CALL FUNCTION 'RS_EU_CROSSREF'
        EXPORTING
          i_find_obj_cls               = lv_objecttype
          i_answer                     = ' '
          no_dialog                    = 'X'
          expand_source_in_batch_mode  = ' '
          expand_source_in_online_mode = ' '
        TABLES
          i_findstrings                = lt_objects
        EXCEPTIONS
          not_executed                 = 1
          not_found                    = 2
          illegal_object               = 3
          no_cross_for_this_object     = 4
          batch                        = 5
          batchjob_error               = 6
          wrong_type                   = 7
          object_not_exist             = 8
          OTHERS                       = 9.
      IF sy-subrc <> 0 AND sy-subrc <> 8.
        ls_result-object_name = lv_name.
        ls_result-object_type = 'View'.
        SELECT SINGLE * FROM dd02l INTO ls_view WHERE tabname = lv_name.
        IF sy-subrc = 0.
          ls_result-changed_by = ls_view-as4user.
          ls_result-changed_on = ls_view-as4date.
          INSERT ls_result INTO TABLE gt_view.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDIF.
ENDFORM.                    " GET_VIEW
*&---------------------------------------------------------------------*
*&      Form  GET_TABLETYPE
*&---------------------------------------------------------------------*
*text
*----------------------------------------------------------------------*
*-->  p1        text
*<--  p2        text
*----------------------------------------------------------------------*
FORM get_tabletype .
  DATA:lv_name            TYPE sobj_name,
       lv_objecttype      TYPE seu_obj,

       ls_package_content TYPE tadir,
       ls_result          TYPE ty_result,
       ls_ttype           TYPE dd40l,

       lt_domain          TYPE TABLE OF trdir,
       lt_package_content TYPE TABLE OF tadir,
       lt_objects         TYPE TABLE OF rsfind.

  IF gt_table_type IS NOT INITIAL.
    RETURN.
  ENDIF.

  SELECT * FROM tadir INTO TABLE lt_package_content WHERE devclass = gv_package_name AND object = 'TTYP'.
  IF sy-subrc = 0.
    LOOP AT lt_package_content INTO ls_package_content.
      REFRESH lt_objects.
      lv_name = ls_package_content-obj_name.
      APPEND lv_name TO lt_objects.
      lv_objecttype = 'TTYP'.

      CALL FUNCTION 'RS_EU_CROSSREF'
        EXPORTING
          i_find_obj_cls               = lv_objecttype
          i_answer                     = ' '
          no_dialog                    = 'X'
          expand_source_in_batch_mode  = ' '
          expand_source_in_online_mode = ' '
        TABLES
          i_findstrings                = lt_objects
        EXCEPTIONS
          not_executed                 = 1
          not_found                    = 2
          illegal_object               = 3
          no_cross_for_this_object     = 4
          batch                        = 5
          batchjob_error               = 6
          wrong_type                   = 7
          object_not_exist             = 8
          OTHERS                       = 9.
      IF sy-subrc <> 0 AND sy-subrc <> 8.
        ls_result-object_name = lv_name.
        ls_result-object_type = 'Table Type'.
        SELECT SINGLE * FROM dd40l INTO ls_ttype WHERE typename = lv_name.
        IF sy-subrc = 0.
          ls_result-changed_by = ls_ttype-as4user.
          ls_result-changed_on = ls_ttype-as4date.
        ENDIF.
        INSERT ls_result INTO TABLE gt_table_type.
      ENDIF.
    ENDLOOP.
  ENDIF.
ENDFORM.                    " GET_TABLETYPE
*&---------------------------------------------------------------------*
*&      Form  GET_CLASS_ATTRIBUTES
*&---------------------------------------------------------------------*
*text
*----------------------------------------------------------------------*
*-->  p1        text
*<--  p2        text
*----------------------------------------------------------------------*
FORM get_class_attributes .

  DATA:lv_object_name     TYPE char4,
       lv_class_name      TYPE seoclsname,
       lv_method_name     TYPE seocmpname,

       ls_package_content TYPE tadir,
       ls_class_name      TYPE ty_class_name,
       ls_class_methods   TYPE vseomethod,
       ls_clsobjname      TYPE ty_clsobjname,
       ls_wbcrossgt       TYPE wbcrossgt,
       ls_result          TYPE ty_result,

       lt_class_name      TYPE TABLE OF ty_class_name,
       lt_package_content TYPE TABLE OF tadir,
       lt_class_methods   TYPE TABLE OF vseomethod,
       lt_clsobjname      TYPE TABLE OF ty_clsobjname,
       lt_wbcrossgt       TYPE TABLE OF wbcrossgt,
       lt_result          TYPE akb_except_type.

  lv_object_name = 'CLAS'.
  IF gt_class_method IS NOT INITIAL.
    RETURN.
  ENDIF.

  SELECT * FROM tadir INTO TABLE lt_package_content WHERE devclass = gv_package_name AND object = lv_object_name .
  IF sy-subrc = 0.
    LOOP AT lt_package_content INTO ls_package_content.
      ls_class_name-clsname = ls_package_content-obj_name.
      INSERT ls_class_name INTO TABLE lt_class_name.
    ENDLOOP.

    DATA :lt_class_attributes TYPE STANDARD TABLE OF seocompo,
          lv_attribute_name   TYPE char100,
          ls_class_attributes TYPE seocompo,
          lt_cls_att_details  TYPE STANDARD TABLE OF seocompodf,
          ls_cls_att_details  TYPE seocompodf.

*   ==> Get all the class attributes
    SELECT * FROM seocompo INTO TABLE lt_class_attributes FOR ALL ENTRIES IN lt_class_name WHERE clsname = lt_class_name-clsname
                                                                                             AND cmptype = 0.
    IF sy-subrc = 0.
      IF lt_class_attributes IS NOT   INITIAL.
        SELECT * FROM seocompodf INTO TABLE lt_cls_att_details FOR ALL ENTRIES IN lt_class_attributes WHERE  clsname  = lt_class_attributes-clsname
                                                                                                         AND cmpname =  lt_class_attributes-cmpname.
      ENDIF.
*     ==> Get class attributes name in the required format
      LOOP AT lt_class_attributes INTO ls_class_attributes.
        CLEAR ls_clsobjname.
        CONCATENATE ls_class_attributes-clsname '\DA:' ls_class_attributes-cmpname INTO ls_clsobjname-name.
        INSERT ls_clsobjname INTO TABLE lt_clsobjname.
      ENDLOOP.

*     ==> Do where-use on all the methods
      IF lt_clsobjname IS NOT INITIAL.
        SELECT * FROM wbcrossgt INTO TABLE lt_wbcrossgt FOR ALL ENTRIES IN lt_clsobjname WHERE name =  lt_clsobjname-name.
        DELETE ADJACENT DUPLICATES FROM lt_wbcrossgt COMPARING name.
      ENDIF.

*     ==> Get class attributes and name. Delete the attributes which have been used
      LOOP AT lt_wbcrossgt INTO ls_wbcrossgt.
        SPLIT ls_wbcrossgt-name AT '\DA:' INTO lv_class_name lv_attribute_name.
        DELETE lt_class_attributes WHERE clsname = lv_class_name AND cmpname = lv_attribute_name.
      ENDLOOP.

      LOOP AT lt_class_attributes INTO ls_class_attributes.
        ls_result-object_name        = ls_class_attributes-cmpname.
        ls_result-parent_object_name = ls_class_attributes-clsname.
        READ TABLE lt_cls_att_details INTO ls_cls_att_details WITH KEY clsname = ls_class_attributes-clsname
                                                                        cmpname = ls_class_attributes-cmpname.
        IF sy-subrc = 0.
          ls_result-changed_by = ls_cls_att_details-changedby.
          ls_result-changed_on = ls_cls_att_details-changedon.
        ENDIF.

        IF lv_object_name = 'CLAS'.
          ls_result-object_type = 'Class Attributes'.
          INSERT ls_result INTO TABLE gt_class_attributes.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDIF.

ENDFORM.                    " GET_CLASS_ATTRIBUTES

*&---------------------------------------------------------------------*
*&      Form  GET_INTERFACE_CONSTANTS
*&---------------------------------------------------------------------*
*text
*----------------------------------------------------------------------*
*-->  p1        text
*<--  p2        text
*----------------------------------------------------------------------*
FORM get_interface_constants .

  DATA:lv_object_name     TYPE char4,
       lv_class_name      TYPE seoclsname,
       lv_method_name     TYPE seocmpname,

       ls_package_content TYPE tadir,
       ls_class_name      TYPE ty_class_name,
       ls_class_methods   TYPE vseomethod,
       ls_clsobjname      TYPE ty_clsobjname,
       ls_wbcrossgt       TYPE wbcrossgt,
       ls_result          TYPE ty_result,

       lt_class_name      TYPE TABLE OF ty_class_name,
       lt_package_content TYPE TABLE OF tadir,
       lt_class_methods   TYPE TABLE OF vseomethod,
       lt_clsobjname      TYPE TABLE OF ty_clsobjname,
       lt_wbcrossgt       TYPE TABLE OF wbcrossgt,
       lt_result          TYPE akb_except_type.

  DATA :lt_class_attributes TYPE STANDARD TABLE OF seocompo,
        lv_attribute_name   TYPE char100,
        ls_class_attributes TYPE seocompo,
        lt_class_constants  TYPE STANDARD TABLE OF seocompodf,
        ls_class_constants  TYPE seocompodf.

  lv_object_name = 'INTF'.
  IF gt_int_method IS NOT INITIAL.
    RETURN.
  ENDIF.

  SELECT * FROM tadir INTO TABLE lt_package_content WHERE devclass = gv_package_name AND object = lv_object_name .
  IF sy-subrc = 0.
    LOOP AT lt_package_content INTO ls_package_content.
      ls_class_name-clsname = ls_package_content-obj_name.
      INSERT ls_class_name INTO TABLE lt_class_name.
    ENDLOOP.

*   ==> Get all the Interface Constants
    SELECT * FROM seocompo INTO TABLE lt_class_attributes FOR ALL ENTRIES IN lt_class_name WHERE clsname = lt_class_name-clsname
                                                                                             AND cmptype = 0.
    IF sy-subrc = 0.
      IF lt_class_attributes  IS NOT INITIAL.
        SELECT * FROM seocompodf INTO TABLE lt_class_constants FOR ALL ENTRIES IN lt_class_attributes WHERE clsname = lt_class_attributes-clsname
                                                                                                    AND cmpname = lt_class_attributes-cmpname
                                                                                                    AND attdecltyp = 2.
        IF sy-subrc = 0.
          LOOP AT lt_class_attributes INTO ls_class_attributes.
            READ TABLE lt_class_constants TRANSPORTING NO FIELDS WITH KEY cmpname = ls_class_attributes-cmpname.
            IF sy-subrc NE 0.
              DELETE lt_class_attributes.
            ENDIF.
          ENDLOOP.
        ELSE.
          CLEAR : lt_class_attributes.
        ENDIF.
      ENDIF.

*     ==> Get Interface attributes name in the required format
      LOOP AT lt_class_attributes INTO ls_class_attributes.
        CLEAR ls_clsobjname.
        CONCATENATE ls_class_attributes-clsname '\DA:' ls_class_attributes-cmpname INTO ls_clsobjname-name.
        INSERT ls_clsobjname INTO TABLE lt_clsobjname.
      ENDLOOP.

*     ==> Do where-use on all the methods
      IF lt_clsobjname IS NOT INITIAL.
        SELECT * FROM wbcrossgt INTO TABLE lt_wbcrossgt FOR ALL ENTRIES IN lt_clsobjname WHERE name =  lt_clsobjname-name.
        DELETE ADJACENT DUPLICATES FROM lt_wbcrossgt COMPARING name.
      ENDIF.

*     ==> Get Interface attributes and name. Delete the attributes which have been used
      LOOP AT lt_wbcrossgt INTO ls_wbcrossgt.
        SPLIT ls_wbcrossgt-name AT '\DA:' INTO lv_class_name lv_attribute_name.
        DELETE lt_class_attributes WHERE clsname = lv_class_name AND cmpname = lv_attribute_name.
      ENDLOOP.

      LOOP AT lt_class_attributes INTO ls_class_attributes.
        ls_result-object_name        = ls_class_attributes-cmpname.
        ls_result-parent_object_name = ls_class_attributes-clsname.
        READ TABLE lt_class_constants INTO ls_class_constants WITH KEY clsname = ls_class_attributes-clsname
                                                                       cmpname = ls_class_attributes-cmpname.
        IF sy-subrc = 0.
          ls_result-changed_by = ls_class_constants-changedby.
          ls_result-changed_on = ls_class_constants-changedon.
        ENDIF.

        IF lv_object_name = 'INTF'.
          ls_result-object_type = 'Interface Constants'.
          INSERT ls_result INTO TABLE gt_interface_constants.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDIF.
ENDFORM.                    " GET_INTERFACE_CONSTANTS

*&---------------------------------------------------------------------*
*&  Include           ZOBSOLETEOBJECTS_DISP
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_data .


  DATA: ls_layout           TYPE slis_layout_alv,
        ls_variant          TYPE disvariant,

        lt_fieldcat         TYPE slis_t_fieldcat_alv.

  FIELD-SYMBOLS: <ls_fieldcat>  TYPE slis_fieldcat_alv.

  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name       = 'ZOBSOBJ_RES'
    CHANGING
      ct_fieldcat            = lt_fieldcat
    EXCEPTIONS
      inconsistent_interface = 1
      program_error          = 2
      OTHERS                 = 3.
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.

  LOOP AT lt_fieldcat ASSIGNING <ls_fieldcat>.
    CASE <ls_fieldcat>-fieldname.

      WHEN 'PARENT_OBJECT_NAME'.
        <ls_fieldcat>-seltext_s   = 'P. Object'.
        <ls_fieldcat>-seltext_m   = 'Parent Object'.
        <ls_fieldcat>-seltext_l   = 'Parent Object'.

      WHEN 'OBJECT_TYPE'.
        <ls_fieldcat>-seltext_s   = 'Obj. Type'.
        <ls_fieldcat>-seltext_m   = 'Object Type'.
        <ls_fieldcat>-seltext_l   = 'Object Type'.

      WHEN 'CHANGED_BY'.
        <ls_fieldcat>-seltext_s   = 'Chg. By'.
        <ls_fieldcat>-seltext_m   = 'Last Changed By'.
        <ls_fieldcat>-seltext_l   = 'Last Changed By'.

    ENDCASE.
    <ls_fieldcat>-reptext_ddic = <ls_fieldcat>-seltext_m.

  ENDLOOP.

  ls_layout-colwidth_optimize = abap_true.

  ls_variant-report    = 'ZOBSOLETEOBJECTS'.
  ls_variant-username  = sy-uname.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program = 'ZOBSOLETEOBJECTS'
      is_layout          = ls_layout
      it_fieldcat        = lt_fieldcat
      is_variant         = ls_variant
      i_save             = 'U'
    TABLES
      t_outtab           = gt_result
    EXCEPTIONS
      program_error      = 1
      OTHERS             = 2.
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.


ENDFORM.                    " DISPLAY_DATA