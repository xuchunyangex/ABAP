According to the trace, /SCMTMS/CL_TOR_A_ROOT_UPD_TOR~/SCMTMS/IF_ACTION_GROUPING~ADD_ACTION is called many times and the handling of the CAPA_REQ_LINK table inside causes a significant part of the runtime:
    "(b) merge action parameters
    ASSIGN <s_actgrp>-act_par->* TO <oldie>.
    ASSERT ID /scmtms/tor_fatal CONDITION sy-subrc EQ 0.
    CHECK sy-subrc EQ 0. "it'll fall back to adding the record
    LOOP AT<param>-capa_req_link ASSIGNING FIELD-SYMBOL(<s_link>).
      READ TABLE<oldie>-capa_req_link TRANSPORTING NO FIELDS
        WITH TABLE KEY source_key =<s_link>-source_key
                       target_key =<s_link>-target_key
                       change_cat =<s_link>-change_cat.
      IF sy-subrc GT 0.
        INSERT<s_link> INTO TABLE<oldie>-capa_req_link.
      ENDIF.
    ENDLOOP.
 
----------
 
/SCMTMS/CL_TRIG_CONTROL_ACTION~/SCMTMS/IF_TRIG_CONTROL~PROCESS_TRIGGER
...
      group_proc_trigger_by_action(
        EXPORTING
          it_trigger      = lt_trigger_w_act
        IMPORTING
          et_action_group = DATA(lt_action_group) ).
***
METHOD group_proc_trigger_by_action.
  DATA: lv_action_key   TYPE /bobf/act_key,
        lt_key          TYPE /bobf/t_frw_key.
  CLEAR et_action_group.
  IF it_trigger IS INITIAL.
    RETURN.
  ENDIF.
  DATA(lt_trigger) = it_trigger.
  SORT lt_trigger BY act_param-s_frw_ctx_act-act_key.
  LOOP AT lt_trigger ASSIGNING FIELD-SYMBOL(<fs_trigger>)
    GROUP BY<fs_trigger>-act_param-s_frw_ctx_act-act_key ASSIGNING FIELD-SYMBOL(<fs_grouped>).
    LOOP AT GROUP<fs_grouped> ASSIGNING FIELD-SYMBOL(<fs_group_ref>).
      CLEAR lt_key.
      INSERT VALUE #( key =<fs_group_ref>-trigger_key ) INTO TABLE lt_key.
      /scmtms/cl_actgrp_factory=>get_instance(<fs_grouped> )->add_action(
        EXPORTING
          is_actgrp = VALUE #( act_key =<fs_grouped>
                               act_par =<fs_group_ref>-act_param-r_s_parameters
                               t_key   = lt_key )
        CHANGING
          ct_actgrp = et_action_group ).
    ENDLOOP.
  ENDLOOP.
ENDMETHOD.
***
...
      process_actions(
        EXPORTING
          it_trigger          = lt_trigger_w_act
          it_action_group     = lt_action_group
          iv_package_size     = iv_package_size
          iv_catch_errors     = iv_catch_errors
          iv_reproc_errors    = iv_reproc_errors
          io_modify           = io_modify
          io_tra_mgr          = mo_tra_mgr
          iv_called_by_report = iv_called_by_report
        IMPORTING
          eo_message          = lo_message
          et_failed_key       = et_failed_key ).
/SCMTMS/CL_TRIG_CONTROL_ACTION~/SCMTMS/IF_TRIG_CONTROL~PROCESS_ACTIONS
...
    LOOP AT it_action_group ASSIGNING FIELD-SYMBOL(<fs_action>).
      lt_key = CORRESPONDING #(<fs_action>-t_key ).
...
*        Get they trigger entries processed with the current call and mark them to be processed
        extract_trig_for_act_key(
          EXPORTING
            it_trigger    = it_trigger
            is_action_grp =<fs_action>
            it_key_rel    = lt_key_current
          IMPORTING
           et_relevant_trigger = DATA(lt_trig_rel) ).
        CHECK lt_trig_rel IS NOT INITIAL.
***
  METHOD extract_trig_for_act_key.
**********************************************************************
*    This method extracts the trigger table entries which are matching the keys, actions and parameters of a given action group
**********************************************************************
    CLEAR et_relevant_trigger.
    LOOP AT it_key_rel ASSIGNING FIELD-SYMBOL(<fs_key>).
      LOOP AT it_trigger ASSIGNING FIELD-SYMBOL(<fs_trigger>) USING KEY trigkey_act
                         WHERE trigger_key =<fs_key>-key      AND
                               act_key     = is_action_grp-act_key.
        IF /scmtms/cl_actgrp_factory=>get_instance( is_action_grp-act_key )->is_action_in_group(
           EXPORTING
             is_actgrp_exist = is_action_grp
             is_actgrp_new   = VALUE #( act_key = is_action_grp-act_key
                                        act_par = is_action_grp-act_par
                                        t_key   = VALUE #( ( key =<fs_key>-key ) ) ) ) = abap_true.
          INSERT<fs_trigger> INTO TABLE et_relevant_trigger.
        ENDIF.
      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.
***
  METHOD /scmtms/if_action_grouping~is_action_in_group.
**********************************************************************
* This class checks if the action group would be merged with an existing action group instance
    DATA: lt_actgrp TYPE /scmtms/if_action_grouping=>tt_actgrp.
    INSERT is_actgrp_exist INTO TABLE lt_actgrp.
    CALL METHOD me->add_action
      EXPORTING
        is_actgrp = is_actgrp_new
      CHANGING
        ct_actgrp = lt_actgrp.
    IF lines( lt_actgrp ) = 1.
      rv_is_part_of_group = abap_true.
    ELSE.
      CLEAR rv_is_part_of_group.
    ENDIF.
  ENDMETHOD.
***
