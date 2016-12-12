*&---------------------------------------------------------------------*
*& Report  ZBOPF_CHANGE_TRACKER_LOG
*& Allows you to track any changes of any fields/nodes happened in BOPF
*&---------------------------------------------------------------------*
*& Authors: Dmitry Baliuk, Andrej Dukhounik
*& Version: 27.11.2016
*&---------------------------------------------------------------------*
REPORT zbopf_change_tracker_log.

TYPES:
  BEGIN OF ty_gs_bopf_log_selection,
    bo_key      TYPE zbopf_tracker_bo_key,
    node_key    TYPE zbopf_tracker_node_key,
    field_name  TYPE /bofu/field_name,
    date        TYPE sydatum,
    time        TYPE syuzeit,
    new_value   TYPE char80,
    record_key  TYPE zbopf_tracker_record_key,
    root_key    TYPE /bobf/conf_key,
    mainprogram TYPE syrepid,
    include     TYPE include,
    line        TYPE int4,
    blocktype   TYPE char12,
    blockname   TYPE char100,
    flag_system TYPE char1,
    change_mode TYPE /bobf/conf_change_mode,
  END OF ty_gs_bopf_log_selection.

TYPE-POOLS:
  icon.

TABLES:
  sscrfields.

CONSTANTS:
  lc_highest_ts          TYPE timestamp VALUE '99990000000000',
  lc_configurable_access TYPE char1 VALUE 'A',
  lc_const_access        TYPE char1 VALUE 'X',
  lc_deny_access         TYPE char1 VALUE ''.

DATA:   "only for selection screen
*=======================================================================
  gs_log_selection TYPE ty_gs_bopf_log_selection.

* Button: Tool Documentation
SELECTION-SCREEN FUNCTION KEY 1. "open bobf tracker maintenance
SELECTION-SCREEN FUNCTION KEY 2. "activate/diactivate
SELECTION-SCREEN FUNCTION KEY 3. "show user's activations
* Display Options
SELECTION-SCREEN BEGIN OF BLOCK bl1 WITH FRAME TITLE text-bl1.
SELECT-OPTIONS s_bo_k FOR gs_log_selection-bo_key.
SELECT-OPTIONS s_nd_k FOR gs_log_selection-node_key.
SELECT-OPTIONS s_fldnm FOR gs_log_selection-field_name.
SELECT-OPTIONS s_date FOR gs_log_selection-date DEFAULT sy-datum.
SELECT-OPTIONS s_time FOR gs_log_selection-time.
SELECT-OPTIONS s_new_v FOR gs_log_selection-new_value.
SELECT-OPTIONS s_rec_k FOR gs_log_selection-record_key.
SELECT-OPTIONS s_rt_k FOR gs_log_selection-root_key.
SELECT-OPTIONS s_mainp FOR gs_log_selection-mainprogram.
SELECT-OPTIONS s_incl FOR gs_log_selection-include.
SELECT-OPTIONS s_line FOR gs_log_selection-line.
SELECT-OPTIONS s_blckt FOR gs_log_selection-blocktype.
SELECT-OPTIONS s_blckn FOR gs_log_selection-blockname.
SELECT-OPTIONS s_c_mode FOR gs_log_selection-change_mode.
SELECTION-SCREEN SKIP.
SELECTION-SCREEN END OF BLOCK bl1.

INITIALIZATION.
*===================================================================
  PERFORM initialization.

*===================================================================
AT SELECTION-SCREEN OUTPUT.
*===================================================================
  PERFORM pbo.

*===================================================================
AT SELECTION-SCREEN.
*===================================================================
  PERFORM pai.

START-OF-SELECTION.
  PERFORM selection.

FORM initialization.
*===================================================================

  DATA ls_smp_dyntxt TYPE smp_dyntxt.
*
* set function key properties (display list of user-ids with active plug-in)
  CLEAR ls_smp_dyntxt.
  ls_smp_dyntxt-text      = text-003.
  ls_smp_dyntxt-icon_id   = icon_display.
  ls_smp_dyntxt-icon_text = text-004.
  sscrfields-functxt_03   = ls_smp_dyntxt.

* set function key properties (display maintenance view for bobf tracker settings)
  CLEAR ls_smp_dyntxt.
  ls_smp_dyntxt-text      = text-002.
  ls_smp_dyntxt-icon_id   = icon_edit_file.
  ls_smp_dyntxt-icon_text = text-011.
  sscrfields-functxt_01   = ls_smp_dyntxt.

ENDFORM.                    "initialization

FORM pbo.
*===================================================================

  DATA lv_is_active  TYPE abap_bool.
  DATA ls_smp_dyntxt TYPE smp_dyntxt.
  DATA lv_icon_id    TYPE smp_dyntxt-icon_id.
  DATA lv_icon_text  TYPE smp_dyntxt-icon_text.

* get activation status
  SELECT  SINGLE config_active
    INTO  lv_is_active
    FROM  /bobf/frw_adnact
    WHERE addon_name = zbopf_cl_change_tracker_adn=>cv_addon_name
      AND uname      = sy-uname.

  IF sy-subrc = 0 AND lv_is_active = abap_true.
    lv_icon_id   = icon_deactivate.
    lv_icon_text = text-008.
  ELSE.
    lv_icon_id = icon_activate.
    lv_icon_text = text-007.
  ENDIF.

* set function key properties (activate / deactivate)
  CLEAR ls_smp_dyntxt.
  ls_smp_dyntxt-text      = text-009.
  ls_smp_dyntxt-icon_id   = lv_icon_id.
  ls_smp_dyntxt-icon_text = lv_icon_text.
  sscrfields-functxt_02   = ls_smp_dyntxt.

ENDFORM.

FORM pai.
*===================================================================

  CASE sscrfields-ucomm.
    WHEN 'FC01'.
*     transaction with BOBF track settings view
      SELECT SINGLE * FROM zbopf_d_track INTO @DATA(lt_tab).
      IF lt_tab IS INITIAL.
        CALL TRANSACTION 'ZBOPF_TRACKER_CONFIG'.
      ELSE.
        CALL TRANSACTION 'ZBOPF_TRACKER_CONFIG' AND SKIP FIRST SCREEN.
      ENDIF.
      CLEAR lt_tab.
    WHEN 'FC02'.
*     activate/diactivate
      PERFORM bobf_change_tracker_activation.
    WHEN 'FC03'.
*     list of user-ids with active plug-in
      PERFORM list_display_activated_user.
  ENDCASE.

ENDFORM.

FORM list_display_activated_user.
*=======================================================================

  DATA:
    lt_list             TYPE /bobf/cl_frw_addon=>tt_addon_activation,
    lo_salv_table       TYPE REF TO cl_salv_table,
    lo_columns          TYPE REF TO cl_salv_columns,
    lo_display_settings TYPE REF TO cl_salv_display_settings,
    lt_salv_column      TYPE salv_t_column_ref,
    ls_salv_column      TYPE salv_s_column_ref,
    lo_salv_column      TYPE REF TO cl_salv_column.


  /bobf/cl_frw_addon=>get_addon_activation(
    EXPORTING
      iv_addon_name       = zbopf_cl_change_tracker_adn=>cv_addon_name
    IMPORTING
      et_addon_activation = lt_list ).

  DELETE lt_list WHERE config_active = abap_false.

  TRY.
      cl_salv_table=>factory(
        IMPORTING
          r_salv_table = lo_salv_table
        CHANGING
          t_table      = lt_list
      ).

      lo_columns = lo_salv_table->get_columns( ).
      " rename the columns
      lt_salv_column = lo_columns->get( ).
      LOOP AT lt_salv_column INTO ls_salv_column.
        IF ls_salv_column-columnname <> 'UNAME'.
          ls_salv_column-r_column->set_technical( if_salv_c_bool_sap=>true ).
        ENDIF.
      ENDLOOP.

      lo_display_settings = lo_salv_table->get_display_settings( ).
      lo_display_settings->set_list_header( text-004 ).
      lo_display_settings->set_striped_pattern( abap_true ).

      lo_salv_table->set_screen_popup(
          start_column = 20
          end_column   = 100
          start_line   = 3
          end_line     = 35
      ).

      lo_salv_table->display( ).

    CATCH cx_salv_msg.
      RETURN.
  ENDTRY.

ENDFORM.                    "list_display_activated_user

FORM bobf_change_tracker_activation.
*===================================================================

  DATA:
    ls_adnact        TYPE /bobf/frw_adnact,
    ls_smp_dyntxt    TYPE smp_dyntxt,
    lt_adnact_delete TYPE TABLE OF /bobf/frw_adnact,
    lt_adnact_modify TYPE TABLE OF /bobf/frw_adnact,
    ls_addon         TYPE /bobf/frw_addon,
    lt_addon_modify  TYPE TABLE OF /bobf/frw_addon.

  ls_smp_dyntxt = sscrfields-functxt_02.

  SELECT * FROM /bobf/frw_addon INTO TABLE @DATA(lt_addon).

  IF ls_smp_dyntxt-icon_id = icon_activate.
    " add record with our addon
    ls_addon-addon_name = zbopf_cl_change_tracker_adn=>cv_addon_name.
    ls_addon-addon_class = zbopf_cl_change_tracker_adn=>cv_addon_class.
    ls_addon-configuration = lc_const_access.
    ls_addon-activation = 1.
    ls_addon-serv_mgr = lc_const_access.
    ls_addon-int_access = lc_const_access.
    APPEND ls_addon TO lt_addon_modify.
  ENDIF.

*   process possible conflicts for current user
  CLEAR ls_adnact.
  ls_adnact-addon_name = zbopf_cl_change_tracker_adn=>cv_addon_name.
  ls_adnact-uname = sy-uname.
  IF ls_smp_dyntxt-icon_id = icon_activate.
    SELECT * FROM /bobf/frw_adnact INTO TABLE @DATA(lt_adnact).
*   find all conflicted addons and turn them off
    LOOP AT lt_adnact ASSIGNING FIELD-SYMBOL(<fs_adnact>).
      " if that user will not be affected by this addon - continue
      IF <fs_adnact>-uname <> sy-uname AND <fs_adnact> IS NOT INITIAL.
        CONTINUE.
      ENDIF.
      READ TABLE lt_addon INTO ls_addon WITH KEY addon_name = <fs_adnact>-addon_name.
      IF sy-subrc = 0.
        " constant access - deny by deleting user setting
        IF ls_addon-serv_mgr = lc_const_access OR ls_addon-int_access = lc_const_access.
          APPEND <fs_adnact> TO lt_adnact_delete.
        ENDIF.
        " configurable access - deny by deleting user setting
        IF ( ls_addon-serv_mgr = lc_configurable_access AND <fs_adnact>-serv_mgr_active = lc_const_access )
        OR ( ls_addon-int_access = lc_configurable_access AND <fs_adnact>-int_access_active = lc_const_access ).
          APPEND <fs_adnact> TO lt_adnact_delete.
        ENDIF.
      ENDIF.
    ENDLOOP.
* turn on our addon
    ls_adnact-config_active = lc_const_access.
    APPEND ls_adnact TO lt_adnact_modify.
  ELSE.
* turn off our addon
    APPEND ls_adnact TO lt_adnact_delete.
  ENDIF.

  MODIFY /bobf/frw_addon FROM TABLE lt_addon_modify.
  DELETE /bobf/frw_adnact FROM TABLE lt_adnact_delete.
  MODIFY /bobf/frw_adnact FROM TABLE lt_adnact_modify.
  CALL FUNCTION 'DB_COMMIT'.

ENDFORM. " bobf_change_tracker_activation.

FORM selection.

  DATA:
    lo_salv_table       TYPE REF TO cl_salv_table,
    lo_display_settings TYPE REF TO cl_salv_display_settings,
    lt_salv_column      TYPE salv_t_column_ref,
    ls_salv_column      TYPE salv_s_column_ref,
    lo_salv_column      TYPE REF TO cl_salv_column.

  CONVERT DATE s_date-low
          TIME s_time-low INTO TIME STAMP DATA(lv_low_ts) TIME ZONE sy-zonlo.
  CONVERT DATE s_date-high
          TIME s_time-high INTO TIME STAMP DATA(lv_high_ts) TIME ZONE sy-zonlo.
  IF lv_high_ts IS INITIAL.
    lv_high_ts = lc_highest_ts.
  ENDIF.

  SELECT * FROM zbopf_d_t_log INTO TABLE @DATA(lt_logs)
    WHERE bo_key IN @s_bo_k[]
      AND node_key IN @s_nd_k[]
      AND field_name IN @s_fldnm[]
      AND operation_time >= @lv_low_ts
      AND operation_time <= @lv_high_ts
      AND new_value IN @s_new_v[]
      AND record_key IN @s_rec_k[]
      AND root_key IN @s_rt_k[]
      AND mainprogram IN @s_mainp[]
      AND include IN @s_incl[]
      AND line IN @s_line[]
      AND blocktype IN @s_blckt[]
      AND blockname IN @s_blckn[]
      AND change_mode IN @s_c_mode[].

  TRY.
      cl_salv_table=>factory(
        IMPORTING
          r_salv_table = lo_salv_table
        CHANGING
          t_table      = lt_logs
      ).

      DATA(lo_functions) = lo_salv_table->get_functions( ).
      lo_functions->set_all( abap_true ).

      DATA(lo_display) = lo_salv_table->get_display_settings( ).

      DATA(lo_columns) = lo_salv_table->get_columns( ).
      DATA(lo_column) = lo_columns->get_column( 'FIELD_NAME' ).
      lo_column->set_output_length( 20 ).

      lo_column = lo_columns->get_column( 'NEW_VALUE' ).
      lo_column->set_long_text( 'New value' ).
      lo_column->set_output_length( 20 ).

      lo_column = lo_columns->get_column( 'ROOT_KEY' ).
      lo_column->set_long_text( 'Root key' ).
      lo_column->set_output_length( 20 ).

      lo_column = lo_columns->get_column( 'LINE' ).
      lo_column->set_long_text( 'Line' ).
      lo_column->set_output_length( 4 ).

      lo_column = lo_columns->get_column( 'BLOCKTYPE' ).
      lo_column->set_long_text( 'Block type' ).
      lo_column->set_output_length( 10 ).

      lo_column = lo_columns->get_column( 'BLOCKNAME' ).
      lo_column->set_long_text( 'Block name' ).
      lo_column->set_output_length( 40 ).

      lo_column = lo_columns->get_column( 'CHANGE_MODE' ).
      lo_column->set_long_text( 'Change mode' ).
      lo_column->set_output_length( 11 ).

      lo_column = lo_columns->get_column( 'MANDT' ).
      lo_column->set_visible( value = if_salv_c_bool_sap=>false ).

      lo_column = lo_columns->get_column( 'LOG_KEY' ).
      lo_column->set_visible( value = if_salv_c_bool_sap=>false ).

      lo_display_settings = lo_salv_table->get_display_settings( ).
      lo_display_settings->set_list_header( text-001 ).
      lo_display_settings->set_striped_pattern( abap_true ).

      DATA(lo_sorts) = lo_salv_table->get_sorts( ).
      lo_sorts->add_sort( columnname = 'OPERATION_TIME' ).

      lo_salv_table->display( ).

    CATCH cx_salv_msg cx_salv_not_found cx_salv_data_error cx_salv_existing.
      RETURN.
  ENDTRY.

ENDFORM.                    "selection