class ZBOPF_CL_CHANGE_TRACKER_ADN definition
  public
  inheriting from /BOBF/CL_FRW_INT_ACCESS
  final
  create public .

public section.

  interfaces /BOBF/IF_FRW_ADDON .
  interfaces /BOBF/IF_TRA_SERVICE_MANAGER .
  interfaces /BOBF/IF_TRA_SERV_MGR_COMPL .
  interfaces /BOBF/IF_TRA_SERV_MGR_TRANSACT .
  interfaces /BOBF/IF_FRW_MESSAGE .
  interfaces /BOBF/IF_FRW_ADDON_OPTIONAL .

  types:
    lt_bobf_change_tracker_log TYPE TABLE OF zbopf_d_t_log .
  types TS_BOBF_CHANGE_TRACKER_LOG type ZBOPF_D_T_LOG .

  class-data GT_CHANGE_LOG_CONTAINER type LT_BOBF_CHANGE_TRACKER_LOG .
  constants CV_ADDON_NAME type /BOBF/OBM_NAME value 'ZBOPF_CHANGE_TRACKER_ADN' ##NO_TEXT.
  class-data CV_ADDON_CLASS type SEOCLSNAME value 'ZBOPF_CL_CHANGE_TRACKER_ADN' ##NO_TEXT.

  methods CONSTRUCTOR
    importing
      !IV_BO_KEY type /BOBF/OBM_BO_KEY optional
      !IO_INTERNAL_ACCESS type ref to /BOBF/CL_FRW_INT_ACCESS optional
      !IO_SERVICE_MANAGER type ref to /BOBF/IF_TRA_SERV_MGR_COMPL optional
      !IO_MESSAGE type ref to /BOBF/IF_FRW_MESSAGE optional .
  class-methods ACTIVATE_ADDON
    importing
      !IO_BREAKPOINT_HANDLER type ref to /BOBF/IF_TOOL_BREAKPOINT_HNDL optional .
  class-methods COUNT_WATCHPOINTS
    returning
      value(RV_COUNT) type INT4 .

  methods /BOBF/IF_FRW_CHECK~CHECK_AND_DETERMINE
    redefinition .
  methods /BOBF/IF_FRW_MODIFY~CREATE
    redefinition .
  methods /BOBF/IF_FRW_MODIFY~DELETE
    redefinition .
  methods /BOBF/IF_FRW_MODIFY~DO_ACTION
    redefinition .
  methods /BOBF/IF_FRW_MODIFY~DO_MODIFY
    redefinition .
  methods /BOBF/IF_FRW_MODIFY~END_MODIFY
    redefinition .
  methods /BOBF/IF_FRW_MODIFY~NOTIFY_ASSOCIATION_CHANGE
    redefinition .
  methods /BOBF/IF_FRW_MODIFY~NOTIFY_CHANGE
    redefinition .
  methods /BOBF/IF_FRW_MODIFY~NOTIFY_PROPERTY_CHANGE
    redefinition .
  methods /BOBF/IF_FRW_MODIFY~UPDATE
    redefinition .
  methods /BOBF/IF_FRW_QUERY~QUERY
    redefinition .
  methods /BOBF/IF_FRW_QUERY~RETRIEVE_DEFAULT_PARAM
    redefinition .
  methods /BOBF/IF_FRW_READ~COMPARE
    redefinition .
  methods /BOBF/IF_FRW_READ~CONVERT_ALTERN_KEY
    redefinition .
  methods /BOBF/IF_FRW_READ~GET_ROOT_KEY
    redefinition .
  methods /BOBF/IF_FRW_READ~RETRIEVE
    redefinition .
  methods /BOBF/IF_FRW_READ~RETRIEVE_BY_ASSOCIATION
    redefinition .
  PROTECTED SECTION.

    METHODS end_modify
         REDEFINITION .
    METHODS invalidate
         REDEFINITION .
    METHODS notify_change_internal
         REDEFINITION .
    METHODS set_context
         REDEFINITION .
    METHODS set_current_state
         REDEFINITION .
    METHODS set_edit_mode
         REDEFINITION .
    METHODS set_last_state
         REDEFINITION .
private section.

  types:
    BEGIN OF ty_watchpoint,
        bo_key    TYPE /bobf/conf_key,
        node_key  TYPE /bobf/conf_key,
        on_create TYPE boole_d,
        on_delete TYPE boole_d,
        on_update TYPE boole_d,
      END OF ty_watchpoint .
  types:
    tt_watchpoint TYPE SORTED TABLE OF ty_watchpoint WITH NON-UNIQUE KEY primary_key COMPONENTS bo_key node_key .

  data MO_ORIGINAL_MESSAGE type ref to /BOBF/IF_FRW_MESSAGE .
  data MV_BO_KEY type /BOBF/CONF_KEY .
  class-data GT_WATCHPOINT type TT_WATCHPOINT .
  data MO_ORIGINAL_SERVICE_MANAGER type ref to /BOBF/IF_TRA_SERV_MGR_COMPL .
  data MO_ORIGINAL_INTERNAL_ACCESS type ref to /BOBF/CL_FRW_INT_ACCESS .
  class-data GO_BREAKPOINT_HANDLER type ref to /BOBF/IF_TOOL_BREAKPOINT_HNDL .
  class-data GT_CHANGE_TRACKER_SETTINGS type ZBOPF_T_CHANGE_TRACKER_SET .
  class-data GV_SETTINGS_READ type FLAG .
  constants CV_SECOND_CONNECTION type CHAR30 value 'R/3*BOBFTRACKLOG' ##NO_TEXT.
  class-data GV_IS_IN_TEST_UI type BOOLEAN value ABAP_FALSE ##NO_TEXT.

  methods CHECK_WATCHPOINT_ON_CREATE
    importing
      !IV_NODE_KEY type /BOBF/OBM_NODE_KEY
    returning
      value(RV_BREAK) type BOOLE_D .
  methods CHECK_WATCHPOINT_ON_DELETE
    importing
      !IV_NODE_KEY type /BOBF/OBM_NODE_KEY
    returning
      value(RV_BREAK) type BOOLE_D .
  methods CHECK_WATCHPOINT_ON_UPDATE
    importing
      !IV_NODE_KEY type /BOBF/OBM_NODE_KEY
    returning
      value(RV_BREAK) type BOOLE_D .
  methods SYNCHRONIZE_INTERNAL_ACCESS .
  methods READ_ABAP_CALL_STACK
    changing
      !CT_CHANGE_LOG type LT_BOBF_CHANGE_TRACKER_LOG .
  methods SETUP_CHANGE_TRACKER_SETTINGS .
  methods TRACK_MODIFICATIONS
    importing
      !IT_MODIFY type /BOBF/T_FRW_MODIFICATION .
  class-methods CHECK_TEST_UI
    returning
      value(RV_IS_IN_TEST_UI) type BOOLEAN .
ENDCLASS.



CLASS ZBOPF_CL_CHANGE_TRACKER_ADN IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZBOPF_CL_CHANGE_TRACKER_ADN=>/BOBF/IF_FRW_ADDON_OPTIONAL~ADJUST_INTERNAL_ACCESS
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_BO_KEY                      TYPE        /BOBF/OBM_BO_KEY
* | [<-->] CO_INTERNAL_ACCESS             TYPE REF TO /BOBF/CL_FRW_INT_ACCESS
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD /bobf/if_frw_addon_optional~adjust_internal_access.

    DATA lo_addon TYPE REF TO zbopf_cl_change_tracker_adn.

    IF check_test_ui( ).
      gv_is_in_test_ui = abap_true.
    ENDIF.

    CHECK gv_is_in_test_ui = abap_false.
    " create an addon instance for this BO
    lo_addon = NEW #(
      iv_bo_key          = iv_bo_key
      io_internal_access = co_internal_access ).

    " return the addon instead of the original int access modify
    co_internal_access ?= lo_addon.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZBOPF_CL_CHANGE_TRACKER_ADN=>/BOBF/IF_FRW_ADDON_OPTIONAL~ADJUST_MESSAGE
* +-------------------------------------------------------------------------------------------------+
* | [<-->] CO_MESSAGE                     TYPE REF TO /BOBF/IF_FRW_MESSAGE
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD /BOBF/IF_FRW_ADDON_OPTIONAL~ADJUST_MESSAGE.
    DATA lo_addon TYPE REF TO /bobf/cl_tool_debug_addon.

    " create an addon instance for this BO
    CREATE OBJECT lo_addon
      EXPORTING
        io_message = co_message.

    " return the addon instead of the original message
    co_message ?= lo_addon.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZBOPF_CL_CHANGE_TRACKER_ADN=>/BOBF/IF_FRW_ADDON~ADJUST_BOPF
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_BO_KEY                      TYPE        /BOBF/OBM_BO_KEY
* | [<-->] CO_BOPF                        TYPE REF TO /BOBF/IF_FRW_SERVICE_LAYER
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD /BOBF/IF_FRW_ADDON~ADJUST_BOPF.

    " this addon don't replace the framework instance
    RAISE EXCEPTION TYPE /bobf/cx_frw_fatal
      EXPORTING
        textid = /bobf/cx_frw_fatal=>sc_application_error.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZBOPF_CL_CHANGE_TRACKER_ADN=>/BOBF/IF_FRW_ADDON~ADJUST_CONFIGURATION
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_BO_KEY                      TYPE        /BOBF/OBM_BO_KEY
* | [--->] IO_CONFIGURATION               TYPE REF TO /BOBF/IF_CONFRT_COMPLETE
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD /BOBF/IF_FRW_ADDON~ADJUST_CONFIGURATION.

*    " this addon don't replace the configuration
*    RAISE EXCEPTION TYPE /bobf/cx_frw_fatal
*      EXPORTING
*        textid = /bobf/cx_frw_fatal=>sc_application_error.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZBOPF_CL_CHANGE_TRACKER_ADN=>/BOBF/IF_FRW_ADDON~ADJUST_SERV_MGR
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_BO_KEY                      TYPE        /BOBF/OBM_BO_KEY
* | [<-->] CO_SERV_MGR                    TYPE REF TO /BOBF/IF_TRA_SERV_MGR_COMPL
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD /bobf/if_frw_addon~adjust_serv_mgr.

    DATA lo_addon TYPE REF TO zbopf_cl_change_tracker_adn.

    IF check_test_ui( ).
      gv_is_in_test_ui = abap_true.
    ENDIF.

    CHECK gv_is_in_test_ui = abap_false.
    " create an addon instance for this BO
    CREATE OBJECT lo_addon
      EXPORTING
        iv_bo_key          = iv_bo_key
        io_service_manager = co_serv_mgr.

    " return the addon instead of the original service manager
    co_serv_mgr ?= lo_addon.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZBOPF_CL_CHANGE_TRACKER_ADN=>/BOBF/IF_FRW_ADDON~ADJUST_TRANS_MGR
* +-------------------------------------------------------------------------------------------------+
* | [<-->] CO_TRANS_MGR                   TYPE REF TO /BOBF/IF_TRA_TRANS_MGR_COMPL
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD /BOBF/IF_FRW_ADDON~ADJUST_TRANS_MGR.

    " this addon don't replace the transaction mgr
    RAISE EXCEPTION TYPE /bobf/cx_frw_fatal
      EXPORTING
        textid = /bobf/cx_frw_fatal=>sc_application_error.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZBOPF_CL_CHANGE_TRACKER_ADN->/BOBF/IF_FRW_ADDON~GET_BOPF
* +-------------------------------------------------------------------------------------------------+
* | [<-()] EO_BOPF                        TYPE REF TO /BOBF/CL_FRW
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD /BOBF/IF_FRW_ADDON~GET_BOPF.

    " this addon don't replace the framework instance
    RAISE EXCEPTION TYPE /bobf/cx_frw_fatal
      EXPORTING
        textid = /bobf/cx_frw_fatal=>sc_application_error.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZBOPF_CL_CHANGE_TRACKER_ADN->/BOBF/IF_FRW_ADDON~GET_SERV_MGR
* +-------------------------------------------------------------------------------------------------+
* | [<-()] EO_SERV_MGR                    TYPE REF TO /BOBF/IF_TRA_SERVICE_MANAGER
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD /BOBF/IF_FRW_ADDON~GET_SERV_MGR.

    eo_serv_mgr = mo_original_service_manager.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZBOPF_CL_CHANGE_TRACKER_ADN->/BOBF/IF_FRW_ADDON~GET_TRANS_MGR
* +-------------------------------------------------------------------------------------------------+
* | [<-()] EO_TRANS_MGR                   TYPE REF TO /BOBF/IF_TRA_TRANSACTION_MGR
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD /BOBF/IF_FRW_ADDON~GET_TRANS_MGR.

    " this addon don't replace the transaction manager
    RAISE EXCEPTION TYPE /bobf/cx_frw_fatal
      EXPORTING
        textid = /bobf/cx_frw_fatal=>sc_application_error.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZBOPF_CL_CHANGE_TRACKER_ADN->/BOBF/IF_FRW_CHECK~CHECK_AND_DETERMINE
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_NODE_KEY                    TYPE        /BOBF/OBM_NODE_KEY
* | [--->] IT_KEY                         TYPE        /BOBF/T_FRW_KEY
* | [--->] IV_CHECK_SCOPE                 TYPE        /BOBF/FRW_SCOPE
* | [<---] EO_MESSAGE                     TYPE REF TO /BOBF/IF_FRW_MESSAGE
* | [<---] EO_CHANGE                      TYPE REF TO /BOBF/IF_FRW_CHANGE
* | [!CX!] /BOBF/CX_FRW
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD /BOBF/IF_FRW_CHECK~CHECK_AND_DETERMINE.

    mo_original_internal_access->/bobf/if_frw_check~check_and_determine(
      EXPORTING
        iv_node_key    = iv_node_key
        it_key         = it_key
        iv_check_scope = iv_check_scope
      IMPORTING
        eo_message     = eo_message
        eo_change      = eo_change ).

    synchronize_internal_access( ).

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZBOPF_CL_CHANGE_TRACKER_ADN->/BOBF/IF_FRW_MESSAGE~ADD
* +-------------------------------------------------------------------------------------------------+
* | [--->] IO_MESSAGE                     TYPE REF TO /BOBF/IF_FRW_MESSAGE
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD /BOBF/IF_FRW_MESSAGE~ADD.
    mo_original_message->add( io_message = io_message ).
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZBOPF_CL_CHANGE_TRACKER_ADN->/BOBF/IF_FRW_MESSAGE~ADD_CM
* +-------------------------------------------------------------------------------------------------+
* | [--->] IO_MESSAGE                     TYPE REF TO /BOBF/CM_FRW(optional)
* | [--->] IT_MESSAGE                     TYPE        /BOBF/CM_FRW=>TT_FRW(optional)
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD /BOBF/IF_FRW_MESSAGE~ADD_CM.
    mo_original_message->add_cm(
      EXPORTING
        io_message = io_message
        it_message = it_message ).
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZBOPF_CL_CHANGE_TRACKER_ADN->/BOBF/IF_FRW_MESSAGE~ADD_CM_WITH_NEW_LOCATION
* +-------------------------------------------------------------------------------------------------+
* | [--->] IO_MESSAGE                     TYPE REF TO /BOBF/CM_FRW
* | [--->] IV_NEW_BOPF_LOCATION           TYPE        /BOBF/CONF_KEY(optional)
* | [--->] IS_NEW_ORIGIN_LOCATION         TYPE        /BOBF/S_FRW_LOCATION(optional)
* | [--->] IT_NEW_ENVIRONMENT_LOCATION    TYPE        /BOBF/T_FRW_LOCATION(optional)
* | [--->] IV_NEW_LIFETIME                TYPE        /BOBF/CM_FRW=>TY_MESSAGE_LIFETIME(optional)
* | [--->] IV_NEW_SYMPTOM                 TYPE        /BOBF/CM_FRW=>TY_MESSAGE_SYMPTOM(optional)
* | [--->] IV_NEW_SEVERITY                TYPE        /BOBF/CM_FRW=>TY_MESSAGE_SEVERITY(optional)
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD /BOBF/IF_FRW_MESSAGE~ADD_CM_WITH_NEW_LOCATION.
    mo_original_message->add_cm_with_new_location(
      EXPORTING
        io_message                  = io_message
        iv_new_bopf_location        = iv_new_bopf_location
        is_new_origin_location      = is_new_origin_location
        it_new_environment_location = it_new_environment_location
        iv_new_lifetime             = iv_new_lifetime
        iv_new_symptom              = iv_new_symptom
        iv_new_severity             = iv_new_severity ).
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZBOPF_CL_CHANGE_TRACKER_ADN->/BOBF/IF_FRW_MESSAGE~ADD_EXCEPTION
* +-------------------------------------------------------------------------------------------------+
* | [--->] IO_EXCEPTION                   TYPE REF TO CX_ROOT
* | [--->] IV_NODE                        TYPE        /BOBF/OBM_NODE_KEY(optional)
* | [--->] IV_KEY                         TYPE        /BOBF/CONF_KEY(optional)
* | [--->] IV_ATTRIBUTE                   TYPE        /BOBF/OBM_NAME(optional)
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD /BOBF/IF_FRW_MESSAGE~ADD_EXCEPTION.
    mo_original_message->add_exception(
      EXPORTING
        io_exception = io_exception
        iv_node      = iv_node
        iv_key       = iv_key
        iv_attribute = iv_attribute ).
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZBOPF_CL_CHANGE_TRACKER_ADN->/BOBF/IF_FRW_MESSAGE~ADD_MESSAGE
* +-------------------------------------------------------------------------------------------------+
* | [--->] IS_MSG                         TYPE        SYMSG
* | [--->] IV_NODE                        TYPE        /BOBF/OBM_NODE_KEY(optional)
* | [--->] IV_KEY                         TYPE        /BOBF/CONF_KEY(optional)
* | [--->] IV_ATTRIBUTE                   TYPE        STRING(optional)
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD /BOBF/IF_FRW_MESSAGE~ADD_MESSAGE.
    mo_original_message->add_message(
      EXPORTING
        is_msg       = is_msg
        iv_node      = iv_node
        iv_key       = iv_key
        iv_attribute = iv_attribute ).
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZBOPF_CL_CHANGE_TRACKER_ADN->/BOBF/IF_FRW_MESSAGE~CHECK
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_CONSISTENCY_MESSAGES        TYPE        BOOLE_D (default =ABAP_TRUE)
* | [--->] IV_ACTION_MESSAGES             TYPE        BOOLE_D (default =ABAP_TRUE)
* | [<-()] EV_ERROR_MESSAGES              TYPE        BOOLE_D
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD /BOBF/IF_FRW_MESSAGE~CHECK.
    mo_original_message->check(
      EXPORTING
        iv_consistency_messages = iv_consistency_messages
        iv_action_messages      = iv_action_messages
      RECEIVING
        ev_error_messages       = ev_error_messages ).
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZBOPF_CL_CHANGE_TRACKER_ADN->/BOBF/IF_FRW_MESSAGE~GET
* +-------------------------------------------------------------------------------------------------+
* | [<---] ET_MESSAGE                     TYPE        /BOBF/CM_FRW=>TT_FRW
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD /BOBF/IF_FRW_MESSAGE~GET.
    mo_original_message->get(
      IMPORTING
        et_message = et_message ).
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZBOPF_CL_CHANGE_TRACKER_ADN->/BOBF/IF_FRW_MESSAGE~GET_MESSAGES
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_SEVERITY                    TYPE        /BOBF/CM_FRW=>TY_MESSAGE_SEVERITY(optional)
* | [--->] IV_CONSISTENCY_MESSAGES        TYPE        BOOLE_D (default =ABAP_TRUE)
* | [--->] IV_ACTION_MESSAGES             TYPE        BOOLE_D (default =ABAP_TRUE)
* | [<---] ET_MESSAGE                     TYPE        /BOBF/T_FRW_MESSAGE_K
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD /BOBF/IF_FRW_MESSAGE~GET_MESSAGES.
    mo_original_message->get_messages(
      EXPORTING
        iv_severity             = iv_severity
        iv_consistency_messages = iv_consistency_messages
        iv_action_messages      = iv_action_messages
      IMPORTING
        et_message              = et_message ).
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZBOPF_CL_CHANGE_TRACKER_ADN->/BOBF/IF_FRW_MODIFY~CREATE
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_NODE                        TYPE        /BOBF/OBM_NODE_KEY
* | [--->] IV_KEY                         TYPE        /BOBF/CONF_KEY(optional)
* | [--->] IV_NODE_CAT                    TYPE        /BOBF/OBM_NODE_CAT_KEY(optional)
* | [--->] IS_DATA                        TYPE REF TO DATA(optional)
* | [--->] IT_CHANGED_FIELDS              TYPE        /BOBF/T_FRW_NAME(optional)
* | [--->] IV_ASSOC_KEY                   TYPE        /BOBF/OBM_ASSOC_KEY(optional)
* | [--->] IV_SOURCE_NODE_KEY             TYPE        /BOBF/OBM_NODE_KEY(optional)
* | [--->] IV_SOURCE_KEY                  TYPE        /BOBF/CONF_KEY(optional)
* | [--->] IV_ROOT_KEY                    TYPE        /BOBF/CONF_KEY(optional)
* | [<---] EV_KEY                         TYPE        /BOBF/CONF_KEY
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD /BOBF/IF_FRW_MODIFY~CREATE.

    " delegate to original internal access instance
    mo_original_internal_access->/bobf/if_frw_modify~create(
      EXPORTING
        iv_node            = iv_node
        iv_key             = iv_key
        iv_node_cat        = iv_node_cat
        is_data            = is_data
        it_changed_fields  = it_changed_fields
        iv_assoc_key       = iv_assoc_key
        iv_source_node_key = iv_source_node_key
        iv_source_key      = iv_source_key
        iv_root_key        = iv_root_key
      IMPORTING
        ev_key             = ev_key ).

    synchronize_internal_access( ).

    track_modifications(
      it_modify = VALUE #( (
        change_mode    = /bobf/if_frw_c=>sc_modify_create
        node           = iv_node
        key            = iv_key
        node_cat       = iv_node_cat
        data           = is_data
        changed_fields = it_changed_fields
        association    = iv_assoc_key
        source_node    = iv_source_node_key
        source_key     = iv_source_key
        root_key       = iv_root_key ) )
    ).

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZBOPF_CL_CHANGE_TRACKER_ADN->/BOBF/IF_FRW_MODIFY~DELETE
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_NODE                        TYPE        /BOBF/OBM_NODE_KEY
* | [--->] IV_KEY                         TYPE        /BOBF/CONF_KEY(optional)
* | [--->] IT_KEY                         TYPE        /BOBF/T_FRW_KEY(optional)
* | [--->] IV_ROOT_KEY                    TYPE        /BOBF/CONF_KEY(optional)
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD /BOBF/IF_FRW_MODIFY~DELETE.

    DATA:
      lt_modif      TYPE /bobf/t_frw_modification,
      lt_change_log TYPE lt_bobf_change_tracker_log.

    " delegate to original internal access instance
    mo_original_internal_access->/bobf/if_frw_modify~delete(
      EXPORTING
        iv_node     = iv_node
        iv_key      = iv_key
        it_key      = it_key
        iv_root_key = iv_root_key ).

    synchronize_internal_access( ).

    lt_modif = VALUE #( FOR ls_key IN it_key
                        ( change_mode = /bobf/if_frw_c=>sc_modify_delete
                          node        = iv_node
                          key         = ls_key-key
                          root_key    = iv_root_key ) ).

    IF iv_key IS NOT INITIAL.
      INSERT VALUE #(
        change_mode = /bobf/if_frw_c=>sc_modify_delete
        node        = iv_node
        key         = iv_key
        root_key    = iv_root_key ) INTO TABLE lt_modif.
    ENDIF.

    track_modifications( it_modify = lt_modif ).

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZBOPF_CL_CHANGE_TRACKER_ADN->/BOBF/IF_FRW_MODIFY~DO_ACTION
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_ACT_KEY                     TYPE        /BOBF/ACT_KEY
* | [--->] IT_KEY                         TYPE        /BOBF/T_FRW_KEY
* | [--->] IS_PARAMETERS                  TYPE REF TO DATA(optional)
* | [<---] EO_MESSAGE                     TYPE REF TO /BOBF/IF_FRW_MESSAGE
* | [<---] ET_FAILED_KEY                  TYPE        /BOBF/T_FRW_KEY
* | [<---] EO_CHANGE                      TYPE REF TO /BOBF/IF_FRW_CHANGE
* | [<---] ET_DATA                        TYPE        INDEX TABLE
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD /BOBF/IF_FRW_MODIFY~DO_ACTION.

    mo_original_internal_access->/bobf/if_frw_modify~do_action(
      EXPORTING
        iv_act_key    = iv_act_key
        it_key        = it_key
        is_parameters = is_parameters
      IMPORTING
        eo_message    = eo_message
        et_failed_key = et_failed_key
        eo_change     = eo_change
        et_data       = et_data ).

    synchronize_internal_access( ).

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZBOPF_CL_CHANGE_TRACKER_ADN->/BOBF/IF_FRW_MODIFY~DO_MODIFY
* +-------------------------------------------------------------------------------------------------+
* | [--->] IT_MODIFICATION                TYPE        /BOBF/T_FRW_MODIFICATION
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD /BOBF/IF_FRW_MODIFY~DO_MODIFY.

    mo_original_internal_access->/bobf/if_frw_modify~do_modify( it_modification = it_modification ).

    synchronize_internal_access( ).

    track_modifications( it_modify = it_modification ).

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZBOPF_CL_CHANGE_TRACKER_ADN->/BOBF/IF_FRW_MODIFY~END_MODIFY
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_PROCESS_IMMEDIATELY         TYPE        BOOLE_D(optional)
* | [<---] EO_MESSAGE                     TYPE REF TO /BOBF/IF_FRW_MESSAGE
* | [<---] EO_CHANGE                      TYPE REF TO /BOBF/IF_FRW_CHANGE
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD /BOBF/IF_FRW_MODIFY~END_MODIFY.

    mo_original_internal_access->/bobf/if_frw_modify~end_modify(
      EXPORTING
        iv_process_immediately = iv_process_immediately
      IMPORTING
        eo_message             = eo_message
        eo_change              = eo_change ).

    synchronize_internal_access( ).

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZBOPF_CL_CHANGE_TRACKER_ADN->/BOBF/IF_FRW_MODIFY~NOTIFY_ASSOCIATION_CHANGE
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_NODE_KEY                    TYPE        /BOBF/OBM_NODE_KEY
* | [--->] IV_KEY                         TYPE        /BOBF/CONF_KEY(optional)
* | [--->] IT_KEY                         TYPE        /BOBF/T_FRW_KEY(optional)
* | [--->] IV_ASSOC_KEY                   TYPE        /BOBF/OBM_ASSOC_KEY(optional)
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD /BOBF/IF_FRW_MODIFY~NOTIFY_ASSOCIATION_CHANGE.

    mo_original_internal_access->/bobf/if_frw_modify~notify_association_change(
      EXPORTING
        iv_node_key  = iv_node_key
        iv_key       = iv_key
        it_key       = it_key
        iv_assoc_key = iv_assoc_key ).

    synchronize_internal_access( ).

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZBOPF_CL_CHANGE_TRACKER_ADN->/BOBF/IF_FRW_MODIFY~NOTIFY_CHANGE
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_NODE_KEY                    TYPE        /BOBF/OBM_NODE_KEY
* | [--->] IV_KEY                         TYPE        /BOBF/CONF_KEY(optional)
* | [--->] IT_KEY                         TYPE        /BOBF/T_FRW_KEY(optional)
* | [--->] IV_CHANGE_MODE                 TYPE        /BOBF/CONF_CHANGE_MODE (default =/BOBF/IF_FRW_C=>SC_MODIFY_UPDATE)
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD /BOBF/IF_FRW_MODIFY~NOTIFY_CHANGE.

    mo_original_internal_access->/bobf/if_frw_modify~notify_change(
      EXPORTING
        iv_node_key    = iv_node_key
        iv_key         = iv_key
        it_key         = it_key
        iv_change_mode = iv_change_mode ).

    synchronize_internal_access( ).

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZBOPF_CL_CHANGE_TRACKER_ADN->/BOBF/IF_FRW_MODIFY~NOTIFY_PROPERTY_CHANGE
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_NODE_KEY                    TYPE        /BOBF/OBM_NODE_KEY
* | [--->] IV_KEY                         TYPE        /BOBF/CONF_KEY(optional)
* | [--->] IT_KEY                         TYPE        /BOBF/T_FRW_KEY(optional)
* | [--->] IV_NODE_CHANGED                TYPE        BOOLE_D (default =ABAP_FALSE)
* | [--->] IV_NODE_ATTRIBUTE_CHANGED      TYPE        BOOLE_D (default =ABAP_FALSE)
* | [--->] IV_ASSOCIATION_CHANGED         TYPE        BOOLE_D (default =ABAP_FALSE)
* | [--->] IV_ACTION_CHANGED              TYPE        BOOLE_D (default =ABAP_FALSE)
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD /BOBF/IF_FRW_MODIFY~NOTIFY_PROPERTY_CHANGE.

    mo_original_internal_access->/bobf/if_frw_modify~notify_property_change(
      EXPORTING
        iv_node_key               = iv_node_key
        iv_key                    = iv_key
        it_key                    = it_key
        iv_node_changed           = iv_node_changed
        iv_node_attribute_changed = iv_node_attribute_changed
        iv_association_changed    = iv_association_changed
        iv_action_changed         = iv_action_changed ).

    synchronize_internal_access( ).

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZBOPF_CL_CHANGE_TRACKER_ADN->/BOBF/IF_FRW_MODIFY~UPDATE
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_NODE                        TYPE        /BOBF/OBM_NODE_KEY
* | [--->] IV_KEY                         TYPE        /BOBF/CONF_KEY
* | [--->] IV_ROOT_KEY                    TYPE        /BOBF/CONF_KEY(optional)
* | [--->] IS_DATA                        TYPE REF TO DATA
* | [--->] IT_CHANGED_FIELDS              TYPE        /BOBF/T_FRW_NAME(optional)
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD /BOBF/IF_FRW_MODIFY~UPDATE.

    " delegate to original internal access instance
    mo_original_internal_access->/bobf/if_frw_modify~update(
      EXPORTING
        iv_node            = iv_node
        iv_key             = iv_key
        is_data            = is_data
        it_changed_fields  = it_changed_fields
        iv_root_key        = iv_root_key ).

    synchronize_internal_access( ).

    track_modifications(
      it_modify = VALUE #( (
        change_mode    = /bobf/if_frw_c=>sc_modify_update
        node           = iv_node
        key            = iv_key
        data           = is_data
        changed_fields = it_changed_fields
        root_key       = iv_root_key ) )
    ).

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZBOPF_CL_CHANGE_TRACKER_ADN->/BOBF/IF_FRW_QUERY~QUERY
* +-------------------------------------------------------------------------------------------------+
* | [--->] IS_CTX                         TYPE        /BOBF/S_FRW_CTX_QUERY
* | [--->] IT_FILTER_KEY                  TYPE        /BOBF/T_FRW_KEY(optional)
* | [--->] IT_SELECTION_PARAMETERS        TYPE        /BOBF/T_FRW_QUERY_SELPARAM(optional)
* | [--->] IO_QUERY_AUTHORITIES           TYPE REF TO /BOBF/IF_FRW_AUTHORITY_QUERY(optional)
* | [--->] IS_QUERY_OPTIONS               TYPE        /BOBF/S_FRW_QUERY_OPTIONS(optional)
* | [--->] IO_QUERY                       TYPE REF TO /BOBF/IF_FRW_QUERY
* | [--->] IO_READ                        TYPE REF TO /BOBF/IF_FRW_READ
* | [--->] IO_MODIFY                      TYPE REF TO /BOBF/IF_FRW_MODIFY(optional)
* | [--->] IV_FILL_DATA                   TYPE        BOOLE_D (default =ABAP_FALSE)
* | [--->] IT_REQUESTED_ATTRIBUTES        TYPE        /BOBF/T_FRW_NAME(optional)
* | [<---] EO_MESSAGE                     TYPE REF TO /BOBF/IF_FRW_MESSAGE
* | [<---] ET_KEY                         TYPE        /BOBF/T_FRW_KEY
* | [<---] ES_QUERY_INFO                  TYPE        /BOBF/S_FRW_QUERY_INFO
* | [<---] ET_DATA                        TYPE        INDEX TABLE
* | [!CX!] /BOBF/CX_FRW
* +--------------------------------------------------------------------------------------</SIGNATURE>
METHOD /bobf/if_frw_query~query.

  mo_original_internal_access->/bobf/if_frw_query~query(
    EXPORTING
      is_ctx                  = is_ctx
      it_filter_key           = it_filter_key
      it_selection_parameters = it_selection_parameters
      io_query_authorities    = io_query_authorities
      is_query_options        = is_query_options
      io_query                = io_query
      io_read                 = io_read
      io_modify               = io_modify
      iv_fill_data            = iv_fill_data
      it_requested_attributes = it_requested_attributes
    IMPORTING
      eo_message              = eo_message
      et_key                  = et_key
      es_query_info           = es_query_info
      et_data                 = et_data ).

  synchronize_internal_access( ).

ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZBOPF_CL_CHANGE_TRACKER_ADN->/BOBF/IF_FRW_QUERY~RETRIEVE_DEFAULT_PARAM
* +-------------------------------------------------------------------------------------------------+
* | [--->] IS_CTX                         TYPE        /BOBF/S_FRW_CTX_QUERY
* | [<-->] CT_SELECTION_PARAMETERS        TYPE        /BOBF/T_FRW_QUERY_SELPARAM
* | [!CX!] /BOBF/CX_FRW
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD /BOBF/IF_FRW_QUERY~RETRIEVE_DEFAULT_PARAM.

    mo_original_internal_access->/bobf/if_frw_query~retrieve_default_param(
      EXPORTING
        is_ctx                  = is_ctx
      CHANGING
        ct_selection_parameters = ct_selection_parameters ).

    synchronize_internal_access( ).

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZBOPF_CL_CHANGE_TRACKER_ADN->/BOBF/IF_FRW_READ~COMPARE
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_NODE_KEY                    TYPE        /BOBF/OBM_NODE_KEY
* | [--->] IT_KEY                         TYPE        /BOBF/T_FRW_KEY
* | [--->] IV_FILL_ATTRIBUTES             TYPE        BOOLE_D (default =ABAP_FALSE)
* | [--->] IV_SCOPE                       TYPE        /BOBF/FRW_SCOPE (default =/BOBF/IF_FRW_C=>SC_SCOPE_LOCAL)
* | [<---] EO_CHANGE                      TYPE REF TO /BOBF/IF_FRW_CHANGE
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD /BOBF/IF_FRW_READ~COMPARE.

    mo_original_internal_access->/bobf/if_frw_read~compare(
      EXPORTING
        iv_node_key        = iv_node_key
        it_key             = it_key
        iv_fill_attributes = iv_fill_attributes
        iv_scope           = iv_scope
      IMPORTING
        eo_change          = eo_change ).

    synchronize_internal_access( ).

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZBOPF_CL_CHANGE_TRACKER_ADN->/BOBF/IF_FRW_READ~CONVERT_ALTERN_KEY
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_NODE_KEY                    TYPE        /BOBF/OBM_NODE_KEY
* | [--->] IV_ALTKEY_KEY                  TYPE        /BOBF/OBM_ALTKEY_KEY
* | [--->] IV_TARGET_ALTKEY_KEY           TYPE        /BOBF/OBM_ALTKEY_KEY (default =/BOBF/IF_FRW_C=>SC_ALTERNATIVE_KEY_KEY)
* | [--->] IT_KEY                         TYPE        INDEX TABLE(optional)
* | [--->] IV_BEFORE_IMAGE                TYPE        BOOLE_D (default =ABAP_FALSE)
* | [--->] IV_INVALIDATE_CACHE            TYPE        BOOLE_D (default =ABAP_FALSE)
* | [<---] ET_RESULT                      TYPE        /BOBF/T_FRW_KEYINDEX
* | [<---] ET_KEY                         TYPE        INDEX TABLE
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD /BOBF/IF_FRW_READ~CONVERT_ALTERN_KEY.

    IF et_key IS SUPPLIED.
      mo_original_internal_access->/bobf/if_frw_read~convert_altern_key(
        EXPORTING
          iv_node_key          = iv_node_key
          iv_altkey_key        = iv_altkey_key
          iv_target_altkey_key = iv_target_altkey_key
          it_key               = it_key
          iv_before_image      = iv_before_image
          iv_invalidate_cache  = iv_invalidate_cache
        IMPORTING
          et_result            = et_result
          et_key               = et_key ).
    ELSE.
      mo_original_internal_access->/bobf/if_frw_read~convert_altern_key(
        EXPORTING
          iv_node_key          = iv_node_key
          iv_altkey_key        = iv_altkey_key
          iv_target_altkey_key = iv_target_altkey_key
          it_key               = it_key
          iv_before_image      = iv_before_image
          iv_invalidate_cache  = iv_invalidate_cache
        IMPORTING
          et_result            = et_result ).
    ENDIF.

    synchronize_internal_access( ).

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZBOPF_CL_CHANGE_TRACKER_ADN->/BOBF/IF_FRW_READ~GET_ROOT_KEY
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_NODE                        TYPE        /BOBF/OBM_NODE_KEY
* | [--->] IT_KEY                         TYPE        /BOBF/T_FRW_KEY
* | [--->] IV_BEFORE_IMAGE                TYPE        BOOLE_D (default =ABAP_FALSE)
* | [<---] ET_TARGET_KEY                  TYPE        /BOBF/T_FRW_KEY
* | [<---] ET_KEY_LINK                    TYPE        /BOBF/T_FRW_KEY_LINK
* | [<---] ET_FAILED_KEY                  TYPE        /BOBF/T_FRW_KEY
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD /BOBF/IF_FRW_READ~GET_ROOT_KEY.

    mo_original_internal_access->/bobf/if_frw_read~get_root_key(
      EXPORTING
        iv_node         = iv_node
        it_key          = it_key
        iv_before_image = iv_before_image
      IMPORTING
        et_target_key   = et_target_key
        et_key_link     = et_key_link
        et_failed_key   = et_failed_key ).

    synchronize_internal_access( ).

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZBOPF_CL_CHANGE_TRACKER_ADN->/BOBF/IF_FRW_READ~RETRIEVE
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_NODE                        TYPE        /BOBF/OBM_NODE_KEY
* | [--->] IT_KEY                         TYPE        /BOBF/T_FRW_KEY
* | [--->] IV_BEFORE_IMAGE                TYPE        BOOLE_D (default =ABAP_FALSE)
* | [--->] IV_FILL_DATA                   TYPE        BOOLE_D (default =ABAP_TRUE)
* | [--->] IT_REQUESTED_ATTRIBUTES        TYPE        /BOBF/T_FRW_NAME(optional)
* | [<---] EO_MESSAGE                     TYPE REF TO /BOBF/IF_FRW_MESSAGE
* | [<---] ET_DATA                        TYPE        INDEX TABLE
* | [<---] ET_FAILED_KEY                  TYPE        /BOBF/T_FRW_KEY
* | [<---] ET_NODE_CAT                    TYPE        /BOBF/T_FRW_NODE_CAT
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD /BOBF/IF_FRW_READ~RETRIEVE.

    mo_original_internal_access->/bobf/if_frw_read~retrieve(
      EXPORTING
        iv_node                 = iv_node
        it_key                  = it_key
        iv_before_image         = iv_before_image
        iv_fill_data            = iv_fill_data
        it_requested_attributes = it_requested_attributes
      IMPORTING
        eo_message              = eo_message
        et_data                 = et_data
        et_failed_key           = et_failed_key
        et_node_cat             = et_node_cat ).

    synchronize_internal_access( ).

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZBOPF_CL_CHANGE_TRACKER_ADN->/BOBF/IF_FRW_READ~RETRIEVE_BY_ASSOCIATION
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_NODE                        TYPE        /BOBF/OBM_NODE_KEY
* | [--->] IT_KEY                         TYPE        /BOBF/T_FRW_KEY
* | [--->] IV_ASSOCIATION                 TYPE        /BOBF/OBM_ASSOC_KEY
* | [--->] IS_PARAMETERS                  TYPE REF TO DATA(optional)
* | [--->] IT_FILTERED_ATTRIBUTES         TYPE        /BOBF/T_FRW_NAME(optional)
* | [--->] IV_FILL_DATA                   TYPE        BOOLE_D (default =ABAP_FALSE)
* | [--->] IV_BEFORE_IMAGE                TYPE        BOOLE_D (default =ABAP_FALSE)
* | [--->] IT_REQUESTED_ATTRIBUTES        TYPE        /BOBF/T_FRW_NAME(optional)
* | [<---] EO_MESSAGE                     TYPE REF TO /BOBF/IF_FRW_MESSAGE
* | [<---] ET_DATA                        TYPE        INDEX TABLE
* | [<---] ET_KEY_LINK                    TYPE        /BOBF/T_FRW_KEY_LINK
* | [<---] ET_TARGET_KEY                  TYPE        /BOBF/T_FRW_KEY
* | [<---] ET_FAILED_KEY                  TYPE        /BOBF/T_FRW_KEY
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD /BOBF/IF_FRW_READ~RETRIEVE_BY_ASSOCIATION.

    mo_original_internal_access->/bobf/if_frw_read~retrieve_by_association(
      EXPORTING
        iv_node                 = iv_node
        it_key                  = it_key
        iv_association          = iv_association
        is_parameters           = is_parameters
        it_filtered_attributes  = it_filtered_attributes
        iv_fill_data            = iv_fill_data
        iv_before_image         = iv_before_image
        it_requested_attributes = it_requested_attributes
      IMPORTING
        eo_message              = eo_message
        et_data                 = et_data
        et_key_link             = et_key_link
        et_target_key           = et_target_key
        et_failed_key           = et_failed_key ).

    synchronize_internal_access( ).

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZBOPF_CL_CHANGE_TRACKER_ADN->/BOBF/IF_TRA_SERVICE_MANAGER~CHECK_ACTION
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_ACT_KEY                     TYPE        /BOBF/ACT_KEY
* | [--->] IT_KEY                         TYPE        /BOBF/T_FRW_KEY
* | [--->] IS_PARAMETERS                  TYPE REF TO DATA(optional)
* | [<---] EO_MESSAGE                     TYPE REF TO /BOBF/IF_FRW_MESSAGE
* | [<---] ET_FAILED_KEY                  TYPE        /BOBF/T_FRW_KEY
* | [<---] ET_FAILED_ACTION_KEY           TYPE        /BOBF/T_FRW_KEY
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD /BOBF/IF_TRA_SERVICE_MANAGER~CHECK_ACTION.

    mo_original_service_manager->/bobf/if_tra_service_manager~check_action(
      EXPORTING
        iv_act_key           = iv_act_key
        it_key               = it_key
        is_parameters        = is_parameters
      IMPORTING
        eo_message           = eo_message
        et_failed_key        = et_failed_key
        et_failed_action_key = et_failed_action_key ).

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZBOPF_CL_CHANGE_TRACKER_ADN->/BOBF/IF_TRA_SERVICE_MANAGER~CHECK_AND_DETERMINE
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_NODE_KEY                    TYPE        /BOBF/OBM_NODE_KEY
* | [--->] IT_KEY                         TYPE        /BOBF/T_FRW_KEY
* | [--->] IV_CHECK_SCOPE                 TYPE        /BOBF/FRW_SCOPE
* | [<---] EO_CHANGE                      TYPE REF TO /BOBF/IF_TRA_CHANGE
* | [<---] EO_MESSAGE                     TYPE REF TO /BOBF/IF_FRW_MESSAGE
* | [<---] EV_REJECTED                    TYPE        BOOLE_D
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD /BOBF/IF_TRA_SERVICE_MANAGER~CHECK_AND_DETERMINE.

    mo_original_service_manager->/bobf/if_tra_service_manager~check_and_determine(
      EXPORTING
        iv_node_key    = iv_node_key
        it_key         = it_key
        iv_check_scope = iv_check_scope
      IMPORTING
        eo_change      = eo_change
        eo_message     = eo_message
        ev_rejected    = ev_rejected ).

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZBOPF_CL_CHANGE_TRACKER_ADN->/BOBF/IF_TRA_SERVICE_MANAGER~CHECK_CONSISTENCY
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_NODE_KEY                    TYPE        /BOBF/OBM_NODE_KEY
* | [--->] IT_KEY                         TYPE        /BOBF/T_FRW_KEY
* | [--->] IV_CHECK_SCOPE                 TYPE        /BOBF/FRW_SCOPE
* | [--->] IV_CHECK_GROUP                 TYPE        /BOBF/OBM_GROUP_KEY(optional)
* | [<---] EO_MESSAGE                     TYPE REF TO /BOBF/IF_FRW_MESSAGE
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD /BOBF/IF_TRA_SERVICE_MANAGER~CHECK_CONSISTENCY.

    mo_original_service_manager->/bobf/if_tra_service_manager~check_consistency(
      EXPORTING
        iv_node_key    = iv_node_key
        it_key         = it_key
        iv_check_scope = iv_check_scope
        iv_check_group = iv_check_group
      IMPORTING
        eo_message     = eo_message ).

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZBOPF_CL_CHANGE_TRACKER_ADN->/BOBF/IF_TRA_SERVICE_MANAGER~CONVERT_ALTERN_KEY
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_NODE_KEY                    TYPE        /BOBF/OBM_NODE_KEY
* | [--->] IV_ALTKEY_KEY                  TYPE        /BOBF/OBM_ALTKEY_KEY (default =/BOBF/IF_FRW_C=>SC_ALTERNATIVE_KEY_KEY)
* | [--->] IV_TARGET_ALTKEY_KEY           TYPE        /BOBF/OBM_ALTKEY_KEY (default =/BOBF/IF_FRW_C=>SC_ALTERNATIVE_KEY_KEY)
* | [--->] IT_KEY                         TYPE        INDEX TABLE
* | [--->] IV_CHECK_EXISTENCE             TYPE        BOOLE_D (default =ABAP_FALSE)
* | [--->] IV_BEFORE_IMAGE                TYPE        BOOLE_D (default =ABAP_FALSE)
* | [--->] IV_INVALIDATE_CACHE            TYPE        BOOLE_D (default =ABAP_FALSE)
* | [<---] EO_CHANGE                      TYPE REF TO /BOBF/IF_TRA_CHANGE
* | [<---] ET_RESULT                      TYPE        /BOBF/T_FRW_KEYINDEX
* | [<---] ET_KEY                         TYPE        INDEX TABLE
* | [<---] EO_MESSAGE                     TYPE REF TO /BOBF/IF_FRW_MESSAGE
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD /BOBF/IF_TRA_SERVICE_MANAGER~CONVERT_ALTERN_KEY.

    IF et_key IS SUPPLIED.
      mo_original_service_manager->/bobf/if_tra_service_manager~convert_altern_key(
        EXPORTING
          iv_node_key          = iv_node_key
          iv_altkey_key        = iv_altkey_key
          iv_target_altkey_key = iv_target_altkey_key
          it_key               = it_key
          iv_check_existence   = iv_check_existence
          iv_before_image      = iv_before_image
          iv_invalidate_cache  = iv_invalidate_cache
        IMPORTING
          eo_change            = eo_change
          et_result            = et_result
          et_key               = et_key ).

    ELSE.
      mo_original_service_manager->/bobf/if_tra_service_manager~convert_altern_key(
        EXPORTING
          iv_node_key          = iv_node_key
          iv_altkey_key        = iv_altkey_key
          iv_target_altkey_key = iv_target_altkey_key
          it_key               = it_key
          iv_check_existence   = iv_check_existence
          iv_before_image      = iv_before_image
          iv_invalidate_cache  = iv_invalidate_cache
        IMPORTING
          eo_change            = eo_change
          et_result            = et_result ).
    ENDIF.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZBOPF_CL_CHANGE_TRACKER_ADN->/BOBF/IF_TRA_SERVICE_MANAGER~DO_ACTION
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_ACT_KEY                     TYPE        /BOBF/ACT_KEY
* | [--->] IT_KEY                         TYPE        /BOBF/T_FRW_KEY(optional)
* | [--->] IS_PARAMETERS                  TYPE REF TO DATA(optional)
* | [<---] EO_CHANGE                      TYPE REF TO /BOBF/IF_TRA_CHANGE
* | [<---] EO_MESSAGE                     TYPE REF TO /BOBF/IF_FRW_MESSAGE
* | [<---] ET_FAILED_KEY                  TYPE        /BOBF/T_FRW_KEY
* | [<---] ET_FAILED_ACTION_KEY           TYPE        /BOBF/T_FRW_KEY
* | [<---] ET_DATA                        TYPE        INDEX TABLE
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD /BOBF/IF_TRA_SERVICE_MANAGER~DO_ACTION.

    mo_original_service_manager->/bobf/if_tra_service_manager~do_action(
      EXPORTING
        iv_act_key           = iv_act_key
        it_key               = it_key
        is_parameters        = is_parameters
      IMPORTING
        eo_change            = eo_change
        eo_message           = eo_message
        et_failed_key        = et_failed_key
        et_failed_action_key = et_failed_action_key
        et_data              = et_data ).

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZBOPF_CL_CHANGE_TRACKER_ADN->/BOBF/IF_TRA_SERVICE_MANAGER~GET_NEW_KEY
* +-------------------------------------------------------------------------------------------------+
* | [<---] EO_MESSAGE                     TYPE REF TO /BOBF/IF_FRW_MESSAGE
* | [<-()] EV_KEY                         TYPE        /BOBF/CONF_KEY
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD /BOBF/IF_TRA_SERVICE_MANAGER~GET_NEW_KEY.

    ev_key = mo_original_service_manager->/bobf/if_tra_service_manager~get_new_key(  ).

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZBOPF_CL_CHANGE_TRACKER_ADN->/BOBF/IF_TRA_SERVICE_MANAGER~MODIFY
* +-------------------------------------------------------------------------------------------------+
* | [--->] IT_MODIFICATION                TYPE        /BOBF/T_FRW_MODIFICATION
* | [<---] EO_CHANGE                      TYPE REF TO /BOBF/IF_TRA_CHANGE
* | [<---] EO_MESSAGE                     TYPE REF TO /BOBF/IF_FRW_MESSAGE
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD /BOBF/IF_TRA_SERVICE_MANAGER~MODIFY.

    " delegate modify to original service manager
    mo_original_service_manager->/bobf/if_tra_service_manager~modify(
      EXPORTING
        it_modification = it_modification
      IMPORTING
        eo_change       = eo_change
        eo_message      = eo_message ).

    track_modifications( it_modify = it_modification ).

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZBOPF_CL_CHANGE_TRACKER_ADN->/BOBF/IF_TRA_SERVICE_MANAGER~QUERY
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_QUERY_KEY                   TYPE        /BOBF/OBM_QUERY_KEY
* | [--->] IT_FILTER_KEY                  TYPE        /BOBF/T_FRW_KEY(optional)
* | [--->] IT_SELECTION_PARAMETERS        TYPE        /BOBF/T_FRW_QUERY_SELPARAM(optional)
* | [--->] IS_QUERY_OPTIONS               TYPE        /BOBF/S_FRW_QUERY_OPTIONS(optional)
* | [--->] IV_FILL_DATA                   TYPE        BOOLE_D (default =ABAP_FALSE)
* | [--->] IT_REQUESTED_ATTRIBUTES        TYPE        /BOBF/T_FRW_NAME(optional)
* | [<---] EO_MESSAGE                     TYPE REF TO /BOBF/IF_FRW_MESSAGE
* | [<---] ES_QUERY_INFO                  TYPE        /BOBF/S_FRW_QUERY_INFO
* | [<---] ET_DATA                        TYPE        INDEX TABLE
* | [<---] ET_KEY                         TYPE        /BOBF/T_FRW_KEY
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD /BOBF/IF_TRA_SERVICE_MANAGER~QUERY.

    mo_original_service_manager->/bobf/if_tra_service_manager~query(
      EXPORTING
        iv_query_key            = iv_query_key
        it_filter_key           = it_filter_key
        it_selection_parameters = it_selection_parameters
        is_query_options        = is_query_options
        iv_fill_data            = iv_fill_data
        it_requested_attributes = it_requested_attributes
      IMPORTING
        eo_message              = eo_message
        es_query_info           = es_query_info
        et_data                 = et_data
        et_key                  = et_key ).

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZBOPF_CL_CHANGE_TRACKER_ADN->/BOBF/IF_TRA_SERVICE_MANAGER~RETRIEVE
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_NODE_KEY                    TYPE        /BOBF/OBM_NODE_KEY
* | [--->] IT_KEY                         TYPE        /BOBF/T_FRW_KEY
* | [--->] IV_BEFORE_IMAGE                TYPE        BOOLE_D (default =ABAP_FALSE)
* | [--->] IV_EDIT_MODE                   TYPE        /BOBF/CONF_EDIT_MODE (default =/BOBF/IF_CONF_C=>SC_EDIT_READ_ONLY)
* | [--->] IV_FILL_DATA                   TYPE        BOOLE_D (default =ABAP_TRUE)
* | [--->] IV_INVALIDATE_CACHE            TYPE        BOOLE_D (default =ABAP_FALSE)
* | [--->] IT_REQUESTED_ATTRIBUTES        TYPE        /BOBF/T_FRW_NAME(optional)
* | [<---] EO_MESSAGE                     TYPE REF TO /BOBF/IF_FRW_MESSAGE
* | [<---] EO_CHANGE                      TYPE REF TO /BOBF/IF_TRA_CHANGE
* | [<---] ET_DATA                        TYPE        INDEX TABLE
* | [<---] ET_FAILED_KEY                  TYPE        /BOBF/T_FRW_KEY
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD /BOBF/IF_TRA_SERVICE_MANAGER~RETRIEVE.

    mo_original_service_manager->/bobf/if_tra_service_manager~retrieve(
      EXPORTING
        iv_node_key             = iv_node_key
        it_key                  = it_key
        iv_before_image         = iv_before_image
        iv_edit_mode            = iv_edit_mode
        iv_fill_data            = iv_fill_data
        iv_invalidate_cache     = iv_invalidate_cache
        it_requested_attributes = it_requested_attributes
      IMPORTING
        eo_message              = eo_message
        eo_change               = eo_change
        et_data                 = et_data
        et_failed_key           = et_failed_key ).

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZBOPF_CL_CHANGE_TRACKER_ADN->/BOBF/IF_TRA_SERVICE_MANAGER~RETRIEVE_BY_ASSOCIATION
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_NODE_KEY                    TYPE        /BOBF/OBM_NODE_KEY
* | [--->] IT_KEY                         TYPE        /BOBF/T_FRW_KEY
* | [--->] IV_ASSOCIATION                 TYPE        /BOBF/OBM_ASSOC_KEY
* | [--->] IS_PARAMETERS                  TYPE REF TO DATA(optional)
* | [--->] IT_FILTERED_ATTRIBUTES         TYPE        /BOBF/T_FRW_NAME(optional)
* | [--->] IV_FILL_DATA                   TYPE        BOOLE_D (default =ABAP_FALSE)
* | [--->] IV_BEFORE_IMAGE                TYPE        BOOLE_D (default =ABAP_FALSE)
* | [--->] IV_INVALIDATE_CACHE            TYPE        BOOLE_D (default =ABAP_FALSE)
* | [--->] IV_EDIT_MODE                   TYPE        /BOBF/CONF_EDIT_MODE (default =/BOBF/IF_CONF_C=>SC_EDIT_READ_ONLY)
* | [--->] IT_REQUESTED_ATTRIBUTES        TYPE        /BOBF/T_FRW_NAME(optional)
* | [<---] EO_MESSAGE                     TYPE REF TO /BOBF/IF_FRW_MESSAGE
* | [<---] EO_CHANGE                      TYPE REF TO /BOBF/IF_TRA_CHANGE
* | [<---] ET_DATA                        TYPE        INDEX TABLE
* | [<---] ET_KEY_LINK                    TYPE        /BOBF/T_FRW_KEY_LINK
* | [<---] ET_TARGET_KEY                  TYPE        /BOBF/T_FRW_KEY
* | [<---] ET_FAILED_KEY                  TYPE        /BOBF/T_FRW_KEY
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD /BOBF/IF_TRA_SERVICE_MANAGER~RETRIEVE_BY_ASSOCIATION.

    mo_original_service_manager->/bobf/if_tra_service_manager~retrieve_by_association(
      EXPORTING
        iv_node_key             = iv_node_key
        it_key                  = it_key
        iv_association          = iv_association
        is_parameters           = is_parameters
        it_filtered_attributes  = it_filtered_attributes
        iv_fill_data            = iv_fill_data
        iv_before_image         = iv_before_image
        iv_invalidate_cache     = iv_invalidate_cache
        iv_edit_mode            = iv_edit_mode
        it_requested_attributes = it_requested_attributes
      IMPORTING
        eo_message              = eo_message
        eo_change               = eo_change
        et_data                 = et_data
        et_key_link             = et_key_link
        et_target_key           = et_target_key
        et_failed_key           = et_failed_key ).

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZBOPF_CL_CHANGE_TRACKER_ADN->/BOBF/IF_TRA_SERVICE_MANAGER~RETRIEVE_CODE_VALUE_SET
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_VSET_KEY                    TYPE        /BOBF/OBM_VALUE_SET_KEY
* | [--->] IT_KEY                         TYPE        /BOBF/T_FRW_KEY(optional)
* | [--->] IV_KEY_IS_DEFAULT              TYPE        BOOLE_D (default =ABAP_FALSE)
* | [--->] IS_PARAMETERS                  TYPE REF TO DATA(optional)
* | [--->] IT_ATTRIBUTES                  TYPE        /BOBF/T_FRW_NAME(optional)
* | [<---] ET_CODE_VALUES                 TYPE        /BOBF/T_FRW_CODE_VALUES
* | [<---] EO_MESSAGE                     TYPE REF TO /BOBF/IF_FRW_MESSAGE
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD /BOBF/IF_TRA_SERVICE_MANAGER~RETRIEVE_CODE_VALUE_SET.

    mo_original_service_manager->/bobf/if_tra_service_manager~retrieve_code_value_set(
      EXPORTING
        iv_vset_key       = iv_vset_key
        it_key            = it_key
        iv_key_is_default = iv_key_is_default
        is_parameters     = is_parameters
        it_attributes     = it_attributes
      IMPORTING
        et_code_values    = et_code_values ).

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZBOPF_CL_CHANGE_TRACKER_ADN->/BOBF/IF_TRA_SERVICE_MANAGER~RETRIEVE_DEFAULT_ACTION_PARAM
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_ACT_KEY                     TYPE        /BOBF/ACT_KEY
* | [--->] IT_KEY                         TYPE        /BOBF/T_FRW_KEY
* | [<---] EO_MESSAGE                     TYPE REF TO /BOBF/IF_FRW_MESSAGE
* | [<-->] CS_PARAMETERS                  TYPE REF TO DATA
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD /BOBF/IF_TRA_SERVICE_MANAGER~RETRIEVE_DEFAULT_ACTION_PARAM.

    mo_original_service_manager->/bobf/if_tra_service_manager~retrieve_default_action_param(
      EXPORTING
        iv_act_key    = iv_act_key
        it_key        = it_key
      CHANGING
        cs_parameters = cs_parameters ).

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZBOPF_CL_CHANGE_TRACKER_ADN->/BOBF/IF_TRA_SERVICE_MANAGER~RETRIEVE_DEFAULT_NODE_VALUES
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_NODE_KEY                    TYPE        /BOBF/OBM_NODE_KEY
* | [--->] IV_SOURCE_KEY                  TYPE        /BOBF/CONF_KEY(optional)
* | [--->] IV_ASSOC_KEY                   TYPE        /BOBF/OBM_ASSOC_KEY(optional)
* | [--->] IS_ASSOC_PARAMETERS            TYPE REF TO DATA(optional)
* | [--->] IT_ASSOC_FILTERED_ATTRIBUTES   TYPE        /BOBF/T_FRW_NAME(optional)
* | [<---] EO_MESSAGE                     TYPE REF TO /BOBF/IF_FRW_MESSAGE
* | [<---] EO_PROPERTY                    TYPE REF TO /BOBF/IF_FRW_PROPERTY
* | [<-->] CT_DATA                        TYPE        INDEX TABLE
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD /BOBF/IF_TRA_SERVICE_MANAGER~RETRIEVE_DEFAULT_NODE_VALUES.

    mo_original_service_manager->/bobf/if_tra_service_manager~retrieve_default_node_values(
      EXPORTING
        iv_node_key                  = iv_node_key
        iv_source_key                = iv_source_key
        iv_assoc_key                 = iv_assoc_key
        is_assoc_parameters          = is_assoc_parameters
        it_assoc_filtered_attributes = it_assoc_filtered_attributes
      IMPORTING
        eo_message                   = eo_message
        eo_property                  = eo_property
      CHANGING
        ct_data                      = ct_data ).

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZBOPF_CL_CHANGE_TRACKER_ADN->/BOBF/IF_TRA_SERVICE_MANAGER~RETRIEVE_DEFAULT_QUERY_PARAM
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_QUERY_KEY                   TYPE        /BOBF/OBM_QUERY_KEY
* | [<---] EO_MESSAGE                     TYPE REF TO /BOBF/IF_FRW_MESSAGE
* | [<-->] CT_SELECTION_PARAMETERS        TYPE        /BOBF/T_FRW_QUERY_SELPARAM
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD /BOBF/IF_TRA_SERVICE_MANAGER~RETRIEVE_DEFAULT_QUERY_PARAM.

    mo_original_service_manager->/bobf/if_tra_service_manager~retrieve_default_query_param(
      EXPORTING
        iv_query_key            = iv_query_key
      CHANGING
        ct_selection_parameters = ct_selection_parameters ).

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZBOPF_CL_CHANGE_TRACKER_ADN->/BOBF/IF_TRA_SERVICE_MANAGER~RETRIEVE_PROPERTY
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_NODE_KEY                    TYPE        /BOBF/OBM_NODE_KEY
* | [--->] IT_KEY                         TYPE        /BOBF/T_FRW_KEY
* | [--->] IV_KEY_IS_DEFAULT              TYPE        BOOLE_D (default =ABAP_FALSE)
* | [--->] IV_NODE_PROPERTY               TYPE        BOOLE_D (default =ABAP_TRUE)
* | [--->] IV_NODE_ATTRIBUTE_PROPERTY     TYPE        BOOLE_D (default =ABAP_TRUE)
* | [--->] IT_NODE_ATTRIBUTE              TYPE        /BOBF/T_FRW_NAME(optional)
* | [--->] IV_ASSOC_PROPERTY              TYPE        BOOLE_D (default =ABAP_TRUE)
* | [--->] IT_ASSOC                       TYPE        /BOBF/T_FRW_KEY2(optional)
* | [--->] IV_ASSOC_ATTRIBUTE_PROPERTY    TYPE        BOOLE_D (default =ABAP_TRUE)
* | [--->] IV_ACTION_PROPERTY             TYPE        BOOLE_D (default =ABAP_TRUE)
* | [--->] IT_ACTION                      TYPE        /BOBF/T_FRW_KEY2(optional)
* | [--->] IV_ACTION_ATTRIBUTE_PROPERTY   TYPE        BOOLE_D (default =ABAP_TRUE)
* | [--->] IV_QUERY_PROPERTY              TYPE        BOOLE_D (default =ABAP_TRUE)
* | [--->] IT_QUERY                       TYPE        /BOBF/T_FRW_KEY2(optional)
* | [--->] IV_QUERY_ATTRIBUTE_PROPERTY    TYPE        BOOLE_D (default =ABAP_TRUE)
* | [<---] EO_PROPERTY                    TYPE REF TO /BOBF/IF_FRW_PROPERTY
* | [<---] EO_MESSAGE                     TYPE REF TO /BOBF/IF_FRW_MESSAGE
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD /BOBF/IF_TRA_SERVICE_MANAGER~RETRIEVE_PROPERTY.

    mo_original_service_manager->/bobf/if_tra_service_manager~retrieve_property(
      EXPORTING
        iv_node_key                  = iv_node_key
        it_key                       = it_key
        iv_key_is_default            = iv_key_is_default
        iv_node_property             = iv_node_property
        iv_node_attribute_property   = iv_node_attribute_property
        it_node_attribute            = it_node_attribute
        iv_assoc_property            = iv_assoc_property
        it_assoc                     = it_assoc
        iv_assoc_attribute_property  = iv_assoc_attribute_property
        iv_action_property           = iv_action_property
        it_action                    = it_action
        iv_action_attribute_property = iv_action_attribute_property
        iv_query_property            = iv_query_property
        it_query                     = it_query
        iv_query_attribute_property  = iv_query_attribute_property
      IMPORTING
        eo_property                  = eo_property
        eo_message                   = eo_message ).

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZBOPF_CL_CHANGE_TRACKER_ADN->/BOBF/IF_TRA_SERV_MGR_TRANSACT~ADJUST_NUMBERS
* +-------------------------------------------------------------------------------------------------+
* | [<---] EO_CHANGE                      TYPE REF TO /BOBF/IF_TRA_CHANGE
* | [<---] EO_MESSAGE                     TYPE REF TO /BOBF/IF_FRW_MESSAGE
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD /BOBF/IF_TRA_SERV_MGR_TRANSACT~ADJUST_NUMBERS.

    mo_original_service_manager->/bobf/if_tra_serv_mgr_transact~adjust_numbers(
      IMPORTING
        eo_change  = eo_change
        eo_message = eo_message ).

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZBOPF_CL_CHANGE_TRACKER_ADN->/BOBF/IF_TRA_SERV_MGR_TRANSACT~AFTER_FAILED_SAVE
* +-------------------------------------------------------------------------------------------------+
* | [<---] EO_MESSAGE                     TYPE REF TO /BOBF/IF_FRW_MESSAGE
* | [<---] EO_CHANGE                      TYPE REF TO /BOBF/IF_TRA_CHANGE
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD /BOBF/IF_TRA_SERV_MGR_TRANSACT~AFTER_FAILED_SAVE.

    mo_original_service_manager->/bobf/if_tra_serv_mgr_transact~after_failed_save(
      IMPORTING
        eo_message = eo_message
        eo_change  = eo_change ).

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZBOPF_CL_CHANGE_TRACKER_ADN->/BOBF/IF_TRA_SERV_MGR_TRANSACT~AFTER_SUCCESSFUL_SAVE
* +-------------------------------------------------------------------------------------------------+
* | [<---] EO_MESSAGE                     TYPE REF TO /BOBF/IF_FRW_MESSAGE
* | [<---] EO_CHANGE                      TYPE REF TO /BOBF/IF_TRA_CHANGE
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD /BOBF/IF_TRA_SERV_MGR_TRANSACT~AFTER_SUCCESSFUL_SAVE.

    mo_original_service_manager->/bobf/if_tra_serv_mgr_transact~after_successful_save(
      IMPORTING
        eo_message = eo_message
        eo_change  = eo_change ).

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZBOPF_CL_CHANGE_TRACKER_ADN->/BOBF/IF_TRA_SERV_MGR_TRANSACT~CHECK_BEFORE_SAVE
* +-------------------------------------------------------------------------------------------------+
* | [<---] EO_MESSAGE                     TYPE REF TO /BOBF/IF_FRW_MESSAGE
* | [<---] EV_REJECTED                    TYPE        BOOLE_D
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD /BOBF/IF_TRA_SERV_MGR_TRANSACT~CHECK_BEFORE_SAVE.

    mo_original_service_manager->/bobf/if_tra_serv_mgr_transact~check_before_save(
      IMPORTING
        eo_message  = eo_message
        ev_rejected = ev_rejected ).

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZBOPF_CL_CHANGE_TRACKER_ADN->/BOBF/IF_TRA_SERV_MGR_TRANSACT~DO_CLEANUP
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_CLEANUP_MODE                TYPE        /BOBF/CONF_CLEANUP_MODE(optional)
* | [<---] EO_MESSAGE                     TYPE REF TO /BOBF/IF_FRW_MESSAGE
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD /BOBF/IF_TRA_SERV_MGR_TRANSACT~DO_CLEANUP.

    mo_original_service_manager->/bobf/if_tra_serv_mgr_transact~do_cleanup(
      EXPORTING
        iv_cleanup_mode = iv_cleanup_mode
      IMPORTING
        eo_message      = eo_message ).

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZBOPF_CL_CHANGE_TRACKER_ADN->/BOBF/IF_TRA_SERV_MGR_TRANSACT~DO_SAVE
* +-------------------------------------------------------------------------------------------------+
* | [<---] EO_MESSAGE                     TYPE REF TO /BOBF/IF_FRW_MESSAGE
* | [<---] EV_REJECTED                    TYPE        BOOLE_D
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD /BOBF/IF_TRA_SERV_MGR_TRANSACT~DO_SAVE.

    mo_original_service_manager->/bobf/if_tra_serv_mgr_transact~do_save(
      IMPORTING
        eo_message  = eo_message
        ev_rejected = ev_rejected ).

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZBOPF_CL_CHANGE_TRACKER_ADN->/BOBF/IF_TRA_SERV_MGR_TRANSACT~FINALIZE
* +-------------------------------------------------------------------------------------------------+
* | [<---] EO_MESSAGE                     TYPE REF TO /BOBF/IF_FRW_MESSAGE
* | [<---] EO_CHANGE                      TYPE REF TO /BOBF/IF_TRA_CHANGE
* | [<---] EV_REJECTED                    TYPE        BOOLE_D
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD /BOBF/IF_TRA_SERV_MGR_TRANSACT~FINALIZE.

    mo_original_service_manager->/bobf/if_tra_serv_mgr_transact~finalize(
      IMPORTING
        eo_message  = eo_message
        eo_change   = eo_change
        ev_rejected = ev_rejected ).

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZBOPF_CL_CHANGE_TRACKER_ADN->/BOBF/IF_TRA_SERV_MGR_TRANSACT~ON_BUSINESS_OBJECT_CHANGED
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_PUBLISHING_BO_KEY           TYPE        /BOBF/OBM_BO_KEY
* | [--->] IO_CHANGE                      TYPE REF TO /BOBF/IF_FRW_CHANGE
* | [<---] EO_CHANGE                      TYPE REF TO /BOBF/IF_TRA_CHANGE
* | [<---] EO_MESSAGE                     TYPE REF TO /BOBF/IF_FRW_MESSAGE
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD /BOBF/IF_TRA_SERV_MGR_TRANSACT~ON_BUSINESS_OBJECT_CHANGED.

    mo_original_service_manager->/bobf/if_tra_serv_mgr_transact~on_business_object_changed(
      EXPORTING
        iv_publishing_bo_key = iv_publishing_bo_key
        io_change            = io_change
      IMPORTING
        eo_change            = eo_change
        eo_message           = eo_message ).

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZBOPF_CL_CHANGE_TRACKER_ADN->/BOBF/IF_TRA_SERV_MGR_TRANSACT~ON_NUMBERS_ADJUSTED
* +-------------------------------------------------------------------------------------------------+
* | [--->] IO_CHANGE                      TYPE REF TO /BOBF/IF_TRA_CHANGE
* | [<---] EO_MESSAGE                     TYPE REF TO /BOBF/IF_FRW_MESSAGE
* | [<---] EO_CHANGE                      TYPE REF TO /BOBF/IF_TRA_CHANGE
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD /BOBF/IF_TRA_SERV_MGR_TRANSACT~ON_NUMBERS_ADJUSTED.

    mo_original_service_manager->/bobf/if_tra_serv_mgr_transact~on_numbers_adjusted(
      EXPORTING
        io_change  = io_change
      IMPORTING
        eo_message = eo_message
        eo_change  = eo_change ).

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZBOPF_CL_CHANGE_TRACKER_ADN=>ACTIVATE_ADDON
* +-------------------------------------------------------------------------------------------------+
* | [--->] IO_BREAKPOINT_HANDLER          TYPE REF TO /BOBF/IF_TOOL_BREAKPOINT_HNDL(optional)
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD ACTIVATE_ADDON.
    go_breakpoint_handler = io_breakpoint_handler.
    TRY.
        /bobf/cl_frw_addon=>activate_addon(
          iv_addon_name = '/BOBF/TOOL_DEBUG_ADDON'
          iv_int_access = abap_true
          iv_serv_mgr   = abap_true  ).
      CATCH /bobf/cx_frw.
        ASSERT 1 = 0.
    ENDTRY.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Private Method ZBOPF_CL_CHANGE_TRACKER_ADN=>CHECK_TEST_UI
* +-------------------------------------------------------------------------------------------------+
* | [<-()] RV_IS_IN_TEST_UI               TYPE        BOOLEAN
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD check_test_ui.

    DATA:
      lt_callstack TYPE abap_callstack.

    CALL FUNCTION 'SYSTEM_CALLSTACK'
      EXPORTING
        max_level = 0
      IMPORTING
        callstack = lt_callstack.

    rv_is_in_test_ui = abap_false.
    LOOP AT lt_callstack ASSIGNING FIELD-SYMBOL(<fs_callstack>).
      IF <fs_callstack>-mainprogram = '/BOBF/TEST_UI'.
        rv_is_in_test_ui = abap_true.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZBOPF_CL_CHANGE_TRACKER_ADN->CHECK_WATCHPOINT_ON_CREATE
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_NODE_KEY                    TYPE        /BOBF/OBM_NODE_KEY
* | [<-()] RV_BREAK                       TYPE        BOOLE_D
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD CHECK_WATCHPOINT_ON_CREATE.

    READ TABLE gt_watchpoint ASSIGNING FIELD-SYMBOL(<ls_watchpoint>)
       WITH TABLE KEY
         bo_key = mv_bo_key
         node_key = iv_node_key.

    rv_break = boolc( sy-subrc = 0 AND <ls_watchpoint>-on_create = abap_true ).

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZBOPF_CL_CHANGE_TRACKER_ADN->CHECK_WATCHPOINT_ON_DELETE
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_NODE_KEY                    TYPE        /BOBF/OBM_NODE_KEY
* | [<-()] RV_BREAK                       TYPE        BOOLE_D
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD CHECK_WATCHPOINT_ON_DELETE.

    READ TABLE gt_watchpoint ASSIGNING FIELD-SYMBOL(<ls_watchpoint>)
       WITH TABLE KEY
         bo_key = mv_bo_key
         node_key = iv_node_key.

    rv_break = boolc( sy-subrc = 0 AND <ls_watchpoint>-on_delete = abap_true ).

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZBOPF_CL_CHANGE_TRACKER_ADN->CHECK_WATCHPOINT_ON_UPDATE
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_NODE_KEY                    TYPE        /BOBF/OBM_NODE_KEY
* | [<-()] RV_BREAK                       TYPE        BOOLE_D
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD CHECK_WATCHPOINT_ON_UPDATE.

    READ TABLE gt_watchpoint ASSIGNING FIELD-SYMBOL(<ls_watchpoint>)
       WITH TABLE KEY
         bo_key = mv_bo_key
         node_key = iv_node_key.

    rv_break = boolc( sy-subrc = 0 AND <ls_watchpoint>-on_update = abap_true ).

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZBOPF_CL_CHANGE_TRACKER_ADN->CONSTRUCTOR
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_BO_KEY                      TYPE        /BOBF/OBM_BO_KEY(optional)
* | [--->] IO_INTERNAL_ACCESS             TYPE REF TO /BOBF/CL_FRW_INT_ACCESS(optional)
* | [--->] IO_SERVICE_MANAGER             TYPE REF TO /BOBF/IF_TRA_SERV_MGR_COMPL(optional)
* | [--->] IO_MESSAGE                     TYPE REF TO /BOBF/IF_FRW_MESSAGE(optional)
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD constructor.

    DATA:
      lo_bopf   TYPE REF TO /bobf/cl_frw,
      lo_conf   TYPE REF TO /bobf/if_frw_configuration,
      lo_buffer TYPE REF TO /bobf/if_frw_buffer,
      lo_change TYPE REF TO /bobf/if_frw_change.

    super->constructor(
      EXPORTING
        io_bopf   = lo_bopf
        io_conf   = lo_conf
        io_buffer = lo_buffer
        io_change = lo_change ).

    " store this addon instance in the message object table
    IF io_message IS BOUND.
      mo_original_message = io_message.
    ENDIF.

    " store this addon instance in the static service manager table
    IF io_service_manager IS BOUND.
      mo_original_service_manager = io_service_manager.
    ENDIF.

    " store this addon instance in the static internal access table
    IF io_internal_access IS BOUND.
      mo_original_internal_access = io_internal_access.
      " set the reference to this addon, as the int access uses "me" to read the callstack
      mo_original_internal_access->mr_addon_reference = me.
    ENDIF.

    " store bo key
    mv_bo_key = iv_bo_key.

    " encapsulate the original service manager instance
    mo_original_service_manager = io_service_manager.
    mo_original_internal_access = io_internal_access.

    setup_change_tracker_settings( ).

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZBOPF_CL_CHANGE_TRACKER_ADN=>COUNT_WATCHPOINTS
* +-------------------------------------------------------------------------------------------------+
* | [<-()] RV_COUNT                       TYPE        INT4
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD COUNT_WATCHPOINTS.
    rv_count = lines( gt_watchpoint ).
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Protected Method ZBOPF_CL_CHANGE_TRACKER_ADN->END_MODIFY
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_PROCESS_IMMEDIATELY         TYPE        BOOLE_D (default =ABAP_FALSE)
* | [--->] IV_CHECK_FOR_FAILED            TYPE        BOOLE_D (default =ABAP_TRUE)
* | [<---] EO_CHANGE                      TYPE REF TO /BOBF/IF_FRW_CHANGE
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD END_MODIFY.

    mo_original_internal_access->end_modify(
      EXPORTING
        iv_process_immediately = iv_process_immediately
        iv_check_for_failed    = iv_check_for_failed
      IMPORTING
        eo_change              = eo_change ).

    synchronize_internal_access( ).

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Protected Method ZBOPF_CL_CHANGE_TRACKER_ADN->INVALIDATE
* +-------------------------------------------------------------------------------------------------+
* | [<---] ET_MOD                         TYPE        /BOBF/T_FRW_MODIFICATION
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD INVALIDATE.

    mo_original_internal_access->invalidate(
      IMPORTING
        et_mod = et_mod ).

    synchronize_internal_access( ).

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Protected Method ZBOPF_CL_CHANGE_TRACKER_ADN->NOTIFY_CHANGE_INTERNAL
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_NODE_KEY                    TYPE        /BOBF/OBM_NODE_KEY
* | [--->] IV_KEY                         TYPE        /BOBF/CONF_KEY(optional)
* | [--->] IT_KEY                         TYPE        /BOBF/T_FRW_KEY(optional)
* | [--->] IV_CHANGE_MODE                 TYPE        /BOBF/CONF_CHANGE_MODE (default =/BOBF/IF_FRW_C=>SC_MODIFY_UPDATE)
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD NOTIFY_CHANGE_INTERNAL.

    mo_original_internal_access->notify_change_internal(
      EXPORTING
        iv_node_key    = iv_node_key
        iv_key         = iv_key
        it_key         = it_key
        iv_change_mode = iv_change_mode ).

    synchronize_internal_access( ).

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZBOPF_CL_CHANGE_TRACKER_ADN->READ_ABAP_CALL_STACK
* +-------------------------------------------------------------------------------------------------+
* | [<-->] CT_CHANGE_LOG                  TYPE        LT_BOBF_CHANGE_TRACKER_LOG
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD READ_ABAP_CALL_STACK.

    DATA:
      lt_callstack TYPE abap_callstack.

    CHECK ct_change_log IS NOT INITIAL.

    CALL FUNCTION 'SYSTEM_CALLSTACK'
      EXPORTING
        max_level = 0
      IMPORTING
        callstack = lt_callstack.

    READ TABLE lt_callstack ASSIGNING FIELD-SYMBOL(<fs_call_stack>) INDEX 4.
    IF sy-subrc = 0.
      LOOP AT ct_change_log ASSIGNING FIELD-SYMBOL(<fs_change_log_line>).
        <fs_change_log_line> = CORRESPONDING #( BASE ( <fs_change_log_line> ) <fs_call_stack> ).
      ENDLOOP.
    ENDIF.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZBOPF_CL_CHANGE_TRACKER_ADN->SETUP_CHANGE_TRACKER_SETTINGS
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD SETUP_CHANGE_TRACKER_SETTINGS.

    IF gv_settings_read = abap_false.

      SELECT * FROM zbopf_d_track
        INTO CORRESPONDING FIELDS OF TABLE gt_change_tracker_settings
        WHERE uname = sy-uname.

      LOOP AT gt_change_tracker_settings ASSIGNING FIELD-SYMBOL(<fs_change_tracker_setting>)
        WHERE watchp_enable = abap_true.

        INSERT CORRESPONDING #( <fs_change_tracker_setting>
          MAPPING
            bo_key = bo_key
            node_key = node_key
            on_create = track_create
            on_update = track_update
            on_delete = track_delete
        ) INTO TABLE gt_watchpoint.

      ENDLOOP.

      gv_settings_read = abap_true.
    ENDIF.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Protected Method ZBOPF_CL_CHANGE_TRACKER_ADN->SET_CONTEXT
* +-------------------------------------------------------------------------------------------------+
* | [--->] IS_ACTION                      TYPE        /BOBF/S_FRW_CTX_ACT(optional)
* | [--->] IS_ASSOCIATION                 TYPE        /BOBF/S_FRW_CTX_ASSOC(optional)
* | [--->] IS_DETERMINATION               TYPE        /BOBF/S_FRW_CTX_DET(optional)
* | [--->] IS_VALIDATION                  TYPE        /BOBF/S_FRW_CTX_VAL(optional)
* | [--->] IS_QUERY                       TYPE        /BOBF/S_FRW_CTX_QUERY(optional)
* | [--->] IV_READ_ALLOWED                TYPE        BOOLE_D (default =ABAP_FALSE)
* | [--->] IV_MODIFY_ALLOWED              TYPE        BOOLE_D (default =ABAP_FALSE)
* | [--->] IV_CHECK_ALLOWED               TYPE        BOOLE_D (default =ABAP_FALSE)
* | [--->] IV_QUERY_ALLOWED               TYPE        BOOLE_D (default =ABAP_FALSE)
* | [--->] IV_PROCESS_IMMEDIATELY_ALLOWED TYPE        BOOLE_D (default =ABAP_FALSE)
* | [--->] IT_MOD                         TYPE        /BOBF/T_FRW_MODIFICATION(optional)
* | [--->] IV_READ_BUFFER_ONLY            TYPE        BOOLE_D (default =ABAP_FALSE)
* | [--->] IV_INVALIDATE_CACHE            TYPE        BOOLE_D (default =ABAP_FALSE)
* | [--->] IV_WITHIN_LOADING              TYPE        BOOLE_D (default =ABAP_FALSE)
* | [--->] IV_WITHIN_RETRIEVE             TYPE        BOOLE_D (default =ABAP_FALSE)
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD SET_CONTEXT.

    mo_original_internal_access->set_context(
      EXPORTING
        is_action                      = is_action
        is_association                 = is_association
        is_determination               = is_determination
        is_validation                  = is_validation
        is_query                       = is_query
        iv_read_allowed                = iv_read_allowed
        iv_modify_allowed              = iv_modify_allowed
        iv_check_allowed               = iv_check_allowed
        iv_query_allowed               = iv_query_allowed
        iv_process_immediately_allowed = iv_process_immediately_allowed
        it_mod                         = it_mod
        iv_read_buffer_only            = iv_read_buffer_only
        iv_invalidate_cache            = iv_invalidate_cache
        iv_within_loading              = iv_within_loading
        iv_within_retrieve             = iv_within_retrieve ).

    synchronize_internal_access( ).

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Protected Method ZBOPF_CL_CHANGE_TRACKER_ADN->SET_CURRENT_STATE
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_CURRENT_STATE               TYPE        /BOBF/CONF_STATE
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD SET_CURRENT_STATE.

    mo_original_internal_access->set_current_state( iv_current_state = iv_current_state ).

    synchronize_internal_access( ).

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Protected Method ZBOPF_CL_CHANGE_TRACKER_ADN->SET_EDIT_MODE
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_EDIT_MODE                   TYPE        /BOBF/CONF_EDIT_MODE
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD SET_EDIT_MODE.

    mo_original_internal_access->set_edit_mode( iv_edit_mode = iv_edit_mode ).

    synchronize_internal_access( ).

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Protected Method ZBOPF_CL_CHANGE_TRACKER_ADN->SET_LAST_STATE
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_LAST_STATE                  TYPE        /BOBF/CONF_STATE
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD SET_LAST_STATE.

    mo_original_internal_access->set_last_state( iv_last_state = iv_last_state ).

    synchronize_internal_access( ).

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZBOPF_CL_CHANGE_TRACKER_ADN->SYNCHRONIZE_INTERNAL_ACCESS
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD SYNCHRONIZE_INTERNAL_ACCESS.
    mo_bopf              = mo_original_internal_access->mo_bopf.
    mo_buffer            = mo_original_internal_access->mo_buffer.
    mo_change            = mo_original_internal_access->mo_change.
    mo_conf              = mo_original_internal_access->mo_conf.
    mo_local_change      = mo_original_internal_access->mo_local_change.
    mo_local_message     = mo_original_internal_access->mo_local_message.
    ms_association       = mo_original_internal_access->ms_association.
    mt_modification      = mo_original_internal_access->mt_modification.
    mt_modification_bopf = mo_original_internal_access->mt_modification_bopf.
    mv_current_state     = mo_original_internal_access->mv_current_state.
    mv_edit_mode         = mo_original_internal_access->mv_edit_mode.
    mv_invalidate_cache  = mo_original_internal_access->mv_invalidate_cache.
    mv_last_state        = mo_original_internal_access->mv_last_state.
    mv_valid_if_check    = mo_original_internal_access->mv_valid_if_check.
    mv_valid_if_modify   = mo_original_internal_access->mv_valid_if_modify.
    mv_valid_if_query    = mo_original_internal_access->mv_valid_if_query.
    mv_valid_if_read     = mo_original_internal_access->mv_valid_if_read.
    mv_within_loading    = mo_original_internal_access->mv_within_loading.
    mv_within_retrieve   = mo_original_internal_access->mv_within_retrieve.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZBOPF_CL_CHANGE_TRACKER_ADN->TRACK_MODIFICATIONS
* +-------------------------------------------------------------------------------------------------+
* | [--->] IT_MODIFY                      TYPE        /BOBF/T_FRW_MODIFICATION
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD track_modifications.

    DATA:
      lt_change_log        TYPE lt_bobf_change_tracker_log,
      lt_change_log_result TYPE lt_bobf_change_tracker_log,
      lt_setting           TYPE zbopf_t_change_tracker_set,
      ls_init_setting      TYPE zbopf_s_change_tracker_set,
      lv_track_changes     TYPE flag.

    FIELD-SYMBOLS:
      <fs_new_value> TYPE any.

    LOOP AT it_modify ASSIGNING FIELD-SYMBOL(<fs_modification>).

      CLEAR: lt_setting, lv_track_changes.

* There are could be several settings for tracking several attributes of the Node.
* It is nesessary to respect them all. For each settings we will have separate records in the LOG
* Read setting for every changed field separately:
      LOOP AT <fs_modification>-changed_fields ASSIGNING FIELD-SYMBOL(<fs_changed_field>).
        READ TABLE gt_change_tracker_settings ASSIGNING FIELD-SYMBOL(<fs_setting>)
         WITH TABLE KEY node_field COMPONENTS
           uname      = sy-uname
           bo_key     = mv_bo_key
           node_key   = <fs_modification>-node
           field_name = <fs_changed_field>.
        IF sy-subrc = 0.
          APPEND <fs_setting> TO lt_setting.
        ENDIF.
      ENDLOOP.

* Read setting for tracking any change in the Node
      READ TABLE gt_change_tracker_settings ASSIGNING FIELD-SYMBOL(<fs_entire_rec_setting>)
        WITH TABLE KEY node_field COMPONENTS
          uname      = sy-uname
          bo_key     = mv_bo_key
          node_key   = <fs_modification>-node
          field_name = ''.
      IF sy-subrc = 0.
        APPEND <fs_entire_rec_setting> TO lt_setting.
      ENDIF.

* If the change fields in modification is initial, then respect all tracker settings for this Node
      IF <fs_modification>-changed_fields IS INITIAL.
        LOOP AT gt_change_tracker_settings ASSIGNING <fs_setting>
          USING KEY node_field
            WHERE uname    = sy-uname
              AND bo_key   = mv_bo_key
              AND node_key = <fs_modification>-node.
          APPEND <fs_setting> TO lt_setting.
        ENDLOOP.
      ENDIF.

* loop at actual settings for current modification
      LOOP AT lt_setting ASSIGNING <fs_setting>.
        CLEAR: lt_change_log.
        "Check if setting record key match actual modif record key
        IF <fs_setting>-record_key IS NOT INITIAL AND <fs_setting>-record_key <> <fs_modification>-key.
          CONTINUE.
        ENDIF.

        CASE <fs_modification>-change_mode.
          WHEN /bobf/if_frw_c=>sc_modify_update.
            IF ( <fs_setting>-field_name IS INITIAL " we have to track, only if selected field has been changed
              OR <fs_modification>-changed_fields IS INITIAL " all fields are going to be changed
              OR line_exists( <fs_modification>-changed_fields[ table_line = <fs_setting>-field_name ] ) ).
              lv_track_changes = <fs_setting>-track_update.
            ENDIF.
          WHEN /bobf/if_frw_c=>sc_modify_create.
            lv_track_changes = <fs_setting>-track_create. " track anyway
          WHEN /bobf/if_frw_c=>sc_modify_delete.
            lv_track_changes = <fs_setting>-track_delete. " track anyway
        ENDCASE.

        IF lv_track_changes = abap_true.

          INSERT VALUE ts_bobf_change_tracker_log(
            log_key     = /bobf/cl_frw_factory=>get_new_key( )
            bo_key      = mv_bo_key
            node_key    = <fs_modification>-node
            record_key  = <fs_modification>-key
            root_key    = <fs_modification>-root_key
            change_mode = <fs_modification>-change_mode
          ) INTO TABLE lt_change_log ASSIGNING FIELD-SYMBOL(<fs_change_log>).

          GET TIME STAMP FIELD <fs_change_log>-operation_time.

          DATA(lv_fill_new_value) = abap_false. "flag for filling new field value
          CASE <fs_modification>-change_mode.
            WHEN /bobf/if_frw_c=>sc_modify_create.
              watchpoint_on_create <fs_modification>-node. "BOPF create watchpoint reached
              IF ( <fs_setting>-field_name IS NOT INITIAL "If field has been defined explicitly and been changed, track new value
                AND ( line_exists( <fs_modification>-changed_fields[ table_line = <fs_setting>-field_name ] )
                OR <fs_modification>-changed_fields IS INITIAL ) ). " all fileds are going to be changed
                lv_fill_new_value = abap_true.
              ENDIF.
            WHEN /bobf/if_frw_c=>sc_modify_delete.
              watchpoint_on_delete <fs_modification>-node. "BOBF delete watchpoint reached
            WHEN /bobf/if_frw_c=>sc_modify_update.
              watchpoint_on_update <fs_modification>-node. "BOBF update watchpoint reached
              IF ( <fs_setting>-field_name IS NOT INITIAL "If field has been defined explicitly and been changed, track new value
                AND ( line_exists( <fs_modification>-changed_fields[ table_line = <fs_setting>-field_name ] )
                OR <fs_modification>-changed_fields IS INITIAL ) ). " all fileds are going to be changed
                lv_fill_new_value = abap_true.
              ENDIF.
          ENDCASE.

          IF lv_fill_new_value = abap_true. "Fill new field value if need
            IF <fs_change_log> IS ASSIGNED AND <fs_modification>-data IS BOUND.
              ASSIGN <fs_modification>-data->(<fs_setting>-field_name) TO <fs_new_value>.
              IF <fs_new_value> IS ASSIGNED.
                <fs_change_log>-field_name = <fs_setting>-field_name.

                cl_abap_container_utilities=>fill_container_c(
                  EXPORTING
                    im_value     = <fs_new_value>
                  IMPORTING
                    ex_container = <fs_change_log>-new_value
                ).
              ENDIF.
            ENDIF.
          ENDIF.
        ENDIF.

        INSERT LINES OF lt_change_log INTO TABLE lt_change_log_result.
      ENDLOOP.
    ENDLOOP.

    read_abap_call_stack(
      CHANGING
        ct_change_log = lt_change_log_result
    ).

    IF lt_change_log_result IS NOT INITIAL.
      INSERT zbopf_d_t_log CONNECTION (cv_second_connection) FROM TABLE lt_change_log_result.
      COMMIT CONNECTION (cv_second_connection).
      INSERT LINES OF lt_change_log_result INTO TABLE gt_change_log_container.
    ENDIF.

  ENDMETHOD.
ENDCLASS.