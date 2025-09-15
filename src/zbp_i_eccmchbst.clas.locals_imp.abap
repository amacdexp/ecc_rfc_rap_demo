"======================================================================
" Local handler for entity alias Mchb
"======================================================================
CLASS lhc_mchb DEFINITION INHERITING FROM cl_abap_behavior_handler.
  PRIVATE SECTION.

    METHODS get_global_authorizations FOR GLOBAL AUTHORIZATION
      IMPORTING request requested_authorizations FOR mchb
      RESULT result.

    METHODS read FOR READ
      IMPORTING keys FOR READ mchb
      RESULT result.

    METHODS lock FOR LOCK
      IMPORTING keys FOR LOCK mchb.

    METHODS transferstock FOR MODIFY
      IMPORTING keys FOR ACTION mchb~transferstock
      RESULT result.

    " --- small helpers ---
    METHODS get_rfc_dest RETURNING VALUE(rv_dest) TYPE rfcdest.

    TYPES ty_char40 TYPE c LENGTH 40.
    METHODS matnr40_to_18
      IMPORTING iv_matnr40 TYPE ty_char40
      RETURNING VALUE(rv_matnr18) TYPE matnr.

ENDCLASS.

CLASS lhc_mchb IMPLEMENTATION.

  "----------------------------------------------------------
  " Allow READ + action globally (strict(2) requires a result)
  "----------------------------------------------------------
    METHOD get_global_authorizations.
      FIELD-SYMBOLS <res_read> TYPE any.
      FIELD-SYMBOLS <res_act>  TYPE any.

      " Allow READ
      ASSIGN COMPONENT '%read' OF STRUCTURE result TO <res_read>.
      IF sy-subrc = 0.
        <res_read> = if_abap_behv=>auth-allowed.
      ENDIF.

      " Allow action TransferStock
      ASSIGN COMPONENT '%action-TransferStock' OF STRUCTURE result TO <res_act>.
      IF sy-subrc = 0.
        <res_act> = if_abap_behv=>auth-allowed.
      ENDIF.
    ENDMETHOD.


  "-----------------------------------------
  " Read list or by key(s) via RFC_READ_TABLE
  "-----------------------------------------
METHOD read.
  DATA lv_dest TYPE rfcdest.
  lv_dest = get_rfc_dest( ).

  " Fields
  DATA lt_fields TYPE STANDARD TABLE OF rfc_db_fld WITH EMPTY KEY.
  DATA ls_field  TYPE rfc_db_fld.
  CLEAR ls_field. ls_field-fieldname = 'MATNR'. APPEND ls_field TO lt_fields.
  CLEAR ls_field. ls_field-fieldname = 'WERKS'. APPEND ls_field TO lt_fields.
  CLEAR ls_field. ls_field-fieldname = 'LGORT'. APPEND ls_field TO lt_fields.
  CLEAR ls_field. ls_field-fieldname = 'CHARG'. APPEND ls_field TO lt_fields.
  CLEAR ls_field. ls_field-fieldname = 'CLABS'. APPEND ls_field TO lt_fields.

  " WHERE
  DATA lt_options TYPE STANDARD TABLE OF rfc_db_opt WITH EMPTY KEY.
  DATA ls_opt     TYPE rfc_db_opt.

    IF keys IS INITIAL.
      RETURN.  " we only support read-by-key here
    ENDIF.

    LOOP AT keys ASSIGNING FIELD-SYMBOL(<k>).

      " quote each value: 'ABC'
      DATA lv_q_matnr TYPE string.
      DATA lv_q_werks TYPE string.
      DATA lv_q_lgort TYPE string.
      DATA lv_q_charg TYPE string.

      CONCATENATE '''' <k>-matnr '''' INTO lv_q_matnr.
      CONCATENATE '''' <k>-plant '''' INTO lv_q_werks.
      CONCATENATE '''' <k>-lgort '''' INTO lv_q_lgort.
      CONCATENATE '''' <k>-charg '''' INTO lv_q_charg.

      CLEAR ls_opt.
      IF sy-tabix = 1.
        ls_opt-text = '('.
      ELSE.
        ls_opt-text = 'OR ('.
      ENDIF.
      APPEND ls_opt TO lt_options.

      CLEAR ls_opt. CONCATENATE 'MATNR = ' lv_q_matnr INTO ls_opt-text. APPEND ls_opt TO lt_options.
      CLEAR ls_opt. CONCATENATE 'AND WERKS = ' lv_q_werks INTO ls_opt-text. APPEND ls_opt TO lt_options.
      CLEAR ls_opt. CONCATENATE 'AND LGORT = ' lv_q_lgort INTO ls_opt-text. APPEND ls_opt TO lt_options.
      CLEAR ls_opt. CONCATENATE 'AND CHARG = ' lv_q_charg INTO ls_opt-text. APPEND ls_opt TO lt_options.

      CLEAR ls_opt. ls_opt-text = ')'. APPEND ls_opt TO lt_options.

    ENDLOOP.

  " Execute
  DATA lt_data  TYPE STANDARD TABLE OF tab512 WITH EMPTY KEY.
  DATA lv_delim TYPE c LENGTH 1. lv_delim = '|'.

  CALL FUNCTION 'RFC_READ_TABLE'
    DESTINATION lv_dest
    EXPORTING
      query_table = 'MCHB'
      delimiter   = lv_delim
      no_data     = ' '
      rowcount    = 0
    TABLES
      fields      = lt_fields
      options     = lt_options
      data        = lt_data
    EXCEPTIONS
      OTHERS      = 7.

  IF sy-subrc <> 0 OR lt_data IS INITIAL.
    RETURN.
  ENDIF.

  " Map -> RESULT (use work area, no constructor commas)
  DATA lt_tokens TYPE STANDARD TABLE OF string WITH EMPTY KEY.
  DATA lv_clabs_s TYPE string.
  DATA lv_clabs_p TYPE p LENGTH 13 DECIMALS 3.
  DATA ls_out     LIKE LINE OF result.

  LOOP AT lt_data INTO DATA(ls_line).
    CLEAR lt_tokens.
    SPLIT ls_line-wa AT lv_delim INTO TABLE lt_tokens.
    IF lines( lt_tokens ) < 5.
      CONTINUE.
    ENDIF.

    CLEAR: lv_clabs_s, lv_clabs_p, ls_out.
    lv_clabs_s = lt_tokens[ 5 ].
    CONDENSE lv_clabs_s NO-GAPS.
    IF lv_clabs_s IS INITIAL.
      lv_clabs_p = 0.
    ELSE.
      lv_clabs_p = lv_clabs_s.
    ENDIF.

    ls_out-matnr = lt_tokens[ 1 ].
    ls_out-plant = lt_tokens[ 2 ].
    ls_out-lgort = lt_tokens[ 3 ].
    ls_out-charg = lt_tokens[ 4 ].
    ls_out-clabs = lv_clabs_p.
    ls_out-uom   = 'L'.
    APPEND ls_out TO result.
  ENDLOOP.

  SORT result BY matnr plant lgort charg.
ENDMETHOD.


  METHOD lock.
    " Read-only + action → nothing to lock
  ENDMETHOD.

  "---------------------------------------------
  " Action: TransferStock (bound, per selected row)
  "---------------------------------------------
METHOD transferstock.
  " RFC destination
  DATA lv_dest TYPE rfcdest.
  lv_dest = get_rfc_dest( ).

  " --- Collect parameters by %tky (use plain 'tky' as our own field name) ---
  TYPES: BEGIN OF ty_param_by_tky,
           tky TYPE sysuuid_c22,
           tgt TYPE lgort_d,
         END OF ty_param_by_tky.
  DATA lt_param_by_tky TYPE HASHED TABLE OF ty_param_by_tky WITH UNIQUE KEY tky.

  LOOP AT keys ASSIGNING FIELD-SYMBOL(<k>).
    FIELD-SYMBOLS <p>     TYPE any.
    FIELD-SYMBOLS <p_tgt> TYPE any.

    ASSIGN COMPONENT '%param' OF STRUCTURE <k> TO <p>.
    IF sy-subrc <> 0 OR <p> IS NOT ASSIGNED.
      CONTINUE.
    ENDIF.
    ASSIGN COMPONENT 'TARGETSTORAGELOCATION' OF STRUCTURE <p> TO <p_tgt>.
    IF sy-subrc <> 0 OR <p_tgt> IS NOT ASSIGNED OR <p_tgt> IS INITIAL.
      CONTINUE.
    ENDIF.

    INSERT VALUE ty_param_by_tky( tky = <k>-%tky  tgt = <p_tgt> )
      INTO TABLE lt_param_by_tky.
  ENDLOOP.
  IF lt_param_by_tky IS INITIAL.
    RETURN.
  ENDIF.

" Read the selected rows (to get CLABS & UOM etc.) using inline key list
    READ ENTITIES OF ZI_ECCMchbST IN LOCAL MODE
      ENTITY Mchb
      FIELDS ( matnr plant lgort charg clabs uom )
      WITH VALUE #(
        FOR k IN keys
        ( %key-matnr = k-matnr
          %key-plant = k-plant
          %key-lgort = k-lgort
          %key-charg = k-charg ) )
      RESULT DATA(lt_inst).

  IF lt_inst IS INITIAL.
    RETURN.
  ENDIF.

  " --- Build one GM doc with one item per selected row ---
  DATA lt_item          TYPE STANDARD TABLE OF bapi2017_gm_item_create WITH EMPTY KEY.
  DATA ls_item          TYPE bapi2017_gm_item_create.
  DATA lt_success_tkys  TYPE SORTED TABLE OF sysuuid_c22 WITH UNIQUE KEY table_line.
  DATA lv_tky           TYPE sysuuid_c22.

  LOOP AT lt_inst ASSIGNING FIELD-SYMBOL(<r>).
    " find the target SLoc for this technical key
    READ TABLE lt_param_by_tky WITH TABLE KEY tky = <r>-%tky ASSIGNING FIELD-SYMBOL(<p_row>).
    IF sy-subrc <> 0.
      CONTINUE.
    ENDIF.

    " skip if nothing to move
    IF <r>-clabs IS INITIAL.
      CONTINUE.
    ENDIF.

    CLEAR ls_item.
    ls_item-move_type   = '311'.                        " SLoc→SLoc within plant
    ls_item-material    = <r>-matnr. "matnr40_to_18( <r>-matnr ).   " convert 40→18
    ls_item-plant       = <r>-plant.
    ls_item-stge_loc    = <r>-lgort.                    " from
    ls_item-batch       = <r>-charg.
    ls_item-move_plant  = <r>-plant.
    ls_item-move_stloc  = <p_row>-tgt.                  " to (param)
    ls_item-move_batch  = <r>-charg.                    " keep same batch
    ls_item-entry_qnt   = <r>-clabs.                    " full unrestricted qty
    IF <r>-uom IS NOT INITIAL.
      ls_item-entry_uom = <r>-uom.
    ENDIF.
    APPEND ls_item TO lt_item.

    lv_tky = <r>-%tky.
    INSERT lv_tky INTO TABLE lt_success_tkys.
  ENDLOOP.

  IF lt_item IS INITIAL.
    RETURN.
  ENDIF.

  " --- Header (hard-coded posting date) ---
  DATA ls_head TYPE bapi2017_gm_head_01.
  DATA ls_code TYPE bapi2017_gm_code.
  CLEAR: ls_head, ls_code.
  ls_head-pstng_date = '20250301'.     " TEMP per environment
  ls_head-doc_date   = sy-datum.
  ls_code-gm_code    = '04'.           " transfer posting

  " --- Post & commit on ECC ---
  DATA lt_return TYPE STANDARD TABLE OF bapiret2 WITH EMPTY KEY.
  DATA lv_mblnr  TYPE mblnr.

  CALL FUNCTION 'BAPI_GOODSMVT_CREATE'
    DESTINATION lv_dest
    EXPORTING
      goodsmvt_header = ls_head
      goodsmvt_code   = ls_code
    IMPORTING
      materialdocument = lv_mblnr
    TABLES
      goodsmvt_item   = lt_item
      return          = lt_return.

  " commit only if no E/A messages
  READ TABLE lt_return WITH KEY type = 'E' TRANSPORTING NO FIELDS.
  IF sy-subrc = 0.
    RETURN.
  ENDIF.
  READ TABLE lt_return WITH KEY type = 'A' TRANSPORTING NO FIELDS.
  IF sy-subrc = 0.
    RETURN.
  ENDIF.

  CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
    DESTINATION lv_dest
    EXPORTING wait = abap_true.

  " --- Echo $self for all posted rows ---
  LOOP AT lt_success_tkys INTO lv_tky.
    APPEND VALUE #( %tky = lv_tky ) TO result.
  ENDLOOP.

ENDMETHOD.





  "---------------------------------------------
  " Helpers
  "---------------------------------------------
  METHOD get_rfc_dest.
    " Reuse your TVARVC setting if available; else fallback
    TRY.
        SELECT SINGLE low
          FROM tvarvc
          WHERE name = 'Z_RFC_DEST_PLANT'
            AND sign = 'I'
            AND opti = 'EQ'
          INTO @rv_dest.
      CATCH cx_sy_open_sql_db.
        CLEAR rv_dest.
    ENDTRY.
    IF rv_dest IS INITIAL.
      rv_dest = 'S4HCLNT100'.
    ENDIF.
  ENDMETHOD.

  METHOD matnr40_to_18.
    DATA lv_len TYPE i.
    DATA lv_tmp TYPE matnr.
    lv_len = strlen( iv_matnr40 ).
    IF lv_len > 18.
      rv_matnr18 = iv_matnr40+22(18).
    ELSEIF iv_matnr40 CO ' 0123456789'.
      lv_tmp = iv_matnr40.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING input  = lv_tmp
        IMPORTING output = lv_tmp.
      rv_matnr18 = lv_tmp.
    ELSE.
      rv_matnr18 = iv_matnr40.
    ENDIF.
  ENDMETHOD.

ENDCLASS.

"======================================================================
" Saver (unmanaged) – no persistence → all empty
"======================================================================
CLASS lsc_zi_eccmchbst DEFINITION INHERITING FROM cl_abap_behavior_saver.
  PROTECTED SECTION.
    METHODS finalize           REDEFINITION.
    METHODS check_before_save  REDEFINITION.
    METHODS save               REDEFINITION.
    METHODS cleanup            REDEFINITION.
    METHODS cleanup_finalize   REDEFINITION.
ENDCLASS.

CLASS lsc_zi_eccmchbst IMPLEMENTATION.
  METHOD finalize.          ENDMETHOD.
  METHOD check_before_save. ENDMETHOD.
  METHOD save.              ENDMETHOD.
  METHOD cleanup.           ENDMETHOD.
  METHOD cleanup_finalize.  ENDMETHOD.
ENDCLASS.
