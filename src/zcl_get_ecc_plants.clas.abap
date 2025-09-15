CLASS zcl_get_ecc_plants DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES if_rap_query_provider.
    INTERFACES if_oo_adt_classrun.  " <-- enables ADT console run

    " Row table type for your CE structure
    TYPES tt_ecc_plant TYPE STANDARD TABLE OF zstr_ecc_plant WITH EMPTY KEY.

    " Resolve RFC destination (TVARVC -> fallback)
    CLASS-METHODS get_rfc_dest
      RETURNING VALUE(rv_dest) TYPE rfcdest.

    " Console tester (prints via ADT console if 'out' is passed)
    CLASS-METHODS test_fetch_plant
      IMPORTING
        p_max  TYPE i       DEFAULT 10
        p_dest TYPE rfcdest OPTIONAL
        io_out TYPE REF TO if_oo_adt_classrun_out OPTIONAL.

  PRIVATE SECTION.
    " RFC proxy (avoid direct T001W)
    TYPES:
      BEGIN OF ty_t001w_proxy,
        mandt  TYPE mandt,          " 3 chars, must be first
        werks  TYPE werks_d,
        name1  TYPE name1_gp,
        filler TYPE c LENGTH 2048,
      END OF ty_t001w_proxy.

    " Shared core used by RAP + console
    CLASS-METHODS fetch_plants_core
      IMPORTING
        iv_dest  TYPE rfcdest OPTIONAL
      EXPORTING
        ev_subrc TYPE i
        ev_msg   TYPE string
      RETURNING
        VALUE(rt_plants) TYPE tt_ecc_plant.

ENDCLASS.


CLASS zcl_get_ecc_plants IMPLEMENTATION.

  METHOD get_rfc_dest.
    " 1) Customizing via TVARVC (maintain Z_RFC_DEST_PLANT), else 2) fallback
    TRY.
        SELECT SINGLE low
          FROM tvarvc
          WHERE name   = 'Z_RFC_DEST_PLANT'
            AND sign   = 'I'
            AND opti = 'EQ'
          INTO @rv_dest.
      CATCH cx_sy_open_sql_db.
        CLEAR rv_dest.
    ENDTRY.
    IF rv_dest IS INITIAL.
      rv_dest = 'S4HCLNT100'.  " <-- set your default destination
    ENDIF.
  ENDMETHOD.


  METHOD fetch_plants_core.
    DATA(lv_dest) = COND rfcdest( WHEN iv_dest IS NOT INITIAL THEN iv_dest
                                  ELSE zcl_get_ecc_plants=>get_rfc_dest( ) ).

    DATA lt_plants_rfc TYPE STANDARD TABLE OF ty_t001w_proxy WITH EMPTY KEY.
    DATA lv_msg        TYPE string.

    CALL FUNCTION 'C_PDM_GET_PLANTS'
      DESTINATION lv_dest
      TABLES
        plants = lt_plants_rfc
      EXCEPTIONS
        communication_failure = 1 "MESSAGE lv_msg
        system_failure        = 2 "MESSAGE lv_msg
        plants_not_found      = 3
        OTHERS                = 4.

    ev_subrc = sy-subrc.
    ev_msg   = lv_msg.

    IF ev_subrc = 0 AND lt_plants_rfc IS NOT INITIAL.
      rt_plants = CORRESPONDING #( lt_plants_rfc
                                   MAPPING plant = werks
                                           name  = name1 ).
    ELSE.
      CLEAR rt_plants.
    ENDIF.
  ENDMETHOD.


METHOD test_fetch_plant.
  " Require ADT console in Cloud-friendly mode
  IF io_out IS NOT BOUND.
    " No classic list output here; just bail if console not available
    RETURN.
  ENDIF.

  DATA lv_subrc     TYPE i.
  DATA lv_msg       TYPE string.
  DATA lt_ce        TYPE tt_ecc_plant.
  DATA lv_dest_used TYPE rfcdest.

  " Resolve destination to use
  IF p_dest IS NOT INITIAL.
    lv_dest_used = p_dest.
  ELSE.
    lv_dest_used = zcl_get_ecc_plants=>get_rfc_dest( ).
  ENDIF.

  " Run the shared core (RFC + mapping)
  lt_ce = zcl_get_ecc_plants=>fetch_plants_core(
           EXPORTING iv_dest  = lv_dest_used
           IMPORTING ev_subrc = lv_subrc
                     ev_msg   = lv_msg ).

  " Header / diagnostics
  io_out->write( |--- ZCL_GET_ECC_PLANTS=>TEST_FETCH_PLANT ---| ).
  io_out->write( |RFC destination         : { lv_dest_used }| ).
  io_out->write( |sy-subrc (core)         : { lv_subrc }| ).

  IF lv_subrc = 0.
    io_out->write( |Mapped CE rows          : { lines( lt_ce ) }| ).
  ELSE.
    io_out->write( |Diagnostic message      : { lv_msg }| ).
  ENDIF.

  IF lt_ce IS INITIAL.
    io_out->write( |No mapped rows. (Either RFC returned none, or mapping fields differ.)| ).
    RETURN.
  ENDIF.

  " Pretty print up to P_MAX rows
  DATA lc   TYPE i VALUE 0.
  DATA ls_ce TYPE zstr_ecc_plant.

  io_out->write( |{ 'Plant' WIDTH = 12 ALIGN = LEFT }  { 'Name' }| ).

  LOOP AT lt_ce INTO ls_ce.
    lc = lc + 1.
    io_out->write( |{ ls_ce-plant WIDTH = 12 ALIGN = LEFT }  { ls_ce-name }| ).
    IF lc >= p_max.
      EXIT.
    ENDIF.
  ENDLOOP.
ENDMETHOD.


  METHOD if_oo_adt_classrun~main.
      " Older/variant signatures expose only OUT → no IN available here
      zcl_get_ecc_plants=>test_fetch_plant(
        p_max  = 10           " tweak as needed
        io_out = out          " write to ADT console
      ).
  ENDMETHOD.



METHOD if_rap_query_provider~select.

  " --- RFC destination + call ---
  DATA lv_dest TYPE rfcdest.
  lv_dest = zcl_get_ecc_plants=>get_rfc_dest( ).

  DATA lt_plants_rfc TYPE STANDARD TABLE OF ty_t001w_proxy WITH EMPTY KEY.
  CALL FUNCTION 'C_PDM_GET_PLANTS'
    DESTINATION lv_dest
    TABLES
      plants = lt_plants_rfc
    EXCEPTIONS
      communication_failure = 1
      system_failure        = 2
      plants_not_found      = 3
      OTHERS                = 4.

  " --- Map RFC rows to CE row type ---
  DATA lt_plants TYPE tt_ecc_plant.
  IF sy-subrc = 0 AND lt_plants_rfc IS NOT INITIAL.
    lt_plants = CORRESPONDING #( lt_plants_rfc
                                 MAPPING plant = werks
                                         name  = name1 ).
    SORT lt_plants BY plant. " stable order for UI
  ELSE.
    CLEAR lt_plants.
  ENDIF.

  " --- Total BEFORE paging ---
  DATA lv_total_i TYPE i.
  lv_total_i = lines( lt_plants ).

  " --- Paging from request (safe: no index reads) ---
  DATA lv_offset    TYPE i.
  DATA lv_page_size TYPE i.
  lv_offset    = io_request->get_paging( )->get_offset( ).
  lv_page_size = io_request->get_paging( )->get_page_size( ).

  IF lv_page_size <> if_rap_query_paging=>page_size_unlimited
     AND lv_page_size > 0.

    DATA lv_from TYPE i.
    DATA lv_to   TYPE i.
    lv_from = lv_offset + 1.
    lv_to   = lv_offset + lv_page_size.

    IF lv_to > lv_total_i.
      lv_to = lv_total_i.
    ENDIF.

    IF lv_from > lv_total_i OR lv_total_i = 0.
      CLEAR lt_plants. " page beyond end -> empty page
    ELSE.
      DATA lt_page TYPE tt_ecc_plant.
      DATA ls_row  TYPE zstr_ecc_plant.
      LOOP AT lt_plants INTO ls_row FROM lv_from TO lv_to.
        APPEND ls_row TO lt_page.
      ENDLOOP.
      lt_plants = lt_page.
    ENDIF.
  ENDIF.

  " --- Return data + total (INT8) ---
  io_response->set_total_number_of_records( CONV int8( lv_total_i ) ).
  io_response->set_data( lt_plants ).

ENDMETHOD.



ENDCLASS.

