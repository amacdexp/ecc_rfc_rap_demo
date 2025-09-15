CLASS zcl_get_ecc_stloc DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES if_rap_query_provider.
    INTERFACES if_oo_adt_classrun.

    " CE table type
    TYPES tt_ecc_stloc TYPE STANDARD TABLE OF zstr_ecc_stloc WITH EMPTY KEY.

    " Destination resolver (TVARVC -> fallback)
    CLASS-METHODS get_rfc_dest
      RETURNING VALUE(rv_dest) TYPE rfcdest.

    " ADT console tester (no classic WRITE)
    CLASS-METHODS test_fetch_stloc
      IMPORTING
        p_max  TYPE i       DEFAULT 10
        p_dest TYPE rfcdest OPTIONAL
        io_out TYPE REF TO if_oo_adt_classrun_out OPTIONAL.

  PRIVATE SECTION.
    " RFC proxy: order & lengths must match FM table row type
    TYPES: BEGIN OF ty_stloc_proxy,
             werks  TYPE c LENGTH 4,   " WERKS
             lgort  TYPE c LENGTH 4,   " LGORT
             lgobe  TYPE c LENGTH 16,  " LGOBE
             "filler TYPE c LENGTH 512, " absorb any trailing fields if present
           END OF ty_stloc_proxy.

    " Core used by RAP + console
    CLASS-METHODS fetch_stloc_core
      IMPORTING
        iv_dest  TYPE rfcdest OPTIONAL
      EXPORTING
        ev_subrc TYPE i
        ev_msg   TYPE string
      RETURNING
        VALUE(rt_loc) TYPE tt_ecc_stloc.
ENDCLASS.



CLASS zcl_get_ecc_stloc IMPLEMENTATION.

  METHOD get_rfc_dest.
    TRY.
        SELECT SINGLE low
          FROM tvarvc
          WHERE name   = 'Z_RFC_DEST_PLANT'
            AND sign   = 'I'
            AND opti   = 'EQ'
          INTO @rv_dest.
      CATCH cx_sy_open_sql_db.
        CLEAR rv_dest.
    ENDTRY.
    IF rv_dest IS INITIAL.
      rv_dest = 'S4HCLNT100'.  " fallback: set yours
    ENDIF.
  ENDMETHOD.


  METHOD fetch_stloc_core.
    DATA lv_dest TYPE rfcdest.
    IF iv_dest IS NOT INITIAL.
      lv_dest = iv_dest.
    ELSE.
      lv_dest = zcl_get_ecc_stloc=>get_rfc_dest( ).
    ENDIF.

    DATA lt_rfc    TYPE STANDARD TABLE OF ty_stloc_proxy WITH EMPTY KEY.
    DATA lt_return TYPE STANDARD TABLE OF bapiret2      WITH EMPTY KEY.
    DATA lv_msg    TYPE string.

    " >>> IMPORTANT: Replace STORAGE_LOCATIONS with the exact TABLES parameter
    " name from SE37 for SRSMOB_GET_STORAGE_LOCATIONS on your system.
    CALL FUNCTION 'SRSMOB_GET_STORAGE_LOCATIONS'
      DESTINATION lv_dest
      TABLES
        LGORT = lt_rfc   " <-- adjust if needed (e.g., ET_STORAGE_LOCATIONS)
        return            = lt_return
      EXCEPTIONS
        communication_failure = 1
        system_failure        = 2
        OTHERS                = 3.

    ev_subrc = sy-subrc.

    " Collate BAPIRET2 error if present
    READ TABLE lt_return WITH KEY type = 'E' INTO DATA(ls_ret).
    IF sy-subrc = 0.
      ev_msg = |{ ls_ret-id } { ls_ret-number } { ls_ret-message }|.
    ENDIF.

    CLEAR rt_loc.
    IF ev_subrc = 0 AND lt_rfc IS NOT INITIAL.
      DATA ls_ce TYPE zstr_ecc_stloc.
      LOOP AT lt_rfc INTO DATA(ls_rfc).
        CLEAR ls_ce.
        ls_ce-plant = ls_rfc-werks.
        ls_ce-lgort = ls_rfc-lgort.
        ls_ce-lgobe = ls_rfc-lgobe.
        APPEND ls_ce TO rt_loc.
      ENDLOOP.
      SORT rt_loc BY plant lgort.
    ENDIF.
  ENDMETHOD.


  METHOD test_fetch_stloc.
    IF io_out IS NOT BOUND.
      RETURN. " console-only (cloud-friendly)
    ENDIF.

    DATA lv_dest  TYPE rfcdest.
    DATA lv_subrc TYPE i.
    DATA lv_msg   TYPE string.
    DATA lt_ce    TYPE tt_ecc_stloc.

    lv_dest = COND rfcdest( WHEN p_dest IS NOT INITIAL THEN p_dest
                            ELSE zcl_get_ecc_stloc=>get_rfc_dest( ) ).

    lt_ce = zcl_get_ecc_stloc=>fetch_stloc_core(
             EXPORTING iv_dest  = lv_dest
             IMPORTING ev_subrc = lv_subrc
                       ev_msg   = lv_msg ).

    io_out->write( |--- ZCL_GET_ECC_STLOC=>TEST_FETCH_STLOC ---| ).
    io_out->write( |RFC destination       : { lv_dest }| ).
    io_out->write( |sy-subrc (core)       : { lv_subrc }| ).
    IF lv_msg IS NOT INITIAL.
      io_out->write( |RETURN msg           : { lv_msg }| ).
    ENDIF.
    io_out->write( |Rows (mapped)         : { lines( lt_ce ) }| ).

    IF lt_ce IS INITIAL.
      io_out->write( |No storage locations returned.| ).
      RETURN.
    ENDIF.

    " Print first p_max
    DATA lc TYPE i VALUE 0.
    io_out->write( |{ 'Plant' WIDTH = 4 ALIGN = LEFT }  { 'LGORT' WIDTH = 6 ALIGN = LEFT }  { 'Description' }| ).
    LOOP AT lt_ce INTO DATA(ls_ce).
      lc = lc + 1.
      io_out->write( |{ ls_ce-plant WIDTH = 4 ALIGN = LEFT }  { ls_ce-lgort WIDTH = 6 ALIGN = LEFT }  { ls_ce-lgobe }| ).
      IF lc >= p_max. EXIT. ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD if_oo_adt_classrun~main.
    zcl_get_ecc_stloc=>test_fetch_stloc(
      p_max  = 20
      io_out = out
    ).
  ENDMETHOD.


  METHOD if_rap_query_provider~select.
    " Get full set via core, then page safely
    DATA lv_subrc TYPE i.
    DATA lv_msg   TYPE string.
    DATA lt_loc   TYPE tt_ecc_stloc.

    lt_loc = zcl_get_ecc_stloc=>fetch_stloc_core(
               IMPORTING ev_subrc = lv_subrc
                         ev_msg   = lv_msg ).

    " Total BEFORE paging
    DATA lv_total_i TYPE i.
    lv_total_i = lines( lt_loc ).

    " RAP paging (safe slice)
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
        CLEAR lt_loc.
      ELSE.
        DATA lt_page TYPE tt_ecc_stloc.
        DATA ls_row  TYPE zstr_ecc_stloc.
        LOOP AT lt_loc INTO ls_row FROM lv_from TO lv_to.
          APPEND ls_row TO lt_page.
        ENDLOOP.
        lt_loc = lt_page.
      ENDIF.
    ENDIF.

    io_response->set_total_number_of_records( CONV int8( lv_total_i ) ).
    io_response->set_data( lt_loc ).
  ENDMETHOD.

ENDCLASS.

