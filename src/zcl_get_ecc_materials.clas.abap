CLASS zcl_get_ecc_materials DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES if_rap_query_provider.
    INTERFACES if_oo_adt_classrun.

    " Row table type for CE rows
    TYPES tt_ecc_material TYPE STANDARD TABLE OF zstr_ecc_material WITH EMPTY KEY.

    " Destination resolver (TVARVC -> fallback). Reuse your existing entry name if you want.
    CLASS-METHODS get_rfc_dest
      RETURNING VALUE(rv_dest) TYPE rfcdest.

    " Console tester (ADT console only; no WRITE)
    CLASS-METHODS test_fetch_material
      IMPORTING
        p_max  TYPE i       DEFAULT 10
        p_dest TYPE rfcdest OPTIONAL
        io_out TYPE REF TO if_oo_adt_classrun_out OPTIONAL.

  PRIVATE SECTION.
    " ---- Local RFC proxies (match *position* of RFC tables) ----

    " Selection rows for MATNRSELECTION (SIGN/OPTION/LOW/HIGH)
    TYPES: BEGIN OF ty_matnrsel_proxy,
             sign       TYPE c LENGTH 1,
             option     TYPE c LENGTH 2,
             matnr_low  TYPE c length 18, "matnr,
             matnr_high TYPE c length 18, "matnr,
             MATNR_HIGH_EXTERNAL TYPE c length 40,
             MATNR_HIGH_GUID TYPE c length 32,
             MATNR_HIGH_VERSION TYPE c length 10,
             MATNR_LOW_EXTERNAL TYPE c length 40,
             MATNR_LOW_GUID TYPE c length 32,
             MATNR_LOW_VERSION TYPE c length 10,
             MATNR_LOW_LONG TYPE c length 40,
             MATNR_HIGH_LONG TYPE c length 40,
           END OF ty_matnrsel_proxy.

    " Return rows for MATNRLIST (BAPIMATLST: MATERIAL + MATL_DESC + others)
    TYPES: BEGIN OF ty_bapimatlst_proxy,
             material    TYPE c length 18, "matnr,
             matl_desc   TYPE c length 40, "maktx,
             "filler      TYPE c LENGTH 512,  " absorb remaining fields

            MATERIAL_EXTERNAL TYPE c length 40,
            MATERIAL_GUID TYPE c length 32,
            MATERIAL_VERSION TYPE c length 10,
            MATERIAL_LONG TYPE c length 40,

           END OF ty_bapimatlst_proxy.

    " Core used by RAP + console
    CLASS-METHODS fetch_materials_core
      IMPORTING
        iv_dest  TYPE rfcdest OPTIONAL
      EXPORTING
        ev_subrc TYPE i
        ev_msg   TYPE string
      RETURNING
        VALUE(rt_mat) TYPE tt_ecc_material.

ENDCLASS.



CLASS zcl_get_ecc_materials IMPLEMENTATION.

  METHOD get_rfc_dest.
    " TVARVC: Z_RFC_DEST_PLANT reused; change the name if you prefer a separate one.
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
      rv_dest = 'S4HCLNT100'.  " fallback
    ENDIF.
  ENDMETHOD.


  METHOD fetch_materials_core.
    DATA lv_dest TYPE rfcdest.
    IF iv_dest IS NOT INITIAL.
      lv_dest = iv_dest.
    ELSE.
      lv_dest = zcl_get_ecc_materials=>get_rfc_dest( ).
    ENDIF.

    " Build selection: I BT '000...0' to 'ZZZ...Z' (full 18 chars) to cover all MATNRs
    DATA lt_sel TYPE STANDARD TABLE OF ty_matnrsel_proxy WITH EMPTY KEY.
    DATA ls_sel TYPE ty_matnrsel_proxy.
    ls_sel-sign      = 'I'.
    ls_sel-option    = 'BT'.
    ls_sel-matnr_low =  '000000000000000000'.  " 18x '0'
    ls_sel-matnr_high = 'ZZZZZZZZZZZZZZZZZZ'.  " 18x 'Z'
    APPEND ls_sel TO lt_sel.

    " Call BAPI
    DATA lt_list TYPE STANDARD TABLE OF ty_bapimatlst_proxy WITH EMPTY KEY.
    DATA lt_return TYPE STANDARD TABLE OF bapiret2 WITH EMPTY KEY.
    DATA lv_msg    TYPE string.

    CALL FUNCTION 'BAPI_MATERIAL_GETLIST'
      DESTINATION lv_dest
      EXPORTING
        maxrows          = 0         " 0 = no limit (we'll page locally)
      TABLES
        matnrselection   = lt_sel
        matnrlist        = lt_list
        return           = lt_return.

    ev_subrc = sy-subrc.

    " Collate errors to a simple message if present
    READ TABLE lt_return WITH KEY type = 'E' INTO DATA(ls_ret).
    IF sy-subrc = 0.
      ev_msg = |{ ls_ret-id } { ls_ret-number } { ls_ret-message }|.
    ENDIF.

    " Map to CE row type
    IF lt_list IS INITIAL.
      CLEAR rt_mat.
      RETURN.
    ENDIF.

    DATA ls_ce TYPE zstr_ecc_material.
    LOOP AT lt_list INTO DATA(ls_bapi).
      CLEAR ls_ce.
      ls_ce-matnr = ls_bapi-material.
      ls_ce-maktx = ls_bapi-matl_desc.
      APPEND ls_ce TO rt_mat.
    ENDLOOP.

    SORT rt_mat BY matnr.  " stable UI order
  ENDMETHOD.


  METHOD test_fetch_material.
    IF io_out IS NOT BOUND.
      RETURN. " cloud-friendly: only ADT console
    ENDIF.

    DATA lv_subrc TYPE i.
    DATA lv_msg   TYPE string.
    DATA lt_ce    TYPE tt_ecc_material.
    DATA lv_dest  TYPE rfcdest.

    IF p_dest IS NOT INITIAL.
      lv_dest = p_dest.
    ELSE.
      lv_dest = zcl_get_ecc_materials=>get_rfc_dest( ).
    ENDIF.

    lt_ce = zcl_get_ecc_materials=>fetch_materials_core(
              EXPORTING iv_dest  = lv_dest
              IMPORTING ev_subrc = lv_subrc
                        ev_msg   = lv_msg ).

    io_out->write( |--- ZCL_GET_ECC_MATERIALS=>TEST_FETCH_MATERIAL ---| ).
    io_out->write( |RFC destination         : { lv_dest }| ).
    io_out->write( |sy-subrc (core)         : { lv_subrc }| ).
    IF lv_msg IS NOT INITIAL.
      io_out->write( |BAPI RETURN              : { lv_msg }| ).
    ENDIF.
    io_out->write( |Rows (mapped)            : { lines( lt_ce ) }| ).

    IF lt_ce IS INITIAL.
      io_out->write( |No materials returned.| ).
      RETURN.
    ENDIF.

    " Print first p_max
    DATA lc TYPE i VALUE 0.
    io_out->write( |{ 'Material' WIDTH = 18 ALIGN = LEFT }  { 'Description' }| ).
    LOOP AT lt_ce INTO DATA(ls_ce).
      lc = lc + 1.
      io_out->write( |{ ls_ce-matnr  WIDTH = 18 ALIGN = LEFT }  { ls_ce-maktx }| ).
      IF lc >= p_max. EXIT. ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD if_oo_adt_classrun~main.
    " Minimal run (no IN param handling)
    zcl_get_ecc_materials=>test_fetch_material(
      p_max  = 10
      io_out = out
    ).
  ENDMETHOD.


METHOD if_rap_query_provider~select.

  TRY.
      " --- Paging & (optional) $search ---
      DATA lv_offset    TYPE i.
      DATA lv_page_size TYPE i.
      DATA lv_search    TYPE string.

      lv_offset    = io_request->get_paging( )->get_offset( ).
      lv_page_size = io_request->get_paging( )->get_page_size( ).
      IF io_request->get_search_expression( ).
        lv_search = io_request->get_search_expression( ).
      ENDIF.

      " --- RAP coverage: acknowledge $orderby (do not remove) ---
      DATA lv_sort_cov TYPE i.
      lv_sort_cov = lines( io_request->get_sort_elements( ) ).

      " --- RFC destination ---
      DATA lv_dest TYPE rfcdest.
      lv_dest = zcl_get_ecc_materials=>get_rfc_dest( ).

      " --- Material selection for BAPI ---
      DATA lt_sel TYPE STANDARD TABLE OF ty_matnrsel_proxy WITH EMPTY KEY.
      DATA ls_sel TYPE ty_matnrsel_proxy.
      DATA lv_any_matnr_filter TYPE abap_bool VALUE abap_false.

      " 1) Try to take MATNR from $filter (preferred in VH)
      DATA lo_filter TYPE REF TO if_rap_query_filter.
      lo_filter = io_request->get_filter( ).  " coverage for $filter
      IF lo_filter IS BOUND.
        DATA lt_name_range_pairs TYPE if_rap_query_filter=>tt_name_range_pairs.
        lt_name_range_pairs = lo_filter->get_as_ranges( ).

        DATA ls_pair LIKE LINE OF lt_name_range_pairs.
        DATA ls_rng  LIKE LINE OF ls_pair-range.
        DATA lv_name TYPE string.

        LOOP AT lt_name_range_pairs INTO ls_pair.
          lv_name = ls_pair-name.
          TRANSLATE lv_name TO UPPER CASE.
          IF lv_name = 'MATNR'.
            LOOP AT ls_pair-range INTO ls_rng WHERE sign = 'I'.
              CLEAR ls_sel.
              ls_sel-sign = 'I'.
              ls_sel-option = ls_rng-option.

              " Normalize/alpha-pad numeric-only inputs for EQ/BT
              DATA lv_low  TYPE c LENGTH 40.
              DATA lv_high TYPE c LENGTH 40.
              lv_low  = ls_rng-low.
              lv_high = ls_rng-high.

              IF lv_low  IS NOT INITIAL AND lv_low  CO ' 0123456789'.
                CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
                  EXPORTING input = lv_low
                  IMPORTING output = lv_low.

                  lv_low = lv_low+22(18).
              ENDIF.
              IF lv_high IS NOT INITIAL AND lv_high CO ' 0123456789'.
                CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
                  EXPORTING input = lv_high
                  IMPORTING output = lv_high.
                lv_high = lv_high+22(18).
              ENDIF.

              " Fill 18-char fields (BAPI classic) and long (if FM supports)
              ls_sel-matnr_low       = lv_low(18).
              ls_sel-matnr_high      = lv_high(18).
              ls_sel-matnr_low_long  = lv_low.
              ls_sel-matnr_high_long = lv_high.

              APPEND ls_sel TO lt_sel.
              lv_any_matnr_filter = abap_true.
            ENDLOOP.
          ENDIF.
        ENDLOOP.
      ENDIF.

      " 2) If no $filter on MATNR: use $search (contains) if present
      IF lv_any_matnr_filter = abap_false AND lv_search IS NOT INITIAL.
        CLEAR ls_sel.
        ls_sel-sign        = 'I'.
        ls_sel-option      = 'CP'.
        " pattern *term* goes into both 18-char and long fields
        DATA lv_pat TYPE string.
        lv_pat = lv_search.
        " Surround with * for contains
        CONCATENATE '*' lv_pat '*' INTO lv_pat.
        ls_sel-matnr_low       = lv_pat(18).
        CLEAR ls_sel-matnr_high.
        ls_sel-matnr_low_long  = lv_pat.
        CLEAR ls_sel-matnr_high_long.
        APPEND ls_sel TO lt_sel.
        lv_any_matnr_filter = abap_true.
      ENDIF.

      " 3) If neither filter nor search: full (modest) range
      IF lv_any_matnr_filter = abap_false.
        CLEAR ls_sel.
        ls_sel-sign        = 'I'.
        ls_sel-option      = 'BT'.
        ls_sel-matnr_low   = '000000000000000000'. " 18x '0'
        ls_sel-matnr_high  = 'ZZZZZZZZZZZZZZZZZZ'. " 18x 'Z'
        CLEAR ls_sel-matnr_low_long.
        CLEAR ls_sel-matnr_high_long.
        APPEND ls_sel TO lt_sel.
      ENDIF.

      " --- Cap backend rows (enough for the page + headroom) ---
      DATA lv_maxrows TYPE i.
      IF lv_page_size = if_rap_query_paging=>page_size_unlimited OR lv_page_size = 0.
        lv_maxrows = 200.
      ELSE.
        lv_maxrows = lv_offset + lv_page_size + 50.
      ENDIF.

      " --- Call BAPI ---
      DATA lt_list   TYPE STANDARD TABLE OF ty_bapimatlst_proxy WITH EMPTY KEY.
      DATA lt_return TYPE STANDARD TABLE OF bapiret2           WITH EMPTY KEY.
      CALL FUNCTION 'BAPI_MATERIAL_GETLIST'
        DESTINATION lv_dest
        EXPORTING
          maxrows        = lv_maxrows
        TABLES
          matnrselection = lt_sel
          matnrlist      = lt_list
          return         = lt_return.

      " Handle BAPI error messages gracefully (avoid 501)
      READ TABLE lt_return WITH KEY type = 'E' INTO DATA(ls_ret).
      IF sy-subrc = 0.
        io_response->set_total_number_of_records( CONV int8( 0 ) ).
        io_response->set_data( VALUE tt_ecc_material( ) ).
        RETURN.
      ENDIF.

      " --- Map to CE rows ---
      DATA lt_mat TYPE tt_ecc_material.
      IF lt_list IS NOT INITIAL.
        DATA ls_ce   TYPE zstr_ecc_material.
        DATA ls_bapi TYPE ty_bapimatlst_proxy.
        LOOP AT lt_list INTO ls_bapi.
          CLEAR ls_ce.
          ls_ce-matnr = ls_bapi-material.
          ls_ce-maktx = ls_bapi-matl_desc.
          APPEND ls_ce TO lt_mat.
        ENDLOOP.
      ENDIF.

      " --- Deterministic order & paging ---
      SORT lt_mat BY matnr.

      DATA lv_total_i TYPE i.
      lv_total_i = lines( lt_mat ).

      IF lv_page_size <> if_rap_query_paging=>page_size_unlimited AND lv_page_size > 0.
        DATA lv_from TYPE i.
        DATA lv_to   TYPE i.
        lv_from = lv_offset + 1.
        lv_to   = lv_offset + lv_page_size.
        IF lv_to > lv_total_i. lv_to = lv_total_i. ENDIF.

        IF lv_from > lv_total_i OR lv_total_i = 0.
          CLEAR lt_mat.
        ELSE.
          DATA lt_page TYPE tt_ecc_material.
          DATA ls_row  TYPE zstr_ecc_material.
          LOOP AT lt_mat INTO ls_row FROM lv_from TO lv_to.
            APPEND ls_row TO lt_page.
          ENDLOOP.
          lt_mat = lt_page.
        ENDIF.
      ENDIF.

      io_response->set_total_number_of_records( CONV int8( lv_total_i ) ).
      io_response->set_data( lt_mat ).

    CATCH cx_root.
      io_response->set_total_number_of_records( CONV int8( 0 ) ).
      io_response->set_data( VALUE tt_ecc_material( ) ).
  ENDTRY.

ENDMETHOD.





ENDCLASS.

