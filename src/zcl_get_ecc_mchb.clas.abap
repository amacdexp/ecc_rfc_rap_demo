CLASS zcl_get_ecc_mchb DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES if_rap_query_provider.
    INTERFACES if_oo_adt_classrun.

    " CE row table type
    TYPES tt_ecc_mchb TYPE STANDARD TABLE OF zstr_ecc_mchb WITH EMPTY KEY.

    " Destination resolver (TVARVC -> fallback)
    CLASS-METHODS get_rfc_dest
      RETURNING VALUE(rv_dest) TYPE rfcdest.

    " ADT console tester (no classic WRITE)
    CLASS-METHODS test_fetch_mchb
      IMPORTING
        p_max  TYPE i       DEFAULT 10
        p_dest TYPE rfcdest OPTIONAL
        io_out TYPE REF TO if_oo_adt_classrun_out OPTIONAL.

  PRIVATE SECTION.
    " RFC_READ_TABLE DDIC types:
    "  - RFC_DB_FLD (FIELDS)
    "  - RFC_DB_OPT (OPTIONS)
    "  - TAB512     (DATA lines, field WA)
    " We'll use them directly to avoid type mismatches.

    " Core used by RAP + console
    CLASS-METHODS fetch_mchb_core
      IMPORTING
        iv_dest  TYPE rfcdest OPTIONAL
      EXPORTING
        ev_subrc TYPE i
        ev_msg   TYPE string
      RETURNING
        VALUE(rt_rows) TYPE tt_ecc_mchb.

ENDCLASS.



CLASS zcl_get_ecc_mchb IMPLEMENTATION.

  METHOD get_rfc_dest.
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
      rv_dest = 'S4HCLNT100'.  " fallback: set your default destination
    ENDIF.
  ENDMETHOD.


  METHOD fetch_mchb_core.
    DATA lv_dest TYPE rfcdest.
    IF iv_dest IS NOT INITIAL.
      lv_dest = iv_dest.
    ELSE.
      lv_dest = zcl_get_ecc_mchb=>get_rfc_dest( ).
    ENDIF.

    " Prepare RFC_READ_TABLE inputs
    DATA lt_fields  TYPE STANDARD TABLE OF rfc_db_fld WITH EMPTY KEY.
    DATA ls_field   TYPE rfc_db_fld.
    CLEAR ls_field. ls_field-fieldname = 'MATNR'. APPEND ls_field TO lt_fields.
    CLEAR ls_field. ls_field-fieldname = 'WERKS'. APPEND ls_field TO lt_fields.
    CLEAR ls_field. ls_field-fieldname = 'LGORT'. APPEND ls_field TO lt_fields.
    CLEAR ls_field. ls_field-fieldname = 'CHARG'. APPEND ls_field TO lt_fields.
    CLEAR ls_field. ls_field-fieldname = 'CLABS'. APPEND ls_field TO lt_fields.

    DATA lt_options TYPE STANDARD TABLE OF rfc_db_opt WITH EMPTY KEY.
    " Optional: add WHERE clauses here, e.g. only non-zero stock
    " APPEND VALUE rfc_db_opt( text = `CLABS <> 0` ) TO lt_options.

    DATA lt_data TYPE STANDARD TABLE OF tab512 WITH EMPTY KEY.

    " Use a delimiter that won't appear in numeric fields
    DATA lv_delim TYPE c LENGTH 1 VALUE '|'.

    " Call RFC_READ_TABLE on MCHB
    CALL FUNCTION 'RFC_READ_TABLE'
      DESTINATION lv_dest
      EXPORTING
        query_table = 'MCHB'
        delimiter   = lv_delim
        no_data     = ' '             " return data
        rowcount    = 0               " 0 = no limit (page later in RAP)
      TABLES
        fields      = lt_fields
        options     = lt_options
        data        = lt_data
      EXCEPTIONS
        table_not_available   = 1
        table_without_data    = 2
        option_not_valid      = 3
        field_not_valid       = 4
        not_authorized        = 5
        data_buffer_exceeded  = 6
        OTHERS                = 7.

    ev_subrc = sy-subrc.
    IF ev_subrc <> 0.
      ev_msg = |RFC_READ_TABLE failed, subrc { sy-subrc }|.
      CLEAR rt_rows.
      RETURN.
    ENDIF.

    " Parse DATA lines: TAB512-WA contains delimited row text
    DATA lt_tokens TYPE STANDARD TABLE OF string WITH EMPTY KEY.
    DATA lv_clabs_s TYPE string.
    DATA lv_clabs_p TYPE p LENGTH 13 DECIMALS 3.
    DATA ls_ce      TYPE zstr_ecc_mchb.

    LOOP AT lt_data INTO DATA(ls_line).
      CLEAR lt_tokens.
      SPLIT ls_line-wa AT lv_delim INTO TABLE lt_tokens.
      IF lines( lt_tokens ) < 5.
        CONTINUE.
      ENDIF.

      CLEAR ls_ce.
      ls_ce-matnr = lt_tokens[ 1 ].
      ls_ce-plant = lt_tokens[ 2 ].
      ls_ce-lgort = lt_tokens[ 3 ].
      ls_ce-charg = lt_tokens[ 4 ].

      lv_clabs_s = lt_tokens[ 5 ].
      CONDENSE lv_clabs_s NO-GAPS.
      IF lv_clabs_s IS INITIAL.
        lv_clabs_p = 0.
      ELSE.
        " RFC_READ_TABLE returns plain number text -> implicit convert to P works
        lv_clabs_p = lv_clabs_s.
      ENDIF.
      ls_ce-clabs = lv_clabs_p.

      ls_ce-uom = 'L'.   " internal unit code (CHAR(3)); adjust if needed

      APPEND ls_ce TO rt_rows.
    ENDLOOP.

    SORT rt_rows BY matnr plant lgort charg.
  ENDMETHOD.


  METHOD test_fetch_mchb.
    IF io_out IS NOT BOUND.
      RETURN. " cloud-friendly: only ADT console
    ENDIF.

    DATA lv_dest  TYPE rfcdest.
    DATA lv_subrc TYPE i.
    DATA lv_msg   TYPE string.
    DATA lt_rows  TYPE tt_ecc_mchb.

    lv_dest = COND rfcdest( WHEN p_dest IS NOT INITIAL THEN p_dest
                            ELSE zcl_get_ecc_mchb=>get_rfc_dest( ) ).

    lt_rows = zcl_get_ecc_mchb=>fetch_mchb_core(
                EXPORTING iv_dest  = lv_dest
                IMPORTING ev_subrc = lv_subrc
                          ev_msg   = lv_msg ).

    io_out->write( |--- ZCL_GET_ECC_MCHB=>TEST_FETCH_MCHB ---| ).
    io_out->write( |RFC destination       : { lv_dest }| ).
    io_out->write( |sy-subrc (core)       : { lv_subrc }| ).
    IF lv_msg IS NOT INITIAL.
      io_out->write( |Message              : { lv_msg }| ).
    ENDIF.
    io_out->write( |Rows (mapped)         : { lines( lt_rows ) }| ).

    IF lt_rows IS INITIAL.
      io_out->write( |No rows returned.| ).
      RETURN.
    ENDIF.

    " Print first p_max
    DATA lc TYPE i VALUE 0.
    io_out->write( |{ 'MATNR' WIDTH = 40 } { 'WERKS' WIDTH = 4 } { 'LGORT' WIDTH = 4 } { 'CHARG' WIDTH = 10 } { 'CLABS' } { 'UoM' WIDTH = 3 }| ).
    LOOP AT lt_rows INTO DATA(ls_row).
      lc = lc + 1.
      io_out->write( |{ ls_row-matnr WIDTH = 40 } { ls_row-plant WIDTH = 4 } { ls_row-lgort WIDTH = 4 } { ls_row-charg WIDTH = 10 } { ls_row-clabs } { ls_row-uom WIDTH = 3 }| ).      IF lc >= p_max. EXIT. ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD if_oo_adt_classrun~main.
    zcl_get_ecc_mchb=>test_fetch_mchb(
      p_max  = 20
      io_out = out
    ).
  ENDMETHOD.

METHOD if_rap_query_provider~select.

  TRY.
      " --- Resolve RFC destination ---
      DATA lv_dest TYPE rfcdest.
      lv_dest = zcl_get_ecc_mchb=>get_rfc_dest( ).

      " --- Paging from request ---
      DATA lv_offset    TYPE i.
      DATA lv_page_size TYPE i.
      lv_offset    = io_request->get_paging( )->get_offset( ).
      lv_page_size = io_request->get_paging( )->get_page_size( ).

      " --- RAP coverage: acknowledge $filter and $orderby ---
      DATA lo_filter TYPE REF TO if_rap_query_filter.
      lo_filter = io_request->get_filter( ).                     " coverage for $filter
      DATA lv_sort_cov TYPE i.
      lv_sort_cov = lines( io_request->get_sort_elements( ) ).   " coverage for $orderby

      " --- RFC_READ_TABLE inputs ---
      DATA lt_fields  TYPE STANDARD TABLE OF rfc_db_fld WITH EMPTY KEY.
      DATA ls_field   TYPE rfc_db_fld.
      CLEAR ls_field. ls_field-fieldname = 'MATNR'. APPEND ls_field TO lt_fields.
      CLEAR ls_field. ls_field-fieldname = 'WERKS'. APPEND ls_field TO lt_fields.
      CLEAR ls_field. ls_field-fieldname = 'LGORT'. APPEND ls_field TO lt_fields.
      CLEAR ls_field. ls_field-fieldname = 'CHARG'. APPEND ls_field TO lt_fields.
      CLEAR ls_field. ls_field-fieldname = 'CLABS'. APPEND ls_field TO lt_fields.

      DATA lt_options TYPE STANDARD TABLE OF rfc_db_opt WITH EMPTY KEY.
      DATA ls_opt     TYPE rfc_db_opt.

      " --- Push $filter to RFC_READ_TABLE WHERE (OPTIONS) ---
      IF lo_filter IS BOUND.

        DATA lt_name_range_pairs TYPE if_rap_query_filter=>tt_name_range_pairs.
        lt_name_range_pairs = lo_filter->get_as_ranges( ).

        DATA ls_pair LIKE LINE OF lt_name_range_pairs.
        DATA lv_name TYPE string.
        DATA ls_rng  LIKE LINE OF ls_pair-range.

        LOOP AT lt_name_range_pairs INTO ls_pair.
          lv_name = ls_pair-name.
          TRANSLATE lv_name TO UPPER CASE.

          CASE lv_name.

            WHEN 'MATNR'.
              LOOP AT ls_pair-range INTO ls_rng WHERE sign = 'I'.
                DATA lv_low_m  TYPE c LENGTH 40.
                DATA lv_high_m TYPE c LENGTH 40.
                lv_low_m  = ls_rng-low.
                lv_high_m = ls_rng-high.

                " ALPHA pad numeric-only inputs for EQ/BT
                IF lv_low_m IS NOT INITIAL AND lv_low_m  CO ' 0123456789'.
                  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
                    EXPORTING input  = lv_low_m
                    IMPORTING output = lv_low_m.
                  lv_low_m = lv_low_m+22(18).
                ENDIF.
                IF lv_high_m IS NOT INITIAL AND lv_high_m CO ' 0123456789'.
                  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
                    EXPORTING input  = lv_high_m
                    IMPORTING output = lv_high_m.
                  lv_high_m = lv_high_m+22(18).
                ENDIF.

                CASE ls_rng-option.
                  WHEN 'EQ'.
                    CLEAR ls_opt.
                    CONCATENATE 'MATNR = ''' lv_low_m '''' INTO ls_opt-text.
                    APPEND ls_opt TO lt_options.

                  WHEN 'BT'.
                    IF lv_low_m IS NOT INITIAL AND lv_high_m IS NOT INITIAL.
                      CLEAR ls_opt.
                      CONCATENATE 'MATNR BETWEEN ''' lv_low_m ''' AND ''' lv_high_m ''''
                        INTO ls_opt-text.
                      APPEND ls_opt TO lt_options.
                    ENDIF.

                  WHEN 'CP'.
                    DATA lv_like_m TYPE string.
                    lv_like_m = lv_low_m.
                    REPLACE ALL OCCURRENCES OF '*' IN lv_like_m WITH '%'.
                    CLEAR ls_opt.
                    CONCATENATE 'MATNR LIKE ''' lv_like_m '''' INTO ls_opt-text.
                    APPEND ls_opt TO lt_options.
                ENDCASE.
              ENDLOOP.

            WHEN 'PLANT'. " -> WERKS
              LOOP AT ls_pair-range INTO ls_rng WHERE sign = 'I'.
                CASE ls_rng-option.
                  WHEN 'EQ'.
                    CLEAR ls_opt.
                    CONCATENATE 'WERKS = ''' ls_rng-low '''' INTO ls_opt-text.
                    APPEND ls_opt TO lt_options.
                  WHEN 'BT'.
                    IF ls_rng-low IS NOT INITIAL AND ls_rng-high IS NOT INITIAL.
                      CLEAR ls_opt.
                      CONCATENATE 'WERKS BETWEEN ''' ls_rng-low ''' AND ''' ls_rng-high ''''
                        INTO ls_opt-text.
                      APPEND ls_opt TO lt_options.
                    ENDIF.
                  WHEN 'CP'.
                    DATA lv_like_w TYPE string.
                    lv_like_w = ls_rng-low.
                    REPLACE ALL OCCURRENCES OF '*' IN lv_like_w WITH '%'.
                    CLEAR ls_opt.
                    CONCATENATE 'WERKS LIKE ''' lv_like_w '''' INTO ls_opt-text.
                    APPEND ls_opt TO lt_options.
                ENDCASE.
              ENDLOOP.

            WHEN 'LGORT'.
              LOOP AT ls_pair-range INTO ls_rng WHERE sign = 'I'.
                CASE ls_rng-option.
                  WHEN 'EQ'.
                    CLEAR ls_opt.
                    CONCATENATE 'LGORT = ''' ls_rng-low '''' INTO ls_opt-text.
                    APPEND ls_opt TO lt_options.
                  WHEN 'BT'.
                    IF ls_rng-low IS NOT INITIAL AND ls_rng-high IS NOT INITIAL.
                      CLEAR ls_opt.
                      CONCATENATE 'LGORT BETWEEN ''' ls_rng-low ''' AND ''' ls_rng-high ''''
                        INTO ls_opt-text.
                      APPEND ls_opt TO lt_options.
                    ENDIF.
                  WHEN 'CP'.
                    DATA lv_like_l TYPE string.
                    lv_like_l = ls_rng-low.
                    REPLACE ALL OCCURRENCES OF '*' IN lv_like_l WITH '%'.
                    CLEAR ls_opt.
                    CONCATENATE 'LGORT LIKE ''' lv_like_l '''' INTO ls_opt-text.
                    APPEND ls_opt TO lt_options.
                ENDCASE.
              ENDLOOP.

            WHEN 'CHARG'.
              LOOP AT ls_pair-range INTO ls_rng WHERE sign = 'I'.
                CASE ls_rng-option.
                  WHEN 'EQ'.
                    CLEAR ls_opt.
                    CONCATENATE 'CHARG = ''' ls_rng-low '''' INTO ls_opt-text.
                    APPEND ls_opt TO lt_options.
                  WHEN 'BT'.
                    IF ls_rng-low IS NOT INITIAL AND ls_rng-high IS NOT INITIAL.
                      CLEAR ls_opt.
                      CONCATENATE 'CHARG BETWEEN ''' ls_rng-low ''' AND ''' ls_rng-high ''''
                        INTO ls_opt-text.
                      APPEND ls_opt TO lt_options.
                    ENDIF.
                  WHEN 'CP'.
                    DATA lv_like_c TYPE string.
                    lv_like_c = ls_rng-low.
                    REPLACE ALL OCCURRENCES OF '*' IN lv_like_c WITH '%'.
                    CLEAR ls_opt.
                    CONCATENATE 'CHARG LIKE ''' lv_like_c '''' INTO ls_opt-text.
                    APPEND ls_opt TO lt_options.
                ENDCASE.
              ENDLOOP.

          ENDCASE.
        ENDLOOP.
      ENDIF. " filters

      " --- Execute RFC_READ_TABLE ---
      DATA lt_data  TYPE STANDARD TABLE OF tab512 WITH EMPTY KEY.
      DATA lv_delim TYPE c LENGTH 1.
      lv_delim = '|'.

      CALL FUNCTION 'RFC_READ_TABLE'
        DESTINATION lv_dest
        EXPORTING
          query_table = 'MCHB'
          delimiter   = lv_delim
          no_data     = ' '
          rowcount    = 0              " consider a cap in prod
        TABLES
          fields      = lt_fields
          options     = lt_options
          data        = lt_data
        EXCEPTIONS
          table_not_available   = 1
          table_without_data    = 2
          option_not_valid      = 3
          field_not_valid       = 4
          not_authorized        = 5
          data_buffer_exceeded  = 6
          OTHERS                = 7.

      " --- Map RFC rows -> CE rows ---
      DATA lt_rows TYPE tt_ecc_mchb.
      IF sy-subrc = 0 AND lt_data IS NOT INITIAL.
        DATA lt_tokens TYPE STANDARD TABLE OF string WITH EMPTY KEY.
        DATA lv_clabs_s TYPE string.
        DATA lv_clabs_p TYPE p LENGTH 13 DECIMALS 3.
        DATA ls_ce      TYPE zstr_ecc_mchb.
        DATA ls_line    TYPE tab512.

        LOOP AT lt_data INTO ls_line.
          CLEAR lt_tokens.
          SPLIT ls_line-wa AT lv_delim INTO TABLE lt_tokens.
          IF lines( lt_tokens ) < 5.
            CONTINUE.
          ENDIF.

          CLEAR ls_ce.
          ls_ce-matnr = lt_tokens[ 1 ].
          ls_ce-plant = lt_tokens[ 2 ].
          ls_ce-lgort = lt_tokens[ 3 ].
          ls_ce-charg = lt_tokens[ 4 ].

          lv_clabs_s = lt_tokens[ 5 ].
          CONDENSE lv_clabs_s NO-GAPS.
          IF lv_clabs_s IS INITIAL.
            lv_clabs_p = 0.
          ELSE.
            lv_clabs_p = lv_clabs_s.
          ENDIF.
          ls_ce-clabs = lv_clabs_p.
          ls_ce-uom   = 'L'.

          APPEND ls_ce TO lt_rows.
        ENDLOOP.
      ENDIF.

      " --- Stable order & paging ---
      SORT lt_rows BY matnr plant lgort charg.

      DATA lv_total_i TYPE i.
      lv_total_i = lines( lt_rows ).

      IF lv_page_size <> if_rap_query_paging=>page_size_unlimited AND lv_page_size > 0.
        DATA lv_from TYPE i.
        DATA lv_to   TYPE i.
        lv_from = lv_offset + 1.
        lv_to   = lv_offset + lv_page_size.
        IF lv_to > lv_total_i. lv_to = lv_total_i. ENDIF.

        IF lv_from > lv_total_i OR lv_total_i = 0.
          CLEAR lt_rows.
        ELSE.
          DATA lt_page TYPE tt_ecc_mchb.
          DATA ls_row  TYPE zstr_ecc_mchb.
          LOOP AT lt_rows INTO ls_row FROM lv_from TO lv_to.
            APPEND ls_row TO lt_page.
          ENDLOOP.
          lt_rows = lt_page.
        ENDIF.
      ENDIF.

      io_response->set_total_number_of_records( CONV int8( lv_total_i ) ).
      io_response->set_data( lt_rows ).

    CATCH cx_root.
      io_response->set_total_number_of_records( CONV int8( 0 ) ).
      io_response->set_data( VALUE tt_ecc_mchb( ) ).
  ENDTRY.

ENDMETHOD.





ENDCLASS.

