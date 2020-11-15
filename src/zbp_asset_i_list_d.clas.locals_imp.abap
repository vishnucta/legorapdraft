CLASS lhc_Asset DEFINITION INHERITING FROM cl_abap_behavior_handler.

*  PUBLIC SECTION.
*    CLASS-DATA:
*                 oref   TYPE REF TO cx_root .

  PRIVATE SECTION.

    METHODS get_instance_features FOR INSTANCE FEATURES
      IMPORTING keys REQUEST requested_features FOR asset RESULT result.

*    METHODS validate_asset FOR VALIDATE ON SAVE IMPORTING keys FOR asset~validateAsset.
*    METHODS validate_ready_date FOR VALIDATE ON SAVE IMPORTING keys FOR asset~validateReadyDate.

ENDCLASS.

CLASS lhc_Asset IMPLEMENTATION.

  METHOD get_instance_features.

    "%control-<fieldname> specifies which fields are read from the entities

*    READ ENTITY zasset_i_list_d FROM VALUE #( FOR keyval IN keys
*                                                      (  %key                    = keyval-%key
*                                                         %control-production_status = if_abap_behv=>mk-on
*                                                        ) )
*                                RESULT DATA(lt_asset_result).






*    READ ENTITIES OF zasset_i_list_d IN LOCAL MODE
*ENTITY asset
*FIELDS ( asset_uuid asset_id )
*WITH CORRESPONDING #( keys )
*RESULT DATA(lt_asset_result)
*FAILED failed.
*
*
**    "Return that controls the feature %features-%action-<action name>
**    result = VALUE #( FOR ls_asset IN lt_asset_result
**                       ( %key                           = ls_asset-%key
**                         %field-asset_id             = if_abap_behv=>fc-f-read_only
**
**                      ) ).
*
*
*    result = VALUE #( FOR ls_asset IN lt_asset_result
*                              ( %tky                   = ls_asset-%tky
*
*                                %field-asset_id      = if_abap_behv=>fc-f-read_only
*
*                              ) ).

  ENDMETHOD.

  " Handler Method for Asset validation
*  METHOD validate_asset.
*
*    DATA lv_numpart TYPE i.
*    DATA lv_charpart(3) TYPE c.
*
*
*
*    " (1) Read relevant asset instance data
*    READ ENTITIES OF zasset_i_list_d IN LOCAL MODE
*    ENTITY asset
*     FIELDS ( asset_uuid asset_id asset_name )
*     WITH CORRESPONDING #(  keys )
*    RESULT DATA(lt_asset).
*
*
*    " (2) Raise msg for Invalid asset id , name and link and validate the Asset ID Format
*    LOOP AT lt_asset INTO DATA(ls_asset).
*
*
*      IF ls_asset-asset_id IS NOT INITIAL.
*        "Extraction of character from Asset ID
*        lv_charpart = ls_asset-asset_id+0(3).
*        " If Asset ID  doesnt have first 3 Character as AID
*        IF lv_charpart NE 'AID'.
*          APPEND VALUE #(  asset_uuid = ls_asset-asset_uuid ) TO failed-asset.
*          APPEND VALUE #(  asset_uuid = ls_asset-asset_uuid
*                           %msg = new_message( id        = 'ZASSET_CM'
*                                               number    = '006'
*                                               v1        = ls_asset-asset_id
*                                               severity  = if_abap_behv_message=>severity-error )
*                           %element-asset_id = if_abap_behv=>mk-on )
*            TO reported-asset.
*        ENDIF.
*
*        "Try catch statement while type casting from String to Integer
*        TRY.
*            lv_numpart = ls_asset-asset_id+3(5).
*          CATCH cx_root INTO oref.
*            "If typecasting fails trigger a user error
*            APPEND VALUE #(  asset_uuid = ls_asset-asset_uuid ) TO failed-asset.
*            APPEND VALUE #(  asset_uuid = ls_asset-asset_uuid
*                             %msg = new_message( id        = 'ZASSET_CM'
*                                                 number    = '005'
*                                                 v1        = ls_asset-asset_id
*                                                 severity  = if_abap_behv_message=>severity-error )
*                             %element-asset_id = if_abap_behv=>mk-on )
*              TO reported-asset.
*        ENDTRY.
*      ENDIF.
*
*      "If User enters Empty Asset ID
*      IF ls_asset-asset_id IS INITIAL.
*
*        APPEND VALUE #(  asset_uuid = ls_asset-asset_uuid ) TO failed-asset.
*        APPEND VALUE #(  asset_uuid = ls_asset-asset_uuid
*                         %msg = new_message( id        = 'ZASSET_CM'
*                                             number    = '001'
*                                             v1        = ls_asset-asset_id
*                                             severity  = if_abap_behv_message=>severity-error )
*                         %element-asset_id = if_abap_behv=>mk-on )
*          TO reported-asset.
*        "If user enters empty Asset Name
*      ELSEIF ls_asset-asset_name IS INITIAL.
*        APPEND VALUE #(  asset_uuid = ls_asset-asset_uuid ) TO failed-asset.
*        APPEND VALUE #(  asset_uuid = ls_asset-asset_uuid
*                         %msg = new_message( id        = 'ZASSET_CM'
*                                             number    = '002'
*                                             v1        = ls_asset-asset_name
*                                             severity  = if_abap_behv_message=>severity-error )
*                         %element-asset_name = if_abap_behv=>mk-on )
*          TO reported-asset.
*        "If User enters empty Asset Link
*      ELSEIF ls_asset-asset_link IS INITIAL.
*        APPEND VALUE #(  asset_uuid = ls_asset-asset_uuid ) TO failed-asset.
*        APPEND VALUE #(  asset_uuid = ls_asset-asset_uuid
*                         %msg = new_message( id        = 'ZASSET_CM'
*                                             number    = '003'
*                                             v1        = ls_asset-asset_link
*                                             severity  = if_abap_behv_message=>severity-error )
*                         %element-asset_link = if_abap_behv=>mk-on )
*          TO reported-asset.
*      ENDIF.
*    ENDLOOP.
*
*
*  ENDMETHOD.

  "Handler method for data validation
*  METHOD validate_ready_date.
*    " (1) Read relevant asset instance data
*    READ ENTITIES OF zasset_i_list_d IN LOCAL MODE
*    ENTITY asset
*     FIELDS ( asset_uuid asset_id ready_date )
*     WITH CORRESPONDING #(  keys )
*    RESULT DATA(lt_asset).
*
*    " (2) Raise msg for past ready date
*    LOOP AT lt_asset INTO DATA(ls_asset).
*      IF ls_asset-ready_date < cl_abap_context_info=>get_system_date( ) AND ls_asset-ready_date IS NOT INITIAL.
*
*        APPEND VALUE #(  asset_uuid = ls_asset-asset_uuid ) TO failed-asset.
*        APPEND VALUE #(  asset_uuid = ls_asset-asset_uuid
*                         %msg = new_message( id        = 'ZASSET_CM'
*                                             number    = '004'
*                                             v1        = ls_asset-ready_date
*                                             severity  = if_abap_behv_message=>severity-error )
*                         %element-ready_date = if_abap_behv=>mk-on )
*          TO reported-asset.
*      ENDIF.
*    ENDLOOP.
*  ENDMETHOD.

ENDCLASS.
