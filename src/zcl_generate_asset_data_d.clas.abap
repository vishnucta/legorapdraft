*//&----------------------------------------------------------------------*
*//& ABAP Class Name    ZCL_GENERATE_ASSET_DATA
*//&----------------------------------------------------------------------*
*//-----------------------------------------------------------------------*
*//* Class         : ZCL_GENERATE_ASSET_DATA
*//* Title         : Class to generate data for Asset table
*//* Create Date   : 10-Nov-2020
*//* Release       : ABAP Platform 1908 (755)
*//* Author        : Vishnu P/ vishnucta@gmail.com(p1940421247)
*//* TR            :
*//*----------------------------------------------------------------------*
*//* Description   : Class to generate data for Asset table
*//*-----------------------------------------------------------------------*
*//* CHANGE HISTORY
*//*-----------------------------------------------------------------------*
*//*Date       | User ID      |Description                   |Change Label *
*//*-----------------------------------------------------------------------*
*//*10-Nov-2020| p1940421247       | Initial                      |        *
*//*-----------------------------------------------------------------------*

CLASS zcl_generate_asset_data_d DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_oo_adt_classrun.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_generate_asset_data_d IMPLEMENTATION.

  METHOD if_oo_adt_classrun~main.

*Contain logic to populate Asset,Market and Status Table

*    DATA itab TYPE TABLE OF zasset_h.
    DATA itab_stat TYPE TABLE OF ZPRODSTAT_d.
    DATA itab_market TYPE TABLE OF zmarket_d.

   "fill internal market table
    itab_market = VALUE #(
      ( marketname = 'Europe' )
      ( marketname = 'APAC' )
      ( marketname = 'MENA' )
      ( marketname = 'Local')
      ( marketname = 'US')
      ).
    DELETE FROM zmarket_d.
    INSERT zmarket_d FROM TABLE @itab_market.

*   output the result as a console message
    out->write( |{ sy-dbcnt }  Market entries inserted successfully!| ).

   "fill internal Status table
    itab_stat = VALUE #(
      ( production_status = 'D' status_name = 'Done')
      ( production_status = 'N' status_name = 'Not Started')
      ( production_status = 'A' status_name = 'Archived' )
      ( production_status = 'I' status_name = 'In Progress')
      ( production_status = 'C' status_name = 'Confidential')
      ).
    DELETE FROM ZPRODSTAT_d.
    INSERT ZPRODSTAT_d FROM TABLE @itab_stat.

*   output the result as a console message
    out->write( |{ sy-dbcnt }  Status entries inserted successfully!| ).

*   fill internal asset table
*    itab = VALUE #(
*      ( asset_id = 'AID13201' asset_link = 'https://developers.sap.com/' asset_name = 'XYZ Poster' ready_date = '20190624' asset_type = 'Image' theme = 'Duplo' sku =  60705 campaign = 'Latest Campaign'
*        description = 'mv' production_status = 'I' audience = 'Kids' sub_audience = 'Boys' market = 'Global' cont_mgr = 'John Doe' )
*      ( asset_id = 'ABC13201' asset_link = 'https://developers.sap.com/' asset_name = 'ABC Video' ready_date = '20190613' asset_type = 'Video' theme = 'Belize' sku = 12345 campaign = 'EMEA Campaign'
*        description = 'Enter your comments here' production_status = 'D' audience = 'Parents' sub_audience = 'Mother' market = 'Europe' cont_mgr = 'Kevin' )
*      ( asset_id = 'XYZ13201' asset_link = 'https://developers.sap.com/' asset_name = 'TUV Image' ready_date = '20190610' asset_type = 'File' theme = 'Dark' sku = 67890 campaign = 'MENA Campaign'
*        description = 'Enter your comments here' production_status = 'N' audience = 'Schools' sub_audience = 'Kinter' market = 'Asia' cont_mgr = 'Sam' )
*    ).

*   delete existing entries in the database table
*    DELETE FROM zasset_h.

*   insert the new table entries
*   INSERT zasset_h FROM TABLE @itab.

*   output the result as a console message
*    out->write( |{ sy-dbcnt }  entries inserted successfully!| ).


  ENDMETHOD.

ENDCLASS.
