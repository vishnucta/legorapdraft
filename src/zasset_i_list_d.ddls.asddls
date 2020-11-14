//-----------------------------------------------------------------------*
//* View Name     : ZASSET_I_LIST
//* Title         : Interface View for Asset Management
//* Create Date   : 10-Nov-2020
//* Release       : ABAP Platform 1908 (755)
//* Author        : Vishnu P/vishnucta@gmail.com(p1940421247)
//*----------------------------------------------------------------------*
//* Description   : Interface View for Asset Management
//*
//*
//*
//*-----------------------------------------------------------------------*
//* CHANGE HISTORY
//*-----------------------------------------------------------------------*
//*Date        | User ID      |Description                   |Change Label *
//*-----------------------------------------------------------------------*
//* 10-Nov-2020|p1940421247   | Initial                      |             *
//*            |              |                              |             *
//*            |              |                              |             *
//*-----------------------------------------------------------------------*
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'Asset List'
define root view entity ZASSET_I_LIST_D
  as select from zasset_d as Asset


  //association [0..1] to ZPRODSTAT_I_VH    as _prodStat   on $projection.production_status = _prodStat.ProdStatus
{

  key asset_uuid,
      asset_id,
      asset_link,
      asset_name,

      production_status,
      description,
      ready_date,
      asset_type,
      theme,
      sku,
      campaign,
      audience,
      sub_audience,
      market,
      cont_mgr,
      //overall_status,
      /*-- Admin data --*/
      @Semantics.user.createdBy: true
      local_created_by,
      @Semantics.systemDateTime.createdAt: true
      local_created_at,
      @Semantics.user.lastChangedBy: true
      local_last_changed_by,
      //@Semantics.systemDateTime.lastChangedAt: true
      @Semantics.systemDateTime.localInstanceLastChangedAt: true
      local_last_changed_at,
      //total ETag field
      @Semantics.systemDateTime.lastChangedAt: true
      last_changed_at       as LastChangedAt

      /* Associations */
      //_prodStat

}
