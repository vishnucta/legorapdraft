//-----------------------------------------------------------------------*
//* View Name     : ZPRODSTAT_I_VH2
//* Title         : Value Help View for Production Status 
//* Create Date   : 12-Nov-2020
//* Release       : ABAP Platform 1809 (754)
//* Author        : Vishnu P/vishnucta@gmail.com(p1940421247)
//*----------------------------------------------------------------------*
//* Description   : Value Help View for Production Status 
//*
//*
//*
//*-----------------------------------------------------------------------*
//* CHANGE HISTORY
//*-----------------------------------------------------------------------*
//*Date        | User ID      |Description                   |Change Label *
//*-----------------------------------------------------------------------*
//* 12-Nov-2020|p1940421247   | Initial                      |             *
//*            |              |                              |             *
//*            |              |                              |             *
//*-----------------------------------------------------------------------*


@AbapCatalog.sqlViewName: 'ZPRODSTATIVH2'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'View for Market Value Help'

@Search.searchable: true


// Use of DB DD07T isnt permitted , cannot access domain ZPRODSTATUS

define view ZPRODSTAT_I_VH2
  as select from zprodstat_d as ProdStat



{
      @Search.defaultSearchElement: true
      @ObjectModel.text.element: ['StatName']
  key production_status as ProdStatus,
      @Semantics.text: true
      status_name       as StatName

}
