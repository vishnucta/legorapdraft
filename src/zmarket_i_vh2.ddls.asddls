//-----------------------------------------------------------------------*
//* View Name     : ZMARKET_I_VH2
//* Title         : Value Help View for Market 
//* Create Date   : 12-Nov-2020
//* Release       : ABAP Platform 1809 (754)
//* Author        : Vishnu P/vishnucta@gmail.com(p1940421247)
//*----------------------------------------------------------------------*
//* Description   : Value Help View for Market 
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

@AbapCatalog.sqlViewName: 'ZMARKETIVH2'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'View for Market Value Help'

@Search.searchable: true

define view ZMARKET_I_VH2
  as select from zmarket_d as Market



{
      @Search.defaultSearchElement: true

  key Market.marketname as Market


}
