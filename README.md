# Asset Management Fiori App using ABAP RESTful Application Programming Model(RAP) -   Draft Enabled

## Description

This repository contains code for Asset Management system using  ABAP RESTful Application Programming Model (RAP) in SAP Cloud Platform, ABAP environment.


Hello Team,

Welcome the Usecase start up guide.

Steps to Run :

1) Configure ABAP Cloud Platform in your personal Eclipse IDE 
    https://developers.sap.com/tutorials/abap-environment-trial-onboarding.html
    
2) Search for a package ZLCASE(ready to run App) in ABAP cloud environment<b> (There are few chances of not finding the package because of distributed instance for trial cloud    infrastructure).
    
    If Package not found , do the below steps
    
    Clone your project into your Custom Package(Total 53 Artifacts)

    Some times even though the log shows 53 , only less files get imported into package. Inference is already files are available in system and locked to a request.

    ![Import Log](https://github.com/vishnucta/vishnucta/blob/main/Importlog.JPG)

3) Do activation as per below order to avoid error. Due to some reason even though Mass activation list all aritifacts , it end up not activating all.
Select all aritifacts in below listed folder(mass activation within a folder) and activate folder by folder.

    - Domains
    - Data Element
    - Database Tables
    - Data Definitions
    - Metadata Extensions
    - Classes
    - Behavior Definitions
    - Service Definitions
    - Service Bindings


5) Just to avoid any unexpected error, just make sure all artifacts are activated in its respective folders.

6) Publish the service Binding and preview the App from the Root BO/View
    ![Preview](https://github.com/vishnucta/vishnucta/blob/main/preview.JPG)


7) Run ABAP class ZCL_GENERATE_ASSET_DATA(Make sure classes are active too) to populate table zmarket_t(Market Value Help) and ZPRODSTAT_T (Status Value Help)




Ready to run :+1:


### Common and easy fix errors : 
- Missing Value help/Smart Filter in List Report(ME View activation) , just cross check after publishing the Service Binding
- No data in Value Help (Run the Data generation class ZCL_GENERATE_ASSET_DATA)
- Unable to publish service binding (Activate in bottom up fashion starting with data element ending with service definition)
- Stuck with some TR issues or unable to delete (Add transport manager view in Eclipse and manage your task and TR appropriately )
- Always import into a clean package to avoid confusion


# Technical Design

## Behavior
It briefly defines the scope of business logic to be implemented.

### Content Manager Role

| Entity | Behavior |
| --- | --- |
| Asset | Operations:<br> <ul><li>Create, update, delete</li></ul><ul><li>Action: create travels by template</li></ul><br>Validations:<ul><li>validate editable input fields</li></ul> <br>Feature Control:<br><ul><li>Static and dynamic field control</li></ul> |


### Product Group Manager Role

| Entity | Behavior |
| --- | --- |
| Asset | Operations:<br> <ul><li>Action: Archive Asset</li></ul><ul><li>Action: Revert Asset</li></ul><br>Feature Control:<br><ul><li>Dynamic action control</li></ul> |


### Local Market Manager Role

| Entity | Behavior |
| --- | --- |
| Asset |Read operations only |


## Projection Layer in Asset Scenario

![Behavior Diagram](https://github.com/vishnucta/vishnucta/blob/main/Behavoir%20Figure.JPG)



# Expected UX 
### Content Manager Role

   <b>Screen 1 : <b>

   ![Cmgr1](https://github.com/vishnucta/vishnucta/blob/main/draft_list.JPG)  

   <b>Screen 2 : <b>

   ![Cmgr1](https://github.com/vishnucta/vishnucta/blob/main/draft_obj.JPG)  


### Product Group Manager Role
    
   <b>Screen 1 : <b>

   ![pgmgr1](https://github.com/vishnucta/vishnucta/blob/main/pgmgr%20scr1.JPG)  

### Local Market Manager Role

   <b>Screen 1 : <b>

   ![pgmgr1](https://github.com/vishnucta/vishnucta/blob/main/Lmmgr%20scr1.JPG)  


For any queries, reachout to me at vishnucta@gmail.com

