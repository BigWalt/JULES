










!******************************COPYRIGHT**************************************
! (c) Centre for Ecology and Hydrology.
! All rights reserved.
!
! This routine has been licensed to the other JULES partners for use and
! distribution under the JULES collaboration agreement, subject to the terms
! and conditions set out therein.
!
! [Met Office Ref SC0237] 
!******************************COPYRIGHT**************************************

MODULE jules_soil_biogeochem_mod

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Contains soil biogeochemistry options and parameters, and a namelist for
!   setting them.
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------

! Public scope by default.

!-----------------------------------------------------------------------------
! Module constants
!-----------------------------------------------------------------------------
! Parameters identifying alternative soil biogeochemistry models.
! (The "bgc" is for biogeochemistry!)
! These should be >0 and unique.
INTEGER, PARAMETER ::                                                       &
  soil_model_1pool = 1,                                                     &
    ! A 1-pool model of soil carbon turnover in which the pool is not
    ! prognostic (not updated). Historically this was only used to
    ! calculate soil respiration when the TRIFFID vegetation model was not
    ! used.
  soil_model_rothc = 2,                                                     &
    ! The RothC (4 pool) model of soil carbon. Historically this was
    ! bundled with the TRIFFID vegetation model, with l_triffid=.TRUE.
    ! effectively implying RothC was used.
  soil_model_ecosse = 3
    ! ECOSSE model of soil carbon and nitrogen.

! Parameters identifying alternative wetland methane substrate models.
! These should be >0 and unique.
INTEGER, PARAMETER ::                                                       &
  ch4_substrate_soil = 1,                                                   &
    ! Soil carbon provides the substrate for wetland methane emissions.
  ch4_substrate_npp = 2,                                                    &
    ! NPP provides the substrate for wetland methane emissions.
  ch4_substrate_soil_resp = 3
    ! Soil respiration provides the substrate for wetland methane emissions.

!-----------------------------------------------------------------------------
! Module variables
!-----------------------------------------------------------------------------

! Items set in namelist jules_soil_biogeochem.

!-----------------------------------------------------------------------------
! Switches that control what soil model is used.
!-----------------------------------------------------------------------------
INTEGER ::                                                                  &
  soil_bgc_model = soil_model_1pool
    ! Indicates choice of soil model.
    ! Valid values are given by the soil_model_* parameters.

!-----------------------------------------------------------------------------
! Namelist variables used by both 1pool and RothC models.
!-----------------------------------------------------------------------------
REAL ::                                                                     &
  q10_soil = 2.0
    ! Q10 factor for soil respiration.

LOGICAL ::                                                                  &
  l_layeredC = .FALSE.,                                                     &
    ! Switch to select layered soil carbon model.
    ! .TRUE.  = use layered model
    ! .FALSE. = no layers (bulk pool)
  l_q10 = .TRUE.,                                                           &
    ! Switch for temperature function for soil respiration.
    ! .TRUE.  = use Q10 formulation
    ! .FALSE. = use RothC formulation
! Switch for bug fix.
    l_soil_resp_lev2 = .FALSE.,                                             &
      ! Switch used to control the soil tempoerature and moisture used
      ! un the soil respiration calculation.
      ! .TRUE.  means use total (frozen+unfrozen) soil moisture.
      ! .FALSE. means use unfrozen soil moisture.
      ! Depending on l_layeredC, l_soil_resp_lev2 can affect the layer from
      ! which the temperature and moisture are taken for respiration.
      ! If l_layeredC=.TRUE.: use T and moisture for each layer.
      ! If l_layeredC=.FALSE.:
      !   l_soil_resp_lev2=T means uses T and moisture from layer 2
      !   l_soil_resp_lev2=F means uses T and moisture from layer 1
    l_ch4_interactive = .FALSE.,                                             &
        ! Switch to couple methane release into the carbon cycle
        ! CH4 flux will be removed from soil carbon pools.
        ! Must have l_ch4_tlayered = .TRUE.
    l_ch4_tlayered = .FALSE.
        ! Switch to calculate CH4 according to layered soil temperature
        ! instead of top 1m average.

INTEGER ::                                                                    &
  ch4_substrate = ch4_substrate_soil
    ! Indicates choice of methane substratel model.
    ! Valid values are given by the ch4_substrate_* parameters.

!-----------------------------------------------------------------------------
! Namelist variables used only by the 1-pool model.
!-----------------------------------------------------------------------------
REAL ::                                                                     &
  kaps = 0.5e-8
    ! Specific soil respiration rate at 25 degC and optimum soil moisture
    ! (s-1). Only used for 1-pool model.

!-----------------------------------------------------------------------------
! Namelist variables used only by the RothC model.
!-----------------------------------------------------------------------------
REAL ::                                                                     &
  bio_hum_CN = 10.0,                                                        &
    ! Soil Bio and Hum CN ratio parameter
  sorp = 10.0,                                                              &
    ! Soil inorganic N factor in leaching.
  N_inorg_turnover = 1.0,                                                   &
    ! Inorganic N turnover rate (per 360 days).
  diff_n_pft = 100.0,                                                       &
    ! Inorganic N diffusion in soil (determines how quickly it reaches the
    ! roots after the roots uptake from the soil around them)
    ! per 360 days. Should be quicker than the turnover rate of inorganic 
    ! N hence choice of value (100 vs 1).
  tau_resp = 2.0
    ! Parameter controlling decay of respiration with depth (m-1)

REAL ::                                                                     &
  kaps_roth(4) = (/ 3.22e-7, 9.65e-9, 2.12e-8, 6.43e-10 /)
    ! Specific soil respiration rate for RothC (s-1).

!-----------------------------------------------------------------------------
! Namelist variables that are potentially used by more than one soil model.
!-----------------------------------------------------------------------------
REAL ::                                                                       &
  tau_lit = 5.0
    ! Parameter controlling the decay of litter inputs with depth (m-1).

!-----------------------------------------------------------------------------
! Namelist variables used in the CH4 Emission Scheme
!-----------------------------------------------------------------------------
REAL ::                                                                     &
  t0_ch4 = 273.15,                                                          &
      ! Reference temperature for the Q10 function in the CH4 calculations
      ! (T_0 in equations 74 and 75 in Clark et al. 2010)
  const_ch4_cs = 7.41e-12,                                                  &
      ! Scale factor for CH4 emissions when soil carbon is the substrate 
      ! (k in equation 74 in Clark et al. 2010)
      ! In the case of UM simulations const_ch4_cs should be set to specific
      ! values depending on vegation model. These are currently set using the
      ! rose app-upgrade functionality
      ! The UM values are:  
      !       IF (l_triffid==false)  const_ch4_cs = 5.41e-12
      !       IF (l_triffid==true)   const_ch4_cs = 5.41e-10
  const_ch4_npp  = 9.99e-3,                                                 &
      ! Scale factor for CH4 emissions when NPP is the substrate 
      ! (k in equation 74 in Clark et al. 2010)
  const_ch4_resps  = 4.36e-3,                                               &
      ! Scale factor for CH4 emissions when soil resp. is the substrate 
      ! (k in equation 74 in Clark et al. 2010)
  q10_ch4_cs = 3.7,                                                         &
      ! Q10 factor for CH4 emissions when soil carbon is the substrate 
      ! ( Q10_CH4(T_0) in equation 75 in Clark et al. 2010 )
  q10_ch4_npp  = 1.5,                                                       &
      ! Q10 factor for CH4 emissions when NPP is the substrate
      ! ( Q10_CH4(T_0) in equation 75 in Clark et al. 2010 )
  q10_ch4_resps  = 1.5
      ! Q10 factor for CH4 emissions when soil resp. is the substrate 
      ! ( Q10_CH4(T_0) in equation 75 in Clark et al. 2010 )

!-----------------------------------------------------------------------------
! Namelist definition 
!-----------------------------------------------------------------------------
NAMELIST  / jules_soil_biogeochem/                                            &
! Shared
    soil_bgc_model, ch4_substrate, kaps, kaps_roth, q10_soil, sorp,           &
    n_inorg_turnover, diff_n_pft, tau_resp, tau_lit, bio_hum_CN, l_layeredC,  &
    l_q10, l_soil_resp_lev2, l_ch4_interactive, l_ch4_tlayered, t0_ch4,       &
    const_ch4_cs, const_ch4_npp, const_ch4_resps, q10_ch4_cs, q10_ch4_npp,    &
    q10_ch4_resps

CHARACTER(LEN=*), PARAMETER, PRIVATE ::                                       &
  ModuleName = 'JULES_SOIL_BIOGEOCHEM_MOD'

CONTAINS
  
!#############################################################################

SUBROUTINE check_jules_soil_biogeochem()

USE jules_surface_mod, ONLY:                                               &
  ! imported scalars
  l_aggregate

USE jules_vegetation_mod, ONLY:                                            &
  ! imported scalars
  l_crop, l_landuse, l_triffid, l_trif_fire, triffid_period


USE ereport_mod, ONLY: ereport

!-----------------------------------------------------------------------------
! Description:
!   Checks JULES_SOIL_BIOGEOCHEM namelist for consistency.
!-----------------------------------------------------------------------------

IMPLICIT NONE

! Local scalar parameters.
INTEGER :: errorstatus
 
CHARACTER(LEN=*), PARAMETER ::                                              &
   RoutineName = 'check_jules_soil_biogeochem'   ! Name of this procedure.

! Check that a valid soil model is selected.
SELECT CASE ( soil_bgc_model )
CASE ( soil_model_1pool, soil_model_rothc, soil_model_ecosse )
  !  Acceptable values.
CASE DEFAULT
  errorstatus = 100
  CALL ereport( TRIM(routineName), errorstatus,                           &
                "Invalid value for soil model" )
END SELECT
    
! Check that a suitable soil model is used with TRIFFID.
IF ( l_triffid ) THEN
  SELECT CASE ( soil_bgc_model )
  CASE ( soil_model_ecosse, soil_model_rothc )
    ! These are OK.
  CASE ( soil_model_1pool ) 
    errorstatus = 100
    CALL ereport(TRIM(RoutineName), errorstatus,                          &
                 'TRIFFID needs a prognostic soil model - use RothC.')
  END SELECT
END IF

SELECT CASE ( ch4_substrate )
CASE (  ch4_substrate_npp, ch4_substrate_soil, ch4_substrate_soil_resp )
  ! Acceptable values, nothing to do.
CASE DEFAULT
  errorstatus = 100
  CALL ereport(TRIM(RoutineName), errorstatus,                              &
               "Invalid value for ch4_substrate" )
END SELECT

IF ( l_ch4_interactive .AND. .NOT. l_ch4_tlayered ) THEN
  errorstatus = 100
  CALL ereport( TRIM(RoutineName), errorstatus,                             &
      'To couple CH4 to soil carbon (l_ch4_interactive) you must use' //    &
      'the layered soil temperature calculation (l_ch4_tlayered)' )
END IF

! Check that certain soil models are only used with a vegetation model.
SELECT CASE ( soil_bgc_model )
CASE ( soil_model_ecosse, soil_model_rothc )
  IF ( .NOT. l_triffid ) THEN
    errorstatus = 100
    CALL ereport( RoutineName, errorstatus,                               &
                  'RothC and ECOSSE soil models need a veg model ' //     &
                  '(TRIFFID). Set l_triffid=T.' )
  END IF
END SELECT

! Check that certain soil models are not used with the aggregate surface
! scheme. At present this follows from needing TRIFFID, but test again.
SELECT CASE ( soil_bgc_model )
CASE ( soil_model_ecosse, soil_model_rothc )
  IF ( l_aggregate ) THEN
    errorstatus = 100
    CALL ereport( RoutineName, errorstatus,                               &
                  'RothC and ECOSSE soil models cannot be used with ' //  &
                  'the aggregated surface scheme (l_aggregate = true)' )
  END IF
END SELECT

! If using the single-pool C model make sure l_q10=T.
! This is done anyway in MICROBE, so might as well do here so that it is
! reported to the user.
IF ( soil_bgc_model == soil_model_1pool ) l_q10 = .TRUE.

! Check that ECOSSE is not used with l_trif_fire=T. This combination
! would require that further code is added to ECOSSE.
IF ( soil_bgc_model == soil_model_ecosse .AND. l_trif_fire ) THEN
  errorstatus = 100
  CALL ereport( RoutineName, errorstatus,                                   &
                'ECOSSE cannot be used with l_trif_fire=T' )
END IF


END SUBROUTINE check_jules_soil_biogeochem

!#############################################################################

SUBROUTINE print_nlist_jules_soil_biogeochem()

USE jules_print_mgr, ONLY: jules_print

IMPLICIT NONE

CHARACTER(LEN=50000) :: lineBuffer

CALL jules_print('jules_soil_biogeochem_mod',                             &
                 'Contents of namelist jules_soil_biogeochem')

WRITE(lineBuffer,*) ' soil_bgc_model = ', soil_bgc_model
CALL jules_print('jules_soil_biogeochem_mod',lineBuffer)

WRITE(lineBuffer, *) ' l_layeredC = ', l_layeredC
CALL jules_print('jules_soil_biogeochem_mod',lineBuffer)

WRITE(lineBuffer,*) ' l_q10 = ',l_q10
CALL jules_print('jules_soil_biogeochem_mod',lineBuffer)

WRITE(lineBuffer,*) ' l_soil_resp_lev2 = ',l_soil_resp_lev2
CALL jules_print('jules_soil_biogeochem_mod',lineBuffer)

WRITE(lineBuffer, *) ' q10_soil = ', q10_soil
CALL jules_print('jules_soil_biogeochem_mod', lineBuffer)

WRITE(lineBuffer, *) ' kaps = ', kaps
CALL jules_print('jules_soil_biogeochem_mod', lineBuffer)

WRITE(lineBuffer, *) ' kaps_roth = ', kaps_roth
CALL jules_print('jules_soil_biogeochem_mod', lineBuffer)

WRITE(lineBuffer, *) ' sorp = ', sorp
CALL jules_print('jules_soil_biogeochem_mod', lineBuffer)

WRITE(lineBuffer, *) ' bio_hum_CN = ', bio_hum_CN
CALL jules_print('jules_soil_biogeochem_mod', lineBuffer)

WRITE(lineBuffer, *) ' n_inorg_turnover = ', n_inorg_turnover
CALL jules_print('jules_soil_biogeochem_mod', lineBuffer)

WRITE(lineBuffer, *) ' tau_resp = ', tau_resp
CALL jules_print('jules_soil_biogeochem_mod', lineBuffer)

WRITE(lineBuffer, *) ' tau_lit = ', tau_lit
CALL jules_print('jules_soil_biogeochem_mod', lineBuffer)

WRITE(lineBuffer, *) ' diff_n_pft = ', diff_n_pft
CALL jules_print('jules_soil_biogeochem_mod', lineBuffer)

WRITE(lineBuffer, *) ' l_ch4_interactive = ', l_ch4_interactive
CALL jules_print('jules_soil_biogeochem_mod',lineBuffer)

WRITE(lineBuffer, *) ' l_ch4_tlayered = ', l_ch4_tlayered
CALL jules_print('jules_soil_biogeochem_mod',lineBuffer)

WRITE(lineBuffer, *) ' ch4_substrate = ', ch4_substrate
CALL jules_print('jules_soil_biogeochem_mod',lineBuffer)

WRITE(lineBuffer, *) '  t0_ch4 = ', t0_ch4
CALL jules_print('jules_soil_biogeochem_mod', lineBuffer)
    
WRITE(lineBuffer, *) '  const_ch4_cs = ', const_ch4_cs
CALL jules_print('jules_soil_biogeochem_mod', lineBuffer)
    
WRITE(lineBuffer, *) '  const_ch4_npp = ', const_ch4_npp
CALL jules_print('jules_soil_biogeochem_mod', lineBuffer)
    
WRITE(lineBuffer, *) '  const_ch4_resps = ', const_ch4_resps
CALL jules_print('jules_soil_biogeochem_mod', lineBuffer)
    
WRITE(lineBuffer, *) '  q10_ch4_cs = ', q10_ch4_cs
CALL jules_print('jules_soil_biogeochem_mod', lineBuffer)
    
WRITE(lineBuffer, *) '  q10_ch4_npp = ', q10_ch4_npp
CALL jules_print('jules_soil_biogeochem_mod', lineBuffer)
    
WRITE(lineBuffer, *) 'q10_ch4_resps = ', q10_ch4_resps
CALL jules_print('jules_soil_biogeochem_mod', lineBuffer)

END SUBROUTINE print_nlist_jules_soil_biogeochem

!#############################################################################


END MODULE jules_soil_biogeochem_mod
