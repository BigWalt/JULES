










! *****************************COPYRIGHT****************************************
! (c) Crown copyright, Met Office. All rights reserved.
!
! This routine has been licensed to the other JULES partners for use and
! distribution under the JULES collaboration agreement, subject to the terms and
! conditions set out therein.
!
! [Met Office Ref SC0237]
! *****************************COPYRIGHT****************************************

! Code Description:
!   Language: FORTRAN 90
!
! Code Owner: Please refer to ModuleLeaders.txt
!

MODULE inferno_mod

USE conversions_mod, ONLY: s_in_day => rsec_per_day
USE parkind1,                       ONLY: jpim

IMPLICIT NONE
!
! Description:
!  Contains the computations needed by INFERNO (INteractive Fire
!  and Emissions algoRithm in Natural envirOnments).
!  Lightning and Population Density can be prescribed to influence
!  the number of ingnitions. The rest of the diagnostics depend
!  on weather (temperature, relatie humidity, precipitation)
!  and vegetation (available biomass).
!  The outputs are burnt area and emissions of key Species
!
! Code Owner: Please refer to ModuleLeaders.txt
!
! Code Description:
!    Language: Fortran 90.

PRIVATE ! Default everything as being private, unlock as needed

PUBLIC   ::  calc_ignitions, calc_flam, calc_burnt_area,                      &
             calc_emitted_carbon, calc_emitted_carbon_soil,                   &
             calc_emission

REAL,    PARAMETER        ::                                                  &
  s_in_month = 2.6280288e6,                                                   &
    ! Seconds in a month.
    ! Note that this is approx. 365 days/12months but is slightly larger.
    ! This should be changed in a future update.
  m2_in_km2  = 1.0e6

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
CHARACTER(LEN=*),  PARAMETER, PRIVATE :: ModuleName = "INFERNO_MOD"

CONTAINS

! Note: calc_ignitions, calc_flam and calc_burnt_area) are
! computed for each landpoint to ascertain no points with
! unrealistic weather contain fires.
! These are then aggregated in inferno_io_mod into pft arrays.

SUBROUTINE calc_ignitions(                                                    &
  ! Point intent(IN)
  pop_den_l, flash_rate_l, ignition_method,                                   &
  ! Point intent(OUT)
  ignitions_l)

! Description:
!     Calculate the number of ignitions/m2/s at each gridpoint
!
! Method:
!     See original paper by Pechony and Shindell (2009),
!     originally proposed for monthly totals, here per timestep.
!
! Code Owner: Please refer to ModuleLeaders.txt
!
!  Code Description:
!    Language:  Fortran 90

USE yomhook,      ONLY: lhook, dr_hook
USE parkind1,     ONLY: jprb

IMPLICIT NONE

INTEGER,    INTENT(IN)    ::                                                  &
  ignition_method
    ! The integer defining the method used for ignitions:
    ! 1 = constant,
    ! 2 = constant (Anthropogenic) + Varying (lightning),
    ! 3 = Varying  (Anthropogenic and lightning)

REAL,       INTENT(IN)    ::                                                  &
  flash_rate_l,                                                               &
    ! The Cloud to Ground lightning flash rate (flashes/km2)
  pop_den_l
    ! The population density (ppl/km2)

REAL,    INTENT(OUT)      ::                                                  &
  ignitions_l
    ! The number of ignitions/m2/s

REAL                      ::                                                  &
  man_ign_l,                                                                  &
    ! Human-induced fire ignition rate (ignitions/km2/s)
  nat_ign_l,                                                                  &
    ! Lightning natural ignition rate (number/km2/sec)
  non_sup_frac_l
    ! Fraction of fire ignition non suppressed by humans

REAL,    PARAMETER        ::                                                  &
  tune_MODIS = 7.7
    ! Parameter originally used by P&S (2009) to match MODIS

REAL(KIND=jprb)               :: zhook_handle
CHARACTER(LEN=*),  PARAMETER :: RoutineName = "CALC_IGNITIONS"

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

IF (ignition_method == 1) THEN
  nat_ign_l   = 2.7 / s_in_month / m2_in_km2 / 12.0 * 0.75
    ! Assume a multi-year annual mean of 2.7/km2/yr
    ! (Huntrieser et al. 2007) 75% are Cloud to Ground flashes
    ! (Prentice and Mackerras 1977)

  man_ign_l   = 1.5 / s_in_month / m2_in_km2
    ! We parameterised 1.5 ignitions/km2/month globally from GFED

  ignitions_l = (man_ign_l + nat_ign_l)

ELSE IF (ignition_method == 2) THEN
  nat_ign_l   = MIN(MAX(flash_rate_l / m2_in_km2 / s_in_day,0.0),1.0)
    ! Flash Rate (Cloud to Ground) always lead to one fire

  man_ign_l   = 1.5 / s_in_month / m2_in_km2
    ! We parameterised 1.5 ignitions/km2/month globally from GFED

  ignitions_l = (man_ign_l + nat_ign_l)

ELSE IF (ignition_method == 3) THEN
  nat_ign_l   = flash_rate_l / m2_in_km2 / s_in_day
    ! Flash Rate (Cloud to Ground) always lead to one fire

  man_ign_l   =  0.2 * pop_den_l**(0.4) / m2_in_km2 / s_in_month

  non_sup_frac_l =  0.05 + 0.9 * EXP(-0.05 * pop_den_l)

  ignitions_l =  (nat_ign_l + man_ign_l) * non_sup_frac_l

  ! Tune ignitions to MODIS data (see Pechony and Shindell, 2009)
  ignitions_l =  ignitions_l * tune_MODIS
END IF

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE  calc_ignitions

SUBROUTINE calc_flam(                                                         &
  !Point Intent(IN)
  temp_l, rhum_l, fuel_l, sm_l, rain_l,                                       &
  !Point Intent(INOUT)
  flam_l)

USE yomhook,      ONLY: lhook, dr_hook
USE parkind1,     ONLY: jprb

IMPLICIT NONE
!
! Description:
!   Performs the calculation of the flammibility
!
! Method:
!   In essence, utilizes weather and vegetation variables to
!   estimate how flammable a m2 is every second.
!
! Code Owner: Please refer to ModuleLeaders.txt
!
! Code Description:
!   Language: Fortran 90

! Subroutine arguments

INTEGER                   ::  land_pts

REAL ,   INTENT(IN)       ::                                                  &
  temp_l,                                                                     &
    ! Surface Air Temperature (K)
  rhum_l,                                                                     &
    ! Relative Humidity (%)
  sm_l,                                                                       &
    ! The INFERNO soil moisture fraction (sthu's 1st level)
  rain_l,                                                                     &
    ! The precipitation rate (kg.m-2.s-1)
  fuel_l
    ! The Fuel Density (0-1)

REAL,    INTENT(INOUT)    ::                                                  &
  flam_l
    ! The flammability of the cell


! These are variables to the Goff-Gratch equation
REAL,    PARAMETER        ::                                                  &
  a=-7.90298,                                                                 &
  d = 11.344,                                                                   &
  c=-1.3816e-07,                                                              &
  b = 5.02808,                                                                  &
  f = 8.1328e-03,                                                               &
  h=-3.49149,                                                                 &
  Ts = 373.16,                                                                  &
    ! Water saturation temperature
  cr=-2.0 * s_in_day,                                                         &
    ! Precipitation factor (-2(day/mm)*(kg/m2/s))
  rhum_up = 90.0,                                                               &
    ! Upper boundary to the relative humidity
  rhum_low = 10.0
    ! Lower boundary to the relative humidity

REAL                      ::                                                  &
  Z_l,                                                                        &
    ! Component of the Goff-Gratch saturation vapor pressure
  TsbyT_l,                                                                    &
    ! Reciprocal of the temperature times ts
  f_rhum_l,                                                                   &
    ! The factor dependence on relative humidity
  f_sm_l,                                                                     &
    ! The factor dependence on soil moisture
  rain_rate

REAL(KIND=jprb)               :: zhook_handle
CHARACTER(LEN=*),  PARAMETER :: RoutineName = "CALC_FLAM"

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

TsbyT_l   =  Ts / temp_l

Z_l       =  a * (TsbyT_l-1.0) + b * LOG10(TsbyT_l)                         &
           + c * (10.0**( d * (1.0 - TsbyT_l)) - 1.0)                         &
           + f * (10.0**( h * (TsbyT_l-1.0)) - 1.0)

f_rhum_l  = (rhum_up - rhum_l) / (rhum_up - rhum_low)

! Create boundary limits
! First for relative humidity
IF (rhum_l < rhum_low) f_rhum_l = 1.0
  ! Always fires for RH < 10%
IF (rhum_l > rhum_up)  f_rhum_l = 0.0
  ! No fires for RH > 90%

f_sm_l    = (1 - sm_l)
  ! The flammability goes down linearly with soil moisture

rain_rate = rain_l * s_in_day
  ! convert rain rate from kg/m2/s to mm/day

flam_l    = MAX(MIN(10.0**Z_l * f_rhum_l * fuel_l * f_sm_l                    &
                     * EXP( cr * rain_rate) ,1.0) ,0.0)

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE calc_flam

SUBROUTINE calc_burnt_area(                                                   &
  ! Point INTENT(IN)
  flam_l, ignitions_l, avg_ba_i,                                              &
  ! Point INTENT(OUT)
  burnt_area_i_l)

!
! Description:
!    Calculate the burnt area
!
! Method:
!    Multiply ignitions by flammability by average PFT burnt area
!
! Code Owner: Please refer to ModuleLeaders.txt
!
! Code Description:
!   Language: Fortran 90

USE yomhook,      ONLY: lhook, dr_hook
USE parkind1,     ONLY: jprb

IMPLICIT NONE

REAL  ,    INTENT(IN)     ::                                                  &
  flam_l,                                                                     &
    ! Flammability (depends on weather and vegetation)
  ignitions_l,                                                                &
    ! Fire ignitions (ignitions/m2/s)
  avg_ba_i
    ! The average burned area (m2) for this PFT

REAL  ,    INTENT(OUT)    ::                                                  &
  burnt_area_i_l
    ! The burnt area (fraction of PFT per s)

REAL(KIND=jprb)               :: zhook_handle
CHARACTER(LEN=*),  PARAMETER :: RoutineName = "CALC_BURNT_AREA"

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

burnt_area_i_l = flam_l * ignitions_l * avg_ba_i

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE calc_burnt_area

SUBROUTINE calc_emitted_carbon(                                               &
  ! Array INTENT(IN)
  land_pts, burnt_area_i, sm, leaf_i, wood_i,                                 &
  ccleaf_min_i, ccleaf_max_i, ccwood_min_i, ccwood_max_i,                     &
  ! Array INTENT(OUT)
  emitted_carbon_i )

!
! Description:
!   Calculate the total emitted carbon from burnt area
!
! Code Owner: Please refer to ModuleLeaders.txt
!
! Code Description:
!   Language: Fortran 90
!

USE yomhook,      ONLY: lhook, dr_hook
USE parkind1,     ONLY: jprb

IMPLICIT NONE

INTEGER                   :: land_pts

REAL ,   INTENT(IN)       ::                                                  &
  burnt_area_i(land_pts),                                                     &
    ! PFT Burnt Area (in frac of PFT s-1)
  sm(land_pts),                                                               &
    ! The INFERNO soil moisture (1st level sthu)
  leaf_i(land_pts),                                                           &
    ! The PFT leaf carbon
  wood_i(land_pts),                                                           &
    ! The PFT wood carbon
  ccleaf_min_i,                                                               &
    ! Leaf min combustion completeness
  ccleaf_max_i,                                                               &
    ! Leaf max combustion completeness
  ccwood_min_i,                                                               &
    ! Wood min combustion completeness
  ccwood_max_i
    ! Wood max combustion completeness

REAL ,   INTENT(OUT)      ::                                                  &
  emitted_carbon_i(land_pts)
    ! The PFT emitted carbon (kgC.m-2.s-1)

INTEGER                   :: l      ! landpoints loop counter

REAL(KIND=jprb)               :: zhook_handle
CHARACTER(LEN=*),  PARAMETER :: RoutineName = "CALC_EMITTED_CARBON"

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

! Initialise to no emitted carbon
emitted_carbon_i(:) = 0.0
DO l = 1, land_pts
  emitted_carbon_i(l) = MAX(burnt_area_i(l) * ( leaf_i(l) * (ccleaf_min_i +   &
                             (ccleaf_max_i - ccleaf_min_i) * (1.0 - sm(l))) +   &
                               wood_i(l) * (ccwood_min_i + (ccwood_max_i -    &
                               ccwood_min_i) * (1.0 - sm(l)))), 0.0 )

END DO

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE calc_emitted_carbon

SUBROUTINE calc_emitted_carbon_soil(                                          &
  ! Array INTENT(IN)
  land_pts, burnt_area, dpm_fuel, rpm_fuel, sm,                               &
  ! Array INTENT(OUT)
  emitted_carbon_DPM, emitted_carbon_RPM )

!
! Description:
!   Calculate the total emitted carbon from burnt area for soil
!   carbon pools (DPM and RPM)

! Code Owner: Please refer to ModuleLeaders.txt
!
! Code Description:
!   Language: Fortran 90
!

USE yomhook,      ONLY: lhook, dr_hook
USE parkind1,     ONLY: jprb

IMPLICIT NONE

INTEGER ,   INTENT(IN)    :: land_pts

REAL ,   INTENT(IN)       ::                                                  &
  burnt_area(land_pts),                                                       &
    ! Gridbox mean burnt area fraction (s-1)
  dpm_fuel(land_pts),                                                         &
    ! Carbon in DPM available (kgC.m-2)
  rpm_fuel(land_pts),                                                         &
    ! Carbon in RPM available (kgC.m-2)
  sm(land_pts)
    ! The soil moisture fraction (sthu at 1st level - sm_wil)

REAL ,   INTENT(OUT)      ::                                                  &
  emitted_carbon_DPM(land_pts),                                               &
    ! The DPM emitted carbon (kg.m-2.s-1)
  emitted_carbon_RPM(land_pts)
    ! The RPM emitted carbon (kg.m-2.s-1)

REAL ,   PARAMETER        ::                                                  &
  ccdpm_min = 0.8,                                                            &
  ccdpm_max = 1.0,                                                            &
    ! Decomposable Plant Material burns between 80 to 100 %
  ccrpm_min = 0.0,                                                            &
  ccrpm_max = 0.2
    ! Resistant Plant Material burns between 0 to 20 %
    ! These values are also set soilcarb and soilcarb_layers to calculate
    ! burnt_carbon_RPM using the soil pools

INTEGER                   :: l      ! landpoint loop counter

REAL(KIND=jprb)               :: zhook_handle
CHARACTER(LEN=*),  PARAMETER :: RoutineName = "CALC_EMITTED_CARBON_SOIL"

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

DO l = 1,land_pts
  emitted_carbon_DPM(l) = MAX(burnt_area(l) * ( dpm_fuel(l) * (ccdpm_min +    &
                              (ccdpm_max - ccdpm_min) * (1.0 - sm(l)))) ,0.0)
  emitted_carbon_RPM(l) = MAX(burnt_area(l) * ( rpm_fuel(l) * (ccrpm_min +    &
                          (ccrpm_max - ccrpm_min) * (1.0 - sm(l)))) ,0.0)
END DO

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE calc_emitted_carbon_soil

SUBROUTINE calc_emission(                                                     &
  ! Array INTENT(IN)
  land_pts, emitted_carbon_i,                                                 &
  fef_co2_i, fef_co_i, fef_ch4_i, fef_nox_i, fef_so2_i,                       &
  fef_oc_i, fef_bc_i,                                                         &
  ! Array INTENT(OUT)
  emission_CO2, emission_CO, emission_CH4,                                    &
  emission_NOx, emission_SO2,                                                 &
  emission_OC, emission_BC) ! Add more as you see fit...

!
! Description:
!  Calculate the emission of each compound from the obtained emitted carbon
!  Uses a look-up table with PFT-emission factor (see Li et al., 2012)
!
! Code Owner: Please refer to ModuleLeaders.txt
!
! Code Description:
!   Language: Fortran 90
!

USE yomhook,      ONLY: lhook, dr_hook
USE parkind1,     ONLY: jprb

IMPLICIT NONE

INTEGER, INTENT(IN)       :: land_pts

REAL ,   INTENT(IN)       ::                                                  &
  emitted_carbon_i(land_pts),                                                 &
    ! PFT (or Soil litter pool) emitted carbon (in kgC.m-2.s-1)
  fef_co2_i,                                                                  &
    ! PFT CO2 emission factor
  fef_co_i,                                                                   &
    ! PFT CO emission factor
  fef_ch4_i,                                                                  &
    ! PFT CH4 emission factor
  fef_nox_i,                                                                  &
    ! PFT NOx emission factor
  fef_so2_i,                                                                  &
    ! PFT SO2 emission factor
  fef_oc_i,                                                                   &
    ! PFT OC emission factor
  fef_bc_i
    ! PFT BC emission factor

REAL ,   INTENT(OUT)    ::                                                    &
  emission_CO2(land_pts),                                                     &
    ! The emission of CO2 (in kg.m-2.s-1)
  emission_CO(land_pts),                                                      &
    ! The emission of CO  (in kg.m-2.s-1)
  emission_CH4(land_pts),                                                     &
    ! The emission of CH4 (in kg.m-2.s-1)
  emission_NOx(land_pts),                                                     &
    ! The emission of NOx (in kg.m-2.s-1)
  emission_SO2(land_pts),                                                     &
    ! The emission of SO2 (in kg.m-2.s-1)
  emission_OC(land_pts),                                                      &
    ! The emission of OC  (in kg.m-2.s-1) - Organic Carbon
  emission_BC(land_pts)
    ! The emission of BC  (in kg.m-2.s-1) - Black Carbon

REAL ,   PARAMETER        ::                                                  &
  ctob = 2.0,                                                                 &
    ! Conversion from Carbon to Biomass (assume 50% of biomass is C)
  gtokg = 1.0e-03
    ! To convert the emission factors in kg kg-1

REAL(KIND=jprb)               :: zhook_handle
CHARACTER(LEN=*),  PARAMETER :: RoutineName = "CALC_EMISSION"

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

! Initialise the output fields
emission_CO2(:) = 0.0
emission_CO(:)  = 0.0
emission_CH4(:) = 0.0
emission_NOx(:) = 0.0
emission_SO2(:) = 0.0
emission_OC(:)  = 0.0
emission_BC(:)  = 0.0

! Resistant Plant Material
emission_CO2(:) = ctob * emitted_carbon_i(:) * fef_co2_i * gtokg
emission_CH4(:) = ctob * emitted_carbon_i(:) * fef_ch4_i  * gtokg
emission_CO(:)  = ctob * emitted_carbon_i(:) * fef_co_i * gtokg
emission_NOx(:) = ctob * emitted_carbon_i(:) * fef_nox_i * gtokg
emission_SO2(:) = ctob * emitted_carbon_i(:) * fef_so2_i * gtokg
emission_OC(:)  = ctob * emitted_carbon_i(:) * fef_oc_i  * gtokg
emission_BC(:)  = ctob * emitted_carbon_i(:) * fef_bc_i  * gtokg

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE calc_emission

END MODULE inferno_mod

