! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
!  ---------------------------------------------------------------------
!  Sructure containing surface exchange diagnostics.
!  This permits easier addition of new surface exchange
!  diagnostics without additional passing of arguments
!  though the boundary layer tree.
!  It also does not require the addition
!  of extra subroutine arguments when adding a new diagnostic.

!  Code Owner: Please refer to ModuleLeaders.txt
!  This file belongs in section: Surface

!- ----------------------------------------------------------------------

MODULE sf_diags_mod

IMPLICIT NONE
SAVE

TYPE strnewsfdiag

  ! Need to create a flag and a pointer.
  ! In general initialise the flag to .FALSE. and use code elsewhere to reset
  ! this if required.

  LOGICAL :: l_ra            = .FALSE. ! 54 aerodynamic resistance
  LOGICAL :: su10            = .FALSE.
  LOGICAL :: sv10            = .FALSE.
  LOGICAL :: sq_t1p5         = .FALSE.
  LOGICAL :: sq1p5           = .FALSE.
  LOGICAL :: st1p5           = .FALSE.
  LOGICAL :: sfme            = .FALSE.
  LOGICAL :: sz0heff         = .FALSE.
  LOGICAL :: slh             = .FALSE.
  LOGICAL :: simlt           = .FALSE.
  LOGICAL :: smlt            = .FALSE.
  LOGICAL :: suv10m_n        = .FALSE.
  LOGICAL :: l_u10m_n        = .FALSE.
  LOGICAL :: l_v10m_n        = .FALSE.
  LOGICAL :: l_mu10m_n       = .FALSE.
  LOGICAL :: l_mv10m_n       = .FALSE.
  LOGICAL :: l_lw_surft      = .FALSE.
  LOGICAL :: l_t10m          = .FALSE. ! 344 t at 10m over sea/sea-ice
  LOGICAL :: l_q10m          = .FALSE. ! 345 q at 10m over sea/sea-ice
  LOGICAL :: l_tstar_sice_weighted_cat = .FALSE.  
  LOGICAL :: l_lw_up_sice_weighted_cat = .FALSE. 
  LOGICAL :: l_ice_present_cat         = .FALSE. 
  LOGICAL :: l_tstar_sice_weighted     = .FALSE. 
  LOGICAL :: l_lw_up_sice_weighted     = .FALSE. 
  LOGICAL :: l_ice_present             = .FALSE. 
  LOGICAL :: l_ftl_ice_sm    = .FALSE.
  LOGICAL :: l_cd_ssi        = .FALSE.
  LOGICAL :: l_ch_ssi        = .FALSE.
  LOGICAL :: l_snice         = .FALSE.
  LOGICAL :: l_et_stom       = .FALSE.
  LOGICAL :: l_et_stom_surft = .FALSE.
  LOGICAL :: l_fsth          = .FALSE.
  LOGICAL :: l_ftemp         = .FALSE.
  LOGICAL :: l_fprf          = .FALSE.

  REAL, ALLOCATABLE :: ra(:)
  !                    54      aerodynamic resistance
  REAL, ALLOCATABLE :: u10m(:,:)
  !                    209      x-cpt of wind at 10m
  REAL, ALLOCATABLE :: v10m(:,:)
  !                    210      y-cpt of wind at 10m
  REAL, ALLOCATABLE :: fme(:,:)
  !                    224      Wind mixing energy
  REAL, ALLOCATABLE :: latent_heat(:,:)
  !                    234      Latent heat flux
  REAL, ALLOCATABLE :: t1p5m(:,:)
  !                    236      Temperature at 1.5m
  REAL, ALLOCATABLE :: q1p5m(:,:)
  !                    237      Specific humidity at 1.5m
  REAL, ALLOCATABLE :: sice_mlt_htf(:,:,:)
  !                    257      Sea ice surface melt heat flux
  REAL, ALLOCATABLE :: snomlt_surf_htf(:,:)
  !                    258      Surface snowmelt heat flux
  REAL, ALLOCATABLE :: t1p5m_surft(:,:)
  !                    328      Temperature at 1.5m over tiles
  REAL, ALLOCATABLE :: q1p5m_surft(:,:)
  !                    329      Specific humidity at 1.5m over tiles
  REAL, ALLOCATABLE :: chr10m(:,:)
  !                    CH at 10m over sea/sea-ice needed for 344/345
  REAL, ALLOCATABLE :: t10m(:,:)
  !                    344      Temperature at 10m over sea/sea-ice
  REAL, ALLOCATABLE :: q10m(:,:)
  !                    345      Specific humidity at 10m over sea/sea-ice
  REAL, ALLOCATABLE :: u10m_n(:, :)
  !                    368      x-cpt of neutral wind (at 10m)
  REAL, ALLOCATABLE :: v10m_n(:, :)
  !                    369      y-cpt of neutral wind (at 10m)
  REAL, ALLOCATABLE :: mu10m_n(:, :)
  !                    370      x-cpt of pseudostress
  REAL, ALLOCATABLE :: mv10m_n(:, :)
  !                    371      y-cpt of pseudostress
  REAL, ALLOCATABLE :: lw_up_surft(:,:)
  !                    383      Upwelling LW radiation on tiles  
  REAL, ALLOCATABLE :: lw_down_surft(:,:)
  !                    384      Downwelling LW radiation on tiles
  REAL, ALLOCATABLE :: ftemp(:,:)
  !                    485      Temperature rate modifier of soil respiration
  REAL, ALLOCATABLE :: fsth(:,:)
  !                    486      Soil moisture rate modifier of soil respiration
  REAL, ALLOCATABLE :: fprf(:)
  !                    489      Soil respiration rate modifier due to vegetation cover

  REAL, ALLOCATABLE :: lw_up_sice_weighted_cat(:,:,:)
  !                    530      category ice area weighted upward LW flux
  !                             over sea ice after boundary layer 
  !                             calculation
  REAL, ALLOCATABLE :: lw_up_sice_weighted(:,:)
  !                    531      category ice area weighted upward LW flux
  !                             over sea ice after boundary layer 
  !                             calculation
  REAL, ALLOCATABLE :: ftl_ice_sm(:,:)
  !                    533      aggregate ice area weighted sensible 
  !                             heat flux over sea ice
  REAL, ALLOCATABLE :: tstar_sice_weighted_cat(:,:,:)
  !                    534      category ice area weighted sea ice surface
  !                             skin temperature
  REAL, ALLOCATABLE :: tstar_sice_weighted(:,:)
  !                    535      category ice area weighted sea ice surface
  !                             skin temperature
  REAL, ALLOCATABLE :: ice_present_cat(:,:,:)
  !                    536      category sea ice time fraction
  REAL, ALLOCATABLE :: ice_present(:,:)
  !                    537      category sea ice time fraction
  REAL, ALLOCATABLE :: cd_ssi(:,:)
  !                    538       Sea and sea ice drag coefficient (momentum)
  REAL, ALLOCATABLE :: ch_ssi(:,:)
  !                    541       Sea and sea ice drag coefficient (heat)
  REAL, ALLOCATABLE :: snice_smb_surft(:,:)
  !                    578      Tiled snow mass rate of change 
  !                             (kg/m2/s)
  REAL, ALLOCATABLE :: snice_m_surft(:,:)
  !                    579      Total internal melt rate of snowpack 
  !                             (kg/m2/s)
  REAL, ALLOCATABLE :: snice_freez_surft(:,:)
  !                    580      Total internal refreezing rate in
  !                             snowpack (kg/m2/s)
  REAL, ALLOCATABLE :: snice_runoff_surft(:,:)
  !                    581      Net rate of liquid leaving snowpack 
  !                             (kg/m2/s)
  REAL, ALLOCATABLE :: snice_sicerate_surft(:,:)
  !                    582      Rate of change of solid mass in 
  !                             snowpack (kg/m2/s)
  REAL, ALLOCATABLE :: snice_sliqrate_surft(:,:)
  !                    583      Rate of change of liquid mass 
  !                             snowpack (kg/m2/s)
  REAL, ALLOCATABLE :: et_stom_ij(:,:)
  !                    539      Transpiration through stom (kg/m2/s)
  REAL, ALLOCATABLE :: et_stom_surft(:,:)
  !                    540      Transpiration through stom (kg/m2/s)
  REAL, ALLOCATABLE :: resfs_stom(:,:)
  !                    Combined stomatal and aerodynamic
  !                    resistance factor for fraction 1-FRACA.

END TYPE strnewsfdiag

TYPE (Strnewsfdiag) :: sf_diag
! ----------------------------------------------------------------------
END MODULE sf_diags_mod
