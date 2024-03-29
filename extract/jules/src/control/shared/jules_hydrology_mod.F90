! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************

MODULE jules_hydrology_mod

!-----------------------------------------------------------------------------
! Description:
!   Contains hydrology options and a namelist for setting them
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Switches
!-----------------------------------------------------------------------------
LOGICAL ::                                                                  &
  l_hydrology = .TRUE.,                                                     &
      ! Turns off hydrology code for UM_JULES
  l_top  = .FALSE.,                                                         &
      ! Switch for TOPMODEL-based hydrology
  l_pdm = .FALSE.,                                                          &
      ! Switch for PDM hydrology
  l_spdmvar = .FALSE.,                                                      &
      ! Switch for slope dependent s_pdm in PDM hydrology
  l_baseflow_corr = .TRUE.,                                                 &
      ! Switch for using a correction to the calculation of baseflow
      ! Only used if l_top = T
  l_var_rainfrac = .FALSE.,                                                 &
      ! Switch for using convective and large scale rain fractions
      ! as calculated in the UM atmosphere
      ! Has no effect in standalone JULES
  l_wetland_unfrozen = .FALSE.                                               
      ! Switch for TOPMODEL-based hydrology with unfrozen wetland inundation
      ! Only used if l_top=.T.

!-----------------------------------------------------------------------------
! PDM parameters
!-----------------------------------------------------------------------------
REAL ::                                                                     &
  dz_pdm = 1.0,                                                             &
      ! Soil layer thickness for PDM (m)
  b_pdm  = 1.0,                                                             &
      ! Shape factor for PDM
  s_pdm = 0.0,                                                              &
! So/Smax factor for PDM 
   slope_pdm_max = 6.0
! Maximum topographic slope (deg) in the slope dependent s_pdm 
! linear function
!-----------------------------------------------------------------------------
! TOPMODEL parameters
!-----------------------------------------------------------------------------
INTEGER ::                                                                  &
  nfita = 20
      ! Number of loops for fitting.
REAL ::                                                                     &
  ti_max = 10.0,                                                            &
      ! Maximum topographic index considered
  ti_wetl = 1.5,                                                            &
      ! Parameter to remove very high water tables from the calculated
      ! wetland fraction
  zw_max = 6.0
      ! Maximum allowed water table depth (m)
!------------------------------------------------------------------------------
! Single namelist definition for UM and standalone
!------------------------------------------------------------------------------
NAMELIST  / jules_hydrology/                                                  &
  l_hydrology, l_top, l_pdm, l_spdmvar, l_baseflow_corr, l_var_rainfrac,    &
  l_wetland_unfrozen,                                                       &
  dz_pdm, b_pdm, s_pdm, slope_pdm_max, ti_max, ti_wetl, zw_max, nfita


CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName='JULES_HYDROLOGY_MOD'

CONTAINS

SUBROUTINE check_jules_hydrology()

USE ereport_mod, ONLY: ereport

!-----------------------------------------------------------------------------
! Description:
!   Checks JULES_HYDROLOGY namelist for consistency
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------

IMPLICIT NONE

INTEGER :: errorstatus

! PDM and TOPMODEL are not allowed together
IF ( l_top .AND. l_pdm ) THEN
  errorstatus = 101
  CALL ereport("check_jules_hydrology", errorstatus,                      &
               "PDM and TOPMODEL cannot be used together")
END IF

END SUBROUTINE check_jules_hydrology


SUBROUTINE print_nlist_jules_hydrology()

USE jules_print_mgr, ONLY: jules_print

IMPLICIT NONE

CHARACTER(LEN=50000) :: lineBuffer

CALL jules_print('jules_hydrology',                                       &
                 'Contents of namelist jules_hydrology')

WRITE(lineBuffer, *) '  l_top = ', l_top
CALL jules_print('jules_hydrology', lineBuffer)

WRITE(lineBuffer, *) '  l_pdm = ', l_pdm
CALL jules_print('jules_hydrology', lineBuffer)

WRITE(lineBuffer, *) '  l_spdmvar = ', l_spdmvar
CALL jules_print('jules_hydrology', lineBuffer)
    
WRITE(lineBuffer, *) '  l_baseflow_corr = ', l_baseflow_corr
CALL jules_print('jules_hydrology', lineBuffer)

WRITE(lineBuffer, *) '  l_var_rainfrac = ', l_var_rainfrac
CALL jules_print('jules_hydrology', lineBuffer)

WRITE(lineBuffer, *) '  l_wetland_unfrozen = ', l_wetland_unfrozen
CALL jules_print('jules_hydrology', lineBuffer)

WRITE(lineBuffer, *) '  dz_pdm = ', dz_pdm
CALL jules_print('jules_hydrology', lineBuffer)

WRITE(lineBuffer, *) '  b_pdm = ', b_pdm
CALL jules_print('jules_hydrology', lineBuffer)

WRITE(lineBuffer, *) '  s_pdm = ', s_pdm
CALL jules_print('jules_hydrology', lineBuffer)
    
WRITE(lineBuffer, *) '  slope_pdm_max = ', s_pdm
CALL jules_print('jules_hydrology', lineBuffer)

WRITE(lineBuffer, *) '  ti_max = ', ti_max
CALL jules_print('jules_hydrology', lineBuffer)

WRITE(lineBuffer, *) '  ti_wetl = ', ti_wetl
CALL jules_print('jules_hydrology', lineBuffer)

WRITE(lineBuffer, *) '  zw_max = ', zw_max
CALL jules_print('jules_hydrology', lineBuffer)

WRITE(lineBuffer, *) '  nfita = ', nfita
CALL jules_print('jules_hydrology', lineBuffer)

END SUBROUTINE print_nlist_jules_hydrology

#if defined(UM_JULES) && !defined(LFRIC)
SUBROUTINE read_nml_jules_hydrology (unitnumber)

! Description:
!  Read the JULES_HYDROLOGY namelist

USE setup_namelist, ONLY: setup_nml_type
USE check_iostat_mod, ONLY: check_iostat
USE UM_parcore,       ONLY: mype

USE parkind1, ONLY: jprb, jpim
USE yomhook, ONLY: lhook, dr_hook

USE errormessagelength_mod, ONLY: errormessagelength

IMPLICIT NONE

! Subroutine arguments
INTEGER, INTENT(IN) :: unitnumber

INTEGER :: my_comm
INTEGER :: mpl_nml_type
INTEGER :: ErrorStatus
INTEGER :: icode
REAL(KIND=jprb) :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='READ_NML_JULES_HYDROLOGY'

CHARACTER(LEN=errormessagelength) :: iomessage

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1

! set number of each type of variable in my_namelist type
INTEGER, PARAMETER :: no_of_types = 3
INTEGER, PARAMETER :: n_int = 1
INTEGER, PARAMETER :: n_real = 7
INTEGER, PARAMETER :: n_log = 7

TYPE my_namelist
  SEQUENCE
  INTEGER :: nfita
  REAL :: dz_pdm
  REAL :: b_pdm
  REAL :: s_pdm
  REAL :: slope_pdm_max
  REAL :: ti_max
  REAL :: ti_wetl
  REAL :: zw_max
  LOGICAL :: l_hydrology
  LOGICAL :: l_top
  LOGICAL :: l_pdm
  LOGICAL :: l_spdmvar
  LOGICAL :: l_baseflow_corr
  LOGICAL :: l_var_rainfrac
  LOGICAL :: l_wetland_unfrozen
END TYPE my_namelist

TYPE (my_namelist) :: my_nml

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

CALL gc_get_communicator(my_comm, icode)

CALL setup_nml_type(no_of_types, mpl_nml_type, n_int_in = n_int,            &
                    n_real_in = n_real, n_log_in = n_log)

IF (mype == 0) THEN

  READ (UNIT = unitnumber, NML = jules_hydrology, IOSTAT = errorstatus,     &
        IOMSG = iomessage)
  CALL check_iostat(errorstatus, "namelist jules_hydrology", iomessage)

  my_nml % nfita  = nfita
  ! end of integers
  my_nml % dz_pdm          = dz_pdm
  my_nml % b_pdm           = b_pdm
  my_nml % s_pdm           = s_pdm
  my_nml % slope_pdm_max   = slope_pdm_max
  my_nml % ti_max          = ti_max
  my_nml % ti_wetl         = ti_wetl
  my_nml % zw_max          = zw_max
  ! end of reals
  my_nml % l_hydrology     = l_hydrology
  my_nml % l_top           = l_top
  my_nml % l_pdm           = l_pdm
  my_nml % l_spdmvar       = l_spdmvar
  my_nml % l_baseflow_corr = l_baseflow_corr
  my_nml % l_var_rainfrac  = l_var_rainfrac
  my_nml % l_wetland_unfrozen  = l_wetland_unfrozen

END IF

CALL mpl_bcast(my_nml,1,mpl_nml_type,0,my_comm,icode)

IF (mype /= 0) THEN

  nfita     = my_nml % nfita
  ! end of integers
  dz_pdm        = my_nml % dz_pdm
  b_pdm         = my_nml % b_pdm
  s_pdm         = my_nml % s_pdm
  slope_pdm_max = my_nml % slope_pdm_max
  ti_max        = my_nml % ti_max
  ti_wetl       = my_nml % ti_wetl
  zw_max        = my_nml % zw_max
  ! end of reals
  l_hydrology     = my_nml % l_hydrology
  l_top           = my_nml % l_top
  l_pdm           = my_nml % l_pdm
  l_spdmvar       = my_nml % l_spdmvar
  l_baseflow_corr = my_nml % l_baseflow_corr
  l_var_rainfrac  = my_nml % l_var_rainfrac
  l_wetland_unfrozen  = my_nml % l_wetland_unfrozen

END IF

CALL mpl_type_free(mpl_nml_type,icode)

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE read_nml_jules_hydrology
#endif

END MODULE jules_hydrology_mod
