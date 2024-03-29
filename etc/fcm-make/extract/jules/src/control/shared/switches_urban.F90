MODULE switches_urban

! Description:
!   Module containing switches for the parametrisations of urban scheme MORUSES
!
! (c) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! -----------------------------------------------------------------------------

IMPLICIT NONE

LOGICAL ::                                                                  &
     l_urban_empirical      = .FALSE.,  & ! Empirical relationships for urban
                                          ! geometry (WRR, HWR & HGT)
                                          ! (Standalone only)
     l_moruses_macdonald    = .FALSE.,  & ! MacDonald formulation for
                                          ! displacement height and effective
                                          ! roughness length for momentum
     l_moruses              = .FALSE.,  & ! MORUSES umbrella switch

! Independent parametristaion switches
     l_moruses_albedo       = .FALSE.,  & ! SW canyon albedo
     l_moruses_emissivity   = .FALSE.,  & ! LW canyon emissivity
     l_moruses_rough        = .FALSE.,  & ! Heat transfer
     l_moruses_storage      = .FALSE.,  & ! Storage
     l_moruses_storage_thin = .FALSE.     ! Storage thin roof

!-----------------------------------------------------------------------
! Set up a namelist to allow switches to be set l_moruses is set by
! inspecting all other moruses parametrisations
!-----------------------------------------------------------------------
NAMELIST  / jules_urban_switches/                                         &
   l_urban_empirical,                                                     &
   l_moruses_albedo,l_moruses_emissivity,l_moruses_rough,                 &
   l_moruses_storage,l_moruses_storage_thin,l_moruses_macdonald

CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName='SWITCHES_URBAN'

CONTAINS

SUBROUTINE check_jules_urban_switches()

!-----------------------------------------------------------------------------
! Description:
!   Checks JULES_URBAN_SWITCHES namelist for consistency
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------

USE ereport_mod, ONLY: ereport
USE jules_print_mgr, ONLY: jules_message, jules_print
USE jules_surface_mod, ONLY: l_urban2t
USE jules_radiation_mod, ONLY: l_cosz

IMPLICIT NONE

INTEGER :: errcode   ! error code to pass to ereport.

#if !defined(UM_JULES)
IF ( l_urban_empirical ) THEN
  errcode = 10
  CALL ereport("check_jules_urban_switches", errcode,                     &
               "l_urban_empirical=T: This cannot be used in the UM.")
END IF
#endif
IF ( .NOT. l_urban2t ) THEN
  ! If the two-tile urban schemes are not used these switches should be false.
  IF ( ANY ( (/ l_urban_empirical,         &
                l_moruses_albedo,          &
                l_moruses_emissivity,      &
                l_moruses_rough,           &
                l_moruses_storage,         &
                l_moruses_storage_thin,    &
                l_moruses_macdonald/) ) ) THEN
    errcode = 10
    CALL ereport("check_jules_urban_switches", errcode,                   &
                 "l_urban2t=F. All of the urban switches should be .false. .")
  END IF
END IF

! If any of the MORUSES parametrisations are used turn on the umbrella switch,
! l_moruses, which copies the roughness length for momentum (ztm) to z0 in
! sparm.
IF ( ANY ( (/ l_moruses_albedo,          &
              l_moruses_emissivity,      &
              l_moruses_rough,           &
              l_moruses_storage,         &
              l_moruses_macdonald/) ) ) THEN
  l_moruses = .TRUE.
  CALL jules_print("check_jules_urban_switches",                         &
                   "MORUSES: At least one parametrisation is being used.")
END IF

! Check MORUSES switch logic
IF ( l_urban_empirical .AND. .NOT. l_moruses_macdonald ) THEN
  errcode = 20
  CALL ereport("check_jules_urban_switches", errcode,                         &
               "l_urban_empirical=T: Empirical relationships are " //         &
               "being used to provide the urban morphology data. " //         &
               "MacDonald (1998) formulation for roughness length for " //    &
               "momentum and displacement height (l_moruses_macdonald) " //   &
               "must be also be used for consistency.")
END IF

IF ( l_moruses_albedo .AND. .NOT. l_cosz ) THEN
  errcode = 30
  CALL ereport("check_jules_urban_switches", errcode,                         &
               "l_moruses_albedo=T: Must also use l_cosz.")
END IF

IF ( l_moruses_storage_thin .AND. .NOT. l_moruses_storage ) THEN
  errcode = 30
  CALL ereport("check_jules_urban_switches", errcode,                         &
               "l_moruses_storage_thin=T: MORUSES storage parametrisation " //&
               "should also be used. Currently l_moruses_storage=F.")
END IF

END SUBROUTINE check_jules_urban_switches

SUBROUTINE print_nlist_jules_urban_switches()
USE jules_print_mgr, ONLY: jules_print
IMPLICIT NONE
CHARACTER(LEN=50000) :: lineBuffer

CALL jules_print('switches_urban',                                        &
    'Contents of namelist jules_urban_switches')

WRITE(lineBuffer,*)' l_moruses_albedo = ',l_moruses_albedo
CALL jules_print('switches_urban',lineBuffer)
WRITE(lineBuffer,*)' l_moruses_emissivity = ',l_moruses_emissivity
CALL jules_print('switches_urban',lineBuffer)
WRITE(lineBuffer,*)' l_moruses_rough = ',l_moruses_rough
CALL jules_print('switches_urban',lineBuffer)
WRITE(lineBuffer,*)' l_moruses_storage = ',l_moruses_storage
CALL jules_print('switches_urban',lineBuffer)
WRITE(lineBuffer,*)' l_moruses_storage_thin = ',l_moruses_storage_thin
CALL jules_print('switches_urban',lineBuffer)
WRITE(lineBuffer,*)' l_moruses_macdonald = ',l_moruses_macdonald
CALL jules_print('switches_urban',lineBuffer)

CALL jules_print('switches_urban',                                        &
    '- - - - - - end of namelist - - - - - -')

END SUBROUTINE print_nlist_jules_urban_switches

#if defined(UM_JULES) && !defined(LFRIC)

SUBROUTINE read_nml_jules_urban_switches (unitnumber)

! Description:
!  Read the JULES_URBAN_SWITCHES namelist

USE setup_namelist,   ONLY: setup_nml_type
USE check_iostat_mod, ONLY: check_iostat
USE UM_parcore,       ONLY: mype

USE parkind1,         ONLY: jprb, jpim
USE yomhook,          ONLY: lhook, dr_hook

USE errormessagelength_mod, ONLY: errormessagelength

IMPLICIT NONE

! Subroutine arguments
INTEGER, INTENT(IN) :: unitnumber

INTEGER :: my_comm
INTEGER :: mpl_nml_type
INTEGER :: ErrorStatus
INTEGER :: icode
CHARACTER(LEN=errormessagelength) :: iomessage
REAL(KIND=jprb) :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='READ_NML_JULES_URBAN_SWITCHES'
INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1

! set number of each type of variable in my_namelist type
INTEGER, PARAMETER :: no_of_types = 1
INTEGER, PARAMETER :: n_log = 6

TYPE my_namelist
  SEQUENCE
  LOGICAL :: l_moruses_albedo
  LOGICAL :: l_moruses_emissivity
  LOGICAL :: l_moruses_rough
  LOGICAL :: l_moruses_storage
  LOGICAL :: l_moruses_storage_thin
  LOGICAL :: l_moruses_macdonald
END TYPE my_namelist

TYPE (my_namelist) :: my_nml

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

CALL gc_get_communicator(my_comm, icode)

CALL setup_nml_type(no_of_types, mpl_nml_type, n_log_in = n_log)

IF (mype == 0) THEN

  READ (UNIT = unitnumber, NML = jules_urban_switches, IOSTAT = errorstatus,  &
        IOMSG = iomessage)
  CALL check_iostat(errorstatus, "namelist jules_urban_switches", iomessage)

  my_nml % l_moruses_albedo       = l_moruses_albedo
  my_nml % l_moruses_emissivity   = l_moruses_emissivity
  my_nml % l_moruses_rough        = l_moruses_rough
  my_nml % l_moruses_storage      = l_moruses_storage
  my_nml % l_moruses_storage_thin = l_moruses_storage_thin
  my_nml % l_moruses_macdonald    = l_moruses_macdonald
END IF

CALL mpl_bcast(my_nml,1,mpl_nml_type,0,my_comm,icode)

IF (mype /= 0) THEN

  l_moruses_albedo       = my_nml % l_moruses_albedo
  l_moruses_emissivity   = my_nml % l_moruses_emissivity
  l_moruses_rough        = my_nml % l_moruses_rough
  l_moruses_storage      = my_nml % l_moruses_storage
  l_moruses_storage_thin = my_nml % l_moruses_storage_thin
  l_moruses_macdonald    = my_nml % l_moruses_macdonald

END IF

CALL mpl_type_free(mpl_nml_type,icode)

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE read_nml_jules_urban_switches
#endif

#if !defined(UM_JULES)
SUBROUTINE read_nml_jules_urban_switches(nml_dir)

! Description:
!  Read the JULES_URBAN_SWITCHES namelist (standalone)

USE io_constants, ONLY: max_file_name_len, namelist_unit

USE string_utils_mod, ONLY: to_string

USE urban_param, ONLY:  jules_urban2t_param

USE logging_mod, ONLY: log_info, log_fatal

USE errormessagelength_mod, ONLY: errormessagelength

IMPLICIT NONE

! Arguments
CHARACTER(LEN=*), INTENT(IN) :: nml_dir  ! The directory containing the
                                         ! namelists

INTEGER :: i,l  ! Index variables

INTEGER :: error  ! Error indicator
CHARACTER(LEN=errormessagelength) :: iomessage

CHARACTER(LEN=max_file_name_len) :: FILE
                      ! The name of the file (or variable name template) to
                      ! use for variables that need to be filled from file

! Open the urban namelist file
OPEN(namelist_unit, FILE=(TRIM(nml_dir) // '/' // 'urban.nml'),             &
               STATUS='old', POSITION='rewind', ACTION='read', IOSTAT = error,&
               IOMSG = iomessage)
IF ( error /= 0 )                                                           &
  CALL log_fatal("init_urban",                                              &
                 "Error opening namelist file urban.nml " //                &
                 "(IOSTAT=" // TRIM(to_string(error)) // " IOMSG=" //       &
                 TRIM(iomessage) // ")")

! There are two namelists to read from this file
CALL log_info("init_urban", "Reading JULES_URBAN_SWITCHES namelist...")
READ(namelist_unit, NML = jules_urban_switches, IOSTAT = error, IOMSG = iomessage)
IF ( error /= 0 )                                                           &
  CALL log_fatal("init_urban",                                              &
                 "Error reading namelist JULES_URBAN_SWITCHES " //          &
                 "(IOSTAT=" // TRIM(to_string(error)) // " IOMSG=" //       &
                 TRIM(iomessage) // ")")

CALL log_info("init_urban", "Reading JULES_URBAN2T_PARAM namelist...")
READ(namelist_unit, NML = jules_urban2t_param, IOSTAT = error, IOMSG = iomessage)
IF ( error /= 0 )                                                           &
  CALL log_fatal("init_urban",                                              &
                 "Error reading namelist JULES_URBAN2T_PARAM " //           &
                 "(IOSTAT=" // TRIM(to_string(error)) // " IOMSG=" //       &
                 TRIM(iomessage) // ")")

! Close the namelist file
CLOSE(namelist_unit, IOSTAT = error, IOMSG = iomessage)
IF ( error /= 0 )                                                           &
  CALL log_fatal("init_urban",                                              &
                 "Error closing namelist file urban.nml " //                &
                 "(IOSTAT=" // TRIM(to_string(error)) // " IOMSG=" //       &
                 TRIM(iomessage) // ")")

END SUBROUTINE read_nml_jules_urban_switches
#endif

END MODULE switches_urban
