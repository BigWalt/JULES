! *****************************COPYRIGHT**************************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT**************************************
!
! Description:
!   Module containing runtime options/data used by the river routing scheme
!
! Method:
!   Switches and associated data values used by the river routing scheme
!   are defined here and assigned default values. These may be overridden
!   by namelist input.
!
!   Any routine wishing to use these options may do so with the 'USE'
!   statement.
!
! Code Owner: Please refer to the UM file CodeOwners.txt
!   This file belongs in section: River Routing

MODULE river_inputs_mod

USE missing_data_mod, ONLY: rmdi,imdi
USE yomhook,  ONLY: lhook, dr_hook
USE parkind1, ONLY: jprb, jpim

IMPLICIT NONE

!===========================================================================
! REAL values set from RUN_RIVERS namelist
!===========================================================================

REAL :: river_vel   = rmdi      ! River velocity

REAL :: river_mcoef = rmdi      ! Meandering coefficient

REAL :: river_step  = rmdi      ! River routing timestep

!===========================================================================
! LOGICAL options set from RUN_RIVERS namelist
!===========================================================================

LOGICAL :: l_rivers = .FALSE.   ! Use global river routing scheme

LOGICAL :: l_inland = .FALSE.   ! Control rerouting of inland basin water


!===========================================================================
! INTEGER options set from RUN_RIVERS namelist
!===========================================================================
INTEGER :: i_river_vn = imdi

!----------------------------------------------------------------------

! Define the run_rivers namelist

NAMELIST  / run_rivers/ river_vel, river_mcoef, river_step, l_rivers,           &
                      l_inland, i_river_vn

!DrHook-related parameters
INTEGER(KIND=jpim), PARAMETER, PRIVATE :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER, PRIVATE :: zhook_out = 1

CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName='RIVER_INPUTS_MOD'

CONTAINS


SUBROUTINE print_nlist_run_rivers()
USE jules_print_mgr, ONLY:                                                &
   jules_message,                                                         &
   jules_print
IMPLICIT NONE
CHARACTER(LEN=50000) :: lineBuffer
REAL(KIND=jprb) :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='PRINT_NLIST_RUN_RIVERS'

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

CALL jules_print('river_inputs_mod',                                      &
   'Contents of namelist run_rivers')

WRITE(lineBuffer,*)' RIVER_VEL = ',river_vel
CALL jules_print('river_inputs_mod',lineBuffer)
WRITE(lineBuffer,*)' RIVER_MCOEF = ',river_mcoef
CALL jules_print('river_inputs_mod',lineBuffer)
WRITE(lineBuffer,*)' river_step = ',river_step
CALL jules_print('river_inputs_mod',lineBuffer)
WRITE(lineBuffer,*)' l_rivers = ',l_rivers
CALL jules_print('river_inputs_mod',lineBuffer)
WRITE(lineBuffer,*)' l_inland = ',l_inland
CALL jules_print('river_inputs_mod',lineBuffer)

CALL jules_print('river_inputs_mod',                                      &
   '- - - - - - end of namelist - - - - - -')

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)

END SUBROUTINE print_nlist_run_rivers

SUBROUTINE read_nml_run_rivers(unit_in)

USE um_parcore, ONLY: mype

USE check_iostat_mod, ONLY: check_iostat
USE errormessagelength_mod, ONLY: errormessagelength
USE setup_namelist, ONLY: setup_nml_type

IMPLICIT NONE

INTEGER,INTENT(IN) :: unit_in
INTEGER :: my_comm
INTEGER :: mpl_nml_type
INTEGER :: ErrorStatus
INTEGER :: icode
CHARACTER(LEN=errormessagelength) :: iomessage
REAL(KIND=jprb) :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='READ_NML_RUN_RIVERS'

! set number of each type of variable in my_namelist type
INTEGER, PARAMETER :: no_of_types = 3
INTEGER, PARAMETER :: n_int = 1
INTEGER, PARAMETER :: n_real = 3
INTEGER, PARAMETER :: n_log = 2

TYPE my_namelist
  SEQUENCE
  INTEGER :: i_river_vn
  REAL :: river_vel
  REAL :: river_mcoef
  REAL :: river_step
  LOGICAL :: l_rivers
  LOGICAL :: l_inland
END TYPE my_namelist

TYPE (my_namelist) :: my_nml

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

CALL gc_get_communicator(my_comm, icode)

CALL setup_nml_type(no_of_types, mpl_nml_type, n_int_in = n_int,            &
                    n_real_in = n_real, n_log_in = n_log)

IF (mype == 0) THEN

  READ(UNIT = unit_in, NML = run_rivers, IOSTAT = ErrorStatus, IOMSG = iomessage)
  CALL check_iostat(errorstatus, "namelist RUN_RIVERS", iomessage)

  my_nml % i_river_vn  = i_river_vn
  my_nml % river_vel   = river_vel
  my_nml % river_mcoef = river_mcoef
  my_nml % river_step  = river_step
  my_nml % l_rivers    = l_rivers
  my_nml % l_inland    = l_inland

END IF

CALL mpl_bcast(my_nml,1,mpl_nml_type,0,my_comm,icode)

IF (mype /= 0) THEN

  i_river_vn  = my_nml % i_river_vn
  river_vel   = my_nml % river_vel
  river_mcoef = my_nml % river_mcoef
  river_step  = my_nml % river_step
  l_rivers    = my_nml % l_rivers
  l_inland    = my_nml % l_inland

END IF

CALL mpl_type_free(mpl_nml_type,icode)

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)

END SUBROUTINE read_nml_run_rivers

END MODULE river_inputs_mod
