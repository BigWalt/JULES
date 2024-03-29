! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
!
! This module contains variables used for reading in triffid data
! and initialisations


! Code Description:
!   Language: FORTRAN 90
!   This code is written to UMDP3 v8.2 programming standards.


MODULE trif_io

USE max_dimensions, ONLY:                                                   &
  npft_max

IMPLICIT NONE

!---------------------------------------------------------------------
! Set up variables to use in IO (a fixed size version of each array
! in trif that we want to initialise).
!---------------------------------------------------------------------
INTEGER ::                                                                  &
  crop_io(npft_max)

REAL ::                                                                     &
  g_area_io(npft_max),                                                      &
  g_grow_io(npft_max),                                                      &
  g_root_io(npft_max),                                                      &
  g_wood_io(npft_max),                                                      &
  lai_max_io(npft_max),                                                     &
  lai_min_io(npft_max),                                                     &
  alloc_fast_io(npft_max),                                                  &
  alloc_med_io(npft_max),                                                   &
  alloc_slow_io(npft_max),                                                  &
  dpm_rpm_ratio_io(npft_max),                                               &
  retran_l_io(npft_max),                                                    &
  retran_r_io(npft_max)

!---------------------------------------------------------------------
! Set up a namelist for reading and writing these arrays
!---------------------------------------------------------------------
NAMELIST  / jules_triffid/ crop_io,g_area_io,g_grow_io,g_root_io,             &
                         g_wood_io,lai_max_io,lai_min_io,                   &
                         alloc_fast_io,alloc_med_io,alloc_slow_io,          &
                         dpm_rpm_ratio_io,retran_l_io,retran_r_io

CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName='TRIF_IO'

CONTAINS
SUBROUTINE print_nlist_jules_triffid()
USE jules_print_mgr, ONLY: jules_print
IMPLICIT NONE
CHARACTER(LEN=50000) :: lineBuffer

CALL jules_print('trif_io',                                               &
    'Contents of namelist jules_triffid')

WRITE(lineBuffer,*)' crop_io = ',crop_io
CALL jules_print('trif_io',lineBuffer)
WRITE(lineBuffer,*)' g_area_io = ',g_area_io
CALL jules_print('trif_io',lineBuffer)
WRITE(lineBuffer,*)' g_grow_io = ',g_grow_io
CALL jules_print('trif_io',lineBuffer)
WRITE(lineBuffer,*)' g_root_io = ',g_root_io
CALL jules_print('trif_io',lineBuffer)
WRITE(lineBuffer,*)' g_wood_io = ',g_wood_io
CALL jules_print('trif_io',lineBuffer)
WRITE(lineBuffer,*)' lai_max_io = ',lai_max_io
CALL jules_print('trif_io',lineBuffer)
WRITE(lineBuffer,*)' lai_min_io = ',lai_min_io
CALL jules_print('trif_io',lineBuffer)
WRITE(lineBuffer,*)' alloc_fast_io = ',alloc_fast_io
CALL jules_print('trif_io',lineBuffer)
WRITE(lineBuffer,*)' alloc_med_io = ',alloc_med_io
CALL jules_print('trif_io',lineBuffer)
WRITE(lineBuffer,*)' alloc_slow_io = ',alloc_slow_io
CALL jules_print('trif_io',lineBuffer)
WRITE(lineBuffer,*)' dpm_rpm_ratio_io = ',dpm_rpm_ratio_io
CALL jules_print('trif_io',lineBuffer)
WRITE(lineBuffer,*)' retran_r_io = ',retran_r_io
CALL jules_print('trif_io',lineBuffer)
WRITE(lineBuffer,*)' retran_l_io = ',retran_l_io
CALL jules_print('trif_io',lineBuffer)

CALL jules_print('trif_io',                                               &
    '- - - - - - end of namelist - - - - - -')

END SUBROUTINE print_nlist_jules_triffid

#if defined(UM_JULES)
SUBROUTINE read_nml_jules_triffid (unitnumber)

! Description:
!  Read the JULES_TRIFFID namelist

USE setup_namelist,   ONLY: setup_nml_type
USE check_iostat_mod, ONLY: check_iostat
USE UM_parcore,       ONLY: mype
USE errormessagelength_mod, ONLY: errormessagelength

USE parkind1,         ONLY: jprb, jpim
USE yomhook,          ONLY: lhook, dr_hook

IMPLICIT NONE

! Subroutine arguments
INTEGER, INTENT(IN) :: unitnumber
INTEGER :: my_comm
INTEGER :: mpl_nml_type
INTEGER :: ErrorStatus
INTEGER :: icode
REAL(KIND=jprb) :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='READ_NML_JULES_TRIFFID'
INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1

CHARACTER(LEN=errormessagelength) :: iomessage

! set number of each type of variable in my_namelist type
INTEGER, PARAMETER :: no_of_types = 2
INTEGER, PARAMETER :: n_int = 1 * npft_max
INTEGER, PARAMETER :: n_real = 12 * npft_max

TYPE my_namelist
  SEQUENCE
  INTEGER :: crop_io(npft_max)
  REAL :: g_area_io(npft_max)
  REAL :: g_grow_io(npft_max)
  REAL :: g_root_io(npft_max)
  REAL :: g_wood_io(npft_max)
  REAL :: lai_max_io(npft_max)
  REAL :: lai_min_io(npft_max)
  REAL :: alloc_fast_io(npft_max)
  REAL :: alloc_med_io(npft_max)
  REAL :: alloc_slow_io(npft_max)
  REAL :: dpm_rpm_ratio_io(npft_max)
  REAL :: retran_l_io(npft_max)
  REAL :: retran_r_io(npft_max)
END TYPE my_namelist

TYPE (my_namelist) :: my_nml

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

CALL gc_get_communicator(my_comm, icode)

CALL setup_nml_type(no_of_types, mpl_nml_type, n_int_in = n_int,            &
                    n_real_in = n_real)

IF (mype == 0) THEN

  READ (UNIT = unitnumber, NML = jules_triffid, IOSTAT = errorstatus,           &
        IOMSG = iomessage)
  CALL check_iostat(errorstatus, "namelist jules_triffid", iomessage)

  my_nml % crop_io    = crop_io
  my_nml % g_area_io  = g_area_io
  my_nml % g_grow_io  = g_grow_io
  my_nml % g_root_io  = g_root_io
  my_nml % g_wood_io  = g_wood_io
  my_nml % lai_max_io = lai_max_io
  my_nml % lai_min_io = lai_min_io
  my_nml % alloc_fast_io = alloc_fast_io
  my_nml % alloc_med_io = alloc_med_io
  my_nml % alloc_slow_io = alloc_slow_io
  my_nml % dpm_rpm_ratio_io = dpm_rpm_ratio_io
  my_nml % retran_r_io = retran_r_io
  my_nml % retran_l_io = retran_l_io
END IF

CALL mpl_bcast(my_nml,1,mpl_nml_type,0,my_comm,icode)

IF (mype /= 0) THEN

  crop_io    = my_nml % crop_io
  g_area_io  = my_nml % g_area_io
  g_grow_io  = my_nml % g_grow_io
  g_root_io  = my_nml % g_root_io
  g_wood_io  = my_nml % g_wood_io
  lai_max_io = my_nml % lai_max_io
  lai_min_io = my_nml % lai_min_io
  alloc_fast_io = my_nml % alloc_fast_io
  alloc_med_io = my_nml % alloc_med_io
  alloc_slow_io = my_nml % alloc_slow_io
  dpm_rpm_ratio_io = my_nml % dpm_rpm_ratio_io
  retran_r_io      = my_nml % retran_r_io
  retran_l_io      = my_nml % retran_l_io
END IF

CALL mpl_type_free(mpl_nml_type,icode)

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE read_nml_jules_triffid
#endif

END MODULE trif_io
