#if defined(UM_JULES)
! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************

! Code Owner: Please refer to ModuleLeaders.txt and UM file CodeOwners.txt

!Intermediate routine to provide a single coupling point when initialising
!via the UM

!Due to a variable typing issue caused by variables usually passed round by
!include files, this routine will not compile as a module
SUBROUTINE surf_couple_initialise(a_step,                                     &
                                  a_inthd_arg,                                &
                                  land_index,                                 &
                                  !Arguments for river routing
                                  a_realhd,                                   &
                                  xpa, xua, xva, ypa, yua, yva)

!USE in relevant subroutines
USE init_veg_mod,           ONLY: init_veg
USE init_riv_mod,           ONLY: init_riv
USE init_a2t_4a_mod,        ONLY: init_a2t_4a
USE jules_init_mod,         ONLY: jules_init
USE init_urban_mod,         ONLY: init_urban

!USE in relevant variables from UM modules
USE nlsizes_namelist_mod,   ONLY: ntiles, land_field,                         &
                                  a_len_inthd, a_len_realhd,                  &
                                  aocpl_row_length, aocpl_p_rows

USE model_time_mod,         ONLY: stepim
USE submodel_mod,           ONLY: atmos_im
USE umPrintMgr
USE ereport_mod,            ONLY: ereport

!USE in relevant variables from JULES modules
USE jules_surface_mod,      ONLY: l_aggregate
USE river_inputs_mod,       ONLY: l_rivers
USE jules_sea_seaice_mod,   ONLY: nice, nice_use
USE jules_radiation_mod,    ONLY: l_snow_albedo

USE parkind1, ONLY: jprb, jpim
USE yomhook, ONLY: lhook, dr_hook

IMPLICIT NONE

!Arguments and includes
INTEGER, INTENT(IN)     :: a_step
INTEGER, INTENT(INOUT)  :: a_inthd_arg(a_len_inthd)
INTEGER, INTENT(IN)     :: land_index(MAX(1,land_field))

!For river routing
REAL,    INTENT(IN)     :: a_realhd(a_len_realhd)
REAL,    INTENT(OUT)    :: xpa(aocpl_row_length+1)  !tp longitude coordinate
REAL,    INTENT(OUT)    :: xua(0:aocpl_row_length)  !u  longitude coordinate
REAL,    INTENT(OUT)    :: xva(aocpl_row_length+1)  !v  longitude coordinate
REAL,    INTENT(OUT)    :: ypa(aocpl_p_rows)        !tp latitude  coordinate
REAL,    INTENT(OUT)    :: yua(aocpl_p_rows)        !u  latitude  coordinate
REAL,    INTENT(OUT)    :: yva(0:aocpl_p_rows)      !v  latitude  coordinate

!Local variables
INTEGER :: triffid_period_arg
INTEGER :: nstep_since_triffid
INTEGER :: nstep_since_riv

! Error reporting
INTEGER                           :: icode  ! =0 normal exit; >0 error exit
CHARACTER(LEN=errormessagelength) :: cmessage
CHARACTER(LEN=*)                  :: RoutineName

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle
PARAMETER(RoutineName='SURF_COUPLE_INITIALISE')

!End of header
IF (lhook) CALL dr_hook(RoutineName,zhook_in,zhook_handle)

!Grab the relevant items from inthd and give them a more memorable name
triffid_period_arg  = a_inthd_arg(22)
  !triffid_period in days
nstep_since_triffid = a_inthd_arg(23)
  !Number of atmosphere timesteps since the last call to TRIFFID
nstep_since_riv     = a_inthd_arg(16)
  !Number of atmosphere timesteps since the last call to River routing

! Set l_aggregate appropriately. REVIEW FOR FLEXIBLE TILES?
IF (ntiles == 1) THEN
  l_aggregate = .TRUE.
ELSE
  l_aggregate = .FALSE.
END IF

! Copying urban prognosics from D1 array to JULES module and set urban logic.
! This needs to be done before sparm is called on init routines as otherwise
! ztm hasn't been passed (nor is the logic set) before it is required. Also
! needs to be called on a CRUN.
CALL init_urban ()

IF ( stepim(atmos_im) == 0 ) THEN
  IF (land_field  >   0) THEN
    CALL init_veg(a_step, triffid_period_arg, nstep_since_triffid)
  ELSE
    !Skip init_veg if land_field=0 for this PE.
    CALL umPrint(                                                             &
      'surf_couple_initialise; skip init_veg, land_field=0 on this PE' ,      &
       src='surf_couple_initialise')
  END IF
END IF

! River routing
IF (l_rivers) THEN
  ! Initialise the step counter for river routing.
  IF (stepim(atmos_im) == 0) THEN
    CALL init_riv(nstep_since_riv, icode, cmessage)
    IF (icode  /=  0) THEN
      CALL Ereport(RoutineName,icode,Cmessage)
    END IF
  END IF

  ! Set up the ATMOS grid lat./long.coords for regridding to river routing grid
  CALL init_a2t_4a(a_realhd, xpa, xua, xva, ypa, yua, yva)
END IF !l_rivers

! 7.5 Initialise JULES variables
! Check if land-surface fields are required.
IF (land_field > 0) THEN
  CALL jules_init(land_index)
END IF

!Copy back the relevant items from inthd and give them a more memorable name
a_inthd_arg(22) = triffid_period_arg
  !triffid_period in days
a_inthd_arg(23) = nstep_since_triffid
  !Number of atmosphere timesteps since the last call to TRIFFID
a_inthd_arg(16) = nstep_since_riv
  !Number of atmosphere timesteps since the last call to River routing

IF (lhook) CALL dr_hook(RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE surf_couple_initialise
#endif
