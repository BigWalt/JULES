










! *****************************COPYRIGHT**************************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT**************************************


PROGRAM jules

!$ USE omp_lib

USE init_mod, ONLY: init

USE io_constants, ONLY: max_file_name_len

USE jules_final_mod, ONLY:                                                 &
!  imported procedures
    jules_final

USE time_varying_input_mod, ONLY:                                          &
  update_prescribed_variables => update_model_variables,                    &
  input_close_all => close_all

USE update_mod, ONLY: update_derived_variables

USE output_mod, ONLY: output_initial_data, sample_data, output_data,       &
                       output_close_all => close_all

USE model_time_mod, ONLY: timestep, start_of_year, end_of_year, end_of_run

USE update_mod, ONLY: l_imogen

USE jules_print_mgr, ONLY: jules_message, jules_print

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   This is the main program routine for JULES
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------
! Work variables
CHARACTER(LEN=max_file_name_len) :: nml_dir  ! Directory containing namelists

INTEGER :: error  ! Error indicator


!-----------------------------------------------------------------------------


!-----------------------------------------------------------------------------
! Initialise the MPI environment
!-----------------------------------------------------------------------------
! We don't check the error since most (all?) MPI implementations will just
! fail if a call is unsuccessful
CALL mpi_init(error)

!-----------------------------------------------------------------------------
! If OpenMP is in use provide an information message to make sure the
! user is aware.
!-----------------------------------------------------------------------------
!$ WRITE(jules_message, '(A, I3, A)') 'Using OpenMP with up to ', &
!$                                        OMP_get_max_threads(), ' thread(s)'
!$ CALL jules_print('jules', jules_message)

!-----------------------------------------------------------------------------
! Try to read a single argument from the command line
!
! If present, that single argument will be the directory we try to read
! namelists from
! If not present, we use current working directory instead
!-----------------------------------------------------------------------------
CALL get_command_argument(1, nml_dir)
! If no argument is given, GET_COMMAND_ARGUMENT returns a blank string
IF ( LEN_TRIM(nml_dir) == 0 ) nml_dir = "."

!-----------------------------------------------------------------------------
! Initialise the model
!-----------------------------------------------------------------------------
CALL init(nml_dir)

!-----------------------------------------------------------------------------
! Loop over timesteps.
! Note that the number of timesteps is of unknown length at the start of run,
! if the model is to determine when it has spun up.
!-----------------------------------------------------------------------------
DO    !  timestep

  !-----------------------------------------------------------------------------
  ! Update the IMOGEN climate variables if required
  !-----------------------------------------------------------------------------
  IF ( l_imogen .AND. start_of_year ) CALL imogen_update_clim()

  !-----------------------------------------------------------------------------
  ! The update of prescribed data is done in two phases
  !  - Update variables provided by files
  !  - Update variables that are derived from those updated in the first phase
  !-----------------------------------------------------------------------------
  CALL update_prescribed_variables()
  CALL update_derived_variables()

  !-----------------------------------------------------------------------------
  ! Check if this is a timestep that we need to output initial data for (i.e.
  ! start of spinup cycle or start of main run), and output that data if
  ! required
  !-----------------------------------------------------------------------------
  CALL output_initial_data()

  !-----------------------------------------------------------------------------
  ! Call the main model science routine
  !-----------------------------------------------------------------------------
  CALL control(timestep)

  !-----------------------------------------------------------------------------
  ! Update IMOGEN carbon if required
  !-----------------------------------------------------------------------------
  IF ( l_imogen .AND. end_of_year ) CALL imogen_update_carb()

  !-----------------------------------------------------------------------------
  ! Sample variables for output
  !-----------------------------------------------------------------------------
  CALL sample_data()

  !-----------------------------------------------------------------------------
  ! Output collected data if required
  !-----------------------------------------------------------------------------
  CALL output_data()

  !-----------------------------------------------------------------------------
  ! Move the model on to the next timestep
  !-----------------------------------------------------------------------------
  CALL next_time()

  IF ( end_of_run ) EXIT

END DO  !  timestep loop

!-----------------------------------------------------------------------------
! Clean up by closing all open files
!-----------------------------------------------------------------------------
CALL input_close_all()
CALL output_close_all()

!-----------------------------------------------------------------------------
! Final messages.
!-----------------------------------------------------------------------------
CALL jules_final()

!-----------------------------------------------------------------------------
! Clean up the MPI environment
!-----------------------------------------------------------------------------
CALL mpi_finalize(error)


END PROGRAM jules
