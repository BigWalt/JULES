#if !defined(UM_JULES)
! *****************************COPYRIGHT**************************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT**************************************


SUBROUTINE file_ncdf_def_attr_real(file, var_id, name, value)

  USE io_constants, ONLY : mode_read, mode_write, attr_global

  USE netcdf, ONLY : nf90_put_att, nf90_global

  IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Define a real valued attribute on the given variable with the given name
!   and value
!   To define a global attribute, specify attr_global as var_id
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------
! Argument types
  TYPE(file_ncdf), INTENT(INOUT) :: file
                                    ! The file to define the attribute in
  INTEGER, INTENT(IN) :: var_id     ! The id of the variable to define
                                    ! attribute on
  CHARACTER(LEN=*), INTENT(IN) :: name
                                    ! The name of the attribute
  REAL, INTENT(IN) :: value         ! The value of the attribute


! Work variables
  INTEGER :: var_id_local  ! Local version of var_id that we can modify
  INTEGER :: error         ! Error indicator

!-----------------------------------------------------------------------------

  SELECT CASE ( file%mode )
    CASE ( mode_read )
!-----------------------------------------------------------------------------
! In read mode, we just ignore the request, as we don't care about attributes
!-----------------------------------------------------------------------------

    CASE ( mode_write )
!-----------------------------------------------------------------------------
! In write mode, we need to define the attribute
!-----------------------------------------------------------------------------
      IF ( var_id == attr_global ) THEN
! If a global attribute has been requested, set var id to the NetCDF
! identifier for global attributes
        var_id_local = nf90_global
      ELSE
! Otherwise get the NetCDF id of the variable from the given index in vars
! array
        var_id_local = file%vars(var_id)%id
      END IF

! Try to actually create the attribute
      error = nf90_put_att(file%id, var_id_local, name, value)
      IF ( error /= nf90_noerr )                                              &
        CALL log_fatal_ncdf("file_ncdf_def_attr_real",                        &
                            "Error defining attribute", error)

! No default case as we already know that mode_read and mode_write are
! the only options
  END SELECT

  RETURN

END SUBROUTINE file_ncdf_def_attr_real


SUBROUTINE file_ncdf_def_attr_int(file, var_id, name, value)

  USE io_constants, ONLY : mode_read, mode_write, attr_global

  USE netcdf, ONLY : nf90_put_att, nf90_global

  IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Define an integer valued attribute on the given variable with the given
!   name and value
!   To define a global attribute, specify attr_global as var_id
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------
! Argument types
  TYPE(file_ncdf), INTENT(INOUT) :: file
                                    ! The file to define the attribute in
  INTEGER, INTENT(IN) :: var_id     ! The id of the variable to define
                                    ! attribute on
  CHARACTER(LEN=*), INTENT(IN) :: name
                                    ! The name of the attribute
  INTEGER, INTENT(IN) :: value      ! The value of the attribute


! Work variables
  INTEGER :: var_id_local  ! Local version of var_id that we can modify
  INTEGER :: error         ! Error indicator

!-----------------------------------------------------------------------------

  SELECT CASE ( file%mode )
    CASE ( mode_read )
!-----------------------------------------------------------------------------
! In read mode, we just ignore the request, as we don't care about attributes
!-----------------------------------------------------------------------------

    CASE ( mode_write )
!-----------------------------------------------------------------------------
! In write mode, we need to define the attribute
!-----------------------------------------------------------------------------
      IF ( var_id == attr_global ) THEN
! If a global attribute has been requested, set var id to the NetCDF
! identifier for global attributes
        var_id_local = nf90_global
      ELSE
! Otherwise get the NetCDF id of the variable from the given index in vars
! array
        var_id_local = file%vars(var_id)%id
      END IF

! Try to actually create the attribute
      error = nf90_put_att(file%id, var_id_local, name, value)
      IF ( error /= nf90_noerr )                                              &
        CALL log_fatal_ncdf("file_ncdf_def_attr_real",                        &
                            "Error defining attribute", error)

! No default case as we already know that mode_read and mode_write are
! the only options
  END SELECT

  RETURN

END SUBROUTINE file_ncdf_def_attr_int


SUBROUTINE file_ncdf_def_attr_char(file, var_id, name, value)

  USE io_constants, ONLY : mode_read, mode_write, attr_global

  USE netcdf, ONLY : nf90_put_att, nf90_global

  IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Define a character valued attribute on the given variable with the given
!   name and value
!   To define a global attribute, specify attr_global as var_id
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------
! Argument types
  TYPE(file_ncdf), INTENT(INOUT) :: file
                                    ! The file to define the attribute in
  INTEGER, INTENT(IN) :: var_id     ! The id of the variable to define
                                    ! attribute on
  CHARACTER(LEN=*), INTENT(IN) :: name
                                    ! The name of the attribute
  CHARACTER(LEN=*), INTENT(IN) :: value
                                    ! The value of the attribute


! Work variables
  INTEGER :: var_id_local  ! Local version of var_id that we can modify
  INTEGER :: error         ! Error indicator

!-----------------------------------------------------------------------------

  SELECT CASE ( file%mode )
    CASE ( mode_read )
!-----------------------------------------------------------------------------
! In read mode, we just ignore the request, as we don't care about attributes
!-----------------------------------------------------------------------------

    CASE ( mode_write )
!-----------------------------------------------------------------------------
! In write mode, we need to define the attribute
!-----------------------------------------------------------------------------
      IF ( var_id == attr_global ) THEN
! If a global attribute has been requested, set var id to the NetCDF
! identifier for global attributes
        var_id_local = nf90_global
      ELSE
! Otherwise get the NetCDF id of the variable from the given index in vars
! array
        var_id_local = file%vars(var_id)%id
      END IF

! Try to actually create the attribute
      error = nf90_put_att(file%id, var_id_local, name, value)
      IF ( error /= nf90_noerr )                                              &
        CALL log_fatal_ncdf("file_ncdf_def_attr_real",                        &
                            "Error defining attribute", error)

! No default case as we already know that mode_read and mode_write are
! the only options
  END SELECT

  RETURN

END SUBROUTINE file_ncdf_def_attr_char
#endif
