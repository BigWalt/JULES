#if !defined(UM_JULES)
! *****************************COPYRIGHT**************************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT**************************************


SUBROUTINE file_ncdf_introspect(file)

  USE io_constants, ONLY : max_sdf_name_len, max_dim_file, mode_read

  USE netcdf, ONLY : nf90_inquire, nf90_inquire_dimension, nf90_inquire_variable

  IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Given an open file in read mode, try to detect the dimensions and
!   variables in the file and define them on the file
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------
! Argument types
  TYPE(file_ncdf), INTENT(INOUT) :: file  ! The file to detect items in

! Work variables

! Information about the dimensions found in the file
  INTEGER :: ndims
  INTEGER :: record_dim_id
  CHARACTER(LEN=max_sdf_name_len) :: dim_name
  INTEGER :: dim_len

! Information about the variables found in the file
  INTEGER :: nvars
  CHARACTER(LEN=max_sdf_name_len) :: var_name
  INTEGER :: var_ndims
  INTEGER, ALLOCATABLE :: var_dim_ids(:)

  INTEGER :: i  ! Index variables

  INTEGER :: dummy  ! Throwaway value

  INTEGER :: error  ! Error indicator


!-----------------------------------------------------------------------------

! We can only do this in read mode
  IF ( file%mode /= mode_read )                                               &
    CALL log_fatal("file_ncdf_introspect",                                    &
                   TRIM(file%name) // ": " //                                 &
                   "Can only introspect files in read mode")

!-----------------------------------------------------------------------------
! For NetCDF files, we can use netcdf library functions to recover the
! information that we need
!-----------------------------------------------------------------------------
! First find out general information about the file
  error = nf90_inquire(file%id, nDimensions=ndims, nVariables=nvars,          &
                                unlimitedDimId=record_dim_id)
  IF ( error /= nf90_noerr )                                                  &
    CALL log_fatal_ncdf("file_ncdf_introspect",                               &
                        TRIM(file%name) // ": " //                            &
                        "Error reading numbers of dimensions and variables",  &
                        error)

! Check if we have code to handle the number of dimensions
  IF ( ndims > max_dim_file )                                                 &
    CALL log_fatal("file_ncdf_introspect",                                    &
                   TRIM(file%name) // ": " //                                 &
                   "Too many dimensions in file - try increasing max_dim_file")

! Check if we have code to handle the number of variables
  IF ( nvars > max_var_file )                                                 &
    CALL log_fatal("file_ncdf_introspect",                                    &
                   TRIM(file%name) // ": " //                                 &
                   "Too many variables in file - try increasing max_var_file")


!-----------------------------------------------------------------------------
! For each dimension, gather information about it and then define it
!-----------------------------------------------------------------------------
  DO i = 1,ndims
    error = nf90_inquire_dimension(file%id, i, dim_name, dim_len)
    IF ( error /= nf90_noerr )                                                &
      CALL log_fatal_ncdf("file_ncdf_introspect",                             &
                          TRIM(file%name) // ": " //                          &
                          "Error reading dimension info", error)

! If we hit the record dimension, then define it as such
    IF ( i == record_dim_id ) THEN
      dummy = file_ncdf_def_record_dim(file, dim_name)
      CYCLE
    END IF

! Otherwise, define a regular dimension. We don't need to collect the ids
! since this driver uses the NetCDF ids for dimensions to define variables
    dummy = file_ncdf_def_dim(file, dim_name, dim_len)
  END DO


!-----------------------------------------------------------------------------
! For each variable, gather information about it and then define it
!-----------------------------------------------------------------------------
  DO i = 1,nvars
! Retrieve the variable name and number of dimensions
    error = nf90_inquire_variable(file%id, i, NAME=var_name, NDIMS=var_ndims)
    IF ( error /= nf90_noerr )                                                &
      CALL log_fatal_ncdf("file_ncdf_introspect",                             &
                          TRIM(file%name) // ": " //                          &
                          "Error reading variable name", error)

! Now we know how many dimensions there are, we can retrieve their ids
    ALLOCATE(var_dim_ids(var_ndims))

    error = nf90_inquire_variable(file%id, i, DIMIDS=var_dim_ids)
    IF ( error /= nf90_noerr )                                                &
      CALL log_fatal_ncdf("file_ncdf_introspect",                             &
                        TRIM(file%name) // ": " //                            &
                        "Error reading variable dimensions", error)

! Now we can define the variable on the file - we don't need to collect
! variable ids
    dummy = file_ncdf_def_var(                                                &
      file, var_name,                                                         &
! The dimensions that we give are the dimensions we found with the record dim
! removed if present
      PACK(var_dim_ids, var_dim_ids /= record_dim_id),                        &
! is_record is .TRUE. if the record dimension is present in the list
      ANY(var_dim_ids == record_dim_id)                                       &
    )

    DEALLOCATE(var_dim_ids)
  END DO

  RETURN

END SUBROUTINE file_ncdf_introspect
#endif
