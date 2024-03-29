#if !defined(UM_JULES)
! *****************************COPYRIGHT**************************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT**************************************


SUBROUTINE file_ascii_read_var_scalar(file, var_id, value, start)

  USE io_constants, ONLY : max_dim_var

  IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Reads a scalar value from a variable in the given file
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------
! Argument types
  TYPE(file_ascii), INTENT(INOUT) :: file  ! The file to read from
  INTEGER, INTENT(IN) :: var_id  ! The id of the variable to read from
  REAL, INTENT(OUT) :: value  ! The value read from file
  INTEGER, INTENT(IN) :: start(:)  ! The point to start reading from
                                   ! (one value for each dimension
                                   ! of the variable in the file)

! Work variables
  REAL :: local_values(1)  ! An array version of value
  INTEGER :: local_count(max_dim_var)  ! A version of count, which will be
                                       ! all 1s

!-----------------------------------------------------------------------------

! To implement this routine, we just delegate to the 1d routine and reshape
! the result
  local_count(:) = 1

  CALL file_ascii_read_var_1d(file, var_id, local_values, start, local_count)

  value = local_values(1)

  RETURN

END SUBROUTINE file_ascii_read_var_scalar


SUBROUTINE file_ascii_read_var_1d(file, var_id, values, start, count_in)

  USE io_constants, ONLY : mode_read

  IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Reads a 1d array from a variable in the given file
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------
! Argument types
  TYPE(file_ascii), INTENT(INOUT) :: file  ! The file to read from
  INTEGER, INTENT(IN) :: var_id  ! The id of the variable to read from
  REAL, DIMENSION(:), INTENT(OUT) :: values  ! The values read from file
  INTEGER, INTENT(IN) :: start(:)  ! The point to start reading from
                                   ! (one value for each dimension
                                   ! of the variable in the file)
  INTEGER, INTENT(IN) :: count_in(:)  ! The number of points to read
                                      ! in each dimension of the variable

! Work variables
  INTEGER :: ndims  ! The number of dimensions that the variable has in file

  INTEGER :: offset  ! The offset to use in the buffer

!-----------------------------------------------------------------------------

! We can't read data if the file is in define mode
  IF ( file%define_mode )                                                     &
    CALL log_fatal("file_ascii_read_var_1d",                                  &
                   "Cannot read data - file is still in define mode")

! We can't read data unless we are in read mode...
  IF ( file%mode /= mode_read )                                               &
    CALL log_fatal("file_ascii_read_var_1d",                                  &
                   "Can only read data if file is opened in read mode")

  ndims = file%var_ndims(var_id)

! ASCII files have the restriction that we can only read the whole variable
  IF ( ANY(start(1:ndims) /= 1) )                                             &
    CALL log_fatal("file_ascii_read_var_1d",                                  &
                   "start must be 1 for all dimensions - reading part of " // &
                   "a variable from an ASCII file is not supported")

  IF ( SIZE(values) /= PRODUCT(count_in(1:ndims)) .AND.                       &
       SIZE(values) /= file%var_sizes(var_id) )                               &
    CALL log_fatal("file_ascii_read_var_1d",                                  &
                   "values must have the same number of elements as the " //  &
                   "variable in file - reading part of a variable from an " //&
                   "ASCII file is not supported")

! Now we know we can read correctly
  offset = file%var_offsets(var_id)
  values(:) = file%buffer(offset:(offset + file%var_sizes(var_id) - 1))

  RETURN

END SUBROUTINE file_ascii_read_var_1d


SUBROUTINE file_ascii_read_var_2d(file, var_id, values, start, count_in)

  IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Reads a 2d array from a variable in the given file
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------
! Argument types
  TYPE(file_ascii), INTENT(INOUT) :: file  ! The file to read from
  INTEGER, INTENT(IN) :: var_id  ! The id of the variable to read from
  REAL, DIMENSION(:,:), INTENT(OUT) :: values  ! The values read from file
  INTEGER, INTENT(IN) :: start(:)  ! The point to start reading from
                                   ! (one value for each dimension
                                   ! of the variable in the file)
  INTEGER, INTENT(IN) :: count_in(:)  ! The number of points to read
                                      ! in each dimension of the variable


! Work variables
  REAL, ALLOCATABLE :: local_values(:)  ! A 1d version of values

!-----------------------------------------------------------------------------

! To implement this routine, we just delegate to the 1d routine and reshape
! the result

  ALLOCATE(local_values(PRODUCT(SHAPE(values))))

  CALL file_ascii_read_var_1d(file, var_id, local_values, start, count_in)

  values = RESHAPE(local_values, SHAPE(values))

  DEALLOCATE(local_values)

  RETURN

END SUBROUTINE file_ascii_read_var_2d


SUBROUTINE file_ascii_read_var_3d(file, var_id, values, start, count_in)

  IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Reads a 3d array from a variable in the given file
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------
! Argument types
  TYPE(file_ascii), INTENT(INOUT) :: file  ! The file to read from
  INTEGER, INTENT(IN) :: var_id  ! The id of the variable to read from
  REAL, DIMENSION(:,:,:), INTENT(OUT) :: values  ! The values read from file
  INTEGER, INTENT(IN) :: start(:)  ! The point to start reading from
                                   ! (one value for each dimension
                                   ! of the variable in the file)
  INTEGER, INTENT(IN) :: count_in(:)  ! The number of points to read
                                      ! in each dimension of the variable


! Work variables
  REAL, ALLOCATABLE :: local_values(:)  ! A 1d version of values

!-----------------------------------------------------------------------------


! To implement this routine, we just delegate to the 1d routine and reshape
! the result

  ALLOCATE(local_values(PRODUCT(SHAPE(values))))

  CALL file_ascii_read_var_1d(file, var_id, local_values, start, count_in)

  values = RESHAPE(local_values, SHAPE(values))

  DEALLOCATE(local_values)

END SUBROUTINE file_ascii_read_var_3d


SUBROUTINE file_ascii_read_var_4d(file, var_id, values, start, count_in)

  IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Reads a 4d array from a variable in the given file
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------
! Argument types
  TYPE(file_ascii), INTENT(INOUT) :: file  ! The file to read from
  INTEGER, INTENT(IN) :: var_id  ! The id of the variable to read from
  REAL, DIMENSION(:,:,:,:), INTENT(OUT) :: values  ! The values read from file
  INTEGER, INTENT(IN) :: start(:)  ! The point to start reading from
                                   ! (one value for each dimension
                                   ! of the variable in the file)
  INTEGER, INTENT(IN) :: count_in(:)  ! The number of points to read
                                      ! in each dimension of the variable


! Work variables
  REAL, ALLOCATABLE :: local_values(:)  ! A 1d version of values

!-----------------------------------------------------------------------------


! To implement this routine, we just delegate to the 1d routine and reshape
! the result

  ALLOCATE(local_values(PRODUCT(SHAPE(values))))

  CALL file_ascii_read_var_1d(file, var_id, local_values, start, count_in)

  values = RESHAPE(local_values, SHAPE(values))

  DEALLOCATE(local_values)

END SUBROUTINE file_ascii_read_var_4d


SUBROUTINE file_ascii_read_var_5d(file, var_id, values, start, count_in)

  IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Reads a 5d array from a variable in the given file
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------
! Argument types
  TYPE(file_ascii), INTENT(INOUT) :: file  ! The file to read from
  INTEGER, INTENT(IN) :: var_id  ! The id of the variable to read from
  REAL, DIMENSION(:,:,:,:,:), INTENT(OUT) :: values  ! The values read from file
  INTEGER, INTENT(IN) :: start(:)  ! The point to start reading from
                                   ! (one value for each dimension
                                   ! of the variable in the file)
  INTEGER, INTENT(IN) :: count_in(:)  ! The number of points to read
                                      ! in each dimension of the variable


! Work variables
  REAL, ALLOCATABLE :: local_values(:)  ! A 1d version of values

!-----------------------------------------------------------------------------


! To implement this routine, we just delegate to the 1d routine and reshape
! the result

  ALLOCATE(local_values(PRODUCT(SHAPE(values))))

  CALL file_ascii_read_var_1d(file, var_id, local_values, start, count_in)

  values = RESHAPE(local_values, SHAPE(values))

  DEALLOCATE(local_values)

END SUBROUTINE file_ascii_read_var_5d


SUBROUTINE file_ascii_read_var_6d(file, var_id, values, start, count_in)

  IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Reads a 6d array from a variable in the given file
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------
! Argument types
  TYPE(file_ascii), INTENT(INOUT) :: file  ! The file to read from
  INTEGER, INTENT(IN) :: var_id  ! The id of the variable to read from
  REAL, DIMENSION(:,:,:,:,:,:), INTENT(OUT) :: values  ! The values read from file
  INTEGER, INTENT(IN) :: start(:)  ! The point to start reading from
                                   ! (one value for each dimension
                                   ! of the variable in the file)
  INTEGER, INTENT(IN) :: count_in(:)  ! The number of points to read
                                      ! in each dimension of the variable


! Work variables
  REAL, ALLOCATABLE :: local_values(:)  ! A 1d version of values

!-----------------------------------------------------------------------------


! To implement this routine, we just delegate to the 1d routine and reshape
! the result

  ALLOCATE(local_values(PRODUCT(SHAPE(values))))

  CALL file_ascii_read_var_1d(file, var_id, local_values, start, count_in)

  values = RESHAPE(local_values, SHAPE(values))

  DEALLOCATE(local_values)

END SUBROUTINE file_ascii_read_var_6d


SUBROUTINE file_ascii_read_var_7d(file, var_id, values, start, count_in)

  IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Reads a 7d array from a variable in the given file
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------
! Argument types
  TYPE(file_ascii), INTENT(INOUT) :: file  ! The file to read from
  INTEGER, INTENT(IN) :: var_id  ! The id of the variable to read from
  REAL, DIMENSION(:,:,:,:,:,:,:), INTENT(OUT) :: values  ! The values read from file
  INTEGER, INTENT(IN) :: start(:)  ! The point to start reading from
                                   ! (one value for each dimension
                                   ! of the variable in the file)
  INTEGER, INTENT(IN) :: count_in(:)  ! The number of points to read
                                      ! in each dimension of the variable


! Work variables
  REAL, ALLOCATABLE :: local_values(:)  ! A 1d version of values

!-----------------------------------------------------------------------------


! To implement this routine, we just delegate to the 1d routine and reshape
! the result

  ALLOCATE(local_values(PRODUCT(SHAPE(values))))

  CALL file_ascii_read_var_1d(file, var_id, local_values, start, count_in)

  values = RESHAPE(local_values, SHAPE(values))

  DEALLOCATE(local_values)

END SUBROUTINE file_ascii_read_var_7d
#endif
