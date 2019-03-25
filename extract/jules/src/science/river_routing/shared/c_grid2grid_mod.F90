!Huw Lewis (MO), Jan 2015
!DEPRECATED CODE
!This code was transferred from the UM repository at UM vn9.2 / JULES vn 4.1.
!Future developments will supercede these subroutines, and as such they
!should be considered deprecated. They will be retained in the codebase to
!maintain backward compatibility with functionality prior to
!UM vn10.0 / JULES vn 4.2, until such time as they become redundant.
!
!
! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
! Code Owner: Please refer to the UM file CodeOwners.txt
! This file belongs in section: River Routing
MODULE c_grid2grid_mod

! NOTE:
! THIS ROUTINE IS CURRENTLY PROVIDED FOR THE BENEFIT OF CODE LEGACY WITH
! ONLINE UM IMPLEMENTATIONS. THIS ROUTINE IS NOW DEPRACATED, TO BE REPLACED
! BY JULES_RIVERSPARM
! THESE PARAMETER SETTINGS SHOULD ALSO BE SET AS NAMELIST OPTIONS IF
! RUNNING JULES STANDALONE

IMPLICIT NONE
! C_GRID2GRID start
! Description: Parameters for 1km UK LAM river routing
! Author: Vicky Bell, CEH Wallingford

!      REAL, PARAMETER :: cland  = 0.4   ! land wave speed (m/s)
!      REAL, PARAMETER :: criver = 0.62  ! subsurf river wave speed (m/s)
!      REAL, PARAMETER :: cbland  = 0.05 ! subsurf land wave speed (m/s)
!      REAL, PARAMETER :: cbriver = 0.05 ! subsurf river wave speed (m/s)
!      REAL, PARAMETER :: runoff_factor = 1.0
                                      ! runoff volume factor
!      REAL, PARAMETER :: retl = 0.005     ! return flow (land squares) (<1)
!      REAL, PARAMETER :: retr = 0.005   ! return flow (river squares) (<1)
!      REAL, PARAMETER :: slfac = 0.    ! slope factor (not used yet)
!      INTEGER, PARAMETER :: a_thresh = 10 ! threshold area (number of cells)
! END C_GRID2GRID
!---------------------------------------------------------------------

! C_GRID2GRID start
! Description: Parameters for 25km EU LAM river routing
! Author: Vicky Bell, CEH Wallingford

!      REAL, PARAMETER :: cland  = 0.4    ! land wave speed (m/s)
!      REAL, PARAMETER :: criver = 1.0    ! subsurf river wave speed (m/s)
!      REAL, PARAMETER :: cbland  = 0.075 ! subsurf land wave speed (m/s)
!      REAL, PARAMETER :: cbriver = 0.1   ! subsurf river wave speed (m/s)
!      REAL, PARAMETER :: runoff_factor = 1.0
                                      ! runoff volume factor
!      REAL, PARAMETER :: retl = 0.0005     ! return flow (land squares) (<1)
!      REAL, PARAMETER :: retr = 0.0005   ! return flow (river squares) (<1)
!      REAL, PARAMETER :: slfac = 0.    ! slope factor (not used yet)
!      INTEGER, PARAMETER :: a_thresh = 1 ! threshold area
! END C_GRID2GRID
!---------------------------------------------------------------------

! C_GRID2GRID start
! Description: Parameters for GLOBAL river routing
! Author: Vicky Bell, CEH Wallingford

REAL, PARAMETER :: cland  = 0.2  ! land wave speed (m/s)
REAL, PARAMETER :: criver = 0.62 ! subsurf river wave speed (m/s)
REAL, PARAMETER :: cbland  = 0.1! subsurf land wave speed (m/s)
REAL, PARAMETER :: cbriver = 0.15! subsurf river wave speed (m/s)
REAL, PARAMETER :: runoff_factor = 1.0
                                ! runoff volume factor
REAL, PARAMETER :: retl = 0.0     ! return flow (land squares) (<1)
REAL, PARAMETER :: retr = 0.005   ! return flow (river squares) (<1)
REAL, PARAMETER :: slfac = 0.0    ! slope factor (not used yet)
INTEGER, PARAMETER :: a_thresh = 0.001 ! threshold area (**km**)
! END C_GRID2GRID
!---------------------------------------------------------------------

! C_GRID2GRID start
! Description: Parameters for default UM river routing
! Author: Vicky Bell, CEH Wallingford

!      REAL, PARAMETER :: cland  = 0.2  ! land wave speed (m/s)
!      REAL, PARAMETER :: criver = 0.2  ! subsurf river wave speed (m/s)
!      REAL, PARAMETER :: cbland  = 0.18! subsurf land wave speed (m/s)
!      REAL, PARAMETER :: cbriver = 0.18! subsurf river wave speed (m/s)
!      REAL, PARAMETER :: runoff_factor = 0.7
!                                      ! runoff volume factor
!      REAL, PARAMETER :: retl = 0.0     ! return flow (land squares) (<1)
!      REAL, PARAMETER :: retr = 0.15   ! return flow (river squares) (<1)
!      REAL, PARAMETER :: slfac = 0.    ! slope factor (not used yet)
!      INTEGER, PARAMETER :: a_thresh = 1 ! threshold area
! END C_GRID2GRID
!---------------------------------------------------------------------


END MODULE c_grid2grid_mod
