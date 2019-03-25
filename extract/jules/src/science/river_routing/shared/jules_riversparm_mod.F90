! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
!
! Module holds parameters for river routing calculations

MODULE jules_riversparm

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Module containing parameters for river routing calculations
!   Default values are set here, but can be initialised in namelists
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
! Author: Vicky Bell, CEH Wallingford
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------

! Scalar parameters for the TRIP river routing algorithm

REAL ::                                                                     &
   rivers_meander = 1.4                                                     &
                            ! meander ratio for rivers - the ratio of
                            ! actual river length to the calculated length
   ,rivers_speed = 0.4
                            ! flow speed for rivers (m s-1)

! Scalar parameters for the RFM river routing algorithm

! Description: Parameters for 1km UK LAM river routing
!      REAL, PARAMETER :: cland  = 0.4   ! land wave speed (m/s)
!      REAL, PARAMETER :: criver = 0.5   ! subsurf river wave speed (m/s)
!      REAL, PARAMETER :: cbland  = 0.05 ! subsurf land wave speed (m/s)
!      REAL, PARAMETER :: cbriver = 0.05 ! subsurf river wave speed (m/s)
!      REAL, PARAMETER :: runoff_factor = 1.0
                                      ! runoff volume factor
!      REAL, PARAMETER :: retl = 0.005     ! return flow (land squares) (<1)
!      REAL, PARAMETER :: retr = 0.005   ! return flow (river squares) (<1)
!      REAL, PARAMETER :: slfac = 0.    ! slope factor (not used yet)
!      INTEGER, PARAMETER :: a_thresh = 13 ! threshold area (number of cells)
!---------------------------------------------------------------------

! Description: Parameters for 25km EU LAM river routing

!      REAL, PARAMETER :: cland  = 0.4    ! land wave speed (m/s)
!      REAL, PARAMETER :: criver = 1.0    ! subsurf river wave speed (m/s)
!      REAL, PARAMETER :: cbland  = 0.075 ! subsurf land wave speed (m/s)
!      REAL, PARAMETER :: cbriver = 0.1   ! subsurf river wave speed (m/s)
!      REAL, PARAMETER :: runoff_factor = 1.0
                                      ! runoff volume factor
!      REAL, PARAMETER :: retl = 0.0005     ! return flow (land squares) (<1)
!      REAL, PARAMETER :: retr = 0.0005   ! return flow (river squares) (<1)
!      REAL, PARAMETER :: slfac = 0.    ! slope factor (not used yet)
!      INTEGER, PARAMETER :: a_thresh = 1 ! threshold area (pixels)
!---------------------------------------------------------------------

! Description: Parameters for GLOBAL river routing

REAL :: cland  = 0.2  ! land wave speed (m/s)
REAL :: criver = 0.62 ! subsurf river wave speed (m/s)
REAL :: cbland  = 0.1! subsurf land wave speed (m/s)
REAL :: cbriver = 0.15! subsurf river wave speed (m/s)
REAL :: runoff_factor = 1.0
                                ! runoff volume factor
REAL :: retl = 0.0     ! return flow (land squares) (<1)
REAL :: retr = 0.005   ! return flow (river squares) (<1)
REAL :: slfac = 0.0    ! slope factor (not used yet
INTEGER :: a_thresh = 1 ! threshold area (pixels)

!---------------------------------------------------------------------
! Description: Parameters for default UM river routing

!      REAL, PARAMETER :: cland  = 0.2  ! land wave speed (m/s)
!      REAL, PARAMETER :: criver = 0.2  ! subsurf river wave speed (m/s)
!      REAL, PARAMETER :: cbland  = 0.18! subsurf land wave speed (m/s)
!      REAL, PARAMETER :: cbriver = 0.18! subsurf river wave speed (m/s)
!      REAL, PARAMETER :: runoff_factor = 0.7
!                                      ! runoff volume factor
!      REAL, PARAMETER :: retl = 0.0     ! return flow (land squares) (<1)
!      REAL, PARAMETER :: retr = 0.15   ! return flow (river squares) (<1)
!      REAL, PARAMETER :: slfac = 0.    ! slope factor (not used yet)
!      INTEGER, PARAMETER :: a_thresh = 1 ! threshold area (pixels)

END MODULE jules_riversparm
