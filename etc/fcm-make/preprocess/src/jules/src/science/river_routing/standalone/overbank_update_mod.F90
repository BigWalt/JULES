










!******************************COPYRIGHT**************************************
! (c) Centre for Ecology and Hydrology. All rights reserved.
!
! This routine has been licensed to the other JULES partners for use and
! distribution under the JULES collaboration agreement, subject to the terms
! and conditions set out therein.
!
! [Met Office Ref SC0237] 
!******************************COPYRIGHT**************************************

MODULE overbank_update_mod

CONTAINS

SUBROUTINE overbank_update( )

!-------------------------------------------------------------------------------
!
! Description:
!
!   Calculate river inundation
!
!  Code Owner: Please refer to ModuleLeaders.txt
!  This file belongs in section: Hydrology
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-------------------------------------------------------------------------------

USE timestep_mod, ONLY: timestep

USE planet_constants_mod, ONLY: planet_radius

USE water_constants_mod, ONLY: rho_water

USE conversions_mod, ONLY: pi_over_180

USE jules_rivers_mod, ONLY:                                                   &
   np_rivers, rivers_boxareas_rp, rfm_rivflow_rp,                             &
   rivers_timestep, rivers_dx, rivers_dlat

USE overbank_inundation_mod, ONLY:                                            &
   !  imported scalar parameters
   l_riv_hypsometry, use_rosgen,                                              &
   !  imported arrays with intent(inout)
   logn_mean_rp, logn_stdev_rp, qbf, dbf, wbf,                                &
   riv_a, riv_b, riv_c, riv_f, ent_ratio,                                     &
   frac_fplain_rp

!-------------------------------------------------------------------------------

IMPLICIT NONE

! internal variables
REAL ::                                                                       &
   riv_width(np_rivers),                                                      &
       ! river width (m)
   riv_depth(np_rivers)
       ! river depth (m)

REAL :: flx, dt, dx

!Values used in the ERF() call
REAL, PARAMETER :: erf_int_half = 0.5
REAL, PARAMETER :: erf_int_sqrt2 = 1.41421356

INTEGER :: ip

!! N.b., USE utils_module for erf rather than intrinsic

!------------------------------------------------------------------------------
! Initialisation
!------------------------------------------------------------------------------

! rivers model timestep (s)
dt = rivers_timestep * timestep  

! horizontal gridsize (m)
IF (rivers_dx <= 0) THEN
  dx = planet_radius * ( ABS( rivers_dlat ) * pi_over_180 )
ELSE
  dx = rivers_dx
END IF

!-------------------------------------------------------------------------------
! Calculate river width and depth, based on flow speed
!-------------------------------------------------------------------------------

DO ip = 1,np_rivers

  IF ( rfm_rivflow_rp(ip) < 0.0 ) rfm_rivflow_rp(ip) = 0.0

  ! Leopold & Maddock (1953: eqn2)
  riv_depth(ip) = riv_c * (rfm_rivflow_rp(ip)** riv_f)

  !-----------------------------------------------------------------------------
  ! Calculate new overbank inundation
  !-----------------------------------------------------------------------------

  frac_fplain_rp(ip) = 0.0
  IF ( l_riv_hypsometry ) THEN 
    ! Calculate inundated area using a hypsometric integral
    !!! N.B. numerical methods forms for ERF() to be put in at a later point

    IF ( riv_depth(ip) > 0.0 .AND. logn_mean_rp(ip) > -999.0 .AND.            &
         logn_stdev_rp(ip) > -999.0 ) THEN
      frac_fplain_rp(ip) = erf_int_half + erf_int_half *                      &
                               ERF((LOG(riv_depth(ip)) - logn_mean_rp(ip)) /  &
                               (logn_stdev_rp(ip) * erf_int_sqrt2) )
    END IF

  ELSE

    ! Calculate as estimated river width divided by (north-south cell extent),
    ! (i.e. assuming that river channel runs straight W-E)

    ! Leopold & Maddock (1953: eqn1)
    riv_width(ip) = riv_a * (rfm_rivflow_rp(ip)** riv_b)

    IF ( use_rosgen .AND. rfm_rivflow_rp(ip) > qbf(ip) ) THEN
      ! If river above bankfull and using Rosgen entrenchment ratio option.
      ! This assumes linear interpolation between (width=wbf, depth=dbf) and
      ! (width=ent_ratio*wbf, depth=2*dbf) (from definition of Rosgen
      ! entrenchment ratio)

      riv_width(ip) = wbf(ip) + ( ( wbf(ip) * (ent_ratio-1.0)                 &
                                  * (riv_depth(ip) - dbf(ip)) ) / dbf(ip) )
    END IF

    IF ( riv_width(ip) > 0.0 ) THEN
      frac_fplain_rp(ip) = MAX( MIN(riv_width(ip) / dx , 1.0 ) , 0.0 )
    END IF

  END IF

END DO

END SUBROUTINE overbank_update

END MODULE overbank_update_mod
