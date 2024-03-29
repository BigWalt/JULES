#if !defined(UM_JULES)
!******************************COPYRIGHT****************************************
! (c) Crown copyright, Met Office. All rights reserved.
!
! This routine has been licensed to the other JULES partners for use and
! distribution under the JULES collaboration agreement, subject to the terms and
! conditions set out therein.
!
! [Met Office Ref SC0237]
!******************************COPYRIGHT****************************************

SUBROUTINE calc_downward_rad(sea_ice_albedo, photosynth_act_rad)

!Module Imports

USE theta_field_sizes, ONLY : t_i_length, t_j_length

USE ancil_info, ONLY :  &
  land_mask, frac_surft, ice_fract_ncat_sicat, nsurft, land_index, land_pts

USE forcing, ONLY :  &
  lw_down_ij, sw_down_ij, diff_rad_ij

USE fluxes, ONLY: &
  land_albedo_ij, alb_surft, sw_surft

USE csigma, ONLY :  &
  sbcon

USE coastal, ONLY: &
  tstar_sice_sicat

USE prognostics, ONLY:  &
  tstar_surft

USE jules_sea_seaice_mod, ONLY : &
  nice_use

USE jules_surface_mod, ONLY : diff_frac

USE jules_radiation_mod, ONLY : wght_alb

USE atm_fields_bounds_mod, ONLY: tdims

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Contains the calculations for downward radiation for standalone JULES
!
! Code Owner: Please refer to ModuleLeaders.txt
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------

!Arguments
REAL, INTENT(IN) :: sea_ice_albedo(t_i_length,t_j_length,4)  ! Sea ice albedo

REAL, INTENT(OUT):: &
photosynth_act_rad(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)
!                                     ! Net downward shortwave radiation
!                                     !  in band 1 (w/m2).

!Local Variables
INTEGER :: i,j,n,k,l

!End of header

!-------------------------------------------------------------------------------
!   Change radiation to be downward components if not using io_rad_type=1
!   NOTE this assumes that the point is either 100% land or 100% sea-ice.
!   1 downward fluxes provided
!   2 net all wavelength flux (downward is positive and is stored in downward
!     longwave variable) and downward shortwave fluxes are provided
!   3 net downward fluxes are provided (in downward variables)
!   4 downward longwave and net downward shortwave are provided
!   One day we would probably like to do this in driveUpdate or the likes, but
!   at present we don't
!   have all the required variables/masks there (I think).
!-------------------------------------------------------------------------------

!Convert net total radiation to net longwave. Net total is currently stored
!in lw_down_ij. Use the average of diffuse albedos in visible and NIR on land.
IF ( io_rad_type == 2 ) THEN
  DO j=1,t_j_length
    DO i=1,t_i_length
      IF ( land_mask(i,j) ) THEN
        lw_down_ij(i,j) = lw_down_ij(i,j) - sw_down_ij(i,j) *                 &
                          (1.0 -                                              &
                           (wght_alb(1) * land_albedo_ij(i,j,1) +             &
                            wght_alb(2) * land_albedo_ij(i,j,2) +             &
                            wght_alb(3) * land_albedo_ij(i,j,3) +             &
                            wght_alb(4) * land_albedo_ij(i,j,4) )) 

      ELSE
        !Assume not using CICE scheme so albedo same for all rad bands
        lw_down_ij(i,j) = lw_down_ij(i,j) -                                   &
          sw_down_ij(i,j) * (1.0-sea_ice_albedo(i,j,1))
      END IF
    END DO
  END DO
END IF   !  io_rad_type

!Convert shortwave from net to downward.
!Net flux is currently stored in sw_down_ij.
!Use the average of diffuse albedos in visible and NIR on land.
IF ( io_rad_type == 3 .OR. io_rad_type == 4 ) THEN
  DO j=1,t_j_length
    DO i=1,t_i_length
      IF ( land_mask(i,j) ) THEN
        sw_down_ij(i,j) = sw_down_ij(i,j) /                                   &
                          (1.0 -                                              &
                           (wght_alb(1) * land_albedo_ij(i,j,1) +             &
                            wght_alb(2) * land_albedo_ij(i,j,2)  +            &
                            wght_alb(3) * land_albedo_ij(i,j,3)  +            &
                            wght_alb(4) * land_albedo_ij(i,j,4) )) 

      ELSE
        !Assume not using CICE scheme so albedo same for all rad bands
        sw_down_ij(i,j) = sw_down_ij(i,j) / ( 1.0 - sea_ice_albedo(i,j,1) )
      END IF
    END DO
  END DO
END IF   !  io_rad_type

!Convert longwave from net to downward. Net longwave is currently stored in
!lw_down_ij.
IF ( io_rad_type == 2 .OR. io_rad_type == 3 ) THEN
  DO j=1,t_j_length
    DO i=1,t_i_length
      IF ( .NOT. land_mask(i,j) ) THEN
        DO n=1,nice_use
          lw_down_ij(i,j) = lw_down_ij(i,j) + ice_fract_ncat_sicat(i,j,n) * sbcon   &
                                      * tstar_sice_sicat(i,j,n)**4.0
        END DO
      END IF
    END DO
  END DO
  DO n=1,nsurft
    DO l=1,land_pts
      j = ( land_index(l)-1 ) / t_i_length + 1
      i = land_index(l) - ( j-1 ) * t_i_length
      lw_down_ij(i,j) = lw_down_ij(i,j) + frac_surft(l,n) * sbcon *           &
                        tstar_surft(l,n)**4.0
    END DO
  END DO
END IF   !  io_rad_type


! Now we know sw_down_ij, we can update diff_frac if required
IF ( use_diff_rad ) THEN
  k = 0
  DO j = 1,t_j_length
    DO i = 1,t_i_length
      k = k + 1
      IF ( sw_down_ij(i,j) > 1.0 ) THEN
        diff_frac(k) = diff_rad_ij(i,j) / sw_down_ij(i,j)
        diff_frac(k) = MIN( 1.0, diff_frac(k) )
      ELSE
        diff_frac(k) = 0.0
      END IF
    END DO
  END DO
ELSE
  diff_frac(:) = diff_frac_const
END IF

!Calculate net SW radiation on tiles.
!Use the average of diffuse albedos in visible and NIR.
DO n=1,nsurft
  DO l=1,land_pts
    j = ( land_index(l)-1 ) / t_i_length + 1
    i = land_index(l) - (j-1)*t_i_length
    sw_surft(l,n) = ( 1.0 - ( wght_alb(1) * alb_surft(l,n,1) +  &
                             wght_alb(2) * alb_surft(l,n,2)  +  &
                             wght_alb(3) * alb_surft(l,n,3)  +  &
                             wght_alb(4) * alb_surft(l,n,4)     &
                   ) ) * sw_down_ij(i,j)

  END DO
END DO

!Calculate photosynthetically active radiation (PAR).
photosynth_act_rad(:,:) = 0.5 * sw_down_ij(:,:)

RETURN
END SUBROUTINE calc_downward_rad
#endif
