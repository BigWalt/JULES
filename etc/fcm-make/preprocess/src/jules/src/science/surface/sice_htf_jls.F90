










! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
MODULE sice_htf_mod

IMPLICIT NONE

CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName='SICE_HTF_MOD'

CONTAINS
!  SUBROUTINE SICE_HTF-----------------------------------------------
!
!  Purpose: Updates sea-ice surface layer temperature.
!
! Arguments:---------------------------------------------------------
SUBROUTINE sice_htf (                                                         &
! IN fields
 flandg,nice,                                                                 &
 di_ncat,ice_fraction,ice_fract_ncat,surf_ht_flux_sice_ncat,                  &
 sstfrz,                                                                      &
! INOUT fields
 ti,sf_diag,                                                                  &
! OUT fields
 ti_gb,sea_ice_htf                                                            &
 )

USE jules_sea_seaice_mod, ONLY: l_sice_heatflux, l_saldep_freeze
USE atm_fields_bounds_mod, ONLY: tdims
USE missing_data_mod, ONLY: rmdi
USE c_sicehc, ONLY: ai
USE c_kappai, ONLY: kappai
USE water_constants_mod, ONLY: tm, tfs
USE sf_diags_mod, ONLY: strnewsfdiag
USE parkind1, ONLY: jprb, jpim
USE yomhook, ONLY: lhook, dr_hook
USE timestep_mod, ONLY: timestep
USE jules_print_mgr, ONLY:                                                   &
    jules_message,                                                            &
    jules_print

IMPLICIT NONE

INTEGER, INTENT(IN) ::                                                        &
 nice
                      ! IN Number of ice categories

REAL, INTENT(IN) ::                                                           &
 flandg(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)                  &
                              ! IN Land fraction.
,di_ncat(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end,nice)            &
                               ! IN "Equivalent thickness" of
!                                   !    sea-ice (m).
,ice_fraction(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)            &
                              ! IN Fraction of gridbox covered by
!                                   !    sea-ice.
,ice_fract_ncat(tdims%i_start:tdims%i_end,                                    &
                tdims%j_start:tdims%j_end,nice)                               &
                                     ! IN Fraction of ice in
!                                 ! gridbox covered by each ice catagory
,surf_ht_flux_sice_ncat(tdims%i_start:tdims%i_end,                            &
                        tdims%j_start:tdims%j_end,nice)                       &
!                                   ! IN Net downward heat flux at
!                                   !    sea-ice surface W/m2
,sstfrz(tdims%i_start:tdims%i_end, tdims%j_start:tdims%j_end)
                              ! IN Sea surface freezing temperature (K).
! Diagnostics
TYPE (strnewsfdiag), INTENT(INOUT) :: sf_diag

REAL, INTENT(INOUT) ::                                                        &
 ti(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end,nice)
                          ! INOUT Sea-ice surface layer
!                                   !       temperature(K) Set to TSTAR
!                                   !       for unfrozen sea, missing
!                                   !       data for land.

REAL, INTENT(OUT) ::                                                          &
 ti_gb(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)                   &
                              ! OUT GBM ice surface temperature (K)
,sea_ice_htf(tdims%i_start:tdims%i_end,                                       &
             tdims%j_start:tdims%j_end,nice)
                              ! OUT Heat flux through sea-ice
!                                   !   (W per sq m, positive downwards)

REAL ::                                                                       &
 tsave                    ! Temporary temperature

REAL ::                                                                       &
 ti_gb_local(tdims%i_start:tdims%i_end,tdims%j_start:tdims%j_end)
                          ! local equivalent to TI_GB

INTEGER :: i,j,n             ! Loop Counter; horizontal field index.

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='SICE_HTF'

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

DO j = tdims%j_start,tdims%j_end
  DO i = tdims%i_start,tdims%i_end
    IF (flandg(i,j) == 1.0) THEN
      DO n = 1,nice
        sea_ice_htf(i,j,n) = 0.0
        ti(i,j,n) = rmdi
      END DO
      ti_gb_local(i,j) = rmdi
    ELSE IF (ice_fraction(i,j) <= 0.0) THEN
      DO n = 1,nice
        sea_ice_htf(i,j,n) = 0.0
        ti(i,j,n) = tfs
      END DO
      !      IF (nice /= nice_use) tstar_sice_cat(i,j,1) = tfs  ! set in sf_imp2
      ti_gb_local(i,j) = tfs
    ELSE
      ti_gb_local(i,j) = 0.0
      DO n = 1,nice
        IF (ice_fract_ncat(i,j,n) > 0.0) THEN
          IF (l_sice_heatflux) THEN
            ! Semi-implicit update of TI
            tsave = ti(i,j,n)
            IF (l_saldep_freeze) THEN
              ! use the salinity dependent freezing of sea water to define sea ice heatflux
              ti(i,j,n)=( ti(i,j,n) + ai * timestep * (                      &
                  surf_ht_flux_sice_ncat(i,j,n) +                            &
                  ((sstfrz(i,j) - tsave * 0.5) * kappai / di_ncat(i,j,n)) )  )  /    &
                  ( 1.0 + kappai * ai * timestep * 0.5 / di_ncat(i,j,n) )
              sea_ice_htf(i,j,n) = kappai *                                  &
                  ((ti(i,j,n) + tsave) * 0.5 - sstfrz(i,j)) / di_ncat(i,j,n)
            ELSE
              ! no salinity dependence - use original scheme
              ti(i,j,n)=( ti(i,j,n) + ai * timestep * (                      &
                   surf_ht_flux_sice_ncat(i,j,n) +                           &
            ((tfs - tsave * 0.5) * kappai / di_ncat(i,j,n)) )  )  /                  &
            ( 1.0 + kappai * ai * timestep * 0.5 / di_ncat(i,j,n) )
              sea_ice_htf(i,j,n) = kappai *                                     &
                        ((ti(i,j,n) + tsave) * 0.5 - tfs) / di_ncat(i,j,n)
            END IF  ! l_saldep_freeze

          ELSE
            ! Original explicit update of TI
            ! (unstable for small ice thicknesses)
            IF (l_saldep_freeze) THEN
              ! use the salinity dependent freezing of sea water to define sea ice heatflux
              sea_ice_htf(i,j,n) = kappai *                                  &
                         (ti(i,j,n) - sstfrz(i,j)) / di_ncat(i,j,n)
            ELSE
              ! no salinity dependence - use original scheme
              sea_ice_htf(i,j,n) = kappai *                                   &
                               (ti(i,j,n) - tfs) / di_ncat(i,j,n)
            END IF
            ti(i,j,n) = ti(i,j,n) + ai * timestep *                           &
               (surf_ht_flux_sice_ncat(i,j,n) - sea_ice_htf(i,j,n))
          END IF ! l_sice_heatflux
          IF ( ti(i,j,n) > tm ) THEN
            IF (sf_diag%simlt) THEN
              sf_diag%sice_mlt_htf(i,j,n) = sf_diag%sice_mlt_htf(i,j,n)       &
                                          + (ti(i,j,n) - tm) / (ai * timestep)
            END IF
            ti(i,j,n) = tm
          END IF
          ti_gb_local(i,j) = ti_gb_local(i,j)                                 &
        + (ice_fract_ncat(i,j,n) / ice_fraction(i,j)) * ti(i,j,n)
        ELSE
          sea_ice_htf(i,j,n) = 0.0
          ti(i,j,n) = tfs
        END IF
      END DO
    END IF
  END DO
END DO

!-----------------------------------------------------------------------
! When NICE=1 the pointers for TI_GB and TI are identical and TI=TI_GB
! Thus we need to ensure that neither is reassigned until the end of the
! summing of the gridbox mean. This does not occur when NICE  /=  1 as
! then the pointers are not equivalent.
!-----------------------------------------------------------------------
DO j = tdims%j_start,tdims%j_end
  DO i = tdims%i_start,tdims%i_end
    ! check for missed settings, possible in last bit of calc above?
    IF (ti_gb_local(i,j) == 0.0) THEN
      WRITE(jules_message,*)'SICE_HTF: TIDYING UP MISSED VALUES OF TI_GB_LOCAL'
      CALL jules_print('sice_htf_jls',jules_message)
      ti_gb_local(i,j) = tfs
    END IF
    ti_gb(i,j) = ti_gb_local(i,j)
  END DO
END DO

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE sice_htf
END MODULE sice_htf_mod
