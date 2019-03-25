! File containing variables for ozone implementation

MODULE ozone_vars

IMPLICIT NONE

! Ozone forcing
REAL, ALLOCATABLE ::                                                        &
  o3_gb(:)        ! Surface ozone concentration (ppb).


! Ozone diagnostics
REAL, ALLOCATABLE ::                                                        &
  flux_o3_pft(:,:)                                                          &
               ! Flux of O3 to stomata (nmol O3/m2/s).
 ,fo3_pft(:,:)  ! Ozone exposure factor.

END MODULE
