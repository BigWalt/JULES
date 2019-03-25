#if defined(UM_JULES)
! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
!
!Code Owner: Please refer to the UM file CodeOwners.txt
!This file belongs in section: River Routing
MODULE do_areaver_mod
USE umPrintMgr
IMPLICIT NONE

CONTAINS


SUBROUTINE do_areaver(gaps_lambda_srce,gaps_phi_srce                    &
,data_srce,gaps_lambda_targ,gaps_phi_targ,count_targ,base_targ          &
,lrow_targ,want,mask_targ,index_srce,weight,data_targ                   &
,global_src_lambda_gaps,global_src_phi_gaps, grid, recv_concern,        &
recv_size)
!     Subroutine DO_AREAVER_PAR ----------------------------------------
!
!   Purpose:
!
!
!
!     Performs parallel area-averaging to transform data from the source
!     grid to the target grid, or adjust the values on the source grid to
!     have the area-averages supplied on the target grid. The latter
!     mode is intended for adjusting values obtained by interpolating
!     from "target" to "source" in order to conserve the area-averages.
!     This mode should be used ONLY if each source box belongs in
!     exactly one target box. ADJUST=0 selects normal area-averaging,
!     ADJUST=1 selects adjustment by addition (use this mode for fields
!     which may have either sign), ADJUST=2 selects adjustment by
!     multiplication (for fields which are positive-definite or
!     negative-definite).
!
!     For two-way conservative coupling, ADJUST=3 makes an adjustment
!     field for fields which may have either sign, ADJUST=4 makes an
!     adjustment field for fields which are positive-definite or
!     negative-definite, ADJUST=5 performs conservative adjustment
!     by addition (use this mode for fields which may have either sign)
!     and ADJUST=6 selects conservative adjustment by multiplication
!     (for fields which are positive-definite or negative-definite).
!
!     The shape of the source and target grids are specified by their
!     dimensions GAPS_aa_bb, which give the number of gaps in the
!     aa=LAMBDA,PHI coordinate in the bb=SRCE,TARG grid. (The product
!     of GAPS_LAMBDA_bb and GAPS_PHI_bb is the number of boxes in the
!     bb grid.)
!
!     The input and output data are supplied as 2D arrays DATA_SRCE and
!     DATA_TARG, whose first dimensions should also be supplied. Speci-
!     fying these sizes separately from the actual dimensions of the
!     grids allows for columns and rows in the arrays to be ignored.
!     A target land/sea mask should be supplied in MASK_TARG, with the
!     value indicating wanted points specified in WANT. Points which
!     are unwanted or which lie outside the source grid are not altered
!     in DATA_TARG. DATA_SRCE can optionally be supplied with its rows
!     in reverse order (i.e. with the first row corresponding to
!     minimum LAMBDA).
!
!     The arrays COUNT_TARG, BASE_TARG, INDEX_SRCE and WEIGHT should be
!     supplied as returned by PRE_AREAVER q.v.
!
!     Programming Standard, UMDP3 vn8.3
!
!
!

USE regrid_utils, ONLY: global_to_local_gridpt, find_value,           &
     gridpt_outside_proc_domain
USE regrid_types

IMPLICIT NONE

INTEGER, INTENT(IN) ::                                                  &
 gaps_lambda_srce                                                       &
                         !   number lambda gaps in source grid
,gaps_phi_srce                                                          &
                         !   number phi gaps in source grid
,gaps_lambda_targ                                                       &
                         !   number lambda gaps in target grid
,gaps_phi_targ                                                          &
                         !   number phi gaps in target grid
,lrow_targ                                                              &
                         !   first dimension of target arrays
,count_targ(gaps_lambda_targ,gaps_phi_targ)                             &
!                              !   no. of source boxes in target box
      ,base_targ(gaps_lambda_targ,gaps_phi_targ)                              &
!                              !   first index in list for target box
      ,index_srce( * )                                                          &
                               !   list of source box indices
      ,global_src_lambda_gaps                                                 &
                               !   lambda gaps in global src grid
      ,global_src_phi_gaps                                                    &
                               !   phi gaps in global src grid
      ,grid                                                                   &
                               ! grid type (e.g. atmos, river ...)
      ,recv_size

LOGICAL, INTENT(IN) ::                                                  &
 want                                                                   &
                         !   indicator of wanted points in mask
,mask_targ(gaps_lambda_targ, gaps_phi_targ)
!   land/sea mask for target grid

REAL, INTENT(IN) ::                                                     &
 data_srce(gaps_lambda_srce,gaps_phi_srce)
                         !    data on source grid

REAL, INTENT(IN) ::                                                     &
 weight( * )
                         !   list of weights for source boxes

REAL, INTENT(OUT) ::                                                    &
 data_targ(gaps_lambda_targ, gaps_phi_targ)
                         !    data on target grid

TYPE(concern), INTENT(IN) :: recv_concern(recv_size)

! local variables

INTEGER ::                                                              &
 ip                                                                     &
                         ! pointer into lists
,i                                                                      &
                         ! loop index
,ix1(gaps_lambda_srce * gaps_phi_srce)                                    &
!                              ! working srce lambda indices
      ,iy1(gaps_lambda_srce * gaps_phi_srce)                                    &
!                              ! working srce phi indices
      ,ix2,iy2                                                                &
                               ! working targ lambda/phi indices
      ,src_phi_gap                                                            &
                               ! phi gap in global domain src grid
      ,src_lambda_gap
                               ! lambda gap in global domain src grid

INTEGER ::                                                              &
 lambda_gap0, lambda_gapf, phi_gap0, phi_gapf, xSrc, ySrc
      ! the local start and end points in the src grid

REAL ::                                                                 &
 temp_targ, data_src

LOGICAL :: found, not_within

!
!     loop over all target boxes and calculate values as required.
!
!     the weights and source box indices are recorded in continuous
!     lists. count_targ indicates how many consecutive entries in these
!     lists apply to each target box.
!
DO iy2 = 1,gaps_phi_targ
  DO ix2 = 1,gaps_lambda_targ
    IF (mask_targ(ix2,iy2) .EQV. want) THEN
      IF (count_targ(ix2,iy2) /= 0) THEN
        temp_targ = 0.0
        DO i = 1,count_targ(ix2,iy2)
          ip = base_targ(ix2,iy2) + i

          ! determine if src requested is within boundary
          ! in global src grid
          src_lambda_gap = MOD(index_srce(ip) - 1,                   &
               global_src_lambda_gaps) + 1
          src_phi_gap = (index_srce(ip) - 1) /                        &
               global_src_lambda_gaps + 1


          ! check if src point is in proc domain
          not_within = gridpt_outside_proc_domain(                 &
          src_lambda_gap, src_phi_gap, grid)

          data_src = 0

          ! if not check in your recv Concerns
          IF (not_within) THEN

            found = .FALSE.
            CALL find_value(src_lambda_gap, src_phi_gap,          &
                 recv_size, recv_concern, data_src, found)
            IF ( .NOT. found) CALL umPrint("Error, Src Not Found!", &
            src='do_areaver')

          ELSE
            xsrc = src_lambda_gap
            ysrc = src_phi_gap
            CALL global_to_local_gridpt(xsrc, ysrc, grid)
            data_src = data_srce(xsrc, ysrc)

          END IF
          ! apply weighting
          temp_targ = temp_targ + weight(ip) * data_src
        END DO
      ELSE
         ! one to one
        temp_targ = data_targ(ix2,iy2)
      END IF
      data_targ(ix2,iy2) = temp_targ
    END IF
  END DO
END DO

RETURN
END SUBROUTINE do_areaver
END MODULE do_areaver_mod
#endif
