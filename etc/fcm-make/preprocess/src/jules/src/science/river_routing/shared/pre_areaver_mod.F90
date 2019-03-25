










! *****************************COPYRIGHT********************************************
! (c) Crown copyright, Met Office. All rights reserved.
!
! This routine has been licensed to the other JULES partners for use and
! distribution under the JULES collaboration agreement, subject to the terms and
! conditions set out therein.
!
! [Met Office Ref SC0237]
! *****************************COPYRIGHT********************************************

MODULE pre_areaver_mod

IMPLICIT NONE

CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName='PRE_AREAVER_MOD'

CONTAINS

SUBROUTINE pre_areaver(gaps_lambda_srce,lambda_srce                     &
,gaps_phi_srce,phi_srce,cyclic_srce,lrow_srce,want,mask_srce            &
,gaps_lambda_targ,lambda_targ,gaps_phi_targ,phi_targ                    &
,cyclic_targ,spherical                                                  &
,maxl,count_targ,base_targ,index_srce,weight,icode,cmessage)

USE conversions_mod, ONLY: pi_over_180

USE yomhook, ONLY: lhook, dr_hook
USE parkind1, ONLY: jprb, jpim

USE errormessagelength_mod, ONLY: errormessagelength

IMPLICIT NONE

!-----------------------------------------------------------------------------
!
! Subroutine: PRE_AREAVER
!
! Description:
!   Calculate weights for area-averaging data on the source grid to
!   data on the target grid.
!
! Note:
!   This code was transferred from the UM repository at UM vn9.2 / JULES vn 4.1.
!   Future developments will supercede these subroutines, and as such they
!   should be considered deprecated. They will be retained in the codebase to
!   maintain backward compatibility with functionality prior to
!   UM vn10.0 / JULES vn 4.2, until such time as they become redundant.
!
! Method:
!   The source grid and target grid are each specified by a lambda set
!   and a phi set of coordinates delimiting the boxes. These sets are
!   supplied in 1D arrays aa_bb for coordinate aa=LAMBDA,PHI on grid
!   bb=SRCE,TARG.  The number of gaps is specified by GAPS_aa_bb,
!   which is equal to the number of lines IF (CYCLIC_bb) and aa is
!   LAMBDA, otherwise one less. (By "gap" we mean the interval
!   spanning a box in one coordinate only. The total number of boxes,
!   or grid points, is the product of the numbers of gaps in the two
!   coordinates.) Whether the axes are cyclic is not known until
!   run-time, so the dimensions of the arrays LAMBDA_bb are not known
!   at compile-time, and they are dimensioned assumed-size. There are
!   no restrictions on the base meridian of lambda, and it does not
!   have to be the same for source and target grids. The lambda
!   coordinates should be in increasing order (running from left to
!   right), the phi increasing (top to bottom). The coordinates must
!   be given in degrees for cyclic axes, because a range of 360 is
!   assumed, or IF (SPHERICAL), when trigonometric functions are
!   used. IF (SPHERICAL), the weights computed are for a spherical
!   surface, assuming that LAMBDA is longitude and PHI latitude.
!   Otherwise, LAMBDA and PHI are treated as Cartesian axes on a
!   plane.
!
!   The array MASK_SRCE is the land/sea mask for the source grid. The
!   logical value indicating points to be used should be supplied in
!   WANT. The first dimension of MASK_SRCE should be supplied in
!   LROW_SRCE. Specifying this separately allows for the possibility
!   of extra rows and columns in MASK_SRCE which are to be ignored.
!
!   The arrays COUNT_TARG and BASE_TARG should be dimensioned in the
!   calling program to the number of boxes in the target array.
!
!   The arrays INDEX_SRCE and WEIGHT are returned in the form which
!   the area-averaging routine DO_AREAVER expects. They are continuous
!   lists comprising consecutive groups of entries. There is a group
!   for each target point, for which the number of entries is spec-
!   ified by COUNT_TARG, and the groups appear in the normal order of
!   grid points. The size required for INDEX_SRCE and WEIGHT depends
!   on how many source boxes go into each target box, on average, and
!   is not known at compile-time. The maximum that could be needed is
!   (GAPS_LAMBDA_SRCE+GAPS_LAMBDA_TARG)*(GAPS_PHI_SRCE+GAPS_PHI_TARG)
!   and the size to which the arrays are actually dimensioned should
!   be supplied in MAXL. The size used is returned in MAXL. It is the
!   responsibility of the calling routine to provide enough space.
!
! Current Code Owner: Please refer to the JULES science module leaders
!   This file belongs in module: Hydrology
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------


!
INTEGER ::                                                              &
 gaps_lambda_srce                                                       &
                         !IN number of lambda gaps in source grid
,gaps_phi_srce                                                          &
                         !IN number of phi gaps in source grid
,gaps_lambda_targ                                                       &
                         !IN number of lambda gaps in target grid
,gaps_phi_targ                                                          &
                         !IN number of phi gaps in target grid
,lrow_srce                                                              &
                         !IN first dimension of MASK_SRCE
,maxl                                                                   &
                         !INOUT maximum entries in output lists
,count_targ(gaps_lambda_targ,gaps_phi_targ)                             &
!                              !OUT no. of source boxes in target box
      ,base_targ(gaps_lambda_targ,gaps_phi_targ)                              &
!                              !OUT first index in list for target box
      ,index_srce(maxl)                                                       &
                               !OUT list of source box indices
      ,icode                   !OUT return code
LOGICAL ::                                                              &
 cyclic_srce                                                            &
                         !IN source grid is cyclic
,cyclic_targ                                                            &
                         !IN target grid is cyclic
,want                                                                   &
                         !IN indicator of wanted points in mask
,mask_srce(lrow_srce,*)                                                 &
                         !IN land/sea mask for source grid
,spherical               !IN calculate weights for a sphere
REAL ::                                                                 &
 lambda_srce( * )                                                         &
                         !IN source lambda line coordinates
,phi_srce( * )                                                            &
                         !IN source phi line coordinates
,lambda_targ( * )                                                         &
                         !IN target lambda line coordinates
,phi_targ( * )                                                            &
                         !IN target phi line coordinates
,weight(maxl)            !OUT list of weights for source boxes
CHARACTER(LEN=errormessagelength) :: cmessage  !OUT error message
!

INTEGER ::                                                              &
 lines_lambda_srce                                                      &
                         ! number of source lambda lines
,lines_phi_srce                                                         &
                         ! number of source phi lines
,lines_lambda_targ                                                      &
                         ! number of target lambda lines
,lines_phi_targ                                                         &
                         ! number of target phi lines
,count_lambda(gaps_lambda_targ)                                         &
!                              ! number of source lambda gaps per target
      ,count_phi(gaps_phi_targ)                                               &
!                              ! number of source phi gaps per target
      ,index_lambda(gaps_lambda_srce + gaps_lambda_targ)                        &
!                              ! source lambda gap indices
      ,index_phi(gaps_phi_srce + gaps_phi_targ)                                 &
!                              ! source phi gap indices
      ,ix1,iy1,ix2,iy2                                                        &
                               ! working SRCE/TARG LAMBDA/PHI indices
      ,ix1n,ix1w                                                              &
                               ! working indices
      ,ixl(gaps_lambda_targ+1)                                                &
                               ! source line on the left of target line
      ,ix2n                                                                   &
                               ! target gap to the right of IX2
      ,iyt(gaps_phi_targ+1)                                                   &
                               ! source line above target line
      ,ixp,iyp,ip                                                             &
                               ! pointers into lists
      ,ix,iy,i                 ! loop indices
REAL ::                                                                 &
 baslam                                                                 &
                         ! minimum lambda for TEMP coordinates
,btarg                                                                  &
                         ! width of target gap
,blo,bhi                                                                &
                         ! limits of gap
,temp_srce(gaps_lambda_srce+1)                                          &
!                              ! adjusted version of LAMBDA_SRCE
      ,temp_targ(gaps_lambda_targ+1)                                          &
!                              ! adjusted version of LAMBDA_TARG
      ,frac_lambda(gaps_lambda_srce + gaps_lambda_targ)                         &
!                              ! fractions of target lambda gaps
      ,frac_phi(gaps_phi_srce + gaps_phi_targ)                                  &
!                              ! fractions of target phi gaps
      ,SUM                     ! sum of weights

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='PRE_AREAVER'

!
!   1  Set up the lambda coordinates to make them easier to handle.
!
!   1.1  Produce in TEMP_SRCE a monotonically increasing set of angles
!   equivalent to LAMBDA_SRCE i.e. equal under modulo 360.
!
IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,          &
                        zhook_handle)
IF (cyclic_srce) THEN
  lines_lambda_srce = gaps_lambda_srce
ELSE
  lines_lambda_srce = gaps_lambda_srce + 1
END IF
baslam = lambda_srce(1)
DO ix1 = 1,lines_lambda_srce
  IF (lambda_srce(ix1) <  baslam) THEN
    temp_srce(ix1) = lambda_srce(ix1) + 360.0
  ELSE
    temp_srce(ix1) = lambda_srce(ix1)
  END IF
END DO
!
!   1.2  Produce in TEMP_TARG a set of angles equivalent to
!   LAMBDA_TARG i.e. equal under modulo 360, but all in the range
!   BASLAM to BASLAM+360, where BASLAM=min(TEMP_LAMBDA).
!
IF (cyclic_targ) THEN
  lines_lambda_targ = gaps_lambda_targ
ELSE
  lines_lambda_targ = gaps_lambda_targ + 1
END IF
DO ix2 = 1,lines_lambda_targ
  temp_targ(ix2) = MOD(lambda_targ(ix2) - baslam,360.0)
  IF (temp_targ(ix2) <  0.0) temp_targ(ix2) = temp_targ(ix2) + 360.0
  temp_targ(ix2) = temp_targ(ix2) + baslam
END DO
!
!   2  For each target lambda line, find the index of the next source
!   lambda line to the left.
!
DO ix2 = 1,lines_lambda_targ
  DO ix1 = 1,lines_lambda_srce
    IF (temp_targ(ix2) >= temp_srce(ix1)) ixl(ix2) = ix1
  END DO
END DO
!
!   3  Find which source lambda gaps cover each target gap and the
!   fractions they contribute.
!
!     At this point IXL(target_line) gives the index of the next source
!     lambda line to the left of the target lambda line, wrapping round
!     if the source grid is cyclic. This is also the index of the source
!     gap in which the target line falls. Similarly, the index of the
!     target line is also that of the target gap of which it is the
!     left-hand limit. Therefore also IXL(target_gap+1, wrapping round
!     if reqd.), is the index of the source gap which contains the
!     right-hand limit of the target gap. For each target gap, we loop
!     over all source gaps and find the fraction covered by each. Record
!     the fraction and the source index in cumulative lists. If the
!     source grid is not cyclic, parts of the target gap lying outside
!     the source grid are neglected.
!
ixp = 0
DO ix2 = 1,gaps_lambda_targ
  ix = 0
  ix2n = MOD(ix2,lines_lambda_targ) + 1
  btarg = temp_targ(ix2n) - temp_targ(ix2)
  IF (btarg <  0.0) THEN
    btarg = btarg + 360.0
    ix1n = ixl(ix2n) + lines_lambda_srce
  ELSE
    ix1n = ixl(ix2n)
  END IF
  DO ix1w = ixl(ix2),ix1n
    ix1 = MOD(ix1w-1,lines_lambda_srce) + 1
    IF (cyclic_srce .OR. ix1 /= lines_lambda_srce) THEN
      IF (ix1w == ixl(ix2)) THEN
        blo = temp_targ(ix2)
      ELSE
        blo = temp_srce(ix1)
      END IF
      IF (ix1w == ix1n) THEN
        bhi = temp_targ(ix2n)
      ELSE
        bhi = temp_srce(MOD(ix1,lines_lambda_srce) + 1)
      END IF
      IF (bhi <  blo) bhi = bhi + 360.0
      IF (ABS(bhi - blo) >  1.0e-7 * ABS(btarg)) THEN
        ix = ix + 1
        index_lambda(ixp + ix) = ix1
        frac_lambda(ixp + ix)=(bhi - blo) / btarg
      END IF
    END IF
  END DO
  count_lambda(ix2) = ix
  ixp = ixp + count_lambda(ix2)
END DO
!
!   4  For each target phi line, find the index of the next source phi
!   line above. Comments as for section 2, without wrap-round. Note
!   that this assumes that the atmosphere gridbox ordering is from
!   south to north; a north to south atmosphere would require LE
!   instead of GE in the following IF test.
!
lines_phi_srce = gaps_phi_srce + 1
lines_phi_targ = gaps_phi_targ + 1
DO iy2 = 1,lines_phi_targ
  iyt(iy2) = 0
  DO iy1 = 1,lines_phi_srce
    IF (phi_targ(iy2) >= phi_srce(iy1)) iyt(iy2) = iy1
  END DO
END DO
!
!   5  Find which source phi gaps cover each target gap and the
!   fractions they contribute. Comments as for section 3, without
!   wrap-round.
!
iyp = 0
DO iy2 = 1,gaps_phi_targ
  iy = 0
  IF (spherical) THEN
    !     Contain angle between +-90. There is no real area outside
    !     these limits on a sphere.
    btarg = SIN(MAX(MIN(phi_targ(iy2+1),90.0),-90.0) * pi_over_180)     &
    -SIN(MAX(MIN(phi_targ(iy2),90.0),-90.0) * pi_over_180)
  ELSE
    btarg = phi_targ(iy2+1) - phi_targ(iy2)
  END IF
  DO iy1 = iyt(iy2),iyt(iy2+1)
    IF ( .NOT. (iy1 == 0 .OR. iy1 == lines_phi_srce)) THEN
      IF (iy1 == iyt(iy2)) THEN
        blo = phi_targ(iy2)
      ELSE
        blo = phi_srce(iy1)
      END IF
      IF (iy1 == iyt(iy2+1)) THEN
        bhi = phi_targ(iy2+1)
      ELSE
        bhi = phi_srce(iy1+1)
      END IF
      IF (spherical) THEN
        blo = MAX(MIN(blo,90.0),-90.0)
        bhi = MAX(MIN(bhi,90.0),-90.0)
      END IF
      IF (ABS(bhi - blo) >  1.0e-7 * ABS(btarg)) THEN
        iy = iy + 1
        index_phi(iyp + iy) = iy1
        !             Both numerator and denominator in the following are -ve.
        IF (spherical) THEN
          frac_phi(iyp + iy)                                              &
          =(SIN(bhi * pi_over_180) - SIN(blo * pi_over_180)) / btarg
        ELSE
          frac_phi(iyp + iy)=(bhi - blo) / btarg
        END IF
      END IF
    END IF
  END DO
  count_phi(iy2) = iy
  iyp = iyp + count_phi(iy2)
END DO
!
!   6  For each target box, loop over contributing source boxes and
!   calculate the weights for each one, ignoring land boxes.
!
!     After the first pass for each target box, go back and normalise
!     the weights to compensate for land source boxes and any outside
!     the source grid. Record the source box index and the weight in
!     cumulative lists.
!
ip = 0
iyp = 0
DO iy2 = 1,gaps_phi_targ
  ixp = 0
  DO ix2 = 1,gaps_lambda_targ
    i = 0
    SUM = 0
    DO iy = iyp+1,iyp + count_phi(iy2)
      DO ix = ixp+1,ixp + count_lambda(ix2)
        IF (mask_srce(index_lambda(ix),index_phi(iy))                   &
        .EQV. want) THEN
          i = i + 1
          index_srce(ip + i) = index_lambda(ix)                             &
          +(index_phi(iy) - 1) * gaps_lambda_srce
          weight(ip + i) = frac_lambda(ix) * frac_phi(iy)
          SUM = SUM + weight(ip + i)
        END IF
      END DO
    END DO
    count_targ(ix2,iy2) = i
    base_targ(ix2,iy2) = ip
    DO i = 1,count_targ(ix2,iy2)
      weight(ip + i) = weight(ip + i) / SUM
    END DO
    ip = ip + count_targ(ix2,iy2)
    ixp = ixp + count_lambda(ix2)
  END DO
  iyp = iyp + count_phi(iy2)
END DO
maxl = ip
!
icode = 0
cmessage=' '
IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,         &
                        zhook_handle)
RETURN
END SUBROUTINE pre_areaver
!
!------------------------------------------------------------
!
END MODULE pre_areaver_mod
