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

MODULE sind_mod

IMPLICIT NONE

CONTAINS

REAL FUNCTION sind(degree)
!
!     obtain sine for degree
!
!
!Code Owner: Please refer to the UM file CodeOwners.txt
!This file belongs in section: River Routing
IMPLICIT NONE
REAL :: degree, pi
DATA pi / 3.141593 /
!
sind = SIN (pi * degree / 180.0)
!
END FUNCTION sind
END MODULE sind_mod
