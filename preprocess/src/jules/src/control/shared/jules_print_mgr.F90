










! *****************************COPYRIGHT**************************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT**************************************

MODULE jules_print_mgr

USE logging_mod, ONLY: log_info

IMPLICIT NONE

INTEGER, PARAMETER, PRIVATE   :: max_line_len = 1024
CHARACTER(LEN=max_line_len)  :: jules_message
!$OMP THREADPRIVATE(jules_message)

INTEGER, PARAMETER :: PrMin   = 1
INTEGER, PARAMETER :: PrNorm  = 2
INTEGER, PARAMETER :: PrOper  = 3
INTEGER, PARAMETER :: PrDiag  = 4
INTEGER, PARAMETER :: PrintStatus = PrDiag

CONTAINS

SUBROUTINE jules_print(src,line,level)
  ! Either send output to the umPrintMgr subsystem for coupled jobs
  ! or send it to stdout otherwise.

IMPLICIT NONE
CHARACTER(LEN=*), INTENT(IN) :: line
CHARACTER(LEN=*), INTENT(IN) :: src
INTEGER, OPTIONAL            :: level
CALL log_info(TRIM(src),TRIM(line))

END SUBROUTINE jules_print

END MODULE jules_print_mgr
