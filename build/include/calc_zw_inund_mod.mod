GFORTRAN module version '10' created from /users/global/waltho/MODELS/jules-vn5.2waltho/preprocess/src/jules/src/science/soil/calc_zw_inund_jls_mod.F90
MD5:7a4007766ec9dfc74921ecdeeec27aa8 -- If you edit this, you'll get what you deserve.

(() () () () () () () () () () () () () () () () () () () () () () ()
() () () ())

()

()

()

()

()

(2 'calc_zw_inund' 'calc_zw_inund_mod' '' 1 ((PROCEDURE UNKNOWN-INTENT
MODULE-PROC DECL UNKNOWN 0 0 SUBROUTINE) (UNKNOWN 0 0 0 0 UNKNOWN ()) 3
0 (4 5 6 7 8 9 10 11 12 13 14 15 16 17) () 0 () () () 0 0)
18 'calc_zw_inund_mod' 'calc_zw_inund_mod' '' 1 ((MODULE UNKNOWN-INTENT
UNKNOWN-PROC UNKNOWN UNKNOWN 0 0) (UNKNOWN 0 0 0 0 UNKNOWN ()) 0 0 () ()
0 () () () 0 0)
4 'npnts' '' '' 3 ((VARIABLE IN UNKNOWN-PROC UNKNOWN UNKNOWN 0 0 DUMMY)
(INTEGER 4 0 0 0 INTEGER ()) 0 0 () () 0 () () () 0 0)
5 'nshyd' '' '' 3 ((VARIABLE IN UNKNOWN-PROC UNKNOWN UNKNOWN 0 0 DUMMY)
(INTEGER 4 0 0 0 INTEGER ()) 0 0 () () 0 () () () 0 0)
6 'soil_pts' '' '' 3 ((VARIABLE IN UNKNOWN-PROC UNKNOWN UNKNOWN 0 0
DUMMY) (INTEGER 4 0 0 0 INTEGER ()) 0 0 () () 0 () () () 0 0)
7 'soil_index' '' '' 3 ((VARIABLE IN UNKNOWN-PROC UNKNOWN UNKNOWN 0 0
DIMENSION DUMMY) (INTEGER 4 0 0 0 INTEGER ()) 0 0 () (1 0 EXPLICIT (
CONSTANT (INTEGER 4 0 0 0 INTEGER ()) 0 '1') (VARIABLE (INTEGER 4 0 0 0
INTEGER ()) 0 4 ())) 0 () () () 0 0)
8 'zdepth' '' '' 3 ((VARIABLE IN UNKNOWN-PROC UNKNOWN UNKNOWN 0 0
DIMENSION DUMMY) (REAL 4 0 0 0 REAL ()) 0 0 () (1 0 EXPLICIT (CONSTANT (
INTEGER 4 0 0 0 INTEGER ()) 0 '0') (VARIABLE (INTEGER 4 0 0 0 INTEGER ())
0 5 ())) 0 () () () 0 0)
9 'bexp' '' '' 3 ((VARIABLE IN UNKNOWN-PROC UNKNOWN UNKNOWN 0 0
DIMENSION DUMMY) (REAL 4 0 0 0 REAL ()) 0 0 () (1 0 EXPLICIT (CONSTANT (
INTEGER 4 0 0 0 INTEGER ()) 0 '1') (VARIABLE (INTEGER 4 0 0 0 INTEGER ())
0 4 ())) 0 () () () 0 0)
10 'sathh' '' '' 3 ((VARIABLE IN UNKNOWN-PROC UNKNOWN UNKNOWN 0 0
DIMENSION DUMMY) (REAL 4 0 0 0 REAL ()) 0 0 () (1 0 EXPLICIT (CONSTANT (
INTEGER 4 0 0 0 INTEGER ()) 0 '1') (VARIABLE (INTEGER 4 0 0 0 INTEGER ())
0 4 ())) 0 () () () 0 0)
11 'smclsat' '' '' 3 ((VARIABLE IN UNKNOWN-PROC UNKNOWN UNKNOWN 0 0
DIMENSION DUMMY) (REAL 4 0 0 0 REAL ()) 0 0 () (2 0 EXPLICIT (CONSTANT (
INTEGER 4 0 0 0 INTEGER ()) 0 '1') (VARIABLE (INTEGER 4 0 0 0 INTEGER ())
0 4 ()) (CONSTANT (INTEGER 4 0 0 0 INTEGER ()) 0 '1') (VARIABLE (
INTEGER 4 0 0 0 INTEGER ()) 0 5 ())) 0 () () () 0 0)
12 'smcl' '' '' 3 ((VARIABLE IN UNKNOWN-PROC UNKNOWN UNKNOWN 0 0
DIMENSION DUMMY) (REAL 4 0 0 0 REAL ()) 0 0 () (2 0 EXPLICIT (CONSTANT (
INTEGER 4 0 0 0 INTEGER ()) 0 '1') (VARIABLE (INTEGER 4 0 0 0 INTEGER ())
0 4 ()) (CONSTANT (INTEGER 4 0 0 0 INTEGER ()) 0 '1') (VARIABLE (
INTEGER 4 0 0 0 INTEGER ()) 0 5 ())) 0 () () () 0 0)
13 'sthu' '' '' 3 ((VARIABLE IN UNKNOWN-PROC UNKNOWN UNKNOWN 0 0
DIMENSION DUMMY) (REAL 4 0 0 0 REAL ()) 0 0 () (2 0 EXPLICIT (CONSTANT (
INTEGER 4 0 0 0 INTEGER ()) 0 '1') (VARIABLE (INTEGER 4 0 0 0 INTEGER ())
0 4 ()) (CONSTANT (INTEGER 4 0 0 0 INTEGER ()) 0 '1') (VARIABLE (
INTEGER 4 0 0 0 INTEGER ()) 0 5 ())) 0 () () () 0 0)
14 'sthzw' '' '' 3 ((VARIABLE IN UNKNOWN-PROC UNKNOWN UNKNOWN 0 0
DIMENSION DUMMY) (REAL 4 0 0 0 REAL ()) 0 0 () (1 0 EXPLICIT (CONSTANT (
INTEGER 4 0 0 0 INTEGER ()) 0 '1') (VARIABLE (INTEGER 4 0 0 0 INTEGER ())
0 4 ())) 0 () () () 0 0)
15 'zw' '' '' 3 ((VARIABLE INOUT UNKNOWN-PROC UNKNOWN UNKNOWN 0 0
DIMENSION DUMMY) (REAL 4 0 0 0 REAL ()) 0 0 () (1 0 EXPLICIT (CONSTANT (
INTEGER 4 0 0 0 INTEGER ()) 0 '1') (VARIABLE (INTEGER 4 0 0 0 INTEGER ())
0 4 ())) 0 () () () 0 0)
16 'zw_inund' '' '' 3 ((VARIABLE OUT UNKNOWN-PROC UNKNOWN UNKNOWN 0 0
DIMENSION DUMMY) (REAL 4 0 0 0 REAL ()) 0 0 () (1 0 EXPLICIT (CONSTANT (
INTEGER 4 0 0 0 INTEGER ()) 0 '1') (VARIABLE (INTEGER 4 0 0 0 INTEGER ())
0 4 ())) 0 () () () 0 0)
17 'wutot' '' '' 3 ((VARIABLE OUT UNKNOWN-PROC UNKNOWN UNKNOWN 0 0
DIMENSION DUMMY) (REAL 4 0 0 0 REAL ()) 0 0 () (1 0 EXPLICIT (CONSTANT (
INTEGER 4 0 0 0 INTEGER ()) 0 '1') (VARIABLE (INTEGER 4 0 0 0 INTEGER ())
0 4 ())) 0 () () () 0 0)
)

('calc_zw_inund' 0 2 'calc_zw_inund_mod' 0 18)