GFORTRAN module version '10' created from /users/global/waltho/MODELS/jules-vn5.2/etc/fcm-make/preprocess/src/jules/src/science/soil/calc_baseflow_jules_mod.F90
MD5:bc76e8209ec8effb0a0c2d90880a5e9c -- If you edit this, you'll get what you deserve.

(() () () () () () () () () () () () () () () () () () () () () () ()
() () () ())

()

()

()

()

()

(2 'calc_baseflow_jules' 'calc_baseflow_jules_mod' '' 1 ((PROCEDURE
UNKNOWN-INTENT MODULE-PROC DECL UNKNOWN 0 0 SUBROUTINE) (UNKNOWN 0 0 0 0
UNKNOWN ()) 3 0 (4 5 6 7 8 9 10 11 12 13 14 15 16 17) () 0 () () () 0 0)
18 'calc_baseflow_jules_mod' 'calc_baseflow_jules_mod' '' 1 ((MODULE
UNKNOWN-INTENT UNKNOWN-PROC UNKNOWN UNKNOWN 0 0) (UNKNOWN 0 0 0 0
UNKNOWN ()) 0 0 () () 0 () () () 0 0)
4 'soil_pts' '' '' 3 ((VARIABLE IN UNKNOWN-PROC UNKNOWN UNKNOWN 0 0
DUMMY) (INTEGER 4 0 0 0 INTEGER ()) 0 0 () () 0 () () () 0 0)
5 'soil_index' '' '' 3 ((VARIABLE IN UNKNOWN-PROC UNKNOWN UNKNOWN 0 0
DIMENSION DUMMY) (INTEGER 4 0 0 0 INTEGER ()) 0 0 () (1 0 EXPLICIT (
CONSTANT (INTEGER 4 0 0 0 INTEGER ()) 0 '1') (VARIABLE (INTEGER 4 0 0 0
INTEGER ()) 0 6 ())) 0 () () () 0 0)
6 'npnts' '' '' 3 ((VARIABLE IN UNKNOWN-PROC UNKNOWN UNKNOWN 0 0 DUMMY)
(INTEGER 4 0 0 0 INTEGER ()) 0 0 () () 0 () () () 0 0)
7 'nshyd' '' '' 3 ((VARIABLE IN UNKNOWN-PROC UNKNOWN UNKNOWN 0 0 DUMMY)
(INTEGER 4 0 0 0 INTEGER ()) 0 0 () () 0 () () () 0 0)
8 'zdepth' '' '' 3 ((VARIABLE UNKNOWN-INTENT UNKNOWN-PROC UNKNOWN
UNKNOWN 0 0 DIMENSION DUMMY) (REAL 4 0 0 0 REAL ()) 0 0 () (1 0 EXPLICIT
(CONSTANT (INTEGER 4 0 0 0 INTEGER ()) 0 '0') (VARIABLE (INTEGER 4 0 0 0
INTEGER ()) 0 7 ())) 0 () () () 0 0)
9 'ksz' '' '' 3 ((VARIABLE IN UNKNOWN-PROC UNKNOWN UNKNOWN 0 0 DIMENSION
DUMMY) (REAL 4 0 0 0 REAL ()) 0 0 () (2 0 EXPLICIT (CONSTANT (INTEGER 4
0 0 0 INTEGER ()) 0 '1') (VARIABLE (INTEGER 4 0 0 0 INTEGER ()) 0 6 ())
(CONSTANT (INTEGER 4 0 0 0 INTEGER ()) 0 '0') (VARIABLE (INTEGER 4 0 0 0
INTEGER ()) 0 7 ())) 0 () () () 0 0)
10 'b' '' '' 3 ((VARIABLE IN UNKNOWN-PROC UNKNOWN UNKNOWN 0 0 DIMENSION
DUMMY) (REAL 4 0 0 0 REAL ()) 0 0 () (2 0 EXPLICIT (CONSTANT (INTEGER 4
0 0 0 INTEGER ()) 0 '1') (VARIABLE (INTEGER 4 0 0 0 INTEGER ()) 0 6 ())
(CONSTANT (INTEGER 4 0 0 0 INTEGER ()) 0 '1') (VARIABLE (INTEGER 4 0 0 0
INTEGER ()) 0 7 ())) 0 () () () 0 0)
11 'fexp' '' '' 3 ((VARIABLE IN UNKNOWN-PROC UNKNOWN UNKNOWN 0 0
DIMENSION DUMMY) (REAL 4 0 0 0 REAL ()) 0 0 () (1 0 EXPLICIT (CONSTANT (
INTEGER 4 0 0 0 INTEGER ()) 0 '1') (VARIABLE (INTEGER 4 0 0 0 INTEGER ())
0 6 ())) 0 () () () 0 0)
12 'ti_mean' '' '' 3 ((VARIABLE IN UNKNOWN-PROC UNKNOWN UNKNOWN 0 0
DIMENSION DUMMY) (REAL 4 0 0 0 REAL ()) 0 0 () (1 0 EXPLICIT (CONSTANT (
INTEGER 4 0 0 0 INTEGER ()) 0 '1') (VARIABLE (INTEGER 4 0 0 0 INTEGER ())
0 6 ())) 0 () () () 0 0)
13 'zw' '' '' 3 ((VARIABLE IN UNKNOWN-PROC UNKNOWN UNKNOWN 0 0 DIMENSION
DUMMY) (REAL 4 0 0 0 REAL ()) 0 0 () (1 0 EXPLICIT (CONSTANT (INTEGER 4
0 0 0 INTEGER ()) 0 '1') (VARIABLE (INTEGER 4 0 0 0 INTEGER ()) 0 6 ()))
0 () () () 0 0)
14 'sthf' '' '' 3 ((VARIABLE INOUT UNKNOWN-PROC UNKNOWN UNKNOWN 0 0
DIMENSION DUMMY) (REAL 4 0 0 0 REAL ()) 0 0 () (2 0 EXPLICIT (CONSTANT (
INTEGER 4 0 0 0 INTEGER ()) 0 '1') (VARIABLE (INTEGER 4 0 0 0 INTEGER ())
0 6 ()) (CONSTANT (INTEGER 4 0 0 0 INTEGER ()) 0 '1') (VARIABLE (
INTEGER 4 0 0 0 INTEGER ()) 0 7 ())) 0 () () () 0 0)
15 'top_crit' '' '' 3 ((VARIABLE OUT UNKNOWN-PROC UNKNOWN UNKNOWN 0 0
DIMENSION DUMMY) (REAL 4 0 0 0 REAL ()) 0 0 () (1 0 EXPLICIT (CONSTANT (
INTEGER 4 0 0 0 INTEGER ()) 0 '1') (VARIABLE (INTEGER 4 0 0 0 INTEGER ())
0 6 ())) 0 () () () 0 0)
16 'qbase' '' '' 3 ((VARIABLE OUT UNKNOWN-PROC UNKNOWN UNKNOWN 0 0
DIMENSION DUMMY) (REAL 4 0 0 0 REAL ()) 0 0 () (1 0 EXPLICIT (CONSTANT (
INTEGER 4 0 0 0 INTEGER ()) 0 '1') (VARIABLE (INTEGER 4 0 0 0 INTEGER ())
0 6 ())) 0 () () () 0 0)
17 'qbase_l' '' '' 3 ((VARIABLE OUT UNKNOWN-PROC UNKNOWN UNKNOWN 0 0
DIMENSION DUMMY) (REAL 4 0 0 0 REAL ()) 0 0 () (2 0 EXPLICIT (CONSTANT (
INTEGER 4 0 0 0 INTEGER ()) 0 '1') (VARIABLE (INTEGER 4 0 0 0 INTEGER ())
0 6 ()) (CONSTANT (INTEGER 4 0 0 0 INTEGER ()) 0 '1') (OP (INTEGER 4 0 0
0 INTEGER ()) 0 PLUS (VARIABLE (INTEGER 4 0 0 0 INTEGER ()) 0 7 ()) (
CONSTANT (INTEGER 4 0 0 0 INTEGER ()) 0 '1'))) 0 () () () 0 0)
)

('calc_baseflow_jules' 0 2 'calc_baseflow_jules_mod' 0 18)
