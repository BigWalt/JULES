GFORTRAN module version '10' created from /users/global/waltho/MODELS/jules-vn5.2/etc/fcm-make/preprocess/src/jules/src/science/surface/sf_orog_jls.F90
MD5:821273a18455c1dedbd959f66bdbd60c -- If you edit this, you'll get what you deserve.

(() () () () () () () () () () () () () () () () () () () () () () ()
() () () ())

()

()

()

()

()

(2 'sf_orog' 'sf_orog_mod' '' 1 ((PROCEDURE UNKNOWN-INTENT MODULE-PROC
DECL UNKNOWN 0 0 SUBROUTINE) (UNKNOWN 0 0 0 0 UNKNOWN ()) 3 0 (4 5 6 7 8
9 10 11 12 13 14 15 16) () 0 () () () 0 0)
17 'sf_orog_mod' 'sf_orog_mod' '' 1 ((MODULE UNKNOWN-INTENT UNKNOWN-PROC
UNKNOWN UNKNOWN 0 0) (UNKNOWN 0 0 0 0 UNKNOWN ()) 0 0 () () 0 () () () 0
0)
4 'land_pts' '' '' 3 ((VARIABLE IN UNKNOWN-PROC UNKNOWN UNKNOWN 0 0
DUMMY) (INTEGER 4 0 0 0 INTEGER ()) 0 0 () () 0 () () () 0 0)
5 'surft_pts' '' '' 3 ((VARIABLE IN UNKNOWN-PROC UNKNOWN UNKNOWN 0 0
DUMMY) (INTEGER 4 0 0 0 INTEGER ()) 0 0 () () 0 () () () 0 0)
6 'land_index' '' '' 3 ((VARIABLE IN UNKNOWN-PROC UNKNOWN UNKNOWN 0 0
DIMENSION DUMMY) (INTEGER 4 0 0 0 INTEGER ()) 0 0 () (1 0 EXPLICIT (
CONSTANT (INTEGER 4 0 0 0 INTEGER ()) 0 '1') (VARIABLE (INTEGER 4 0 0 0
INTEGER ()) 0 4 ())) 0 () () () 0 0)
7 'surft_index' '' '' 3 ((VARIABLE IN UNKNOWN-PROC UNKNOWN UNKNOWN 0 0
DIMENSION DUMMY) (INTEGER 4 0 0 0 INTEGER ()) 0 0 () (1 0 EXPLICIT (
CONSTANT (INTEGER 4 0 0 0 INTEGER ()) 0 '1') (VARIABLE (INTEGER 4 0 0 0
INTEGER ()) 0 4 ())) 0 () () () 0 0)
8 'fd_stab_dep' '' '' 3 ((VARIABLE IN UNKNOWN-PROC UNKNOWN UNKNOWN 0 0
DUMMY) (INTEGER 4 0 0 0 INTEGER ()) 0 0 () () 0 () () () 0 0)
9 'orog_drag_param' '' '' 3 ((VARIABLE IN UNKNOWN-PROC UNKNOWN UNKNOWN 0
0 DUMMY) (REAL 4 0 0 0 REAL ()) 0 0 () () 0 () () () 0 0)
10 'ho2r2_orog' '' '' 3 ((VARIABLE IN UNKNOWN-PROC UNKNOWN UNKNOWN 0 0
DIMENSION DUMMY) (REAL 4 0 0 0 REAL ()) 0 0 () (1 0 EXPLICIT (CONSTANT (
INTEGER 4 0 0 0 INTEGER ()) 0 '1') (VARIABLE (INTEGER 4 0 0 0 INTEGER ())
0 4 ())) 0 () () () 0 0)
11 'rib' '' '' 3 ((VARIABLE IN UNKNOWN-PROC UNKNOWN UNKNOWN 0 0
DIMENSION DUMMY) (REAL 4 0 0 0 REAL ()) 0 0 () (1 0 EXPLICIT (CONSTANT (
INTEGER 4 0 0 0 INTEGER ()) 0 '1') (VARIABLE (INTEGER 4 0 0 0 INTEGER ())
0 4 ())) 0 () () () 0 0)
12 'sil_orog' '' '' 3 ((VARIABLE IN UNKNOWN-PROC UNKNOWN UNKNOWN 0 0
DIMENSION DUMMY) (REAL 4 0 0 0 REAL ()) 0 0 () (1 0 EXPLICIT (CONSTANT (
INTEGER 4 0 0 0 INTEGER ()) 0 '1') (VARIABLE (INTEGER 4 0 0 0 INTEGER ())
0 4 ())) 0 () () () 0 0)
13 'z0m' '' '' 3 ((VARIABLE IN UNKNOWN-PROC UNKNOWN UNKNOWN 0 0
DIMENSION DUMMY) (REAL 4 0 0 0 REAL ()) 0 0 () (1 0 EXPLICIT (CONSTANT (
INTEGER 4 0 0 0 INTEGER ()) 0 '1') (VARIABLE (INTEGER 4 0 0 0 INTEGER ())
0 4 ())) 0 () () () 0 0)
14 'z1' '' '' 3 ((VARIABLE IN UNKNOWN-PROC UNKNOWN UNKNOWN 0 0 DIMENSION
DUMMY) (REAL 4 0 0 0 REAL ()) 0 0 () (2 0 EXPLICIT (VARIABLE (INTEGER 4
0 0 0 INTEGER ()) 0 18 ((COMPONENT 19 20))) (VARIABLE (INTEGER 4 0 0 0
INTEGER ()) 0 18 ((COMPONENT 19 21))) (VARIABLE (INTEGER 4 0 0 0 INTEGER
()) 0 18 ((COMPONENT 19 22))) (VARIABLE (INTEGER 4 0 0 0 INTEGER ()) 0
18 ((COMPONENT 19 23)))) 0 () () () 0 0)
15 'wind_profile_factor' '' '' 3 ((VARIABLE OUT UNKNOWN-PROC UNKNOWN
UNKNOWN 0 0 DIMENSION DUMMY) (REAL 4 0 0 0 REAL ()) 0 0 () (1 0 EXPLICIT
(CONSTANT (INTEGER 4 0 0 0 INTEGER ()) 0 '1') (VARIABLE (INTEGER 4 0 0 0
INTEGER ()) 0 4 ())) 0 () () () 0 0)
16 'z0m_eff' '' '' 3 ((VARIABLE OUT UNKNOWN-PROC UNKNOWN UNKNOWN 0 0
DIMENSION DUMMY) (REAL 4 0 0 0 REAL ()) 0 0 () (1 0 EXPLICIT (CONSTANT (
INTEGER 4 0 0 0 INTEGER ()) 0 '1') (VARIABLE (INTEGER 4 0 0 0 INTEGER ())
0 4 ())) 0 () () () 0 0)
18 'tdims' 'atm_fields_bounds_mod' '' 3 ((VARIABLE UNKNOWN-INTENT
UNKNOWN-PROC UNKNOWN EXPLICIT-SAVE 0 0) (DERIVED 19 0 0 0 DERIVED ()) 0
0 () () 0 () () () 0 0)
19 'Array_dims' 'atm_fields_bounds_mod' '' 3 ((DERIVED UNKNOWN-INTENT
UNKNOWN-PROC UNKNOWN UNKNOWN 0 0) (UNKNOWN 0 0 0 0 UNKNOWN ()) 0 0 () ()
0 ((20 'i_start' (INTEGER 4 0 0 0 INTEGER ()) () (UNKNOWN-FL
UNKNOWN-INTENT UNKNOWN-PROC UNKNOWN UNKNOWN 0 0) UNKNOWN-ACCESS (
CONSTANT (INTEGER 4 0 0 0 INTEGER ()) 0 '-2147483647')) (21 'i_end' (
INTEGER 4 0 0 0 INTEGER ()) () (UNKNOWN-FL UNKNOWN-INTENT UNKNOWN-PROC
UNKNOWN UNKNOWN 0 0) UNKNOWN-ACCESS (CONSTANT (INTEGER 4 0 0 0 INTEGER ())
0 '2147483647')) (22 'j_start' (INTEGER 4 0 0 0 INTEGER ()) () (
UNKNOWN-FL UNKNOWN-INTENT UNKNOWN-PROC UNKNOWN UNKNOWN 0 0)
UNKNOWN-ACCESS (CONSTANT (INTEGER 4 0 0 0 INTEGER ()) 0 '-2147483647'))
(23 'j_end' (INTEGER 4 0 0 0 INTEGER ()) () (UNKNOWN-FL UNKNOWN-INTENT
UNKNOWN-PROC UNKNOWN UNKNOWN 0 0) UNKNOWN-ACCESS (CONSTANT (INTEGER 4 0
0 0 INTEGER ()) 0 '2147483647')) (24 'k_start' (INTEGER 4 0 0 0 INTEGER
()) () (UNKNOWN-FL UNKNOWN-INTENT UNKNOWN-PROC UNKNOWN UNKNOWN 0 0)
UNKNOWN-ACCESS (CONSTANT (INTEGER 4 0 0 0 INTEGER ()) 0 '-2147483647'))
(25 'k_end' (INTEGER 4 0 0 0 INTEGER ()) () (UNKNOWN-FL UNKNOWN-INTENT
UNKNOWN-PROC UNKNOWN UNKNOWN 0 0) UNKNOWN-ACCESS (CONSTANT (INTEGER 4 0
0 0 INTEGER ()) 0 '2147483647'))) PUBLIC (() () () ()) () 0 0 52022423)
)

('sf_orog' 0 2 'sf_orog_mod' 0 17)
