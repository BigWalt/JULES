GFORTRAN module version '10' created from /users/global/waltho/MODELS/jules-vn5.2waltho/preprocess/src/jules/src/science/soil_biogeochem/ecosse/ecosse_utils_mod.F90
MD5:fa34f635dbdbafbbcc699e5daeb10d6f -- If you edit this, you'll get what you deserve.

(() () () () () () () () () () () () () () () () () () () () () () ()
() () () ())

()

()

()

()

()

(2 'adjust_soil' 'ecosse_utils_mod' '' 1 ((PROCEDURE UNKNOWN-INTENT
MODULE-PROC DECL UNKNOWN 0 0 SUBROUTINE ALWAYS_EXPLICIT) (UNKNOWN 0 0 0
0 UNKNOWN ()) 3 0 (4 5 6 7 8 9 10 11 12 13 14 15 16 17) () 0 () () () 0
0)
18 'get_residual_n' 'ecosse_utils_mod' '' 1 ((PROCEDURE UNKNOWN-INTENT
MODULE-PROC DECL UNKNOWN 0 0 SUBROUTINE) (UNKNOWN 0 0 0 0 UNKNOWN ()) 19
0 (20 21 22 23) () 0 () () () 0 0)
24 'nt' 'ecosse_utils_mod' '' 1 ((PARAMETER UNKNOWN-INTENT UNKNOWN-PROC
UNKNOWN IMPLICIT-SAVE 0 0) (INTEGER 4 0 0 0 INTEGER ()) 0 0 () (
CONSTANT (INTEGER 4 0 0 0 INTEGER ()) 0 '2') () 0 () () () 0 0)
25 'transfer_layers' 'ecosse_utils_mod' '' 1 ((PROCEDURE UNKNOWN-INTENT
MODULE-PROC DECL UNKNOWN 0 0 DIMENSION FUNCTION ALWAYS_EXPLICIT) (REAL 4
0 0 0 REAL ()) 26 0 (27 28 29 30 31 32 33 34 35 36) (2 0 EXPLICIT (
CONSTANT (INTEGER 4 0 0 0 INTEGER ()) 0 '1') (VARIABLE (INTEGER 4 0 0 0
INTEGER ()) 0 27 ()) (CONSTANT (INTEGER 4 0 0 0 INTEGER ()) 0 '1') (
VARIABLE (INTEGER 4 0 0 0 INTEGER ()) 0 30 ())) 37 () () () 0 0)
4 'land_pts' '' '' 3 ((VARIABLE IN UNKNOWN-PROC UNKNOWN UNKNOWN 0 0
DUMMY) (INTEGER 4 0 0 0 INTEGER ()) 0 0 () () 0 () () () 0 0)
5 'vs_pts' '' '' 3 ((VARIABLE IN UNKNOWN-PROC UNKNOWN UNKNOWN 0 0 DUMMY)
(INTEGER 4 0 0 0 INTEGER ()) 0 0 () () 0 () () () 0 0)
6 'initial_call' '' '' 3 ((VARIABLE IN UNKNOWN-PROC UNKNOWN UNKNOWN 0 0
DUMMY) (LOGICAL 4 0 0 0 LOGICAL ()) 0 0 () () 0 () () () 0 0)
7 'vs_index' '' '' 3 ((VARIABLE IN UNKNOWN-PROC UNKNOWN UNKNOWN 0 0
DIMENSION DUMMY) (INTEGER 4 0 0 0 INTEGER ()) 0 0 () (1 0 EXPLICIT (
CONSTANT (INTEGER 4 0 0 0 INTEGER ()) 0 '1') (VARIABLE (INTEGER 4 0 0 0
INTEGER ()) 0 4 ())) 0 () () () 0 0)
8 'biohum_nc' '' '' 3 ((VARIABLE IN UNKNOWN-PROC UNKNOWN UNKNOWN 0 0
DIMENSION DUMMY) (REAL 4 0 0 0 REAL ()) 0 0 () (2 0 EXPLICIT (CONSTANT (
INTEGER 4 0 0 0 INTEGER ()) 0 '1') (VARIABLE (INTEGER 4 0 0 0 INTEGER ())
0 4 ()) (CONSTANT (INTEGER 4 0 0 0 INTEGER ()) 0 '1') (VARIABLE (
INTEGER 4 0 0 0 INTEGER ()) 0 38 ())) 0 () () () 0 0)
9 'c_dpm' '' '' 3 ((VARIABLE INOUT UNKNOWN-PROC UNKNOWN UNKNOWN 0 0
DIMENSION DUMMY) (REAL 4 0 0 0 REAL ()) 0 0 () (2 0 EXPLICIT (CONSTANT (
INTEGER 4 0 0 0 INTEGER ()) 0 '1') (VARIABLE (INTEGER 4 0 0 0 INTEGER ())
0 4 ()) (CONSTANT (INTEGER 4 0 0 0 INTEGER ()) 0 '1') (VARIABLE (
INTEGER 4 0 0 0 INTEGER ()) 0 38 ())) 0 () () () 0 0)
10 'c_rpm' '' '' 3 ((VARIABLE INOUT UNKNOWN-PROC UNKNOWN UNKNOWN 0 0
DIMENSION DUMMY) (REAL 4 0 0 0 REAL ()) 0 0 () (2 0 EXPLICIT (CONSTANT (
INTEGER 4 0 0 0 INTEGER ()) 0 '1') (VARIABLE (INTEGER 4 0 0 0 INTEGER ())
0 4 ()) (CONSTANT (INTEGER 4 0 0 0 INTEGER ()) 0 '1') (VARIABLE (
INTEGER 4 0 0 0 INTEGER ()) 0 38 ())) 0 () () () 0 0)
11 'c_bio' '' '' 3 ((VARIABLE INOUT UNKNOWN-PROC UNKNOWN UNKNOWN 0 0
DIMENSION DUMMY) (REAL 4 0 0 0 REAL ()) 0 0 () (2 0 EXPLICIT (CONSTANT (
INTEGER 4 0 0 0 INTEGER ()) 0 '1') (VARIABLE (INTEGER 4 0 0 0 INTEGER ())
0 4 ()) (CONSTANT (INTEGER 4 0 0 0 INTEGER ()) 0 '1') (VARIABLE (
INTEGER 4 0 0 0 INTEGER ()) 0 38 ())) 0 () () () 0 0)
12 'c_hum' '' '' 3 ((VARIABLE INOUT UNKNOWN-PROC UNKNOWN UNKNOWN 0 0
DIMENSION DUMMY) (REAL 4 0 0 0 REAL ()) 0 0 () (2 0 EXPLICIT (CONSTANT (
INTEGER 4 0 0 0 INTEGER ()) 0 '1') (VARIABLE (INTEGER 4 0 0 0 INTEGER ())
0 4 ()) (CONSTANT (INTEGER 4 0 0 0 INTEGER ()) 0 '1') (VARIABLE (
INTEGER 4 0 0 0 INTEGER ()) 0 38 ())) 0 () () () 0 0)
13 'n_dpm' '' '' 3 ((VARIABLE INOUT UNKNOWN-PROC UNKNOWN UNKNOWN 0 0
DIMENSION DUMMY) (REAL 4 0 0 0 REAL ()) 0 0 () (2 0 EXPLICIT (CONSTANT (
INTEGER 4 0 0 0 INTEGER ()) 0 '1') (VARIABLE (INTEGER 4 0 0 0 INTEGER ())
0 4 ()) (CONSTANT (INTEGER 4 0 0 0 INTEGER ()) 0 '1') (VARIABLE (
INTEGER 4 0 0 0 INTEGER ()) 0 38 ())) 0 () () () 0 0)
14 'n_rpm' '' '' 3 ((VARIABLE INOUT UNKNOWN-PROC UNKNOWN UNKNOWN 0 0
DIMENSION DUMMY) (REAL 4 0 0 0 REAL ()) 0 0 () (2 0 EXPLICIT (CONSTANT (
INTEGER 4 0 0 0 INTEGER ()) 0 '1') (VARIABLE (INTEGER 4 0 0 0 INTEGER ())
0 4 ()) (CONSTANT (INTEGER 4 0 0 0 INTEGER ()) 0 '1') (VARIABLE (
INTEGER 4 0 0 0 INTEGER ()) 0 38 ())) 0 () () () 0 0)
15 'n_bio' '' '' 3 ((VARIABLE INOUT UNKNOWN-PROC UNKNOWN UNKNOWN 0 0
DIMENSION DUMMY) (REAL 4 0 0 0 REAL ()) 0 0 () (2 0 EXPLICIT (CONSTANT (
INTEGER 4 0 0 0 INTEGER ()) 0 '1') (VARIABLE (INTEGER 4 0 0 0 INTEGER ())
0 4 ()) (CONSTANT (INTEGER 4 0 0 0 INTEGER ()) 0 '1') (VARIABLE (
INTEGER 4 0 0 0 INTEGER ()) 0 38 ())) 0 () () () 0 0)
16 'n_hum' '' '' 3 ((VARIABLE INOUT UNKNOWN-PROC UNKNOWN UNKNOWN 0 0
DIMENSION DUMMY) (REAL 4 0 0 0 REAL ()) 0 0 () (2 0 EXPLICIT (CONSTANT (
INTEGER 4 0 0 0 INTEGER ()) 0 '1') (VARIABLE (INTEGER 4 0 0 0 INTEGER ())
0 4 ()) (CONSTANT (INTEGER 4 0 0 0 INTEGER ()) 0 '1') (VARIABLE (
INTEGER 4 0 0 0 INTEGER ()) 0 38 ())) 0 () () () 0 0)
17 'co2_soil_gb' '' '' 3 ((VARIABLE INOUT UNKNOWN-PROC UNKNOWN UNKNOWN 0
0 DIMENSION DUMMY) (REAL 4 0 0 0 REAL ()) 0 0 () (1 0 ASSUMED_SHAPE (
CONSTANT (INTEGER 4 0 0 0 INTEGER ()) 0 '1') ()) 0 () () () 0 0)
20 'land_pts' '' '' 19 ((VARIABLE IN UNKNOWN-PROC UNKNOWN UNKNOWN 0 0
DUMMY) (INTEGER 4 0 0 0 INTEGER ()) 0 0 () () 0 () () () 0 0)
21 'vs_pts' '' '' 19 ((VARIABLE IN UNKNOWN-PROC UNKNOWN UNKNOWN 0 0
DUMMY) (INTEGER 4 0 0 0 INTEGER ()) 0 0 () () 0 () () () 0 0)
22 'vs_index' '' '' 19 ((VARIABLE IN UNKNOWN-PROC UNKNOWN UNKNOWN 0 0
DIMENSION DUMMY) (INTEGER 4 0 0 0 INTEGER ()) 0 0 () (1 0 EXPLICIT (
CONSTANT (INTEGER 4 0 0 0 INTEGER ()) 0 '1') (VARIABLE (INTEGER 4 0 0 0
INTEGER ()) 0 20 ())) 0 () () () 0 0)
23 'residual_n' '' '' 19 ((VARIABLE OUT UNKNOWN-PROC UNKNOWN UNKNOWN 0 0
DIMENSION DUMMY) (REAL 4 0 0 0 REAL ()) 0 0 () (2 0 EXPLICIT (CONSTANT (
INTEGER 4 0 0 0 INTEGER ()) 0 '1') (VARIABLE (INTEGER 4 0 0 0 INTEGER ())
0 20 ()) (CONSTANT (INTEGER 4 0 0 0 INTEGER ()) 0 '1') (VARIABLE (
INTEGER 4 0 0 0 INTEGER ()) 0 39 ())) 0 () () () 0 0)
27 'land_pts' '' '' 26 ((VARIABLE IN UNKNOWN-PROC UNKNOWN UNKNOWN 0 0
DUMMY) (INTEGER 4 0 0 0 INTEGER ()) 0 0 () () 0 () () () 0 0)
28 'vs_pts' '' '' 26 ((VARIABLE IN UNKNOWN-PROC UNKNOWN UNKNOWN 0 0
DUMMY) (INTEGER 4 0 0 0 INTEGER ()) 0 0 () () 0 () () () 0 0)
29 'nzin' '' '' 26 ((VARIABLE IN UNKNOWN-PROC UNKNOWN UNKNOWN 0 0 DUMMY)
(INTEGER 4 0 0 0 INTEGER ()) 0 0 () () 0 () () () 0 0)
30 'nzout' '' '' 26 ((VARIABLE IN UNKNOWN-PROC UNKNOWN UNKNOWN 0 0 DUMMY)
(INTEGER 4 0 0 0 INTEGER ()) 0 0 () () 0 () () () 0 0)
31 'l_midpoint' '' '' 26 ((VARIABLE IN UNKNOWN-PROC UNKNOWN UNKNOWN 0 0
DUMMY) (LOGICAL 4 0 0 0 LOGICAL ()) 0 0 () () 0 () () () 0 0)
32 'l_interp' '' '' 26 ((VARIABLE IN UNKNOWN-PROC UNKNOWN UNKNOWN 0 0
DUMMY) (LOGICAL 4 0 0 0 LOGICAL ()) 0 0 () () 0 () () () 0 0)
33 'vs_index' '' '' 26 ((VARIABLE IN UNKNOWN-PROC UNKNOWN UNKNOWN 0 0
DIMENSION DUMMY) (INTEGER 4 0 0 0 INTEGER ()) 0 0 () (1 0 EXPLICIT (
CONSTANT (INTEGER 4 0 0 0 INTEGER ()) 0 '1') (VARIABLE (INTEGER 4 0 0 0
INTEGER ()) 0 27 ())) 0 () () () 0 0)
34 'dzin' '' '' 26 ((VARIABLE IN UNKNOWN-PROC UNKNOWN UNKNOWN 0 0
DIMENSION DUMMY) (REAL 4 0 0 0 REAL ()) 0 0 () (1 0 EXPLICIT (CONSTANT (
INTEGER 4 0 0 0 INTEGER ()) 0 '1') (VARIABLE (INTEGER 4 0 0 0 INTEGER ())
0 29 ())) 0 () () () 0 0)
35 'dzout' '' '' 26 ((VARIABLE IN UNKNOWN-PROC UNKNOWN UNKNOWN 0 0
DIMENSION DUMMY) (REAL 4 0 0 0 REAL ()) 0 0 () (1 0 EXPLICIT (CONSTANT (
INTEGER 4 0 0 0 INTEGER ()) 0 '1') (VARIABLE (INTEGER 4 0 0 0 INTEGER ())
0 30 ())) 0 () () () 0 0)
36 'inval' '' '' 26 ((VARIABLE IN UNKNOWN-PROC UNKNOWN UNKNOWN 0 0
DIMENSION DUMMY) (REAL 4 0 0 0 REAL ()) 0 0 () (2 0 EXPLICIT (CONSTANT (
INTEGER 4 0 0 0 INTEGER ()) 0 '1') (VARIABLE (INTEGER 4 0 0 0 INTEGER ())
0 27 ()) (CONSTANT (INTEGER 4 0 0 0 INTEGER ()) 0 '1') (VARIABLE (
INTEGER 4 0 0 0 INTEGER ()) 0 29 ())) 0 () () () 0 0)
37 'outval' '' '' 26 ((VARIABLE UNKNOWN-INTENT UNKNOWN-PROC UNKNOWN
UNKNOWN 0 0 DIMENSION RESULT ALWAYS_EXPLICIT) (REAL 4 0 0 0 REAL ()) 0 0
() (2 0 EXPLICIT (CONSTANT (INTEGER 4 0 0 0 INTEGER ()) 0 '1') (
VARIABLE (INTEGER 4 0 0 0 INTEGER ()) 0 27 ()) (CONSTANT (INTEGER 4 0 0
0 INTEGER ()) 0 '1') (VARIABLE (INTEGER 4 0 0 0 INTEGER ()) 0 30 ())) 0
() () () 0 0)
38 'dim_cslayer' 'ancil_info' '' 3 ((VARIABLE UNKNOWN-INTENT
UNKNOWN-PROC UNKNOWN IMPLICIT-SAVE 0 0) (INTEGER 4 0 0 0 INTEGER ()) 0 0
() () 0 () () () 0 0)
39 'dim_cslayer' 'ancil_info' '' 19 ((VARIABLE UNKNOWN-INTENT
UNKNOWN-PROC UNKNOWN IMPLICIT-SAVE 0 0) (INTEGER 4 0 0 0 INTEGER ()) 0 0
() () 0 () () () 0 0)
)

('adjust_soil' 0 2 'get_residual_n' 0 18 'nt' 0 24 'transfer_layers' 0
25)
