GFORTRAN module version '10' created from /users/global/waltho/MODELS/jules-vn5.2/etc/fcm-make/preprocess/src/jules/src/io/input/input_mod.F90
MD5:8fe9cc3a939097cd6fc3446d42c9c303 -- If you edit this, you'll get what you deserve.

(() () () () () () () () () () () () () () () () () () () () () () ()
() () () ())

()

()

()

()

()

(2 'fill_variables_from_file' 'input_mod' '' 1 ((PROCEDURE
UNKNOWN-INTENT MODULE-PROC DECL UNKNOWN 0 0 SUBROUTINE ALWAYS_EXPLICIT)
(UNKNOWN 0 0 0 0 UNKNOWN ()) 3 0 (4 5 6) () 0 () () () 0 0)
7 'grid' 'input_mod' '' 1 ((VARIABLE UNKNOWN-INTENT UNKNOWN-PROC UNKNOWN
EXPLICIT-SAVE 0 0) (DERIVED 8 0 0 0 DERIVED ()) 0 0 () () 0 () () () 0 0)
9 'subgrid' 'input_mod' '' 1 ((VARIABLE UNKNOWN-INTENT UNKNOWN-PROC
UNKNOWN EXPLICIT-SAVE 0 0) (DERIVED 10 0 0 0 DERIVED ()) 0 0 () () 0 ()
() () 0 0)
11 'use_subgrid' 'input_mod' '' 1 ((VARIABLE UNKNOWN-INTENT UNKNOWN-PROC
UNKNOWN IMPLICIT-SAVE 0 0) (LOGICAL 4 0 0 0 LOGICAL ()) 0 0 () () 0 () ()
() 0 0)
4 'file_name' '' '' 3 ((VARIABLE IN UNKNOWN-PROC UNKNOWN UNKNOWN 0 0
DUMMY) (CHARACTER 1 0 0 0 CHARACTER (())) 0 0 () () 0 () () () 0 0)
5 'identifiers' '' '' 3 ((VARIABLE IN UNKNOWN-PROC UNKNOWN UNKNOWN 0 0
DIMENSION DUMMY) (CHARACTER 1 0 0 0 CHARACTER (())) 0 0 () (1 0
ASSUMED_SHAPE (CONSTANT (INTEGER 4 0 0 0 INTEGER ()) 0 '1') ()) 0 () ()
() 0 0)
6 'sdf_names' '' '' 3 ((VARIABLE IN UNKNOWN-PROC UNKNOWN UNKNOWN 0 0
DIMENSION DUMMY) (CHARACTER 1 0 0 0 CHARACTER (())) 0 0 () (1 0
ASSUMED_SHAPE (CONSTANT (INTEGER 4 0 0 0 INTEGER ()) 0 '1') ()) 0 () ()
() 0 0)
8 'Grid_info' 'grid_utils_mod' '' 1 ((DERIVED UNKNOWN-INTENT
UNKNOWN-PROC UNKNOWN UNKNOWN 0 0) (UNKNOWN 0 0 0 0 UNKNOWN ()) 0 0 () ()
0 ((12 'is_1d' (LOGICAL 4 0 0 0 LOGICAL ()) () (UNKNOWN-FL
UNKNOWN-INTENT UNKNOWN-PROC UNKNOWN UNKNOWN 0 0) UNKNOWN-ACCESS (
CONSTANT (LOGICAL 4 0 0 0 LOGICAL ()) 0 0)) (13 'dim_name' (CHARACTER 1
0 0 0 CHARACTER ((CONSTANT (INTEGER 4 0 0 0 INTEGER ()) 0 '55'))) () (
UNKNOWN-FL UNKNOWN-INTENT UNKNOWN-PROC UNKNOWN UNKNOWN 0 0)
UNKNOWN-ACCESS (CONSTANT (CHARACTER 1 0 0 0 CHARACTER (())) 0 55
'                                                       ')) (14 'x_name'
(CHARACTER 1 0 0 0 CHARACTER ((CONSTANT (INTEGER 4 0 0 0 INTEGER ()) 0
'55'))) () (UNKNOWN-FL UNKNOWN-INTENT UNKNOWN-PROC UNKNOWN UNKNOWN 0 0)
UNKNOWN-ACCESS (CONSTANT (CHARACTER 1 0 0 0 CHARACTER (())) 0 55
'                                                       ')) (15 'y_name'
(CHARACTER 1 0 0 0 CHARACTER ((CONSTANT (INTEGER 4 0 0 0 INTEGER ()) 0
'55'))) () (UNKNOWN-FL UNKNOWN-INTENT UNKNOWN-PROC UNKNOWN UNKNOWN 0 0)
UNKNOWN-ACCESS (CONSTANT (CHARACTER 1 0 0 0 CHARACTER (())) 0 55
'                                                       ')) (16 'nx' (
INTEGER 4 0 0 0 INTEGER ()) () (UNKNOWN-FL UNKNOWN-INTENT UNKNOWN-PROC
UNKNOWN UNKNOWN 0 0) UNKNOWN-ACCESS (CONSTANT (INTEGER 4 0 0 0 INTEGER ())
0 '0')) (17 'ny' (INTEGER 4 0 0 0 INTEGER ()) () (UNKNOWN-FL
UNKNOWN-INTENT UNKNOWN-PROC UNKNOWN UNKNOWN 0 0) UNKNOWN-ACCESS (
CONSTANT (INTEGER 4 0 0 0 INTEGER ()) 0 '0'))) PUBLIC (() () () ()) () 0
0 72620739)
10 'Subgrid_info' 'grid_utils_mod' '' 1 ((DERIVED UNKNOWN-INTENT
UNKNOWN-PROC UNKNOWN UNKNOWN 0 0 POINTER_COMP) (UNKNOWN 0 0 0 0 UNKNOWN
()) 0 0 () () 0 ((18 'parent' (DERIVED 8 0 0 0 DERIVED ()) () (
UNKNOWN-FL UNKNOWN-INTENT UNKNOWN-PROC UNKNOWN UNKNOWN 0 0)
UNKNOWN-ACCESS (STRUCTURE (DERIVED 8 0 0 0 DERIVED ()) 0 (((CONSTANT (
LOGICAL 4 0 0 0 LOGICAL ()) 0 0) ()) ((CONSTANT (CHARACTER 1 0 0 0
CHARACTER (())) 0 55
'                                                       ') ()) ((
CONSTANT (CHARACTER 1 0 0 0 CHARACTER (())) 0 55
'                                                       ') ()) ((
CONSTANT (CHARACTER 1 0 0 0 CHARACTER (())) 0 55
'                                                       ') ()) ((
CONSTANT (INTEGER 4 0 0 0 INTEGER ()) 0 '0') ()) ((CONSTANT (INTEGER 4 0
0 0 INTEGER ()) 0 '0') ())) ())) (19 'nx' (INTEGER 4 0 0 0 INTEGER ()) ()
(UNKNOWN-FL UNKNOWN-INTENT UNKNOWN-PROC UNKNOWN UNKNOWN 0 0)
UNKNOWN-ACCESS (CONSTANT (INTEGER 4 0 0 0 INTEGER ()) 0 '0')) (20 'ny' (
INTEGER 4 0 0 0 INTEGER ()) () (UNKNOWN-FL UNKNOWN-INTENT UNKNOWN-PROC
UNKNOWN UNKNOWN 0 0) UNKNOWN-ACCESS (CONSTANT (INTEGER 4 0 0 0 INTEGER ())
0 '0')) (21 'x_start' (INTEGER 4 0 0 0 INTEGER ()) () (UNKNOWN-FL
UNKNOWN-INTENT UNKNOWN-PROC UNKNOWN UNKNOWN 0 0) UNKNOWN-ACCESS (
CONSTANT (INTEGER 4 0 0 0 INTEGER ()) 0 '-1')) (22 'y_start' (INTEGER 4
0 0 0 INTEGER ()) () (UNKNOWN-FL UNKNOWN-INTENT UNKNOWN-PROC UNKNOWN
UNKNOWN 0 0) UNKNOWN-ACCESS (CONSTANT (INTEGER 4 0 0 0 INTEGER ()) 0 '-1'))
(23 'points' (INTEGER 4 0 0 0 INTEGER ()) (2 0 DEFERRED () () () ()) (
UNKNOWN-FL UNKNOWN-INTENT UNKNOWN-PROC UNKNOWN UNKNOWN 0 0 DIMENSION
POINTER) UNKNOWN-ACCESS (NULL (UNKNOWN 0 0 0 0 UNKNOWN ()) 0))) PUBLIC (
() () () ()) () 0 0 35283819)
)

('fill_variables_from_file' 0 2 'grid' 0 7 'subgrid' 0 9 'use_subgrid' 0
11)
