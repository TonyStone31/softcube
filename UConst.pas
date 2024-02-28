unit UConst;

(*
  --------------------------------------------------------------------------------
  Original Source Code obtained from: https://codes-sources.commentcamarche.net/source/53132-rubik-s-cube

  Modifications made by: Tony Stone
  Date of Modification: 2/25/2024

  This source code is made available under the terms and conditions outlined in the
  General Conditions of Use of the CodeS-SourceS.CommentCaMarche.net website.
  The terms of use can be found at: https://codes-sources.commentcamarche.net/contents/1-conditions-generales-d-utilisation

  The original work is being modified while respecting the applicable terms of use
  as specified on the CodeS-SourceS.CommentCaMarche.net website.

  Please refer to the above link for detailed information on the usage rights and
  limitations according to the General Conditions of Use.
  --------------------------------------------------------------------------------
*)

{$MODE Delphi}

interface

uses
  Graphics,
  types;

const
  C_FACE_GRID_POS: array[1..6, 0..2, 0..2] of TPoint =
    (
    // White face (top, above Green)
    (((x: 3; y: 0), (x: 4; y: 0), (x: 5; y: 0)),
    ((x: 3; y: 1), (x: 4; y: 1), (x: 5; y: 1)),   //1?
    ((x: 3; y: 2), (x: 4; y: 2), (x: 5; y: 2)))
    ,
    // Green face (center)
    (((x: 3; y: 3), (x: 4; y: 3), (x: 5; y: 3)),
    ((x: 3; y: 4), (x: 4; y: 4), (x: 5; y: 4)),   //2?
    ((x: 3; y: 5), (x: 4; y: 5), (x: 5; y: 5)))
    ,
    // Red face (right of Green)
    (((x: 6; y: 3), (x: 7; y: 3), (x: 8; y: 3)),
    ((x: 6; y: 4), (x: 7; y: 4), (x: 8; y: 4)),   //3?
    ((x: 6; y: 5), (x: 7; y: 5), (x: 8; y: 5)))
    ,
    // Blue face (far right)
    (((x: 9; y: 3), (x: 10; y: 3), (x: 11; y: 3)),
    ((x: 9; y: 4), (x: 10; y: 4), (x: 11; y: 4)), //4?
    ((x: 9; y: 5), (x: 10; y: 5), (x: 11; y: 5)))
    ,
    //ORANGE  face (left-center)
    (((x: 0; y: 3), (x: 1; y: 3), (x: 2; y: 3)),
    ((x: 0; y: 4), (x: 1; y: 4), (x: 2; y: 4)),   //5?
    ((x: 0; y: 5), (x: 1; y: 5), (x: 2; y: 5)))
    ,
    // Yellow face (bottom, below Green)
    (((x: 3; y: 6), (x: 4; y: 6), (x: 5; y: 6)),
    ((x: 3; y: 7), (x: 4; y: 7), (x: 5; y: 7)),   //6?
    ((x: 3; y: 8), (x: 4; y: 8), (x: 5; y: 8)))
    );

  CFacePlace: array[1..6, 0..8] of TPoint = (
    // White face (top, above Green)
    ((x: 3; y: 0), (x: 4; y: 0), (x: 5; y: 0),
     (x: 3; y: 1), (x: 4; y: 1), (x: 5; y: 1),    //1?
     (x: 3; y: 2), (x: 4; y: 2), (x: 5; y: 2)),
    // Green face (center)
    ((x: 3; y: 3), (x: 4; y: 3), (x: 5; y: 3),
     (x: 3; y: 4), (x: 4; y: 4), (x: 5; y: 4),    //2?
     (x: 3; y: 5), (x: 4; y: 5), (x: 5; y: 5)),
    // Red face (right of Green)
    ((x: 6; y: 3), (x: 7; y: 3), (x: 8; y: 3),
     (x: 6; y: 4), (x: 7; y: 4), (x: 8; y: 4),    //3?
     (x: 6; y: 5), (x: 7; y: 5), (x: 8; y: 5)),
    // Blue face (far right)
    ((x: 9; y: 3), (x: 10; y: 3), (x: 11; y: 3),
     (x: 9; y: 4), (x: 10; y: 4), (x: 11; y: 4),  //4?
     (x: 9; y: 5), (x: 10; y: 5), (x: 11; y: 5)),
    // Orange face (left of Green)
    ((x: 0; y: 3), (x: 1; y: 3), (x: 2; y: 3),
     (x: 0; y: 4), (x: 1; y: 4), (x: 2; y: 4),    //5?
     (x: 0; y: 5), (x: 1; y: 5), (x: 2; y: 5)),
    // Yellow face (bottom, below Green)
    ((x: 3; y: 6), (x: 4; y: 6), (x: 5; y: 6),
     (x: 3; y: 7), (x: 4; y: 7), (x: 5; y: 7),    //6?
     (x: 3; y: 8), (x: 4; y: 8), (x: 5; y: 8))
  );



  ConvertNumber = 31;
  StrFrom_To: array[0..ConvertNumber * 2 - 1] of string =
    ('/', ''
    , 'U2', 'UU'
    , 'D2', 'DD'
    , 'L2', 'LL'
    , 'R2', 'RR'
    , 'B2', 'BB'
    , 'F2', 'FF'

    , 'U''', 'UUU'
    , 'D''', 'DDD'
    , 'L''', 'LLL'
    , 'R''', 'RRR'
    , 'B''', 'BBB'
    , 'F''', 'FFF'

    , 'UUUU', ''
    , 'DDDD', ''
    , 'LLLL', ''
    , 'RRRR', ''
    , 'BBBB', ''
    , 'FFFF', ''

    , 'UUU', 'U'''
    , 'DDD', 'D'''
    , 'LLL', 'L'''
    , 'RRR', 'R'''
    , 'BBB', 'B'''
    , 'FFF', 'F'''

    , 'UU', 'U2'
    , 'DD', 'D2'
    , 'LL', 'L2'
    , 'RR', 'R2'
    , 'BB', 'B2'
    , 'FF', 'F2'
    );

  EDGE_POSITIONS: array[0..23, 0..4] of integer =
    ((1, 7, 2, 1, 12), (1, 5, 3, 1, 13), (1, 1, 4, 1, 14), (1, 3, 5, 1, 15),
    (2, 1, 1, 7, 21), (2, 5, 3, 3, 23), (2, 3, 5, 5, 25), (2, 7, 6, 1, 26),
    (3, 1, 1, 5, 31), (3, 3, 2, 5, 32), (3, 5, 4, 3, 34), (3, 7, 6, 5, 36),
    (4, 1, 1, 1, 41), (4, 3, 3, 5, 43), (4, 5, 5, 3, 45), (4, 7, 6, 7, 46),
    (5, 1, 1, 3, 51), (5, 5, 2, 3, 52), (5, 3, 4, 5, 54), (5, 7, 6, 3, 56),
    (6, 1, 2, 7, 62), (6, 5, 3, 7, 63), (6, 7, 4, 7, 64), (6, 3, 5, 7, 65));

  CORNER_POSITION: array[0..23, 0..6] of integer =
    ((1, 8, 2, 2, 3, 0, 123),      //18 22 30     //face 1 2 3
    (2, 2, 3, 0, 1, 8, 231),
    (3, 0, 1, 8, 2, 2, 312),
    (1, 2, 3, 2, 4, 0, 134),      //12 32 40     //face 1 3 4
    (3, 2, 4, 0, 1, 2, 341),
    (4, 0, 1, 2, 3, 2, 413),
    (1, 0, 4, 2, 5, 0, 145),      //10 42 50     //face 1 4 5
    (4, 2, 5, 0, 1, 0, 451),
    (5, 0, 1, 0, 4, 2, 514),
    (1, 6, 5, 2, 2, 0, 152),      //16 52 20     //face 1 5 2
    (5, 2, 2, 0, 1, 6, 521),
    (2, 0, 1, 6, 5, 2, 215),

    (6, 2, 3, 6, 2, 8, 632),      //62 36 28
    (2, 8, 6, 2, 3, 6, 263),
    (3, 6, 2, 8, 6, 2, 326),
    (6, 8, 4, 6, 3, 8, 643),      //68 46 38
    (3, 8, 6, 8, 4, 6, 364),
    (4, 6, 3, 8, 6, 8, 436),
    (6, 6, 5, 6, 4, 8, 654),      //66 56 48
    (4, 8, 6, 6, 5, 6, 465),
    (5, 6, 4, 8, 6, 6, 546),
    (6, 0, 2, 6, 5, 8, 625),      //60 26 58
    (5, 8, 6, 0, 2, 6, 562),
    (2, 6, 5, 8, 6, 0, 256));


const
  FACE_NAMES: array[0..6] of string = ('void', 'U', 'L', 'F', 'R', 'B', 'D');

type
  T3dPoint = array[0..2] of single;
  TunitRubik = packed array[1..6, 0..2, 0..2] of byte;
  TLinRubik = packed array[0..53] of byte;
  TFaceRubik = packed array[1..6, 0..8] of byte;
  TCube3D = array[0..269, 1..4] of T3dPoint;
  TRubik = TFaceRubik;


const
  CUBE_TOP = 1;
  CUBE_FRONT = 2;
  CUBE_RIGHT = 3;
  CUBE_BACK = 4;
  CUBE_LEFT = 5;
  CUBE_BOTTOM = 6;
  CUBE_CENTER_VERTICAL = 7;
  CUBE_CENTER_DEPTH = 8;
  CUBE_CENTER_HORIZONTAL = 9;


  REFERENCE_X: T3dPoint = (1, 0, 0);
  REFERENCE_Y: T3dPoint = (0, 1, 0);
  REFERENCE_Z: T3dPoint = (0, 0, 1);



  C_COLOR: array[0..6] of tcolor = (ClSilver, ClWhite, ClLime, ClRed, ClBlue, $0080FF, ClYellow);
  C_CUBE_SIZE = 30;
  C_CUBE_COMPLETE: TRubik =
    ((1, 1, 1, 1, 1, 1, 1, 1, 1),
    (2, 2, 2, 2, 2, 2, 2, 2, 2),
    (3, 3, 3, 3, 3, 3, 3, 3, 3),
    (4, 4, 4, 4, 4, 4, 4, 4, 4),
    (5, 5, 5, 5, 5, 5, 5, 5, 5),
    (6, 6, 6, 6, 6, 6, 6, 6, 6));

 {

   00 01 02
   03 04 05
   06 07 08

   09 10 11   18 19 20    27 28 29    36 37 38
   12 13 14   21 22 23    30 31 32    39 40 41
   15 16 17   24 25 26    33 34 35    42 43 44

   45 46 47
   48 49 50
   51 52 53

 }


  C3dFaceRotation: array[1..6, 0..20] of integer =
    ((00, 01, 02, 03, 04, 05, 06, 07, 08, 09, 10, 11, 18, 19, 20, 27, 28, 29, 36, 37, 38)
    , (09, 10, 11, 12, 13, 14, 15, 16, 17, 06, 07, 08, 18, 21, 24, 45, 46, 47, 38, 41, 44)
    , (18, 19, 20, 21, 22, 23, 24, 25, 26, 02, 05, 08, 11, 14, 17, 27, 30, 33, 47, 50, 53)
    , (27, 28, 29, 30, 31, 32, 33, 34, 35, 00, 01, 02, 20, 23, 26, 36, 39, 42, 51, 52, 53)
    , (36, 37, 38, 39, 40, 41, 42, 43, 44, 00, 03, 06, 29, 32, 35, 09, 12, 15, 45, 48, 51)
    , (45, 46, 47, 48, 49, 50, 51, 52, 53, 15, 16, 17, 24, 25, 26, 33, 34, 35, 42, 43, 44));

  VIEW_OF_3D_CUBE: TCube3D =
    (// face 1
    ((-3.0, +3.0, +3.0), (-1.0, +3.0, +3.0), (-1.1, +3.1, +2.9), (-2.9, +3.1, +2.9)),   //0
    ((-1.0, +3.0, +3.0), (-1.0, +3.0, +1.0), (-1.1, +3.1, +1.1), (-1.1, +3.1, +2.9)),
    ((-1.0, +3.0, +1.0), (-3.0, +3.0, +1.0), (-2.9, +3.1, +1.1), (-1.1, +3.1, +1.1)),
    ((-3.0, +3.0, +1.0), (-3.0, +3.0, +3.0), (-2.9, +3.1, +2.9), (-2.9, +3.1, +1.1)),
    ((-2.9, +3.1, +2.9), (-1.1, +3.1, +2.9), (-1.1, +3.1, +1.1), (-2.9, +3.1, +1.1)),
    ((-1.0, +3.0, +3.0), (+1.0, +3.0, +3.0), (+0.8, +3.1, +2.9), (-0.8, +3.1, +2.9)),   //1
    ((+1.0, +3.0, +3.0), (+1.0, +3.0, +1.0), (+0.8, +3.1, +1.1), (+0.8, +3.1, +2.9)),
    ((+1.0, +3.0, +1.0), (-1.0, +3.0, +1.0), (-0.8, +3.1, +1.1), (+0.8, +3.1, +1.1)),
    ((-1.0, +3.0, +1.0), (-1.0, +3.0, +3.0), (-0.8, +3.1, +2.9), (-0.8, +3.1, +1.1)),
    ((-0.8, +3.1, +2.9), (+0.8, +3.1, +2.9), (+0.8, +3.1, +1.1), (-0.8, +3.1, +1.1)),
    ((+1.0, +3.0, +3.0), (+3.0, +3.0, +3.0), (+2.9, +3.1, +2.9), (+1.1, +3.1, +2.9)),   //2
    ((+3.0, +3.0, +3.0), (+3.0, +3.0, +1.0), (+2.9, +3.1, +1.1), (+2.9, +3.1, +2.9)),
    ((+3.0, +3.0, +1.100), (+1.0, +3.0, +1.0), (+1.1, +3.1, +1.1), (+2.9, +3.1, +1.1)),
    ((+1.0, +3.0, +1.0), (+1.0, +3.0, +3.0), (+1.1, +3.1, +2.9), (+1.1, +3.1, +1.1)),
    ((+1.1, +3.1, +2.9), (+2.9, +3.1, +2.9), (+2.9, +3.1, +1.1), (+1.1, +3.1, +1.1)),

    ((-3.0, +3.0, +1.0), (-1.0, +3.0, +1.0), (-1.1, +3.1, +0.8), (-2.9, +3.1, +0.8)),   //3
    ((-1.0, +3.0, +1.0), (-1.0, +3.0, -1.0), (-1.1, +3.1, -0.8), (-1.1, +3.1, +0.8)),
    ((-1.0, +3.0, -1.0), (-3.0, +3.0, -1.0), (-2.9, +3.1, -0.8), (-1.1, +3.1, -0.8)),
    ((-3.0, +3.0, -1.0), (-3.0, +3.0, +1.0), (-2.9, +3.1, +0.8), (-2.9, +3.1, -0.8)),
    ((-2.9, +3.1, +0.8), (-1.1, +3.1, +0.8), (-1.1, +3.1, -0.8), (-2.9, +3.1, -0.8)),
    ((-1.0, +3.0, +1.0), (+1.0, +3.0, +1.0), (+0.8, +3.1, +0.8), (-0.8, +3.1, +0.8)),  //4
    ((+1.0, +3.0, +1.0), (+1.0, +3.0, -1.0), (+0.8, +3.1, -0.8), (+0.8, +3.1, +0.8)),
    ((+1.0, +3.0, -1.0), (-1.0, +3.0, -1.0), (-0.8, +3.1, -0.8), (+0.8, +3.1, -0.8)),
    ((-1.0, +3.0, -1.0), (-1.0, +3.0, +1.0), (-0.8, +3.1, +0.8), (-0.8, +3.1, -0.8)),
    ((-0.8, +3.1, +0.8), (+0.8, +3.1, +0.8), (+0.8, +3.1, -0.8), (-0.8, +3.1, -0.8)),
    ((+1.0, +3.0, +1.0), (+3.0, +3.0, +1.0), (+2.9, +3.1, +0.8), (+1.1, +3.1, +0.8)),   //5
    ((+3.0, +3.0, +1.0), (+3.0, +3.0, -1.0), (+2.9, +3.1, -0.8), (+2.9, +3.1, +0.8)),
    ((+3.0, +3.0, -1.0), (+1.0, +3.0, -1.0), (+1.1, +3.1, -0.8), (+2.9, +3.1, -0.8)),
    ((+1.0, +3.0, -1.0), (+1.0, +3.0, +1.0), (+1.1, +3.1, +0.8), (+1.1, +3.1, -0.8)),
    ((+1.1, +3.1, +0.8), (+2.9, +3.1, +0.8), (+2.9, +3.1, -0.8), (+1.1, +3.1, -0.8)),

    ((-3.0, +3.0, -1.0), (-1.0, +3.0, -1.0), (-1.1, +3.1, -1.1), (-2.9, +3.1, -1.1)),  //6
    ((-1.0, +3.0, -1.0), (-1.0, +3.0, -3.0), (-1.1, +3.1, -2.9), (-1.1, +3.1, -1.1)),
    ((-1.0, +3.0, -3.0), (-3.0, +3.0, -3.0), (-2.9, +3.1, -2.9), (-1.1, +3.1, -2.9)),
    ((-3.0, +3.0, -3.0), (-3.0, +3.0, -1.0), (-2.9, +3.1, -1.1), (-2.9, +3.1, -2.9)),
    ((-2.9, +3.1, -1.1), (-1.1, +3.1, -1.1), (-1.1, +3.1, -2.9), (-2.9, +3.1, -2.9)),
    ((-1.0, +3.0, -1.0), (+1.0, +3.0, -1.0), (+0.8, +3.1, -1.1), (-0.8, +3.1, -1.1)),
    ((+1.0, +3.0, -1.0), (+1.0, +3.0, -3.0), (+0.8, +3.1, -2.9), (+0.8, +3.1, -1.1)),  //7
    ((+1.0, +3.0, -3.0), (-1.0, +3.0, -3.0), (-0.8, +3.1, -2.9), (+0.8, +3.1, -2.9)),
    ((-1.0, +3.0, -3.0), (-1.0, +3.0, -1.0), (-0.8, +3.1, -1.1), (-0.8, +3.1, -2.9)),
    ((-0.8, +3.1, -1.1), (+0.8, +3.1, -1.1), (+0.8, +3.1, -2.9), (-0.8, +3.1, -2.9)),
    ((+1.0, +3.0, -1.0), (+3.0, +3.0, -1.0), (+2.9, +3.1, -1.1), (+1.1, +3.1, -1.1)),  //8
    ((+3.0, +3.0, -1.0), (+3.0, +3.0, -3.0), (+2.9, +3.1, -2.9), (+2.9, +3.1, -1.1)),
    ((+3.0, +3.0, -3.0), (+1.0, +3.0, -3.0), (+1.1, +3.1, -2.9), (+2.9, +3.1, -2.9)),
    ((+1.0, +3.0, -3.0), (+1.0, +3.0, -1.0), (+1.1, +3.1, -1.1), (+1.1, +3.1, -2.9)),
    ((+1.1, +3.1, -1.1), (+2.9, +3.1, -1.1), (+2.9, +3.1, -2.9), (+1.1, +3.1, -2.9)),

    // face 2
    ((-3.0, +3.0, -3.0), (-1.0, +3.0, -3.0), (-1.1, +2.9, -3.1), (-2.9, +2.9, -3.1)),
    ((-1.0, +3.0, -3.0), (-1.0, +1.0, -3.0), (-1.1, +1.1, -3.1), (-1.1, +2.9, -3.1)),
    ((-1.0, +1.0, -3.0), (-3.0, +1.0, -3.0), (-2.9, +1.1, -3.1), (-1.1, +1.1, -3.1)),
    ((-3.0, +1.0, -3.0), (-3.0, +3.0, -3.0), (-2.9, +2.9, -3.1), (-2.9, +1.1, -3.1)),
    ((-2.9, +2.9, -3.1), (-1.1, +2.9, -3.1), (-1.1, +1.1, -3.1), (-2.9, +1.1, -3.1)),
    ((-1.0, +3.0, -3.0), (+1.0, +3.0, -3.0), (+0.8, +2.9, -3.1), (-0.8, +2.9, -3.1)),
    ((+1.0, +3.0, -3.0), (+1.0, +1.0, -3.0), (+0.8, +1.1, -3.1), (+0.8, +2.9, -3.1)),
    ((+1.0, +1.0, -3.0), (-1.0, +1.0, -3.0), (-0.8, +1.1, -3.1), (+0.8, +1.1, -3.1)),
    ((-1.0, +1.0, -3.0), (-1.0, +3.0, -3.0), (-0.8, +2.9, -3.1), (-0.8, +1.1, -3.1)),
    ((-0.8, +2.9, -3.1), (+0.8, +2.9, -3.1), (+0.8, +1.1, -3.1), (-0.8, +1.1, -3.1)),
    ((+1.0, +3.0, -3.0), (+3.0, +3.0, -3.0), (+2.9, +2.9, -3.1), (+1.1, +2.9, -3.1)),
    ((+3.0, +3.0, -3.0), (+3.0, +1.0, -3.0), (+2.9, +1.1, -3.1), (+2.9, +2.9, -3.1)),
    ((+3.0, +1.0, -3.0), (+1.0, +1.0, -3.0), (+1.1, +1.1, -3.1), (+2.9, +1.1, -3.1)),
    ((+1.0, +1.0, -3.0), (+1.0, +3.0, -3.0), (+1.1, +2.9, -3.1), (+1.1, +1.1, -3.1)),
    ((+1.1, +2.9, -3.1), (+2.9, +2.9, -3.1), (+2.9, +1.1, -3.1), (+1.1, +1.1, -3.1)),

    ((-3.0, +1.0, -3.0), (-1.0, +1.0, -3.0), (-1.1, +0.8, -3.1), (-2.9, +0.8, -3.1)),
    ((-1.0, +1.0, -3.0), (-1.0, -1.0, -3.0), (-1.1, -0.8, -3.1), (-1.1, +0.8, -3.1)),
    ((-1.0, -1.0, -3.0), (-3.0, -1.0, -3.0), (-2.9, -0.8, -3.1), (-1.1, -0.8, -3.1)),
    ((-3.0, -1.0, -3.0), (-3.0, +1.0, -3.0), (-2.9, +0.8, -3.1), (-2.9, -0.8, -3.1)),
    ((-2.9, +0.8, -3.1), (-1.1, +0.8, -3.1), (-1.1, -0.8, -3.1), (-2.9, -0.8, -3.1)),
    ((-1.0, +1.0, -3.0), (+1.0, +1.0, -3.0), (+0.8, +0.8, -3.1), (-0.8, +0.8, -3.1)),
    ((+1.0, +1.0, -3.0), (+1.0, -1.0, -3.0), (+0.8, -0.8, -3.1), (+0.8, +0.8, -3.1)),
    ((+1.0, -1.0, -3.0), (-1.0, -1.0, -3.0), (-0.8, -0.8, -3.1), (+0.8, -0.8, -3.1)),
    ((-1.0, -1.0, -3.0), (-1.0, +1.0, -3.0), (-0.8, +0.8, -3.1), (-0.8, -0.8, -3.1)),
    ((-0.8, +0.8, -3.1), (+0.8, +0.8, -3.1), (+0.8, -0.8, -3.1), (-0.8, -0.8, -3.1)),
    ((+1.0, +1.0, -3.0), (+3.0, +1.0, -3.0), (+2.9, +0.8, -3.1), (+1.1, +0.8, -3.1)),
    ((+3.0, +1.0, -3.0), (+3.0, -1.0, -3.0), (+2.9, -0.8, -3.1), (+2.9, +0.8, -3.1)),
    ((+3.0, -1.0, -3.0), (+1.0, -1.0, -3.0), (+1.1, -0.8, -3.1), (+2.9, -0.8, -3.1)),
    ((+1.0, -1.0, -3.0), (+1.0, +1.0, -3.0), (+1.1, +0.8, -3.1), (+1.1, -0.8, -3.1)),
    ((+1.1, +0.8, -3.1), (+2.9, +0.8, -3.1), (+2.9, -0.8, -3.1), (+1.1, -0.8, -3.1)),

    ((-3.0, -1.0, -3.0), (-1.0, -1.0, -3.0), (-1.1, -1.1, -3.1), (-2.9, -1.1, -3.1)),
    ((-1.0, -1.0, -3.0), (-1.0, -3.0, -3.0), (-1.1, -2.9, -3.1), (-1.1, -1.1, -3.1)),
    ((-1.0, -3.0, -3.0), (-3.0, -3.0, -3.0), (-2.9, -2.9, -3.1), (-1.1, -2.9, -3.1)),
    ((-3.0, -3.0, -3.0), (-3.0, -1.0, -3.0), (-2.9, -1.1, -3.1), (-2.9, -2.9, -3.1)),
    ((-2.9, -1.1, -3.1), (-1.1, -1.1, -3.1), (-1.1, -2.9, -3.1), (-2.9, -2.9, -3.1)),
    ((-1.0, -1.0, -3.0), (+1.0, -1.0, -3.0), (+0.8, -1.1, -3.1), (-0.8, -1.1, -3.1)),
    ((+1.0, -1.0, -3.0), (+1.0, -3.0, -3.0), (+0.8, -2.9, -3.1), (+0.8, -1.1, -3.1)),
    ((+1.0, -3.0, -3.0), (-1.0, -3.0, -3.0), (-0.8, -2.9, -3.1), (+0.8, -2.9, -3.1)),
    ((-1.0, -3.0, -3.0), (-1.0, -1.0, -3.0), (-0.8, -1.1, -3.1), (-0.8, -2.9, -3.1)),
    ((-0.8, -1.1, -3.1), (+0.8, -1.1, -3.1), (+0.8, -2.9, -3.1), (-0.8, -2.9, -3.1)),
    ((+1.0, -1.0, -3.0), (+3.0, -1.0, -3.0), (+2.9, -1.1, -3.1), (+1.1, -1.1, -3.1)),
    ((+3.0, -1.0, -3.0), (+3.0, -3.0, -3.0), (+2.9, -2.9, -3.1), (+2.9, -1.1, -3.1)),
    ((+3.0, -3.0, -3.0), (+1.0, -3.0, -3.0), (+1.1, -2.9, -3.1), (+2.9, -2.9, -3.1)),
    ((+1.0, -3.0, -3.0), (+1.0, -1.0, -3.0), (+1.1, -1.1, -3.1), (+1.1, -2.9, -3.1)),
    ((+1.1, -1.1, -3.1), (+2.9, -1.1, -3.1), (+2.9, -2.9, -3.1), (+1.1, -2.9, -3.1)),

    // face 3
    ((+3.0, +3.0, -3.0), (+3.0, +3.0, -1.0), (+3.1, +2.9, -1.1), (+3.1, +2.9, -2.9)),
    ((+3.0, +3.0, -1.0), (+3.0, +1.0, -1.0), (+3.1, +1.1, -1.1), (+3.1, +2.9, -1.1)),
    ((+3.0, +1.0, -1.0), (+3.0, +1.0, -3.0), (+3.1, +1.1, -2.9), (+3.1, +1.1, -1.1)),
    ((+3.0, +1.0, -3.0), (+3.0, +3.0, -3.0), (+3.1, +2.9, -2.9), (+3.1, +1.1, -2.9)),
    ((+3.1, +2.9, -2.9), (+3.1, +2.9, -1.1), (+3.1, +1.1, -1.1), (+3.1, +1.1, -2.9)),
    ((+3.0, +3.0, -1.0), (+3.0, +3.0, +1.0), (+3.1, +2.9, +0.8), (+3.1, +2.9, -0.8)),
    ((+3.0, +3.0, +1.0), (+3.0, +1.0, +1.0), (+3.1, +1.1, +0.8), (+3.1, +2.9, +0.8)),
    ((+3.0, +1.0, +1.0), (+3.0, +1.0, -1.0), (+3.1, +1.1, -0.8), (+3.1, +1.1, +0.8)),
    ((+3.0, +1.0, -1.0), (+3.0, +3.0, -1.0), (+3.1, +2.9, -0.8), (+3.1, +1.1, -0.8)),
    ((+3.1, +2.9, -0.8), (+3.1, +2.9, +0.8), (+3.1, +1.1, +0.8), (+3.1, +1.1, -0.8)),
    ((+3.0, +3.0, +1.0), (+3.0, +3.0, +3.0), (+3.1, +2.9, +2.9), (+3.1, +2.9, +1.1)),
    ((+3.0, +3.0, +3.0), (+3.0, +1.0, +3.0), (+3.1, +1.1, +2.9), (+3.1, +2.9, +2.9)),
    ((+3.0, +1.0, +3.0), (+3.0, +1.0, +1.0), (+3.1, +1.1, +1.1), (+3.1, +1.1, +2.9)),
    ((+3.0, +1.0, +1.0), (+3.0, +3.0, +1.0), (+3.1, +2.9, +1.1), (+3.1, +1.1, +1.1)),
    ((+3.1, +2.9, +1.1), (+3.1, +2.9, +2.9), (+3.1, +1.1, +2.9), (+3.1, +1.1, +1.1)),

    ((+3.0, +1.0, -3.0), (+3.0, +1.0, -1.0), (+3.1, +0.8, -1.1), (+3.1, +0.8, -2.9)),
    ((+3.0, +1.0, -1.0), (+3.0, -1.0, -1.0), (+3.1, -0.8, -1.1), (+3.1, +0.8, -1.1)),
    ((+3.0, -1.0, -1.0), (+3.0, -1.0, -3.0), (+3.1, -0.8, -2.9), (+3.1, -0.8, -1.1)),
    ((+3.0, -1.0, -3.0), (+3.0, +1.0, -3.0), (+3.1, +0.8, -2.9), (+3.1, -0.8, -2.9)),
    ((+3.1, +0.8, -2.9), (+3.1, +0.8, -1.1), (+3.1, -0.8, -1.1), (+3.1, -0.8, -2.9)),
    ((+3.0, +1.0, -1.0), (+3.0, +1.0, +1.0), (+3.1, +0.8, +0.8), (+3.1, +0.8, -0.8)),
    ((+3.0, +1.0, +1.0), (+3.0, -1.0, +1.0), (+3.1, -0.8, +0.8), (+3.1, +0.8, +0.8)),
    ((+3.0, -1.0, +1.0), (+3.0, -1.0, -1.0), (+3.1, -0.8, -0.8), (+3.1, -0.8, +0.8)),
    ((+3.0, -1.0, -1.0), (+3.0, +1.0, -1.0), (+3.1, +0.8, -0.8), (+3.1, -0.8, -0.8)),
    ((+3.1, +0.8, -0.8), (+3.1, +0.8, +0.8), (+3.1, -0.8, +0.8), (+3.1, -0.8, -0.8)),
    ((+3.0, +1.0, +1.0), (+3.0, +1.0, +3.0), (+3.1, +0.8, +2.9), (+3.1, +0.8, +1.1)),
    ((+3.0, +1.0, +3.0), (+3.0, -1.0, +3.0), (+3.1, -0.8, +2.9), (+3.1, +0.8, +2.9)),
    ((+3.0, -1.0, +3.0), (+3.0, -1.0, +1.0), (+3.1, -0.8, +1.1), (+3.1, -0.8, +2.9)),
    ((+3.0, -1.0, +1.0), (+3.0, +1.0, +1.0), (+3.1, +0.8, +1.1), (+3.1, -0.8, +1.1)),
    ((+3.1, +0.8, +1.1), (+3.1, +0.8, +2.9), (+3.1, -0.8, +2.9), (+3.1, -0.8, +1.1)),

    ((+3.0, -1.0, -3.0), (+3.0, -1.0, -1.0), (+3.1, -1.1, -1.1), (+3.1, -1.1, -2.9)),
    ((+3.0, -1.0, -1.0), (+3.0, -3.0, -1.0), (+3.1, -2.9, -1.1), (+3.1, -1.1, -1.1)),
    ((+3.0, -3.0, -1.0), (+3.0, -3.0, -3.0), (+3.1, -2.9, -2.9), (+3.1, -2.9, -1.1)),
    ((+3.0, -3.0, -3.0), (+3.0, -1.0, -3.0), (+3.1, -1.1, -2.9), (+3.1, -2.9, -2.9)),
    ((+3.1, -1.1, -2.9), (+3.1, -1.1, -1.1), (+3.1, -2.9, -1.1), (+3.1, -2.9, -2.9)),
    ((+3.0, -1.0, -1.0), (+3.0, -1.0, +1.0), (+3.1, -1.1, +0.8), (+3.1, -1.1, -0.8)),
    ((+3.0, -1.0, +1.0), (+3.0, -3.0, +1.0), (+3.1, -2.9, +0.8), (+3.1, -1.1, +0.8)),
    ((+3.0, -3.0, +1.0), (+3.0, -3.0, -1.0), (+3.1, -2.9, -0.8), (+3.1, -2.9, +0.8)),
    ((+3.0, -3.0, -1.0), (+3.0, -1.0, -1.0), (+3.1, -1.1, -0.8), (+3.1, -2.9, -0.8)),
    ((+3.1, -1.1, -0.8), (+3.1, -1.1, +0.8), (+3.1, -2.9, +0.8), (+3.1, -2.9, -0.8)),
    ((+3.0, -1.0, +1.0), (+3.0, -1.0, +3.0), (+3.1, -1.1, +2.9), (+3.1, -1.1, +1.1)),
    ((+3.0, -1.0, +3.0), (+3.0, -3.0, +3.0), (+3.1, -2.9, +2.9), (+3.1, -1.1, +2.9)),
    ((+3.0, -3.0, +3.0), (+3.0, -3.0, +1.0), (+3.1, -2.9, +1.1), (+3.1, -2.9, +2.9)),
    ((+3.0, -3.0, +1.0), (+3.0, -1.0, +1.0), (+3.1, -1.1, +1.1), (+3.1, -2.9, +1.1)),
    ((+3.1, -1.1, +1.1), (+3.1, -1.1, +2.9), (+3.1, -2.9, +2.9), (+3.1, -2.9, +1.1)),

    // face 4
    ((+3.0, +3.0, +3.0), (+1.0, +3.0, +3.0), (+1.1, +2.9, +3.1), (+2.9, +2.9, +3.1)),
    ((+1.0, +3.0, +3.0), (+1.0, +1.0, +3.0), (+1.1, +1.1, +3.1), (+1.1, +2.9, +3.1)),
    ((+1.0, +1.0, +3.0), (+3.0, +1.0, +3.0), (+2.9, +1.1, +3.1), (+1.1, +1.1, +3.1)),
    ((+3.0, +1.0, +3.0), (+3.0, +3.0, +3.0), (+2.9, +2.9, +3.1), (+2.9, +1.1, +3.1)),
    ((+2.9, +2.9, +3.1), (+1.1, +2.9, +3.1), (+1.1, +1.1, +3.1), (+2.9, +1.1, +3.1)),
    ((+1.0, +3.0, +3.0), (-1.0, +3.0, +3.0), (-0.8, +2.9, +3.1), (+0.8, +2.9, +3.1)),
    ((-1.0, +3.0, +3.0), (-1.0, +1.0, +3.0), (-0.8, +1.1, +3.1), (-0.8, +2.9, +3.1)),
    ((-1.0, +1.0, +3.0), (+1.0, +1.0, +3.0), (+0.8, +1.1, +3.1), (-0.8, +1.1, +3.1)),
    ((+1.0, +1.0, +3.0), (+1.0, +3.0, +3.0), (+0.8, +2.9, +3.1), (+0.8, +1.1, +3.1)),
    ((+0.8, +2.9, +3.1), (-0.8, +2.9, +3.1), (-0.8, +1.1, +3.1), (+0.8, +1.1, +3.1)),
    ((-1.0, +3.0, +3.0), (-3.0, +3.0, +3.0), (-2.9, +2.9, +3.1), (-1.1, +2.9, +3.1)),
    ((-3.0, +3.0, +3.0), (-3.0, +1.0, +3.0), (-2.9, +1.1, +3.1), (-2.9, +2.9, +3.1)),
    ((-3.0, +1.0, +3.0), (-1.0, +1.0, +3.0), (-1.1, +1.1, +3.1), (-2.9, +1.1, +3.1)),
    ((-1.0, +1.0, +3.0), (-1.0, +3.0, +3.0), (-1.1, +2.9, +3.1), (-1.1, +1.1, +3.1)),
    ((-1.1, +2.9, +3.1), (-2.9, +2.9, +3.1), (-2.9, +1.1, +3.1), (-1.1, +1.1, +3.1)),

    ((+3.0, +1.0, +3.0), (+1.0, +1.0, +3.0), (+1.1, +0.8, +3.1), (+2.9, +0.8, +3.1)),
    ((+1.0, +1.0, +3.0), (+1.0, -1.0, +3.0), (+1.1, -0.8, +3.1), (+1.1, +0.8, +3.1)),
    ((+1.0, -1.0, +3.0), (+3.0, -1.0, +3.0), (+2.9, -0.8, +3.1), (+1.1, -0.8, +3.1)),
    ((+3.0, -1.0, +3.0), (+3.0, +1.0, +3.0), (+2.9, +0.8, +3.1), (+2.9, -0.8, +3.1)),
    ((+2.9, +0.8, +3.1), (+1.1, +0.8, +3.1), (+1.1, -0.8, +3.1), (+2.9, -0.8, +3.1)),
    ((+1.0, +1.0, +3.0), (-1.0, +1.0, +3.0), (-0.8, +0.8, +3.1), (+0.8, +0.8, +3.1)),
    ((-1.0, +1.0, +3.0), (-1.0, -1.0, +3.0), (-0.8, -0.8, +3.1), (-0.8, +0.8, +3.1)),
    ((-1.0, -1.0, +3.0), (+1.0, -1.0, +3.0), (+0.8, -0.8, +3.1), (-0.8, -0.8, +3.1)),
    ((+1.0, -1.0, +3.0), (+1.0, +1.0, +3.0), (+0.8, +0.8, +3.1), (+0.8, -0.8, +3.1)),
    ((+0.8, +0.8, +3.1), (-0.8, +0.8, +3.1), (-0.8, -0.8, +3.1), (+0.8, -0.8, +3.1)),
    ((-1.0, +1.0, +3.0), (-3.0, +1.0, +3.0), (-2.9, +0.8, +3.1), (-1.1, +0.8, +3.1)),
    ((-3.0, +1.0, +3.0), (-3.0, -1.0, +3.0), (-2.9, -0.8, +3.1), (-2.9, +0.8, +3.1)),
    ((-3.0, -1.0, +3.0), (-1.0, -1.0, +3.0), (-1.1, -0.8, +3.1), (-2.9, -0.8, +3.1)),
    ((-1.0, -1.0, +3.0), (-1.0, +1.0, +3.0), (-1.1, +0.8, +3.1), (-1.1, -0.8, +3.1)),
    ((-1.1, +0.8, +3.1), (-2.9, +0.8, +3.1), (-2.9, -0.8, +3.1), (-1.1, -0.8, +3.1)),

    ((+3.0, -1.0, +3.0), (+1.0, -1.0, +3.0), (+1.1, -1.1, +3.1), (+2.9, -1.1, +3.1)),
    ((+1.0, -1.0, +3.0), (+1.0, -3.0, +3.0), (+1.1, -2.9, +3.1), (+1.1, -1.1, +3.1)),
    ((+1.0, -3.0, +3.0), (+3.0, -3.0, +3.0), (+2.9, -2.9, +3.1), (+1.1, -2.9, +3.1)),
    ((+3.0, -3.0, +3.0), (+3.0, -1.0, +3.0), (+2.9, -1.1, +3.1), (+2.9, -2.9, +3.1)),
    ((+2.9, -1.1, +3.1), (+1.1, -1.1, +3.1), (+1.1, -2.9, +3.1), (+2.9, -2.9, +3.1)),
    ((+1.0, -1.0, +3.0), (-1.0, -1.0, +3.0), (-0.8, -1.1, +3.1), (+0.8, -1.1, +3.1)),
    ((-1.0, -1.0, +3.0), (-1.0, -3.0, +3.0), (-0.8, -2.9, +3.1), (-0.8, -1.1, +3.1)),
    ((-1.0, -3.0, +3.0), (+1.0, -3.0, +3.0), (+0.8, -2.9, +3.1), (-0.8, -2.9, +3.1)),
    ((+1.0, -3.0, +3.0), (+1.0, -1.0, +3.0), (+0.8, -1.1, +3.1), (+0.8, -2.9, +3.1)),
    ((+0.8, -1.1, +3.1), (-0.8, -1.1, +3.1), (-0.8, -2.9, +3.1), (+0.8, -2.9, +3.1)),
    ((-1.0, -1.0, +3.0), (-3.0, -1.0, +3.0), (-2.9, -1.1, +3.1), (-1.1, -1.1, +3.1)),
    ((-3.0, -1.0, +3.0), (-3.0, -3.0, +3.0), (-2.9, -2.9, +3.1), (-2.9, -1.1, +3.1)),
    ((-3.0, -3.0, +3.0), (-1.0, -3.0, +3.0), (-1.1, -2.9, +3.1), (-2.9, -2.9, +3.1)),
    ((-1.0, -3.0, +3.0), (-1.0, -1.0, +3.0), (-1.1, -1.1, +3.1), (-1.1, -2.9, +3.1)),
    ((-1.1, -1.1, +3.1), (-2.9, -1.1, +3.1), (-2.9, -2.9, +3.1), (-1.1, -2.9, +3.1)),

    // face 5
    ((-3.0, +3.0, +3.0), (-3.0, +3.0, +1.0), (-3.1, +2.9, +1.1), (-3.1, +2.9, +2.9)),
    ((-3.0, +3.0, +1.0), (-3.0, +1.0, +1.0), (-3.1, +1.1, +1.1), (-3.1, +2.9, +1.1)),
    ((-3.0, +1.0, +1.0), (-3.0, +1.0, +3.0), (-3.1, +1.1, +2.9), (-3.1, +1.1, +1.1)),
    ((-3.0, +1.0, +3.0), (-3.0, +3.0, +3.0), (-3.1, +2.9, +2.9), (-3.1, +1.1, +2.9)),
    ((-3.1, +2.9, +2.9), (-3.1, +2.9, +1.1), (-3.1, +1.1, +1.1), (-3.1, +1.1, +2.9)),
    ((-3.0, +3.0, +1.0), (-3.0, +3.0, -1.0), (-3.1, +2.9, -0.8), (-3.1, +2.9, +0.8)),
    ((-3.0, +3.0, -1.0), (-3.0, +1.0, -1.0), (-3.1, +1.1, -0.8), (-3.1, +2.9, -0.8)),
    ((-3.0, +1.0, -1.0), (-3.0, +1.0, +1.0), (-3.1, +1.1, +0.8), (-3.1, +1.1, -0.8)),
    ((-3.0, +1.0, +1.0), (-3.0, +3.0, +1.0), (-3.1, +2.9, +0.8), (-3.1, +1.1, +0.8)),
    ((-3.1, +2.9, +0.8), (-3.1, +2.9, -0.8), (-3.1, +1.1, -0.8), (-3.1, +1.1, +0.8)),
    ((-3.0, +3.0, -1.0), (-3.0, +3.0, -3.0), (-3.1, +2.9, -2.9), (-3.1, +2.9, -1.1)),
    ((-3.0, +3.0, -3.0), (-3.0, +1.0, -3.0), (-3.1, +1.1, -2.9), (-3.1, +2.9, -2.9)),
    ((-3.0, +1.0, -3.0), (-3.0, +1.0, -1.0), (-3.1, +1.1, -1.1), (-3.1, +1.1, -2.9)),
    ((-3.0, +1.0, -1.0), (-3.0, +3.0, -1.0), (-3.1, +2.9, -1.1), (-3.1, +1.1, -1.1)),
    ((-3.1, +2.9, -1.1), (-3.1, +2.9, -2.9), (-3.1, +1.1, -2.9), (-3.1, +1.1, -1.1)),

    ((-3.0, +1.0, +3.0), (-3.0, +1.0, +1.0), (-3.1, +0.8, +1.1), (-3.1, +0.8, +2.9)),
    ((-3.0, +1.0, +1.0), (-3.0, -1.0, +1.0), (-3.1, -0.8, +1.1), (-3.1, +0.8, +1.1)),
    ((-3.0, -1.0, +1.0), (-3.0, -1.0, +3.0), (-3.1, -0.8, +2.9), (-3.1, -0.8, +1.1)),
    ((-3.0, -1.0, +3.0), (-3.0, +1.0, +3.0), (-3.1, +0.8, +2.9), (-3.1, -0.8, +2.9)),
    ((-3.1, +0.8, +2.9), (-3.1, +0.8, +1.1), (-3.1, -0.8, +1.1), (-3.1, -0.8, +2.9)),
    ((-3.0, +1.0, +1.0), (-3.0, +1.0, -1.0), (-3.1, +0.8, -0.8), (-3.1, +0.8, +0.8)),
    ((-3.0, +1.0, -1.0), (-3.0, -1.0, -1.0), (-3.1, -0.8, -0.8), (-3.1, +0.8, -0.8)),
    ((-3.0, -1.0, -1.0), (-3.0, -1.0, +1.0), (-3.1, -0.8, +0.8), (-3.1, -0.8, -0.8)),
    ((-3.0, -1.0, +1.0), (-3.0, +1.0, +1.0), (-3.1, +0.8, +0.8), (-3.1, -0.8, +0.8)),
    ((-3.1, +0.8, +0.8), (-3.1, +0.8, -0.8), (-3.1, -0.8, -0.8), (-3.1, -0.8, +0.8)),
    ((-3.0, +1.0, -1.0), (-3.0, +1.0, -3.0), (-3.1, +0.8, -2.9), (-3.1, +0.8, -1.1)),
    ((-3.0, +1.0, -3.0), (-3.0, -1.0, -3.0), (-3.1, -0.8, -2.9), (-3.1, +0.8, -2.9)),
    ((-3.0, -1.0, -3.0), (-3.0, -1.0, -1.0), (-3.1, -0.8, -1.1), (-3.1, -0.8, -2.9)),
    ((-3.0, -1.0, -1.0), (-3.0, +1.0, -1.0), (-3.1, +0.8, -1.1), (-3.1, -0.8, -1.1)),
    ((-3.1, +0.8, -1.1), (-3.1, +0.8, -2.9), (-3.1, -0.8, -2.9), (-3.1, -0.8, -1.1)),

    ((-3.0, -1.0, +3.0), (-3.0, -1.0, +1.0), (-3.1, -1.1, +1.1), (-3.1, -1.1, +2.9)),
    ((-3.0, -1.0, +1.0), (-3.0, -3.0, +1.0), (-3.1, -2.9, +1.1), (-3.1, -1.1, +1.1)),
    ((-3.0, -3.0, +1.0), (-3.0, -3.0, +3.0), (-3.1, -2.9, +2.9), (-3.1, -2.9, +1.1)),
    ((-3.0, -3.0, +3.0), (-3.0, -1.0, +3.0), (-3.1, -1.1, +2.9), (-3.1, -2.9, +2.9)),
    ((-3.1, -1.1, +2.9), (-3.1, -1.1, +1.1), (-3.1, -2.9, +1.1), (-3.1, -2.9, +2.9)),
    ((-3.0, -1.0, +1.0), (-3.0, -1.0, -1.0), (-3.1, -1.1, -0.8), (-3.1, -1.1, +0.8)),
    ((-3.0, -1.0, -1.0), (-3.0, -3.0, -1.0), (-3.1, -2.9, -0.8), (-3.1, -1.1, -0.8)),
    ((-3.0, -3.0, -1.0), (-3.0, -3.0, +1.0), (-3.1, -2.9, +0.8), (-3.1, -2.9, -0.8)),
    ((-3.0, -3.0, +1.0), (-3.0, -1.0, +1.0), (-3.1, -1.1, +0.8), (-3.1, -2.9, +0.8)),
    ((-3.1, -1.1, +0.8), (-3.1, -1.1, -0.8), (-3.1, -2.9, -0.8), (-3.1, -2.9, +0.8)),
    ((-3.0, -1.0, -1.0), (-3.0, -1.0, -3.0), (-3.1, -1.1, -2.9), (-3.1, -1.1, -1.1)),
    ((-3.0, -1.0, -3.0), (-3.0, -3.0, -3.0), (-3.1, -2.9, -2.9), (-3.1, -1.1, -2.9)),
    ((-3.0, -3.0, -3.0), (-3.0, -3.0, -1.0), (-3.1, -2.9, -1.1), (-3.1, -2.9, -2.9)),
    ((-3.0, -3.0, -1.0), (-3.0, -1.0, -1.0), (-3.1, -1.1, -1.1), (-3.1, -2.9, -1.1)),
    ((-3.1, -1.1, -1.1), (-3.1, -1.1, -2.9), (-3.1, -2.9, -2.9), (-3.1, -2.9, -1.1)),

    // face 6
    ((-3.0, -3.0, -3.0), (-1.0, -3.0, -3.0), (-1.1, -3.1, -2.9), (-2.9, -3.1, -2.9)),
    ((-1.0, -3.0, -3.0), (-1.0, -3.0, -1.0), (-1.1, -3.1, -1.1), (-1.1, -3.1, -2.9)),
    ((-1.0, -3.0, -1.0), (-3.0, -3.0, -1.0), (-2.9, -3.1, -1.1), (-1.1, -3.1, -1.1)),
    ((-3.0, -3.0, -1.0), (-3.0, -3.0, -3.0), (-2.9, -3.1, -2.9), (-2.9, -3.1, -1.1)),
    ((-2.9, -3.1, -2.9), (-1.1, -3.1, -2.9), (-1.1, -3.1, -1.1), (-2.9, -3.1, -1.1)),
    ((-1.0, -3.0, -3.0), (+1.0, -3.0, -3.0), (+0.8, -3.1, -2.9), (-0.8, -3.1, -2.9)),
    ((+1.0, -3.0, -3.0), (+1.0, -3.0, -1.0), (+0.8, -3.1, -1.1), (+0.8, -3.1, -2.9)),
    ((+1.0, -3.0, -1.0), (-1.0, -3.0, -1.0), (-0.8, -3.1, -1.1), (+0.8, -3.1, -1.1)),
    ((-1.0, -3.0, -1.0), (-1.0, -3.0, -3.0), (-0.8, -3.1, -2.9), (-0.8, -3.1, -1.1)),
    ((-0.8, -3.1, -2.9), (+0.8, -3.1, -2.9), (+0.8, -3.1, -1.1), (-0.8, -3.1, -1.1)),
    ((+1.0, -3.0, -3.0), (+3.0, -3.0, -3.0), (+2.9, -3.1, -2.9), (+1.1, -3.1, -2.9)),
    ((+3.0, -3.0, -3.0), (+3.0, -3.0, -1.0), (+2.9, -3.1, -1.1), (+2.9, -3.1, -2.9)),
    ((+3.0, -3.0, -1.0), (+1.0, -3.0, -1.0), (+1.1, -3.1, -1.1), (+2.9, -3.1, -1.1)),
    ((+1.0, -3.0, -1.0), (+1.0, -3.0, -3.0), (+1.1, -3.1, -2.9), (+1.1, -3.1, -1.1)),
    ((+1.1, -3.1, -2.9), (+2.9, -3.1, -2.9), (+2.9, -3.1, -1.1), (+1.1, -3.1, -1.1)),

    ((-3.0, -3.0, -1.0), (-1.0, -3.0, -1.0), (-1.1, -3.1, -0.8), (-2.9, -3.1, -0.8)),
    ((-1.0, -3.0, -1.0), (-1.0, -3.0, +1.0), (-1.1, -3.1, +0.8), (-1.1, -3.1, -0.8)),
    ((-1.0, -3.0, +1.0), (-3.0, -3.0, +1.0), (-2.9, -3.1, +0.8), (-1.1, -3.1, +0.8)),
    ((-3.0, -3.0, +1.0), (-3.0, -3.0, -1.0), (-2.9, -3.1, -0.8), (-2.9, -3.1, +0.8)),
    ((-2.9, -3.1, -0.8), (-1.1, -3.1, -0.8), (-1.1, -3.1, +0.8), (-2.9, -3.1, +0.8)),
    ((-1.0, -3.0, -1.0), (+1.0, -3.0, -1.0), (+0.8, -3.1, -0.8), (-0.8, -3.1, -0.8)),
    ((+1.0, -3.0, -1.0), (+1.0, -3.0, +1.0), (+0.8, -3.1, +0.8), (+0.8, -3.1, -0.8)),
    ((+1.0, -3.0, +1.0), (-1.0, -3.0, +1.0), (-0.8, -3.1, +0.8), (+0.8, -3.1, +0.8)),
    ((-1.0, -3.0, +1.0), (-1.0, -3.0, -1.0), (-0.8, -3.1, -0.8), (-0.8, -3.1, +0.8)),
    ((-0.8, -3.1, -0.8), (+0.8, -3.1, -0.8), (+0.8, -3.1, +0.8), (-0.8, -3.1, +0.8)),
    ((+1.0, -3.0, -1.0), (+3.0, -3.0, -1.0), (+2.9, -3.1, -0.8), (+1.1, -3.1, -0.8)),
    ((+3.0, -3.0, -1.0), (+3.0, -3.0, +1.0), (+2.9, -3.1, +0.8), (+2.9, -3.1, -0.8)),
    ((+3.0, -3.0, +1.0), (+1.0, -3.0, +1.0), (+1.1, -3.1, +0.8), (+2.9, -3.1, +0.8)),
    ((+1.0, -3.0, +1.0), (+1.0, -3.0, -1.0), (+1.1, -3.1, -0.8), (+1.1, -3.1, +0.8)),
    ((+1.1, -3.1, -0.8), (+2.9, -3.1, -0.8), (+2.9, -3.1, +0.8), (+1.1, -3.1, +0.8)),

    ((-3.0, -3.0, +1.0), (-1.0, -3.0, +1.0), (-1.1, -3.1, +1.1), (-2.9, -3.1, +1.1)),
    ((-1.0, -3.0, +1.0), (-1.0, -3.0, +3.0), (-1.1, -3.1, +2.9), (-1.1, -3.1, +1.1)),
    ((-1.0, -3.0, +3.0), (-3.0, -3.0, +3.0), (-2.9, -3.1, +2.9), (-1.1, -3.1, +2.9)),
    ((-3.0, -3.0, +3.0), (-3.0, -3.0, +1.0), (-2.9, -3.1, +1.1), (-2.9, -3.1, +2.9)),
    ((-2.9, -3.1, +1.1), (-1.1, -3.1, +1.1), (-1.1, -3.1, +2.9), (-2.9, -3.1, +2.9)),
    ((-1.0, -3.0, +1.0), (+1.0, -3.0, +1.0), (+0.8, -3.1, +1.1), (-0.8, -3.1, +1.1)),
    ((+1.0, -3.0, +1.0), (+1.0, -3.0, +3.0), (+0.8, -3.1, +2.9), (+0.8, -3.1, +1.1)),
    ((+1.0, -3.0, +3.0), (-1.0, -3.0, +3.0), (-0.8, -3.1, +2.9), (+0.8, -3.1, +2.9)),
    ((-1.0, -3.0, +3.0), (-1.0, -3.0, +1.0), (-0.8, -3.1, +1.1), (-0.8, -3.1, +2.9)),
    ((-0.8, -3.1, +1.1), (+0.8, -3.1, +1.1), (+0.8, -3.1, +2.9), (-0.8, -3.1, +2.9)),
    ((+1.0, -3.0, +1.0), (+3.0, -3.0, +1.0), (+2.9, -3.1, +1.1), (+1.1, -3.1, +1.1)),
    ((+3.0, -3.0, +1.0), (+3.0, -3.0, +3.0), (+2.9, -3.1, +2.9), (+2.9, -3.1, +1.1)),
    ((+3.0, -3.0, +3.0), (+1.0, -3.0, +3.0), (+1.1, -3.1, +2.9), (+2.9, -3.1, +2.9)),
    ((+1.0, -3.0, +3.0), (+1.0, -3.0, +1.0), (+1.1, -3.1, +1.1), (+1.1, -3.1, +2.9)),
    ((+1.1, -3.1, +1.1), (+2.9, -3.1, +1.1), (+2.9, -3.1, +2.9), (+1.1, -3.1, +2.9))


    );

implementation

end.
