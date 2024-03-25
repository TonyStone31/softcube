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

  {
  EDGE_POSITIONS: Defines the positions of the edge pieces on the Rubik's Cube. Each entry in the array
  represents a specific edge piece and its orientation relative to the cube's faces.

  The array is structured as follows:
  (Face number, Position on face, Adjacent face number, Position on adjacent face, Internal edge index)

  - The first two integers represent the face number (1 to 6, corresponding to U, F, R, B, L, D) and the
  position of the edge piece on that face (0 to 7, following the cube's edge enumeration pattern).
  - The next two integers similarly represent the adjacent face and position of the same edge piece,
  indicating how edge pieces bridge two faces.
  - The final integer is an internal index used to uniquely identify the edge piece throughout the cube's
  transformation processes.

  For example, the entry (1, 7, 2, 1, 12) represents an edge piece that is at position 7 on face 1 (U) and at
  position 1 on face 2 (F), with an internal index of 12.

  This array is essential for algorithms that solve or manipulate the cube, allowing for precise tracking and
  manipulation of the cube's edge pieces as moves are applied. It supports the identification of edge pieces'
  locations and their orientations, which is crucial for determining the next steps in a solving algorithm or
  for visualizing the cube's state.
}


  EDGE_POSITIONS: array[0..23, 0..4] of integer =
    ((1, 7, 2, 1, 12), (1, 5, 3, 1, 13), (1, 1, 4, 1, 14), (1, 3, 5, 1, 15),
    (2, 1, 1, 7, 21), (2, 5, 3, 3, 23), (2, 3, 5, 5, 25), (2, 7, 6, 1, 26),
    (3, 1, 1, 5, 31), (3, 3, 2, 5, 32), (3, 5, 4, 3, 34), (3, 7, 6, 5, 36),
    (4, 1, 1, 1, 41), (4, 3, 3, 5, 43), (4, 5, 5, 3, 45), (4, 7, 6, 7, 46),
    (5, 1, 1, 3, 51), (5, 5, 2, 3, 52), (5, 3, 4, 5, 54), (5, 7, 6, 3, 56),
    (6, 1, 2, 7, 62), (6, 5, 3, 7, 63), (6, 7, 4, 7, 64), (6, 3, 5, 7, 65));

  {
    CORNER_POSITION array outlines the positions of the corner pieces on the Rubik's Cube
    relative to their orientation across three adjacent faces. Each sub-array represents a corner
    piece and is detailed in the format (Face1, Position1, Face2, Position2, Face3, Position3, CornerIndex),
    where:

    - Face1, Face2, and Face3 are the indices (1-6) of the three faces that converge at the corner.
    - Position1, Position2, and Position3 describe the specific position of the corner piece on each of those faces.
    - CornerIndex is a sequence that represents the orientation or permutation of the corner across the faces,
      helping in identifying the specific corner piece and its orientation.

    This structure is vital for solving algorithms to understand and adjust the orientation of corner pieces,
    facilitating the process of solving the cube by recognizing and correcting corner positions and orientations.
  }



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
  FACE_NAMES: array[0..6] of string = ('void', 'U', 'F', 'R', 'B', 'L', 'D');

type

  TunitRubik = packed array[1..6, 0..2, 0..2] of byte;
  TLinRubik = packed array[0..53] of byte;
  TFaceRubik = packed array[1..6, 0..8] of byte;

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




  C_COLOR: array[0..6] of tcolor = (ClSilver, ClWhite, ClLime, ClRed, ClBlue, $0080FF, ClYellow);
  //C_CUBE_SIZE = 24;
  C_CUBE_COMPLETE: TRubik =
    ((1, 1, 1, 1, 1, 1, 1, 1, 1),
    (2, 2, 2, 2, 2, 2, 2, 2, 2),
    (3, 3, 3, 3, 3, 3, 3, 3, 3),
    (4, 4, 4, 4, 4, 4, 4, 4, 4),
    (5, 5, 5, 5, 5, 5, 5, 5, 5),
    (6, 6, 6, 6, 6, 6, 6, 6, 6));

 {

                 U00 U01 U02
                 U03 U04 U05
                 U06 U07 U08

   L09 L10 L11   F18 F19 F20  R27 R28 R29    B36 B37 B38
   L12 L13 L14   F21 F22 F23  R30 R31 R32    B39 B40 B41
   L15 L16 L17   F24 F25 F26  R33 R34 R35    B42 B43 B44

                 D45 D46 D47
                 D48 D49 D50
                 D51 D52 D53

 }




implementation

end.
