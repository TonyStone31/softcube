unit nxn_min2phase_defs;
// Two-Phase Algorithm definitions for 3x3 Rubik's Cube
// Ported from Herbert Kociemba's CubeExplorer (CubeDefs.pas)
// Stripped of all GUI dependencies

{$mode objfpc}{$H+}

interface

const
  N_CORNER = 8;
  N_EDGE = 12;
  N_FACE = 6;
  N_MOVE = 18;      // 6 axes × 3 powers (X, X2, X')
  N_TWIST = 2187;   // 3^7 corner orientations
  N_FLIP = 2048;    // 2^11 edge orientations
  N_SLICE = 495;    // C(12,4) UD-slice positions
  N_FLIPSLICE_CLASS = 64430;  // FlipUDSlice sym-coord classes
  N_CORNERS_PERM = 40320;     // 8! corner permutations
  N_UD_EDGES_PERM = 40320;    // 8! UD-edge permutations
  N_SLICE_SORTED = 11880;     // C(12,4)*4! = 495*24
  N_SYM = 48;       // Total symmetries
  N_SYM16 = 16;     // Symmetries preserving UD axis
  N_CORNPERM_CLASS = 2768;    // CornPerm sym-coord classes

  // Phase 2 moves: U, U2, U', D, D2, D', R2, F2, L2, B2
  N_MOVE_P2 = 10;

type
  // Corners: URF=0, UFL=1, ULB=2, UBR=3, DFR=4, DLF=5, DBL=6, DRB=7
  TCorner = 0..7;
  // Edges: UR=0, UF=1, UL=2, UB=3, DR=4, DF=5, DL=6, DB=7, FR=8, FL=9, BL=10, BR=11
  TEdge = 0..11;
  // Axes: U=0, R=1, F=2, D=3, L=4, B=5
  TAxis = 0..5;

  TOrientedCorner = record
    c: byte;   // corner identity (0..7)
    o: shortint; // orientation (0, 1, 2; or 3..5 for reflections)
  end;
  TOrientedEdge = record
    e: byte;   // edge identity (0..11)
    o: shortint; // orientation (0 or 1)
  end;
  TCornerCubie = array[0..7] of TOrientedCorner;
  TEdgeCubie = array[0..11] of TOrientedEdge;

  TCubieCube = record
    corn: TCornerCubie;
    edge: TEdgeCubie;
  end;

const
  // Corner names
  URF = 0; UFL = 1; ULB = 2; UBR = 3;
  DFR = 4; DLF = 5; DBL = 6; DRB = 7;
  // Edge names
  cUR = 0; cUF = 1; cUL = 2; cUB = 3;
  cDR = 4; cDF = 5; cDL = 6; cDB = 7;
  cFR = 8; cFL = 9; cBL = 10; cBR = 11;
  // Axis names
  axU = 0; axR = 1; axF = 2; axD = 3; axL = 4; axB = 5;

  // Corner facelets: for each corner, the 3 facelet positions (0-indexed in URFDLB string)
  CF: array[0..7, 0..2] of byte = (
    (8, 9, 20),    // URF: U9, R1, F3
    (6, 18, 38),   // UFL: U7, F1, L3
    (0, 36, 47),   // ULB: U1, L1, B3
    (2, 45, 11),   // UBR: U3, B1, R3
    (29, 26, 15),  // DFR: D3, F9, R7
    (27, 44, 24),  // DLF: D1, L9, F7
    (33, 53, 42),  // DBL: D7, B9, L7
    (35, 17, 51)   // DRB: D9, R9, B7
  );

  // Edge facelets: for each edge, the 2 facelet positions
  EF: array[0..11, 0..1] of byte = (
    (5, 10),   // UR: U6, R2
    (7, 19),   // UF: U8, F2
    (3, 37),   // UL: U4, L2
    (1, 46),   // UB: U2, B2
    (32, 16),  // DR: D6, R8
    (28, 25),  // DF: D2, F8
    (30, 43),  // DL: D4, L8
    (34, 52),  // DB: D8, B8
    (23, 12),  // FR: F6, R4
    (21, 41),  // FL: F4, L6
    (50, 39),  // BL: B6, L4
    (48, 14)   // BR: B4, R6
  );

  // Corner color indices: which face color each corner facelet belongs to
  // Color 0=U, 1=R, 2=F, 3=D, 4=L, 5=B
  CCI: array[0..7, 0..2] of byte = (
    (0, 1, 2), // URF
    (0, 2, 4), // UFL
    (0, 4, 5), // ULB
    (0, 5, 1), // UBR
    (3, 2, 1), // DFR
    (3, 4, 2), // DLF
    (3, 5, 4), // DBL
    (3, 1, 5)  // DRB
  );

  // Edge color indices
  ECI: array[0..11, 0..1] of byte = (
    (0, 1), // UR
    (0, 2), // UF
    (0, 4), // UL
    (0, 5), // UB
    (3, 1), // DR
    (3, 2), // DF
    (3, 4), // DL
    (3, 5), // DB
    (2, 1), // FR
    (2, 4), // FL
    (5, 4), // BL
    (5, 1)  // BR
  );

  // How corner cubies change under each face turn (U, R, F, D, L, B)
  CornerCubieMove: array[0..5] of TCornerCubie = (
    // U
    ((c:3;o:0),(c:0;o:0),(c:1;o:0),(c:2;o:0),(c:4;o:0),(c:5;o:0),(c:6;o:0),(c:7;o:0)),
    // R
    ((c:4;o:2),(c:1;o:0),(c:2;o:0),(c:0;o:1),(c:7;o:1),(c:5;o:0),(c:6;o:0),(c:3;o:2)),
    // F
    ((c:1;o:1),(c:5;o:2),(c:2;o:0),(c:3;o:0),(c:0;o:2),(c:4;o:1),(c:6;o:0),(c:7;o:0)),
    // D
    ((c:0;o:0),(c:1;o:0),(c:2;o:0),(c:3;o:0),(c:5;o:0),(c:6;o:0),(c:7;o:0),(c:4;o:0)),
    // L
    ((c:0;o:0),(c:2;o:1),(c:6;o:2),(c:3;o:0),(c:4;o:0),(c:1;o:2),(c:5;o:1),(c:7;o:0)),
    // B
    ((c:0;o:0),(c:1;o:0),(c:3;o:1),(c:7;o:2),(c:4;o:0),(c:5;o:0),(c:2;o:2),(c:6;o:1))
  );

  // How edge cubies change under each face turn
  EdgeCubieMove: array[0..5] of TEdgeCubie = (
    // U: UB<-UR<-UF<-UL<-UB
    ((e:3;o:0),(e:0;o:0),(e:1;o:0),(e:2;o:0),(e:4;o:0),(e:5;o:0),(e:6;o:0),(e:7;o:0),(e:8;o:0),(e:9;o:0),(e:10;o:0),(e:11;o:0)),
    // R: FR<-UR<-BR<-DR<-FR
    ((e:8;o:0),(e:1;o:0),(e:2;o:0),(e:3;o:0),(e:11;o:0),(e:5;o:0),(e:6;o:0),(e:7;o:0),(e:4;o:0),(e:9;o:0),(e:10;o:0),(e:0;o:0)),
    // F
    ((e:0;o:0),(e:9;o:1),(e:2;o:0),(e:3;o:0),(e:4;o:0),(e:8;o:1),(e:6;o:0),(e:7;o:0),(e:1;o:1),(e:5;o:1),(e:10;o:0),(e:11;o:0)),
    // D
    ((e:0;o:0),(e:1;o:0),(e:2;o:0),(e:3;o:0),(e:5;o:0),(e:6;o:0),(e:7;o:0),(e:4;o:0),(e:8;o:0),(e:9;o:0),(e:10;o:0),(e:11;o:0)),
    // L
    ((e:0;o:0),(e:1;o:0),(e:10;o:0),(e:3;o:0),(e:4;o:0),(e:5;o:0),(e:9;o:0),(e:7;o:0),(e:8;o:0),(e:2;o:0),(e:6;o:0),(e:11;o:0)),
    // B
    ((e:0;o:0),(e:1;o:0),(e:2;o:0),(e:11;o:1),(e:4;o:0),(e:5;o:0),(e:6;o:0),(e:10;o:1),(e:8;o:0),(e:9;o:0),(e:3;o:1),(e:7;o:1))
  );

  // Symmetry generators for corners (S_URF3, S_F2, S_U4, S_LR2)
  CornerCubieSym: array[0..3] of TCornerCubie = (
    // S_URF3
    ((c:0;o:1),(c:4;o:2),(c:5;o:1),(c:1;o:2),(c:3;o:2),(c:7;o:1),(c:6;o:2),(c:2;o:1)),
    // S_F2
    ((c:5;o:0),(c:4;o:0),(c:7;o:0),(c:6;o:0),(c:1;o:0),(c:0;o:0),(c:3;o:0),(c:2;o:0)),
    // S_U4
    ((c:3;o:0),(c:0;o:0),(c:1;o:0),(c:2;o:0),(c:7;o:0),(c:4;o:0),(c:5;o:0),(c:6;o:0)),
    // S_LR2
    ((c:1;o:3),(c:0;o:3),(c:3;o:3),(c:2;o:3),(c:5;o:3),(c:4;o:3),(c:7;o:3),(c:6;o:3))
  );

  // Symmetry generators for edges
  EdgeCubieSym: array[0..3] of TEdgeCubie = (
    // S_URF3
    ((e:1;o:1),(e:8;o:0),(e:5;o:1),(e:9;o:0),(e:3;o:1),(e:11;o:0),(e:7;o:1),(e:10;o:0),(e:0;o:1),(e:4;o:1),(e:6;o:1),(e:2;o:1)),
    // S_F2
    ((e:6;o:0),(e:5;o:0),(e:4;o:0),(e:7;o:0),(e:2;o:0),(e:1;o:0),(e:0;o:0),(e:3;o:0),(e:9;o:0),(e:8;o:0),(e:11;o:0),(e:10;o:0)),
    // S_U4
    ((e:3;o:0),(e:0;o:0),(e:1;o:0),(e:2;o:0),(e:7;o:0),(e:4;o:0),(e:5;o:0),(e:6;o:0),(e:11;o:1),(e:8;o:1),(e:9;o:1),(e:10;o:1)),
    // S_LR2
    ((e:2;o:0),(e:1;o:0),(e:0;o:0),(e:3;o:0),(e:6;o:0),(e:5;o:0),(e:4;o:0),(e:7;o:0),(e:9;o:0),(e:8;o:0),(e:11;o:0),(e:10;o:0))
  );

  // Phase 2 move indices within the 18-move array
  // U(0), U2(1), U'(2), D(9), D2(10), D'(11), R2(4), F2(7), L2(13), B2(16)
  Phase2MoveIdx: array[0..9] of integer = (0, 1, 2, 9, 10, 11, 4, 7, 13, 16);

  // Move names for output
  MoveNames: array[0..17] of string = (
    'U', 'U2', 'U''', 'R', 'R2', 'R''', 'F', 'F2', 'F''',
    'D', 'D2', 'D''', 'L', 'L2', 'L''', 'B', 'B2', 'B'''
  );

// Binomial coefficient C(n,k)
function Cnk(n, k: integer): integer;

// Corner cubie multiplication: prod = a * b
procedure CornMult(const a, b: TCornerCubie; out prod: TCornerCubie);
// Edge cubie multiplication: prod = a * b
procedure EdgeMult(const a, b: TEdgeCubie; out prod: TEdgeCubie);
// Corner cubie inverse
procedure CornInv(const a: TCornerCubie; out inv: TCornerCubie);
// Edge cubie inverse
procedure EdgeInv(const a: TEdgeCubie; out inv: TEdgeCubie);

// Full cube multiplication
procedure CubieMult(const a, b: TCubieCube; out prod: TCubieCube);

// Create identity cube
procedure CubieIdentity(out cc: TCubieCube);

// Apply a face move (axis 0..5) to a cubie cube
procedure CubieMove(var cc: TCubieCube; axis: integer);

// Apply a symmetry generator (0..3) to a cubie cube: cc := sym * cc
procedure CubieSymMult(var cc: TCubieCube; symGen: integer);

implementation

function Cnk(n, k: integer): integer;
var
  s, j: integer;
begin
  if n < k then begin Result := 0; exit; end;
  s := 1;
  if k > n div 2 then k := n - k;
  for j := 1 to k do
  begin
    s := (s * n) div j;
    Dec(n);
  end;
  Result := s;
end;

procedure CornMult(const a, b: TCornerCubie; out prod: TCornerCubie);
var
  co: integer;
  oriA, oriB, ori: shortint;
begin
  for co := 0 to 7 do
  begin
    prod[co].c := a[b[co].c].c;
    oriA := a[b[co].c].o;
    oriB := b[co].o;
    if (oriA < 3) and (oriB < 3) then
    begin
      ori := oriA + oriB;
      if ori >= 3 then ori := ori - 3;
    end
    else if (oriA < 3) and (oriB >= 3) then
    begin
      ori := oriA + oriB;
      if ori >= 6 then ori := ori - 3;
    end
    else if (oriA >= 3) and (oriB < 3) then
    begin
      ori := oriA - oriB;
      if ori < 3 then ori := ori + 3;
    end
    else // both >= 3
    begin
      ori := oriA - oriB;
      if ori < 0 then ori := ori + 3;
    end;
    prod[co].o := ori;
  end;
end;

procedure EdgeMult(const a, b: TEdgeCubie; out prod: TEdgeCubie);
var
  ed: integer;
  ori: shortint;
begin
  for ed := 0 to 11 do
  begin
    prod[ed].e := a[b[ed].e].e;
    ori := b[ed].o + a[b[ed].e].o;
    if ori = 2 then ori := 0;
    prod[ed].o := ori;
  end;
end;

procedure CornInv(const a: TCornerCubie; out inv: TCornerCubie);
var
  co: integer;
  ori: shortint;
begin
  for co := 0 to 7 do
    inv[a[co].c].c := co;
  for co := 0 to 7 do
  begin
    ori := a[inv[co].c].o;
    if ori >= 3 then
      inv[co].o := ori
    else
    begin
      inv[co].o := -ori;
      if inv[co].o < 0 then inv[co].o := inv[co].o + 3;
    end;
  end;
end;

procedure EdgeInv(const a: TEdgeCubie; out inv: TEdgeCubie);
var
  ed: integer;
begin
  for ed := 0 to 11 do
    inv[a[ed].e].e := ed;
  for ed := 0 to 11 do
    inv[ed].o := a[inv[ed].e].o;
end;

procedure CubieMult(const a, b: TCubieCube; out prod: TCubieCube);
begin
  CornMult(a.corn, b.corn, prod.corn);
  EdgeMult(a.edge, b.edge, prod.edge);
end;

procedure CubieIdentity(out cc: TCubieCube);
var
  i: integer;
begin
  for i := 0 to 7 do begin cc.corn[i].c := i; cc.corn[i].o := 0; end;
  for i := 0 to 11 do begin cc.edge[i].e := i; cc.edge[i].o := 0; end;
end;

procedure CubieMove(var cc: TCubieCube; axis: integer);
var
  tmp: TCubieCube;
begin
  tmp := cc;
  CornMult(tmp.corn, CornerCubieMove[axis], cc.corn);
  EdgeMult(tmp.edge, EdgeCubieMove[axis], cc.edge);
end;

procedure CubieSymMult(var cc: TCubieCube; symGen: integer);
var
  tmp: TCubieCube;
begin
  tmp := cc;
  CornMult(tmp.corn, CornerCubieSym[symGen], cc.corn);
  EdgeMult(tmp.edge, EdgeCubieSym[symGen], cc.edge);
end;

end.
