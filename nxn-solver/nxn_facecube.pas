unit nxn_facecube;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, nxn_cubedefs;

function Cnk(n, k: integer): integer;

type
  faceletCube = class
  private
  public
    size: integer; // number of cubies on an edge
    origSize: integer; // original cube size before even→odd padding
    pix: integer; // number of pixels for 1/3 facelet
    // face 0..5,row index 0..size-1, column index 0..size-1
    faceCols: array of array of array of ColorIndex;
    cubiCorn: array [URF .. DRB] of OrientedCorner;
    cubiEdge: array [UR .. BR] of OrientedEdge;
    ecls: array of array of byte; //array for edgeclusters
    ns: integer;//for counting purposes
    moveLog: string; // accumulated move notation for external collection
    fallbackAttempt: integer; // cross-cycle fallback counter to vary face moves
    dispU: integer; // net xU quarter-turn displacement for ImprovePosition search
    dispD: integer; // net xD quarter-turn displacement for ImprovePosition search
    improveOrb: integer; // orbit being improved (for badAllTotal check)
    badAllStart: integer; // starting badAllTotal for oscillation prevention
    ph5LastTick: QWord; // last progress report tick for Phase 5
    ph5StartTick: QWord; // Phase 5 start tick
    ph5TotalSlots: integer; // total edge slots for progress reporting
    ph5InitialBad: integer; // initial bad slots count


    mvIdx: integer; // Helper variables for search function
    found: boolean;
    fxymoves: array [0 .. 25] of moves; // f-moves und xy-slice moves
    // permutations of the remapped edgecluster (0,y). filled with getEdgeCluster(y)

    function getSize: integer;
    procedure initCluster(x, y: integer);
    // slice move 0<=slice<size
    procedure move(a: Axis; slice: integer);
    // Wendet die Symmetrie auf das Cluster (x,y) an
    procedure applySymmetry(x, y: integer; s: Symmetry);
    procedure applySymmetryByIndex(x, y: integer; idx: integer);
    procedure applyInvSymmetryByIndex(x, y: integer; idx: integer);
    //create corners on the cubie level
    function setCornerCubies: CornerStatus;
    //create middle edges on the cubie level
    function setMiddleEdgeCubies: EdgeStatus;
    function cornerParityEven: boolean;
    function edgeParity(y: integer): integer;

    // phase 1
    function nextMovePh1(idx: integer; currMove: moves): moves;
    function Phase1CenterCoord(x, y: integer): integer; // 0<=cc<735471
    procedure InvPhase1CenterCoord(cc, x, y: integer);

    function Phase1Brick256Coord(x, y: integer): integer;
    procedure InvPhase1Brick256Coord(cc, x, y: integer);

    function MakeUDPlusCross1(x: integer): boolean; // Findet Zugfolge
    procedure SearchUDPlusCross1(cc, togo: integer);

    function MakeUDXCross(x: integer): boolean;
    procedure SearchUDXCross(ccx, slx, togo: integer);

    // phase 2
    function nextMovePh2(idx: integer; currMove: moves): moves;
    function Phase2CenterCoord(x, y: integer): integer; // 0<=cc<12870
    procedure InvPhase2CenterCoord(cc, x, y: integer);

    function Phase2SliceCoord(x, y: integer): integer; // 0<=cc<16
    procedure InvPhase2SliceCoord(cc, x, y: integer);

    function MakeFBPlusCross(x: integer): boolean; // Findet Zugfolge
    procedure SearchFBPlusCross(cc, ep, togo: integer);
    function MakeFBFullCenter(x, y: integer): boolean;
    procedure SearchFBFullCenter(ccx, slx, ccy, togo: integer);
    function MakeFBXCross(x: integer): boolean;
    procedure SearchFBXCross(ccx, sly, togo: integer);

    // phase 3
    function nextMovePh3(idx: integer; currMove: moves): moves;
    function Phase3CenterCoord(x, y: integer; a: Axis): integer;
    procedure InvPhase3CenterCoord(cc, x, y: integer; a: Axis);
    function Ph3RLFBCenterCoord(x, y: integer): UInt16;
    procedure InvPh3RLFBCenterCoord(cc, x, y: integer);

    function getPh3Brick702RLFBCentDepth(x, y: integer): integer;


    function Ph3Brick702Coord(x: integer): integer;
    procedure InvPh3Brick702Coord(br, x: integer);
    function MakePh3Cent702(x, y: integer): boolean;
    procedure SearchPh3Cent702(bx, by, cx, cy, dx, dy, togo: integer);
    function MakePh3XCross(x: integer): boolean;
    procedure SearchPh3XCross(bx, cx, togo: integer);
    function MakePh3RLFBPlusCross(x: integer): boolean;
    procedure SearchPh3RLFBPlusCross(bx, togo: integer);

    // phase 4
    function nextMovePh4(idx: integer; currMove: moves): moves;
    function MakePh4UDPlusCross(x: integer): boolean;
    procedure SearchPh4UDPlusCross(c, b, togo: integer);
    function MakePh4UDCenters(x, y: integer): boolean;
    procedure SearchPh4UDCenter(cx, cy, bxy, bo, togo: integer);
    function MakePh4XCross(x: integer): boolean;
    procedure SearchPh4XCross(cx, b, bo, togo: integer);

    function Phase4RLFBBrickCoord(x, y: integer): integer;
    procedure InvPhase4RLFBBrickCoord(cc, x, y: integer);

    function Phase4UDBrickCoord(x, y: integer): integer;
    procedure InvPhase4UDBrickCoord(cc, x, y: integer);

    //phase 5
    procedure edgemove(x: integer; mv: Moves);
    procedure applyEdgeMoves(x: integer);
    function MakeFledge: boolean;
    function badEdgeCnt(e: Edge): integer;
    //function badEdgeCntOrbit(orb: integer): integer;
    function badEdgeCntIdx(orb, idx: integer): integer;
    procedure store(ed: Edge);
    function AnyBadHorizontalExcept(buf: Edge): boolean;
    function badVerticalTotal(orb: integer): integer;
    function badAllTotal(orb: integer): integer;
    procedure addmove(m: Moves);
    procedure ReportPh5Progress(togo: integer; const context: string);
    function tryDirectImprove(orb, brick, badEdgesStart: integer): boolean;
    function improvePosition(orb, brick, badedgesStart, badedgesLast: integer;
      rturn, fturn, lturn, bturn: Moves; dist, togo: integer): boolean;
    function improveVertical(orb, badStart, badLast: integer;
      rturn, fturn, lturn, bturn: Moves; dist, togo: integer;
      countAll: boolean = False): boolean;



    procedure printMoves(i, j: integer);
    procedure applyMoves(i, j: integer);

    function clusterColorIndex(x, y, i: integer): ColorIndex;
    procedure setClusterColorIndex(x, y, i: integer; col: ColorIndex);
    procedure getEdgeCluster(y: integer);
    // Read edge cluster from physical position physPos, store in ecls[orbit]
    procedure getEdgeClusterAt(orbit, physPos: integer);

    constructor Create(sz: integer); overload;
    // creates cube with odd size
    constructor Create(fc: faceletCube); overload;

    function printMovesStr(i, j: integer): string;

  end;


implementation

uses Math, nxn_util, nxn_phase1_tables, nxn_phase2_tables, nxn_phase3_tables,
  nxn_phase4_tables;
// Wendet die Symmetrie mit dem Index 0..15 auf Cluster(x,y) an
// idx = 8*LR2 + 4*F2 + U4
procedure faceletCube.applySymmetryByIndex(x, y, idx: integer);
var
  s, i: integer;
begin
  s := idx div 8;
  for i := 0 to s - 1 do
    applySymmetry(x, y, S_LR2);
  idx := idx mod 8;

  s := idx div 4;
  for i := 0 to s - 1 do
    applySymmetry(x, y, S_F2);
  s := idx mod 4;

  for i := 0 to s - 1 do
    applySymmetry(x, y, S_U4);
end;

// Wendet die die inverse Symmetrie zu dem Index 0..7 auf Cluster(x,y) an
procedure faceletCube.applyInvSymmetryByIndex(x, y, idx: integer);
var
  s, i: integer;
begin
  s := 3 - idx mod 4;
  for i := 0 to s do
    applySymmetry(x, y, S_U4);

  idx := idx div 4;
  s := 1 - idx mod 2;
  for i := 0 to s do
    applySymmetry(x, y, S_F2);

  s := 1 - idx div 2;
  for i := 0 to s do
    applySymmetry(x, y, S_LR2);
end;



// next move for xy-cluster
// Dazu muss die  UDPlusCrossCoord ID sein für die Zugfolge der Länge idx-1
function faceletCube.nextMovePh1(idx: integer; currMove: moves): moves;
var
  pm: moves;
begin
  if currMove = yB3 then // done
    Exit(NoMove);
  if idx = 0 then
    Exit(Succ(currMove))
  else
  begin
    pm := fxymoves[idx - 1]; // predecessor
    while True do
    begin
      currMove := Succ(currMove);

      if currMove = NoMove then
        Exit(NoMove);

      if Ord(pm) < Ord(xU1) then //previous move is face move
      begin
        if (Ord(currMove) <= Ord(pm)) then
          //all face moves commute restricted to the centers
          continue;
        if Ord(currMove) >= Ord(xU1) then  //face move followed by slice move
          // always valid
          Exit(currMove)
        else // pm<currMove<xU1
        begin
          if Ord(pm) div 3 = Ord(currMove) div 3 then
            // same face
            continue
          else
            Exit(currMove);
        end;
      end;

      //Ord(pm) >= Ord(xU1), previous move is slice move
      if (Ord(pm) div 6) mod 3 <> (Ord(currMove) div 6) mod 3 then
        Exit(currMove);
      // pm and currMove are on different axes and hence do not commute


      // both moves are on the same axis and commute
      // we can force an order
      if Ord(currMove) <= Ord(pm) then
        continue;

      // if the  prefixes f,x,y are different for both moves, currmove is valid
      if Ord(currMove) div 18 <> Ord(pm) div 18 then
        Exit(currMove);

      // we have the same prefix and the same axis
      if (Ord(currMove) mod 6) div 3 <> (Ord(pm) mod 6) div 3 then
        // moves are on different slices of the axis
        Exit(currMove)
      else
        continue;
    end;
  end;
end;

procedure faceletCube.applySymmetry(x, y: integer; s: Symmetry);
var
  tmp: ColorIndex;
  a: Axis;
begin
  case s of

    S_URF3:
    begin
      tmp := faceCols[Ord(U), x, y];
      faceCols[Ord(U), x, y] := faceCols[Ord(F), size - 1 - y, x];
      faceCols[Ord(F), size - 1 - y, x] := faceCols[Ord(R), size - 1 - x, size - 1 - y];
      faceCols[Ord(R), size - 1 - x, size - 1 - y] := tmp;

      tmp := faceCols[Ord(D), x, y];
      faceCols[Ord(D), x, y] := faceCols[Ord(B), size - 1 - y, x];
      faceCols[Ord(B), size - 1 - y, x] := faceCols[Ord(L), x, y];
      faceCols[Ord(L), x, y] := tmp;

      if (y = x) and (size - 1 = 2 * x) then
        exit;

      tmp := faceCols[Ord(U), y, size - 1 - x];
      faceCols[Ord(U), y, size - 1 - x] := faceCols[Ord(F), x, y];
      faceCols[Ord(F), x, y] := faceCols[Ord(R), size - 1 - y, x];
      faceCols[Ord(R), size - 1 - y, x] := tmp;

      tmp := faceCols[Ord(U), size - 1 - x, size - 1 - y];
      faceCols[Ord(U), size - 1 - x, size - 1 - y] := faceCols[Ord(F), y, size - 1 - x];
      faceCols[Ord(F), y, size - 1 - x] := faceCols[Ord(R), x, y];
      faceCols[Ord(R), x, y] := tmp;

      tmp := faceCols[Ord(U), size - 1 - y, x];
      faceCols[Ord(U), size - 1 - y, x] := faceCols[Ord(F), size - 1 - x, size - 1 - y];
      faceCols[Ord(F), size - 1 - x, size - 1 - y] := faceCols[Ord(R), y, size - 1 - x];
      faceCols[Ord(R), y, size - 1 - x] := tmp;

      tmp := faceCols[Ord(D), y, size - 1 - x];
      faceCols[Ord(D), y, size - 1 - x] := faceCols[Ord(B), x, y];
      faceCols[Ord(B), x, y] := faceCols[Ord(L), y, size - 1 - x];
      faceCols[Ord(L), y, size - 1 - x] := tmp;

      tmp := faceCols[Ord(D), size - 1 - x, size - 1 - y];
      faceCols[Ord(D), size - 1 - x, size - 1 - y] := faceCols[Ord(B), y, size - 1 - x];
      faceCols[Ord(B), y, size - 1 - x] := faceCols[Ord(L), size - 1 - x, size - 1 - y];
      faceCols[Ord(L), size - 1 - x, size - 1 - y] := tmp;

      tmp := faceCols[Ord(D), size - 1 - y, x];
      faceCols[Ord(D), size - 1 - y, x] := faceCols[Ord(B), size - 1 - x, size - 1 - y];
      faceCols[Ord(B), size - 1 - x, size - 1 - y] := faceCols[Ord(L), size - 1 - y, x];
      faceCols[Ord(L), size - 1 - y, x] := tmp;
    end;

    S_F2:
    begin

      tmp := faceCols[Ord(U), x, y];
      faceCols[Ord(U), x, y] := faceCols[Ord(D), size - 1 - x, size - 1 - y];
      faceCols[Ord(D), size - 1 - x, size - 1 - y] := tmp;

      tmp := faceCols[Ord(R), x, y];
      faceCols[Ord(R), x, y] := faceCols[Ord(L), size - 1 - x, size - 1 - y];
      faceCols[Ord(L), size - 1 - x, size - 1 - y] := tmp;

      if (y = x) and (size - 1 = 2 * x) then
        exit;//center facelets for odd cubes

      tmp := faceCols[Ord(U), y, size - 1 - x];
      faceCols[Ord(U), y, size - 1 - x] := faceCols[Ord(D), size - 1 - y, x];
      faceCols[Ord(D), size - 1 - y, x] := tmp;

      tmp := faceCols[Ord(U), size - 1 - x, size - 1 - y];
      faceCols[Ord(U), size - 1 - x, size - 1 - y] := faceCols[Ord(D), x, y];
      faceCols[Ord(D), x, y] := tmp;

      tmp := faceCols[Ord(U), size - 1 - y, x];
      faceCols[Ord(U), size - 1 - y, x] := faceCols[Ord(D), y, size - 1 - x];
      faceCols[Ord(D), y, size - 1 - x] := tmp;



      tmp := faceCols[Ord(F), x, y];
      faceCols[Ord(F), x, y] := faceCols[Ord(F), size - 1 - x, size - 1 - y];
      faceCols[Ord(F), size - 1 - x, size - 1 - y] := tmp;

      tmp := faceCols[Ord(F), y, size - 1 - x];
      faceCols[Ord(F), y, size - 1 - x] := faceCols[Ord(F), size - 1 - y, x];
      faceCols[Ord(F), size - 1 - y, x] := tmp;

      tmp := faceCols[Ord(B), x, y];
      faceCols[Ord(B), x, y] := faceCols[Ord(B), size - 1 - x, size - 1 - y];
      faceCols[Ord(B), size - 1 - x, size - 1 - y] := tmp;

      tmp := faceCols[Ord(B), y, size - 1 - x];
      faceCols[Ord(B), y, size - 1 - x] := faceCols[Ord(B), size - 1 - y, x];
      faceCols[Ord(B), size - 1 - y, x] := tmp;




      tmp := faceCols[Ord(R), y, size - 1 - x];
      faceCols[Ord(R), y, size - 1 - x] := faceCols[Ord(L), size - 1 - y, x];
      faceCols[Ord(L), size - 1 - y, x] := tmp;

      tmp := faceCols[Ord(R), size - 1 - x, size - 1 - y];
      faceCols[Ord(R), size - 1 - x, size - 1 - y] := faceCols[Ord(L), x, y];
      faceCols[Ord(L), x, y] := tmp;

      tmp := faceCols[Ord(R), size - 1 - y, x];
      faceCols[Ord(R), size - 1 - y, x] := faceCols[Ord(L), y, size - 1 - x];
      faceCols[Ord(L), y, size - 1 - x] := tmp;
    end;
    S_U4:
    begin
      tmp := faceCols[Ord(U), x, y];
      faceCols[Ord(U), x, y] := faceCols[Ord(U), size - 1 - y, x];
      faceCols[Ord(U), size - 1 - y, x] :=
        faceCols[Ord(U), size - 1 - x, size - 1 - y];
      faceCols[Ord(U), size - 1 - x, size - 1 - y] :=
        faceCols[Ord(U), y, size - 1 - x];
      faceCols[Ord(U), y, size - 1 - x] := tmp;

      tmp := faceCols[Ord(D), x, y];
      faceCols[Ord(D), x, y] := faceCols[Ord(D), y, size - 1 - x];
      faceCols[Ord(D), y, size - 1 - x] :=
        faceCols[Ord(D), size - 1 - x, size - 1 - y];
      faceCols[Ord(D), size - 1 - x, size - 1 - y] :=
        faceCols[Ord(D), size - 1 - y, x];
      faceCols[Ord(D), size - 1 - y, x] := tmp;

      tmp := faceCols[Ord(F), x, y];
      faceCols[Ord(F), x, y] := faceCols[Ord(R), x, y];
      faceCols[Ord(R), x, y] := faceCols[Ord(B), x, y];
      faceCols[Ord(B), x, y] := faceCols[Ord(L), x, y];
      faceCols[Ord(L), x, y] := tmp;
      if (y = x) and (size - 1 = 2 * x) then
        exit;//center facelets for odd cubes

      tmp := faceCols[Ord(F), y, size - 1 - x];
      faceCols[Ord(F), y, size - 1 - x] := faceCols[Ord(R), y, size - 1 - x];
      faceCols[Ord(R), y, size - 1 - x] := faceCols[Ord(B), y, size - 1 - x];
      faceCols[Ord(B), y, size - 1 - x] := faceCols[Ord(L), y, size - 1 - x];
      faceCols[Ord(L), y, size - 1 - x] := tmp;

      tmp := faceCols[Ord(F), size - 1 - x, size - 1 - y];
      faceCols[Ord(F), size - 1 - x, size - 1 - y] :=
        faceCols[Ord(R), size - 1 - x, size - 1 - y];
      faceCols[Ord(R), size - 1 - x, size - 1 - y] :=
        faceCols[Ord(B), size - 1 - x, size - 1 - y];
      faceCols[Ord(B), size - 1 - x, size - 1 - y] :=
        faceCols[Ord(L), size - 1 - x, size - 1 - y];
      faceCols[Ord(L), size - 1 - x, size - 1 - y] := tmp;

      tmp := faceCols[Ord(F), size - 1 - y, x];
      faceCols[Ord(F), size - 1 - y, x] := faceCols[Ord(R), size - 1 - y, x];
      faceCols[Ord(R), size - 1 - y, x] := faceCols[Ord(B), size - 1 - y, x];
      faceCols[Ord(B), size - 1 - y, x] := faceCols[Ord(L), size - 1 - y, x];
      faceCols[Ord(L), size - 1 - y, x] := tmp;
    end;
    S_LR2:  //Only valid for clusters with reflectional symmetry, x or +cluster!
    begin
      for a := U to B do
      begin
        if (a = R) or (a = L) then
          continue;
        if y <> size - 1 - y then
        begin
          tmp := faceCols[Ord(a), x, y];
          faceCols[Ord(a), x, y] := faceCols[Ord(a), x, size - 1 - y];
          faceCols[Ord(a), x, size - 1 - y] := tmp;

          tmp := faceCols[Ord(a), size - 1 - x, size - 1 - y];
          faceCols[Ord(a), size - 1 - x, size - 1 - y] :=
            faceCols[Ord(a), size - 1 - x, y];
          faceCols[Ord(a), size - 1 - x, y] := tmp;

          tmp := faceCols[Ord(a), size - 1 - y, x];
          faceCols[Ord(a), size - 1 - y, x] :=
            faceCols[Ord(a), size - 1 - y, size - 1 - x];
          faceCols[Ord(a), size - 1 - y, size - 1 - x] := tmp;
        end;

        tmp := faceCols[Ord(a), y, size - 1 - x];
        faceCols[Ord(a), y, size - 1 - x] := faceCols[Ord(a), y, x];
        faceCols[Ord(a), y, x] := tmp;
      end;

      tmp := faceCols[Ord(L), x, y];
      faceCols[Ord(L), x, y] := faceCols[Ord(R), x, size - 1 - y];
      faceCols[Ord(R), x, size - 1 - y] := tmp;

      if (y = x) and (size - 1 = 2 * x) then
        exit;//center facelets for odd cubes

      tmp := faceCols[Ord(L), y, size - 1 - x];
      faceCols[Ord(L), y, size - 1 - x] := faceCols[Ord(R), y, x];
      faceCols[Ord(R), y, x] := tmp;

      tmp := faceCols[Ord(L), size - 1 - x, size - 1 - y];
      faceCols[Ord(L), size - 1 - x, size - 1 - y] :=
        faceCols[Ord(R), size - 1 - x, y];
      faceCols[Ord(R), size - 1 - x, y] := tmp;

      tmp := faceCols[Ord(L), size - 1 - y, x];
      faceCols[Ord(L), size - 1 - y, x] :=
        faceCols[Ord(R), size - 1 - y, size - 1 - x];
      faceCols[Ord(R), size - 1 - y, size - 1 - x] := tmp;

      if y <> size - 1 - y then
      begin
        tmp := faceCols[Ord(R), x, y];
        faceCols[Ord(R), x, y] := faceCols[Ord(L), x, size - 1 - y];
        faceCols[Ord(L), x, size - 1 - y] := tmp;

        tmp := faceCols[Ord(R), y, size - 1 - x];
        faceCols[Ord(R), y, size - 1 - x] := faceCols[Ord(L), y, x];
        faceCols[Ord(L), y, x] := tmp;

        tmp := faceCols[Ord(R), size - 1 - x, size - 1 - y];
        faceCols[Ord(R), size - 1 - x, size - 1 - y] :=
          faceCols[Ord(L), size - 1 - x, y];
        faceCols[Ord(L), size - 1 - x, y] := tmp;

        tmp := faceCols[Ord(R), size - 1 - y, x];
        faceCols[Ord(R), size - 1 - y, x] :=
          faceCols[Ord(L), size - 1 - y, size - 1 - x];
        faceCols[Ord(L), size - 1 - y, size - 1 - x] := tmp;
      end;
    end;

    S_R4:
      ;
    S_F4:
      ;
  end;
  // GUI invalidate removed
end;



function Cnk(n, k: integer): integer;
var
  s, j: integer;
begin
  if n < k then
    Result := 0
  else
  begin
    s := 1;
    if (k > n div 2) then
      k := n - k;
    for j := 1 to k do
    begin
      s := (s * n) div j;
      n := n - 1;
    end;
    Result := s;
  end;
end;


// Gives  the colorIndex of facelet 0<=i<24 for the cluster 0<=x,y<N
// x counts from upper left edge down, y to the right
function faceletCube.clusterColorIndex(x, y, i: integer): ColorIndex;
var
  faceIdx, face, xf, yf: integer;
begin
  xf := 0;
  yf := 0;//initialize to prevent compiler warnings
  faceIdx := i mod 4;
  case faceIdx of
    0:
    begin
      xf := x;
      yf := y;
    end;
    1:
    begin
      xf := y;
      yf := size - 1 - x;
    end;
    2:
    begin
      xf := size - 1 - x;
      yf := size - 1 - y;
    end;
    3:
    begin
      xf := size - 1 - y;
      yf := x;
    end;
  end;
  face := i div 4;
  Result := faceCols[face, xf, yf];
end;


procedure faceletCube.setClusterColorIndex(x, y, i: integer; col: ColorIndex);
//i is 0..23 and describes all facelets within the cluster which has one facelet
//at coordinate (x,y)
var
  faceIdx, face, xf, yf: integer;
begin
  xf := 0;
  yf := 0;//initialize to prevent compiler warnings
  faceIdx := i mod 4;
  case faceIdx of
    0:
    begin
      xf := x;
      yf := y;
    end;
    1:
    begin
      xf := y;
      yf := size - 1 - x;
    end;
    2:
    begin
      xf := size - 1 - x;
      yf := size - 1 - y;
    end;
    3:
    begin
      xf := size - 1 - y;
      yf := x;
    end;
  end;
  face := i div 4;
  faceCols[face, xf, yf] := col;
end;


procedure faceletCube.move(a: Axis; slice: integer);
// 0<=slice<=size-1
var
  i, j: integer;
  tmp: array of array of ColorIndex;
  c: ColorIndex;
begin
  SetLength(tmp, size, size);

  // a <= F then
  case a of
    U, R, F:
    begin
      if slice = 0 then //face turn
      begin
        for i := 0 to size - 1 do
          for j := 0 to size - 1 do
          begin
            tmp[j, size - 1 - i] := faceCols[Ord(a), i, j];
          end;
        for i := 0 to size - 1 do
          for j := 0 to size - 1 do
          begin
            faceCols[Ord(a), i, j] := tmp[i, j];
          end;
      end
      else if slice = size - 1 then  //equivalent to inverse opposite face turn
      begin
        for i := 0 to size - 1 do
          for j := 0 to size - 1 do
          begin
            tmp[i, j] := faceCols[(Ord(a) + 1), j, size - 1 - i];
          end;
        for i := 0 to size - 1 do
          for j := 0 to size - 1 do
          begin
            faceCols[(Ord(a) + 1), i, j] := tmp[i, j];
          end;
      end;
    end;
  end;

  case a of
    U:
    begin
      for j := 0 to size - 1 do
      begin
        c := faceCols[Ord(F), slice, j];
        faceCols[Ord(F), slice, j] := faceCols[Ord(R), slice, j];
        faceCols[Ord(R), slice, j] := faceCols[Ord(B), slice, j];
        faceCols[Ord(B), slice, j] := faceCols[Ord(L), slice, j];
        faceCols[Ord(L), slice, j] := c;
      end;
    end;

    R:
    begin
      for j := 0 to size - 1 do
      begin
        c := faceCols[Ord(U), j, size - 1 - slice];
        faceCols[Ord(U), j, size - 1 - slice] :=
          faceCols[Ord(F), j, size - 1 - slice];
        faceCols[Ord(F), j, size - 1 - slice] :=
          faceCols[Ord(D), j, size - 1 - slice];
        faceCols[Ord(D), j, size - 1 - slice] :=
          faceCols[Ord(B), size - 1 - j, slice];
        faceCols[Ord(B), size - 1 - j, slice] := c;
      end;
    end;

    F:
    begin
      for j := 0 to size - 1 do
      begin
        c := faceCols[Ord(U), size - 1 - slice, j];
        faceCols[Ord(U), size - 1 - slice, j] :=
          faceCols[Ord(L), size - 1 - j, size - 1 - slice];
        faceCols[Ord(L), size - 1 - j, size - 1 - slice] :=
          faceCols[Ord(D), slice, size - 1 - j];
        faceCols[Ord(D), slice, size - 1 - j] := faceCols[Ord(R), j, slice];
        faceCols[Ord(R), j, slice] := c;
      end;
    end;
    D:
      for i := 1 to 3 do
        move(U, size - 1 - slice);
    L:
      for i := 1 to 3 do
        move(R, size - 1 - slice);
    B:
      for i := 1 to 3 do
        move(F, size - 1 - slice);

  end;
  // GUI invalidate removed
end;


function faceletCube.getSize: integer;
begin
  Result := size;
end;

// Drawing methods removed for CLI

function faceletCube.cornerParityEven: boolean;
var
  i, j: Corner;
  s: integer;
begin
  s := 0;
  for i := DRB downto Succ(URF) do
    for j := Pred(i) downto URF do
      if cubiCorn[j].c > cubiCorn[i].c then
        Inc(s);
  if Odd(s) then
    Result := False
  else
    Result := True;
end;

// return the parity of the (0,y)
function faceletCube.edgeParity(y: integer): integer;
var
  i, j, s: integer;
begin
  getEdgeCluster(y);
  s := 0;
  for i := 0 to 22 do
    for j := i + 1 to 23 do
      if ecls[y, j] < ecls[y, i] then
        Inc(s);
  Result := s mod 2;
end;




function faceletCube.setCornerCubies: CornerStatus;
var
  i, j: Corner;
  ori: integer;
  col1, col2: ColorIndex;
begin
  for i := URF to DRB do
    cubiCorn[i].c := NNN;
  for i := URF to DRB do
  begin
    // get orientation of corner i
    ori := 0;
    while (faceCols[Ord(CCI[i, ori]), CFRow[i, ori] * (size - 1),
        CFCol[i, ori] * (size - 1)] <> UCol) and
      (faceCols[Ord(CCI[i, ori]), CFRow[i, ori] * (size - 1),
        CFCol[i, ori] * (size - 1)] <> DCol) and (ori < 2) do
      Inc(ori);
    // ori now contains orientation of corner i, provided ther is a DCol or
    //Ucol at corner i
    ori := (ori + 1) mod 3;
    col1 := faceCols[Ord(CCI[i, ori]), CFRow[i, ori] * (size - 1),
      CFCol[i, ori] * (size - 1)];
    ori := (ori + 1) mod 3;
    col2 := faceCols[Ord(CCI[i, ori]), CFRow[i, ori] * (size - 1),
      CFCol[i, ori] * (size - 1)];
    ori := (ori + 1) mod 3; // restore original orientation
    for j := URF to DRB do
      if (col1 = CCI[j, 1]) and (col2 = CCI[j, 2]) then
      begin
        cubiCorn[i].c := j; // corner j sits in corner i's clean cube position
        cubiCorn[i].o := ori; // twist of corner j
      end;
  end;
  //check if we have a valid corner configuration
  for i := URF to DRB do
    if cubiCorn[i].c = NNN then
      Exit(CORNPERMUTATIONERROR);
  ori := 0;
  for i := URF to DRB do
    Inc(ori, cubiCorn[i].o);
  if ori mod 3 <> 0 then
    Exit(CORNORIENTATIONERROR);
  Result := CORNNOERROR;
end;

function faceletCube.setMiddleEdgeCubies: EdgeStatus;
var
  i, j: Edge;
  ori, sz2: integer;
  col0, col1: ColorIndex;
begin
  if not Odd(size) then
    Exit(EDGESIZEERROR);
  sz2 := (size - 1) div 2;
  for i := UR to BR do
    cubiEdge[i].e := NN;
  for i := UR to BR do
  begin
    // get orientation of middle edge
    col0 := faceCols[Ord(ECI[i, 0]), EFRow[i, 0] * sz2, EFCol[i, 0] * sz2];
    col1 := faceCols[Ord(ECI[i, 1]), EFRow[i, 1] * sz2, EFCol[i, 1] * sz2];
    for j := UR to BR do
      if (col0 = ECI[j, 0]) and (col1 = ECI[j, 1]) then
      begin
        cubiEdge[i].e := j;
        cubiEdge[i].o := 0;
      end
      else if (col0 = ECI[j, 1]) and (col1 = ECI[j, 0]) then
      begin
        cubiEdge[i].e := j;
        cubiEdge[i].o := 1;
      end;
  end;
  for i := UR to BR do
    if cubiEdge[i].e = NN then
      Exit(EDGEPERMUTATIONERROR);
  ori := 0;
  for i := UR to BR do
    Inc(ori, cubiEdge[i].o);
  if Odd(ori) then
    Exit(EDGEORIENTATIONERROR);
  Result := EDGENOERROR;
end;

//set the solved position for a cluster
procedure faceletcube.initCluster(x, y: integer);
var
  i: integer;
begin
  for i := 0 to 23 do
    setClusterColorIndex(x, y, i, ColorIndex(i div 4));
end;

//return coodinate for index i (see cubedefs)
function getPos(i, cl, sz: integer): integer;
begin
  case i of
    0:
      Result := 0;
    1:
      Result := cl;
    2:
      Result := sz - 1;
    3:
      Result := sz - 1 - cl;
  end;
end;

procedure faceletCube.getEdgeCluster(y: integer);
var
  i, j: integer;
  c: array [0 .. 1] of ColorIndex;
begin
  for i := 0 to 23 do
  begin
    c[0] := faceCols[Ord(ECCI[i, 0]), getPos(ECFRow[i, 0], y, size),
      getPos(ECFCol[i, 0], y, size)];
    c[1] := faceCols[Ord(ECCI[i, 1]), getPos(ECFRow[i, 1], y, size),
      getPos(ECFCol[i, 1], y, size)];
    j := 0;
    while (j < 24) and ((ECCI[j, 0] <> c[0]) or (ECCI[j, 1] <> c[1])) do
      Inc(j);
    if j >= 24 then
    begin
      TSWriteLn(Format('getEdgeCluster(%d) FAIL: slot %d, face0=%d[%d,%d]=%d, face1=%d[%d,%d]=%d',
        [y, i,
         Ord(ECCI[i, 0]), getPos(ECFRow[i, 0], y, size), getPos(ECFCol[i, 0], y, size), Ord(c[0]),
         Ord(ECCI[i, 1]), getPos(ECFRow[i, 1], y, size), getPos(ECFCol[i, 1], y, size), Ord(c[1])]));
      j := 0; // fallback to avoid crash
    end;
    ecls[y, remapEdges[i]] := remapEdges[j];
  end;
end;

procedure faceletCube.getEdgeClusterAt(orbit, physPos: integer);
var
  i, j: integer;
  c: array [0 .. 1] of ColorIndex;
begin
  for i := 0 to 23 do
  begin
    c[0] := faceCols[Ord(ECCI[i, 0]), getPos(ECFRow[i, 0], physPos, size),
      getPos(ECFCol[i, 0], physPos, size)];
    c[1] := faceCols[Ord(ECCI[i, 1]), getPos(ECFRow[i, 1], physPos, size),
      getPos(ECFCol[i, 1], physPos, size)];
    j := 0;
    while (ECCI[j, 0] <> c[0]) or (ECCI[j, 1] <> c[1]) do
      Inc(j);
    ecls[orbit, remapEdges[i]] := remapEdges[j];
  end;
end;

constructor faceletCube.Create(sz: integer);
var
  i, j: integer;
  a: Axis;
  c: ColorIndex;
begin
  size := sz;
  origSize := sz;
  SetLength(faceCols, 6, size, size);
  for a := U to B do
  begin
    c := ColorIndex(Ord(a));
    for i := 0 to size - 1 do
      for j := 0 to size - 1 do
        faceCols[Ord(a), i, j] := c;
  end;
  // Pre-allocate edge cluster array (needed by edgeParity in Phase 2+)
  SetLength(ecls, size div 2 + 1, 24);
end;

// copy constructor, erzeugt aber immer einen cube mit ungerader size
constructor faceletCube.Create(fc: faceletCube);
var
  i, j: integer;
  a: Axis;
  c: ColorIndex;
begin
  origSize := fc.size; // remember original size before padding
  // cv removed for CLI
  if Odd(fc.size) then
  begin
    size := fc.size;
    SetLength(faceCols, 6, size, size);
    for a := U to B do
    begin
      c := ColorIndex(Ord(a));
      for i := 0 to size - 1 do
        for j := 0 to size - 1 do
          faceCols[Ord(a), i, j] := fc.faceCols[Ord(a), i, j];
    end;
  end
  else
  begin
    size := fc.size + 1;//for even sizes, add a middle slice
    SetLength(faceCols, 6, size, size);
    for a := U to B do
    begin
      c := ColorIndex(Ord(a));
      for i := 0 to (size - 1) div 2 - 1 do
        for j := 0 to (size - 1) div 2 - 1 do
          faceCols[Ord(a), i, j] := fc.faceCols[Ord(a), i, j];

      for i := 0 to (size - 1) div 2 - 1 do
        for j := (size - 1) div 2 to size - 2 do
          faceCols[Ord(a), i, j + 1] := fc.faceCols[Ord(a), i, j];

      for i := (size - 1) div 2 to size - 2 do
        for j := 0 to (size - 1) div 2 - 1 do
          faceCols[Ord(a), i + 1, j] := fc.faceCols[Ord(a), i, j];

      for i := (size - 1) div 2 to size - 2 do
        for j := (size - 1) div 2 to size - 2 do
          faceCols[Ord(a), i + 1, j + 1] := fc.faceCols[Ord(a), i, j];

      for i := 0 to size - 1 do
        faceCols[Ord(a), i, (size - 1) div 2] := c;
      for j := 0 to size - 1 do
        faceCols[Ord(a), (size - 1) div 2, j] := c;

    end;

    // Corner parity fix must run AFTER all 6 faces are fully constructed.
    // Running it inside the loop reads partially-initialized corner data
    // and can cause facelet count imbalance (F and B colors swapped unevenly).
    setCornerCubies;
    if not cornerParityEven then // swap two middle edges to fix parity
    begin
      faceCols[Ord(F), 0, (size - 1) div 2] := BCol;
      faceCols[Ord(B), 0, (size - 1) div 2] := FCol;
    end;

  end;
  // Pre-allocate edge cluster array (needed by edgeParity in Phase 2+)
  SetLength(ecls, size div 2 + 1, 24);
end;


//A subgroup of the full brick group generated by only applying xR1,xF1,xL1,xB1
//and yR1,yF1,yL1,yB1 moves
function faceletCube.Phase1Brick256Coord(x, y: integer): integer;
var
  i, m, idx: integer;
  c: ColorIndex;
begin
  m := (size - 1) div 2; // Index der mittleren Slice
  idx := 0;
  for i := 0 to 3 do // Es reicht die Positionen von U zu betrachten
  begin
    c := clusterColorIndex(x, m, i);
    idx := 2 * idx;
    if (c <> UCol) and (c <> DCol) then
      Inc(idx); // slice gedreht
  end;
  for i := 0 to 3 do
  begin
    c := clusterColorIndex(y, m, i);
    idx := 2 * idx;
    if (c <> UCol) and (c <> DCol) then
      Inc(idx);
  end;
  Result := idx;
end;

procedure faceletCube.InvPhase1Brick256Coord(cc, x, y: integer);
var
  sc, m: integer;
begin
  m := (size - 1) div 2; // Index der mittleren Slice
  sc := cc mod 2; // Orientierung der L-y-Slice
  cc := cc div 2;
  if sc = 0 then
  begin
    setClusterColorIndex(y, m, 3, UCol);
    setClusterColorIndex(y, m, 3 + 4, DCol);
    setClusterColorIndex(y, m, 3 + 16, FCol);
    setClusterColorIndex(y, m, 1 + 20, BCol);
  end
  else
  begin
    setClusterColorIndex(y, m, 3, BCol);
    setClusterColorIndex(y, m, 3 + 4, FCol);
    setClusterColorIndex(y, m, 3 + 16, UCol);
    setClusterColorIndex(y, m, 1 + 20, DCol);
  end;
  sc := cc mod 2; // Orientierung der F-y-Slice
  cc := cc div 2;
  if sc = 0 then
  begin
    setClusterColorIndex(y, m, 2, UCol);
    setClusterColorIndex(y, m, 0 + 4, DCol);
    setClusterColorIndex(y, m, 1 + 12, LCol);
    setClusterColorIndex(y, m, 3 + 8, RCol);
  end
  else
  begin
    setClusterColorIndex(y, m, 2, LCol);
    setClusterColorIndex(y, m, 0 + 4, RCol);
    setClusterColorIndex(y, m, 1 + 12, DCol);
    setClusterColorIndex(y, m, 3 + 8, UCol);
  end;

  sc := cc mod 2; // Orientierung der R-y-Slice
  cc := cc div 2;
  if sc = 0 then
  begin
    setClusterColorIndex(y, m, 1, UCol);
    setClusterColorIndex(y, m, 1 + 4, DCol);
    setClusterColorIndex(y, m, 1 + 16, FCol);
    setClusterColorIndex(y, m, 3 + 20, BCol);
  end
  else
  begin
    setClusterColorIndex(y, m, 1, FCol);
    setClusterColorIndex(y, m, 1 + 4, BCol);
    setClusterColorIndex(y, m, 1 + 16, DCol);
    setClusterColorIndex(y, m, 3 + 20, UCol);
  end;

  sc := cc mod 2; // Orientierung der B-y-Slice
  cc := cc div 2;
  if sc = 0 then
  begin
    setClusterColorIndex(y, m, 0, UCol);
    setClusterColorIndex(y, m, 2 + 4, DCol);
    setClusterColorIndex(y, m, 1 + 8, RCol);
    setClusterColorIndex(y, m, 3 + 12, LCol);
  end
  else
  begin
    setClusterColorIndex(y, m, 0, RCol);
    setClusterColorIndex(y, m, 2 + 4, LCol);
    setClusterColorIndex(y, m, 1 + 8, DCol);
    setClusterColorIndex(y, m, 3 + 12, UCol);
  end;

  sc := cc mod 2; // Orientierung der L-x-Slice
  cc := cc div 2;
  if sc = 0 then
  begin
    setClusterColorIndex(x, m, 3, UCol);
    setClusterColorIndex(x, m, 3 + 4, DCol);
    setClusterColorIndex(x, m, 3 + 16, FCol);
    setClusterColorIndex(x, m, 1 + 20, BCol);
  end
  else
  begin
    setClusterColorIndex(x, m, 3, BCol);
    setClusterColorIndex(x, m, 3 + 4, FCol);
    setClusterColorIndex(x, m, 3 + 16, UCol);
    setClusterColorIndex(x, m, 1 + 20, DCol);
  end;
  sc := cc mod 2; // Orientierung der F-x-Slice
  cc := cc div 2;
  if sc = 0 then
  begin
    setClusterColorIndex(x, m, 2, UCol);
    setClusterColorIndex(x, m, 0 + 4, DCol);
    setClusterColorIndex(x, m, 1 + 12, LCol);
    setClusterColorIndex(x, m, 3 + 8, RCol);
  end
  else
  begin
    setClusterColorIndex(x, m, 2, LCol);
    setClusterColorIndex(x, m, 0 + 4, RCol);
    setClusterColorIndex(x, m, 1 + 12, DCol);
    setClusterColorIndex(x, m, 3 + 8, UCol);
  end;

  sc := cc mod 2; // Orientierung der R-x-Slice
  cc := cc div 2;
  if sc = 0 then
  begin
    setClusterColorIndex(x, m, 1, UCol);
    setClusterColorIndex(x, m, 1 + 4, DCol);
    setClusterColorIndex(x, m, 1 + 16, FCol);
    setClusterColorIndex(x, m, 3 + 20, BCol);
  end
  else
  begin
    setClusterColorIndex(x, m, 1, FCol);
    setClusterColorIndex(x, m, 1 + 4, BCol);
    setClusterColorIndex(x, m, 1 + 16, DCol);
    setClusterColorIndex(x, m, 3 + 20, UCol);
  end;

  sc := cc mod 2; // Orientierung der B-x-Slice
  if sc = 0 then
  begin
    setClusterColorIndex(x, m, 0, UCol);
    setClusterColorIndex(x, m, 2 + 4, DCol);
    setClusterColorIndex(x, m, 1 + 8, RCol);
    setClusterColorIndex(x, m, 3 + 12, LCol);
  end
  else
  begin
    setClusterColorIndex(x, m, 0, RCol);
    setClusterColorIndex(x, m, 2 + 4, LCol);
    setClusterColorIndex(x, m, 1 + 8, DCol);
    setClusterColorIndex(x, m, 3 + 12, UCol);
  end;

end;


function faceletCube.Phase1CenterCoord(x, y: integer): integer;
  //UD-Centers coordinate of clusters (x,y)
  // solved position has index 0
var
  occupied: array [0 .. 23] of boolean;
  c: ColorIndex;
  i, k, n: integer;
begin
  for i := 0 to 23 do
  begin
    c := clusterColorIndex(x, y, i);
    if (c = UCol) or (c = DCol) then
      occupied[i] := True
    else
      occupied[i] := False;
  end;

  Result := 0;
  n := 0;
  k := 1;

  while k <= 8 do  //8 positions of the UD cluster are occupied
  begin
    if occupied[n] then
    begin
      Inc(Result, Cnk(n, k));
      Inc(k);
    end;
    Inc(n);
  end;
end;

procedure faceletCube.InvPhase1CenterCoord(cc, x, y: integer);
var
  n, k, v, c: integer;
  occupied: array [0 .. 23] of boolean;
  col, setCol: ColorIndex;
begin
  for n := 0 to 23 do
    occupied[n] := False;
  n := 23;
  k := 8;//8 positions are set
  while k >= 1 do
  begin
    v := Cnk(n, k);
    if cc >= v then
    begin
      occupied[n] := True;
      Dec(cc, v);
      Dec(k);
    end;
    Dec(n);
  end;

  n := 0;
  setCol := UCol;
  for c := 0 to 23 do
    if occupied[c] then
    begin
      col := clusterColorIndex(x, y, c);
      if col <> setCol then
      begin
        k := 0;
        while (clusterColorIndex(x, y, k) <> setCol) or
          (occupied[k] and (k < c) (* do not change already set *)) do
          Inc(k);
        setClusterColorIndex(x, y, c, setCol);
        setClusterColorIndex(x, y, k, col);
      end;
      Inc(n);
      if n = 4 then // use DCol for n>=4
        setCol := DCol;
    end;
end;


procedure faceletCube.printMoves(i, j: integer);
var
  mm: string;
begin
  mm := printMovesStr(i, j);
  WriteLnVerbose(mm);
end;

function faceletCube.printMovesStr(i, j: integer): string;
var
  mm, s, cleanMoves: string;
  k: integer;
begin
  mm := '';
  cleanMoves := '';
  for k := 0 to mvIdx - 1 do
  begin
    s := moveStrings[fxymoves[k]];
    s := StringReplace(s, '1', '', []);
    s := StringReplace(s, '3', '''', []);
    s := StringReplace(s, 'x', IntToStr(i + 1), []);
    s := StringReplace(s, 'y', IntToStr(j + 1), []);
    s := StringReplace(s, 'f', '', []);
    mm := mm + ' ' + s;
    if cleanMoves <> '' then
      cleanMoves := cleanMoves + ' ' + s
    else
      cleanMoves := s;
  end;
  // Append clean notation to moveLog for external collection
  if cleanMoves <> '' then
  begin
    if moveLog <> '' then
      moveLog := moveLog + ' ' + cleanMoves
    else
      moveLog := cleanMoves;
  end;
  mm := mm + ' (' + IntToStr(mvIdx) + ')';
  if (i > 0) and (j > 0) then
    mm := '(' + IntToStr(i) + ',' + IntToStr(j) + '):' + mm;
  Result := mm;
end;

procedure faceletCube.applyMoves(i, j: integer);
var
  mv: moves;
  k, n, slc, pow: integer;
  a: Axis;
begin
  for k := 0 to mvIdx - 1 do
  begin
    mv := fxymoves[k];
    if mv < xU1 then
      slc := 0
    else if mv < yU1 then
      slc := i
    else
      slc := j;
    n := Ord(mv) mod 18;
    a := Axis(n div 3);
    pow := n mod 3;
    for n := 0 to pow do
      move(a, slc);
  end;
end;





procedure faceletCube.applyEdgeMoves(x: integer);
//x is necessary for slice orbits
var
  mv: moves;
  i, k: integer;
begin
  for k := 0 to mvIdx - 1 do
  begin
    mv := fxymoves[k];
    edgemove(x, mv);
  end;
end;




function faceletCube.MakeUDXCross(x: integer): boolean;
var
  idx, togo: integer;
begin
  togo := 0;
  found := False;
  for idx := Low(fxymoves) to High(fxymoves) do
    fxymoves[idx] := InitMove;

  while found = False do
  begin
    mvIdx := 0; // 1.freier Platz in fxymoves
    SearchUDXCross(Phase1CenterCoord(x, x), Phase1Brick256Coord(x, x) and $F, togo);
    Inc(togo);
  end;
  Result := True;
end;

function faceletCube.MakeUDPlusCross1(x: integer): boolean;
var
  idx, togo: integer;
begin
  togo := 0;
  found := False;
  for idx := Low(fxymoves) to High(fxymoves) do
    fxymoves[idx] := InitMove;
  while found = False do
  begin
    mvIdx := 0; // 1.freier Platz in fxymoves
    SearchUDPlusCross1(Phase1CenterCoord(x, size div 2), togo);
    // 0 ist die slicecoord der schon gelösten cluster.
    // diese dürfen ja nicht wieder zerstört werden
    Inc(togo);
  end;
  Result := True;
end;

procedure faceletCube.SearchUDPlusCross1(cc, togo: integer);
var
  mv: moves;
  newcc: integer;
begin
  if (UDPlusCross1Prun[cc] > togo) then
    Exit;
  if togo = 0 then
  begin
    found := True;
  end
  else
  begin
    mv := InitMove;
    while True do
    begin
      if mvIdx = 0 then
        mv := nextMovePhase1[NoMove, mv]
      else
        mv := nextMovePhase1[fxymoves[mvIdx - 1], mv];

      if mv = NoMove then
      begin
        Exit;
      end
      else
      begin
        if mv < yU1 then
          newcc := UDCenterMove[cc, Ord(mv)]
        else
          continue;

        if (newcc = cc) then
          continue;

        fxymoves[mvIdx] := mv;
        Inc(mvIdx);
        SearchUDPlusCross1(newcc, togo - 1);

        if found then
          // kehre zurück, ohne mvIdx zu verändern
          Exit;
        Dec(mvIdx);
      end;
    end;

  end;
end;

procedure faceletCube.SearchUDXCross(ccx, slx, togo: integer);
var
  mv: moves;
begin

  if UDXCrossPrun[B_24_8 * slx + ccx] > togo then
    Exit;
  if togo = 0 then
  begin
    found := True;
  end
  else
  begin
    mv := InitMove;
    while True do
    begin
      if mvIdx = 0 then
        mv := nextMovePhase1[NoMove, mv]
      else
        mv := nextMovePhase1[fxymoves[mvIdx - 1], mv];
      if (mv < xU1) and not UDfaceMoveAllowed[slx, Ord(mv)] then
        continue;
      if mv > xB3 then // no y-moves used
      begin
        Exit;
      end
      else
      begin
        fxymoves[mvIdx] := mv;
        Inc(mvIdx);
        if mv < xU1 then
          SearchUDXCross(UDXCrossMove[ccx, Ord(mv)], slx, togo - 1)
        else
          SearchUDXCross(UDXCrossMove[ccx, Ord(mv)],
            UDBrick256Move[slx, Ord(mv) + 18], togo - 1);
        if found then
          Exit;
        Dec(mvIdx);
      end;
    end;

  end;

end;

function faceletCube.nextMovePh2(idx: integer; currMove: moves): moves;
var
  pm: moves;
begin
  if currMove = yB3 then // done
    Exit(NoMove);
  if idx = 0 then
  begin
    if currMove = InitMove then
    begin
      Exit(fR2); // U,D moves useless
    end
    else
    begin
      while not Phase2Allowed[Ord(Succ(currMove))] do
        Inc(currMove);
      Exit(Succ(currMove));
    end;
  end

  else
  begin
    pm := fxymoves[idx - 1]; // predecessor move
    while True do
    begin
      while not Phase2Allowed[Ord(Succ(currMove))] do
        Inc(currMove);
      currMove := Succ(currMove);
      if currMove = NoMove then
        Exit(NoMove);

      if Ord(pm) < Ord(xU1) then //previous move is face move
      begin
        if (Ord(currMove) <= Ord(pm)) then
          //all face moves commute restricted to the centers
          continue;
        if Ord(currMove) >= Ord(xU1) then //face move followed by slice move
          // always valid
          Exit(currMove)
        else // pm<currmove<xU1
        begin
          if Ord(pm) div 3 = Ord(currMove) div 3 then
            // same face
            continue
          else
            Exit(currMove);
        end;
      end;

      //Ord(pm) >= Ord(xU1), previous move is slice move
      if (Ord(pm) div 6) mod 3 <> (Ord(currMove) div 6) mod 3 then
        Exit(currMove);
      // pm and currMove are on different axes and hence do not commute


      // both moves are on the same axis and commute
      // we can force an order
      if Ord(currMove) <= Ord(pm) then
        continue;

      // if the  prefixes f,x,y are different for both moves, currmove is valid
      if Ord(currMove) div 18 <> Ord(pm) div 18 then
        Exit(currMove);

      // we have the same prefix and the same axis
      if (Ord(currMove) mod 6) div 3 <> (Ord(pm) mod 6) div 3 then
        // moves are on different slices of the axis
        Exit(currMove)
      else
        continue;
    end;
  end;
end;

// coordinate of FB-centers of cluster (x,y)
// phase 1 has to be finished already!
function faceletCube.Phase2CenterCoord(x, y: integer): integer;
var
  occupied: array [0 .. 15] of boolean;
  c: ColorIndex;
  i, s, k, n: integer;
begin
  for i := 0 to 15 do
  begin
    c := clusterColorIndex(x, y, i + 8); // +8 because we start with R-face
    if (c = FCol) or (c = BCol) then
      occupied[i] := True
    else
      occupied[i] := False;
  end;

  s := 0;
  k := 7; // 8 cubies
  n := 15; // on 16 Positionen
  while k >= 0 do
  begin
    if occupied[n] then
      Dec(k)
    else
      s := s + Cnk(n, k);
    Dec(n);
  end;
  Result := s; // ID is 0
end;

procedure faceletCube.InvPhase2CenterCoord(cc, x, y: integer);
var
  n, k, v, c: integer;
  occupied: array [0 .. 15] of boolean;
  col, setCol: ColorIndex;
begin
  for n := 0 to 15 do
    occupied[n] := False;
  n := 15;
  k := 7;
  while k >= 0 do
  begin
    v := Cnk(n, k);
    if cc < v then
    begin
      Dec(k);
      occupied[n] := True;
    end
    else
      Dec(cc, v);
    Dec(n);
  end;

  n := 0;
  setCol := FCol;
  for c := 0 to 15 do
    if occupied[c] then
    begin
      col := clusterColorIndex(x, y, c + 8);
      if col <> setCol then
      begin
        k := 0;
        while (clusterColorIndex(x, y, k + 8) <> setCol) or
          (occupied[k] and (k < c) (* do not change already set *)) do
          Inc(k);
        setClusterColorIndex(x, y, c + 8, setCol);
        setClusterColorIndex(x, y, k + 8, col);
      end;
      Inc(n);
      if n = 4 then // use DBCol now
        setCol := BCol;
    end;
end;


function faceletCube.MakeFBPlusCross(x: integer): boolean;
var
  idx, togo: integer;
begin
  togo := 0;
  found := False;
  for idx := Low(fxymoves) to High(fxymoves) do
    fxymoves[idx] := InitMove;
  while found = False do
  begin
    mvIdx := 0; // 1.freier Platz in fxymoves
    SearchFBPlusCross(Phase2CenterCoord(x, size div 2), edgeparity(x), togo);
    Inc(togo);
  end;
  Result := True;
end;

procedure faceletCube.SearchFBPlusCross(cc, ep, togo: integer);
var
  mv: moves;
  newcc, newep: integer;
begin
  if (FBPlusCrossPrun[2 * cc + ep] > togo) then
    Exit;
  if togo = 0 then
  begin
    found := True;
  end
  else
  begin
    mv := InitMove;
    while True do
    begin
      if mvIdx = 0 then
        mv := nextMovePhase2Arr[NoMove, mv]
      else
        mv := nextMovePhase2Arr[fxymoves[mvIdx - 1], mv];

      if mv = NoMove then
      begin
        Exit;
      end
      else
      begin

        if mv < yU1 then
        begin
          newcc := FBCenterMove[cc, Ord(mv)];
          if (mv = xU1) or (mv = xU3) or (mv = xD1) or (mv = xD3) then
            newep := 1 - ep
          else
            newep := ep;
        end

        else
          continue;//no y-moves for +cluster

        if (newcc = cc) and (newep = ep) then
          continue;

        fxymoves[mvIdx] := mv;
        Inc(mvIdx);
        SearchFBPlusCross(newcc, newep, togo - 1);

        if found then
          // return without changing mvIdx
          Exit;
        Dec(mvIdx);
      end;
    end;

  end;
end;

// x<>y und +cross muss schon gesetzt sein!
function faceletCube.Phase2SliceCoord(x, y: integer): integer;
var
  m, idx: integer;
  c: ColorIndex;
begin
  m := (size - 1) div 2; // Index der mittleren Slice
  idx := 0;

  // Es reicht die Positionen von F zu betrachten
  c := clusterColorIndex(x, m, 16); // xU-slice
  idx := 2 * idx;
  if (c <> FCol) and (c <> BCol) then
    Inc(idx);
  c := clusterColorIndex(x, m, 18); // xD-slice
  idx := 2 * idx;
  if (c <> FCol) and (c <> BCol) then
    Inc(idx);

  c := clusterColorIndex(y, m, 16); // yU-slice
  idx := 2 * idx;
  if (c <> FCol) and (c <> BCol) then
    Inc(idx);
  c := clusterColorIndex(y, m, 18); // yD-slice
  idx := 2 * idx;
  if (c <> FCol) and (c <> BCol) then
    Inc(idx);
  Result := idx;
end;


procedure faceletCube.InvPhase2SliceCoord(cc, x, y: integer);
var
  sc, m: integer;
begin

  m := (size - 1) div 2;
  sc := cc mod 2; // Orientierung der yD-Slice
  cc := cc div 2;
  if sc = 0 then
  begin
    setClusterColorIndex(y, m, 18, FCol);
    setClusterColorIndex(y, m, 14, LCol);
    setClusterColorIndex(y, m, 22, BCol);
    setClusterColorIndex(y, m, 10, RCol);
  end
  else
  begin
    setClusterColorIndex(y, m, 18, LCol);
    setClusterColorIndex(y, m, 14, BCol);
    setClusterColorIndex(y, m, 22, RCol);
    setClusterColorIndex(y, m, 10, FCol);
  end;
  sc := cc mod 2; // Orientierung der yU-Slice
  cc := cc div 2;
  if sc = 0 then
  begin
    setClusterColorIndex(y, m, 16, FCol);
    setClusterColorIndex(y, m, 12, LCol);
    setClusterColorIndex(y, m, 20, BCol);
    setClusterColorIndex(y, m, 8, RCol);
  end
  else
  begin
    setClusterColorIndex(y, m, 16, LCol);
    setClusterColorIndex(y, m, 12, BCol);
    setClusterColorIndex(y, m, 20, RCol);
    setClusterColorIndex(y, m, 8, FCol);
  end;

  sc := cc mod 2; // Orientierung der xD-Slice
  cc := cc div 2;
  if sc = 0 then
  begin
    setClusterColorIndex(x, m, 18, FCol);
    setClusterColorIndex(x, m, 14, LCol);
    setClusterColorIndex(x, m, 22, BCol);
    setClusterColorIndex(x, m, 10, RCol);
  end
  else
  begin
    setClusterColorIndex(x, m, 18, LCol);
    setClusterColorIndex(x, m, 14, BCol);
    setClusterColorIndex(x, m, 22, RCol);
    setClusterColorIndex(x, m, 10, FCol);
  end;

  sc := cc mod 2; // Orientierung der xU-Slice
  cc := cc div 2;
  if sc = 0 then
  begin
    setClusterColorIndex(x, m, 16, FCol);
    setClusterColorIndex(x, m, 12, LCol);
    setClusterColorIndex(x, m, 20, BCol);
    setClusterColorIndex(x, m, 8, RCol);
  end
  else
  begin
    setClusterColorIndex(x, m, 16, LCol);
    setClusterColorIndex(x, m, 12, BCol);
    setClusterColorIndex(x, m, 20, RCol);
    setClusterColorIndex(x, m, 8, FCol);
  end;
end;

procedure faceletCube.SearchFBFullCenter(ccx, slx, ccy, togo: integer);
var
  mv: moves;

  newccx, newccy, newslx: integer;

begin

  // Application.ProcessMessages removed

  if (FBFullCenterSlicePrun[UInt64(B_16_8) * (UInt64(B_16_8) * slx + ccx) +
    ccy] > togo) then
    Exit;
  if togo = 0 then
  begin
    found := True;
  end
  else
  begin
    mv := InitMove;

    while True do
    begin
      if mvIdx = 0 then
        mv := nextMovePhase2Arr[NoMove, mv]
      else
        mv := nextMovePhase2Arr[fxymoves[mvIdx - 1], mv];
      if (mv < xU1) and not FBfaceMoveAllowed[slx, Ord(mv)] then
        continue;

      if mv = NoMove then
      begin
        Exit;
      end
      else
      begin
        case mv of
          fU1..FB3:
          begin
            newccx := FBCenterMove[ccx, Ord(mv)];
            newccy := FBCenterMove[ccy, Ord(mv)];
            newslx := slx;
          end;
          xU1..xB3:
          begin
            newccx := FBCenterMove[ccx, Ord(mv)];
            newccy := FBCenterMove[ccy, Ord(mv) + 18];
            newslx := FBSliceMove[slx, Ord(mv)];
          end;
          yU1..yB3:
          begin
            newccx := FBCenterMove[ccx, Ord(mv)];
            newccy := FBCenterMove[ccy, Ord(mv) - 18];
            newslx := FBSliceMove[slx, Ord(mv)];
          end;
        end;

        // do nothing if coordinates do not change
        if (newccx = ccx) and (newslx = slx) and (newccy = ccy) then
          continue;

        fxymoves[mvIdx] := mv;
        Inc(mvIdx);
        SearchFBFullCenter(newccx, newslx, newccy, togo - 1);

        if found then
          Exit;
        Dec(mvIdx);
      end;
    end;

  end;
end;


function faceletCube.MakeFBFullCenter(x, y: integer): boolean;
var
  idx, togo: integer;
begin
  togo := 0;
  found := False;
  for idx := Low(fxymoves) to High(fxymoves) do
    fxymoves[idx] := InitMove;
  while found = False do
  begin
    mvIdx := 0; // 1.freier Platz in fxymoves
    SearchFBFullCenter(Phase2CenterCoord(x, y), Phase2SliceCoord(x, y),
      Phase2CenterCoord(y, x), togo);
    Inc(togo);
  end;
  Result := True;
end;

procedure faceletCube.SearchFBXCross(ccx, sly, togo: integer);
var
  mv: moves;
begin

  // Application.ProcessMessages removed
  if FBXCrossPrun[B_16_8 * sly + ccx] > togo then
    Exit;
  if togo = 0 then
  begin
    found := True;
  end
  else
  begin
    mv := InitMove;
    while True do
    begin
      if mvIdx = 0 then
        mv := nextMovePhase2Arr[NoMove, mv]
      else
        mv := nextMovePhase2Arr[fxymoves[mvIdx - 1], mv];
      if (mv < xU1) and not FBfaceMoveAllowed[sly, Ord(mv)] then
        continue;
      if mv > xB3 then
      begin
        Exit;
      end
      else
      begin

        fxymoves[mvIdx] := mv;
        Inc(mvIdx);
        if mv < xU1 then
          SearchFBXCross(FBXCrossMove[ccx, Ord(mv)], sly, togo - 1)
        else
          SearchFBXCross(FBXCrossMove[ccx, Ord(mv)],
            FBSliceMove[sly, Ord(mv) + 18], togo - 1);
        // y-Koordinate hat die beiden low bits
        if found then
          // kehre zurück, ohne mvIdx zu verändern
          Exit;
        Dec(mvIdx);
      end;
    end;

  end;

end;

function faceletCube.MakeFBXCross(x: integer): boolean;
var
  idx, togo: integer;
begin
  togo := 0;
  found := False;
  for idx := Low(fxymoves) to High(fxymoves) do
    fxymoves[idx] := InitMove;

  while found = False do
  begin
    mvIdx := 0; // 1.freier Platz in fxymoves
    SearchFBXCross(Phase2CenterCoord(x, x), Phase2SliceCoord(x, x) and $3, togo);
    // $3 um die y-slice coordinate zu erhalten, die die hinteren Bits enthält
    Inc(togo);
  end;
  Result := True;

end;

/// ////////////////////////// Phase 3 /////////////////////////////////////////


function faceletCube.nextMovePh3(idx: integer; currMove: moves): moves;
var
  pm: moves;
begin
  if currMove = yB3 then // done
    Exit(NoMove);
  if idx = 0 then
  begin
    while not Phase3Allowed[Ord(Succ(currMove))] do
      Inc(currMove);
    Exit(Succ(currMove));
  end
  else
  begin
    pm := fxymoves[idx - 1]; // predecessor move
    while True do
    begin
      while not Phase3Allowed[Ord(Succ(currMove))] do
        Inc(currMove);
      currMove := Succ(currMove);
      if currMove = NoMove then
        Exit(NoMove);

      if Ord(pm) < Ord(xU1) then //previous move is face move
      begin
        if (Ord(currMove) <= Ord(pm)) then
          //all face moves commute restricted to the centers
          continue;
        if Ord(currMove) >= Ord(xU1) then //face move followed by slice move
          // always valid
          Exit(currMove)
        else // pm<currmove<xU1
        begin
          if Ord(pm) div 3 = Ord(currMove) div 3 then
            // same face
            continue
          else
            Exit(currMove);
        end;
      end;

      //Ord(pm) >= Ord(xU1), previous move is slice move

      if (Ord(pm) div 18 = Ord(currmove) div 18) and (Ord(currMove) <= Ord(pm)) then
        continue;
      // all x-slice moves commute for  (x,y)-orbit with x<>y
      // all y-slice moves commute for  (x,y)-orbit with x<>y

      if (Ord(pm) div 6) mod 3 <> (Ord(currMove) div 6) mod 3 then
        Exit(currMove);
      // pm and currMove are on different axes and hence do not commute


      // both moves are on the same axis and commute
      // we can force an order
      if Ord(currMove) <= Ord(pm) then
        continue;

      // if the  prefixes f,x,y are different for both moves, currmove is valid
      if Ord(currMove) div 18 <> Ord(pm) div 18 then
        Exit(currMove);

      // we have the same prefix and the same axis
      if (Ord(currMove) mod 6) div 3 <> (Ord(pm) mod 6) div 3 then
        // moves are on different slices of the axis
        Exit(currMove)
      else
        continue;
    end;
  end;
end;

function faceletCube.Ph3RLFBCenterCoord(x, y: integer): UInt16;
  //0<=cc<B_8_4^2
begin
  Result := 70 * Phase3CenterCoord(x, y, R) + Phase3CenterCoord(x, y, F);
end;

procedure faceletCube.InvPh3RLFBCenterCoord(cc, x, y: integer);
begin
  InvPhase3CenterCoord(cc mod 70, x, y, F);
  InvPhase3CenterCoord(cc div 70, x, y, R);
end;


function faceletcube.Ph3Brick702Coord(x: integer): integer;
  //0<=bc<B_8_4^2
begin
  Result := B_8_4 * Phase3CenterCoord(x, size div 2, R) +
    Phase3CenterCoord(x, size div 2, F);
end;

procedure faceletcube.InvPh3Brick702Coord(br, x: integer);

begin
  InvPhase3CenterCoord(br div B_8_4, x, size div 2, R);
  InvPhase3CenterCoord(br mod B_8_4, x, size div 2, F);
end;



// in phase 3 a (x,y) cluster separates into 3 subclusters of size 8
//one for each direction of the cube
function faceletCube.Phase3CenterCoord(x, y: integer; a: Axis): integer;
var
  occupied: array [0 .. 7] of boolean;
  c, centc: ColorIndex;
  i, s, k, n: integer;
begin
  centc := faceCols[Ord(a), size div 2, size div 2]; //center color
  for i := 0 to 7 do
  begin
    c := clusterColorIndex(x, y, i + 4 * Ord(a));
    if c = centc then
      occupied[i] := True
    else
      occupied[i] := False;
  end;

  s := 0;
  k := 3; // 4 Cubies
  n := 7; // auf 8 Positionen
  while k >= 0 do
  begin
    if occupied[n] then
      Dec(k)
    else
      s := s + Cnk(n, k);
    Dec(n);
  end;
  Result := 69 - s; // ID soll 0 sein
end;

//axis 0,2,4 are used
procedure faceletCube.InvPhase3CenterCoord(cc, x, y: integer; a: Axis);
var
  n, k, v, c: integer;
  occupied: array [0 .. 7] of boolean;
  col, setCol: ColorIndex;
begin
  cc := 69 - cc;
  for n := 0 to 7 do
    occupied[n] := False;
  n := 7;
  k := 3;
  while k >= 0 do
  begin
    v := Cnk(n, k);
    if cc < v then
    begin
      Dec(k);
      occupied[n] := True;
    end
    else
      Dec(cc, v);
    Dec(n);
  end;

  n := 0;
  setCol := faceCols[Ord(a), size div 2, size div 2]; //center color
  for c := 0 to 7 do
    if occupied[c] then
    begin
      col := clusterColorIndex(x, y, c + 4 * Ord(a));
      if col <> setCol then
      begin
        k := 0;
        while (clusterColorIndex(x, y, k + 4 * Ord(a)) <> setCol) or
          (occupied[k] and (k < c) (* do not change already set colors *)) do
          Inc(k);
        setClusterColorIndex(x, y, c + 4 * Ord(a), setCol);
        setClusterColorIndex(x, y, k + 4 * Ord(a), col);
      end;
      Inc(n);
      if n = 4 then
        setCol := faceCols[Ord(a) + 1, size div 2, size div 2]; //opposite center color
    end;
end;


function faceletCube.getPh3Brick702RLFBCentDepth(x, y: integer): integer;
var
  i, bx, bx_class, bx_sym, bx1, bx1_class, bx1_sym, by, by1, byt, cx,
  cx1, cxt, depth_mod3: integer;
  m: Moves;
begin
  bx := Ph3Brick702Coord(x);
  bx_class := Ph3Brick702CoordToSymCoord[bx].c_idx;
  bx_sym := Ph3Brick702CoordToSymCoord[bx].sym;
  i := 0;//find one of the bitwise coded symmetries
  while (bx_sym and (1 shl i)) = 0 do
    Inc(i);
  bx_sym := i;

  by := Ph3Brick702Coord(y);
  cx := Ph3RLFBCenterCoord(x, y);

  by1 := Ph3RLFBCentCoordSymTransform[by, bx_sym];
  cx1 := Ph3RLFBCentCoordSymTransform[cx, bx_sym];
  depth_mod3 := get_bycx_depth3(bx_class, 4900 * by1 + cx1);


  Result := 0;
  while (bx <> 0) or (by <> 0) or (cx <> 0) do
  begin
    if depth_mod3 = 0 then
      depth_mod3 := 3;

    for m := fU1 to yB3 do
    begin
      if not Phase3Allowed[Ord(m)] then
        continue;
      case m of
        fU1..fB3:
        begin
          bx1 := Ph3RLFBCenterMove[bx, Ord(m)];
          cx1 := Ph3RLFBCenterMove[cx, Ord(m)];
          by1 := Ph3RLFBCenterMove[by, Ord(m)];
        end;
        xU1..xB3:
        begin
          bx1 := Ph3RLFBCenterMove[bx, Ord(m)];
          cx1 := Ph3RLFBCenterMove[cx, Ord(m)];
          by1 := by;// x moves move only x bricks
        end;
        yU1..yB3:
        begin
          bx1 := bx;// y moves move only y bricks
          cx1 := Ph3RLFBCenterMove[cx, Ord(m)];
          by1 := Ph3RLFBCenterMove[by, Ord(m) - 18];
        end;
      end;


      bx1_class := Ph3Brick702CoordToSymCoord[bx1].c_idx;
      bx1_sym := Ph3Brick702CoordToSymCoord[bx1].sym;
      i := 0;//find one bitwise coded symmetry
      while (bx1_sym and (1 shl i)) = 0 do
        Inc(i);
      bx1_sym := i;

      byt := Ph3RLFBCentCoordSymTransform[by1, bx1_sym];
      cxt := Ph3RLFBCentCoordSymTransform[cx1, bx1_sym];
      if get_bycx_depth3(bx1_class, 4900 * byt + cxt) = depth_mod3 - 1 then
      begin
        //Form1.Memo1.Lines.Add(Format('%s', [MoveStrings[m]]));
        ///////////////////////////////////////////////////
        Inc(Result);
        bx := bx1;
        cx := cx1;
        by := by1;
        Dec(depth_mod3);
        break;
      end;
    end;
  end;
end;


function faceletCube.MakePh3RLFBPlusCross(x: integer): boolean;
var
  idx, togo: integer;
begin
  togo := 0;
  found := False;
  for idx := Low(fxymoves) to High(fxymoves) do
    fxymoves[idx] := InitMove;
  while found = False do
  begin
    mvIdx := 0; // 1.freier Platz in fxymoves
    SearchPh3RLFBPlusCross(Ph3RLFBCenterCoord(x, size div 2), togo);
    Inc(togo);
  end;
  Result := True;
end;

procedure faceletCube.SearchPh3RLFBPlusCross(bx, togo: integer);
var
  mv: moves;
  bx1: UInt16;
begin
  if (Ph3RLFBPlusCrossPrun[bx] > togo) then
    Exit;
  if togo = 0 then
  begin
    found := True;
  end
  else
  begin
    mv := InitMove;
    while True do
    begin
      mv := Succ(mv);
      while not Phase3Allowed[Ord(mv)] do
        mv := Succ(mv);
      if mv = NoMove then
      begin
        Exit;
      end
      else
      begin
        if mv < yU1 then
        begin
          bx1 := Ph3RLFBCenterMove[bx, Ord(mv)];
        end
        else
          continue; // no y-moves

        fxymoves[mvIdx] := mv;
        Inc(mvIdx);
        SearchPh3RLFBPlusCross(bx1, togo - 1);

        if found then
          // return without changing mvIdx
          Exit;
        Dec(mvIdx);
      end;
    end;

  end;
end;


function faceletCube.MakePh3Cent702(x, y: integer): boolean;
var
  idx, togo, dx, dy: integer;
begin
  found := False;
  for idx := Low(fxymoves) to High(fxymoves) do
    fxymoves[idx] := InitMove;

  dx := getPh3Brick702RLFBCentDepth(x, y);
  dy := getPh3Brick702RLFBCentDepth(y, x);
  togo := Max(dx, dy);
  while found = False do
  begin
    mvIdx := 0; // 1. empty place in  fxymoves
    //Form1.Memo1.Lines.Add(Format('Searching depth %d...', [togo]));
    SearchPh3Cent702(Ph3Brick702Coord(x), Ph3Brick702Coord(y),
      Ph3RLFBCenterCoord(x, y), Ph3RLFBCenterCoord(y, x), dx, dy, togo);
    Inc(togo);
  end;
  if found then
  begin
    Result := True;

  end
  else
    Result := False;
end;

procedure faceletCube.SearchPh3Cent702(bx, by, cx, cy, dx, dy, togo: integer);
var
  mv: moves;
  bx1, by1, bxt, byt, bx1_class, bx1_sym, by1_class, by1_sym: UInt16;
  cx1, cy1, cxt, cyt, dx1, dy1, i, bycx_distmod3, bxcy_distmod3: integer;

begin
  // Application.ProcessMessages removed
  //Form1.Memo1.Lines.Add(Format('bx: %d, togo: %d, mvidx: %d', [bx, togo, mvidx]));
  //printmoves(1,2);
  if stopProgram then
    exit;

  if (togo = 0) then
  begin
    found := True;
  end
  else
  begin
    mv := initmove; // skip fU1..fD3 moves
    while True do
    begin
      if mvIdx = 0 then
        mv := nextMovePhase3Arr[NoMove, mv]
      else
        mv := nextMovePhase3Arr[fxymoves[mvIdx - 1], mv];

      if mv < fR1 then
        continue; //No U or D moves necessary

      if mv = NoMove then
      begin
        Exit;
      end
      else
      begin

        case mv of
          fU1..fB3:
          begin
            bx1 := Ph3RLFBCenterMove[bx, Ord(mv)];
            cx1 := Ph3RLFBCenterMove[cx, Ord(mv)];
            by1 := Ph3RLFBCenterMove[by, Ord(mv)];
            cy1 := Ph3RLFBCenterMove[cy, Ord(mv)];
          end;
          xU1..xB3:
          begin
            bx1 := Ph3RLFBCenterMove[bx, Ord(mv)];
            cx1 := Ph3RLFBCenterMove[cx, Ord(mv)];
            by1 := by;// x moves move only x bricks
            cy1 := Ph3RLFBCenterMove[cy, Ord(mv) + 18];
          end;
          yU1..yB3:
          begin
            bx1 := bx;// y moves move only y bricks
            cx1 := Ph3RLFBCenterMove[cx, Ord(mv)];
            by1 := Ph3RLFBCenterMove[by, Ord(mv) - 18];
            cy1 := Ph3RLFBCenterMove[cy, Ord(mv) - 18];
          end;
        end;

        bx1_class := Ph3Brick702CoordToSymCoord[bx1].c_idx;
        bx1_sym := Ph3Brick702CoordToSymCoord[bx1].sym;
        i := 0;//find one bitwise coded symmetry
        while (bx1_sym and (1 shl i)) = 0 do
          Inc(i);
        bx1_sym := i;

        byt := Ph3RLFBCentCoordSymTransform[by1, bx1_sym];
        cxt := Ph3RLFBCentCoordSymTransform[cx1, bx1_sym];

        bycx_distmod3 := get_bycx_depth3(bx1_class, 4900 * byt + cxt);
        dx1 := distance[3 * dx + bycx_distmod3];
        if dx1 >= togo then
          continue;

        by1_class := Ph3Brick702CoordToSymCoord[by1].c_idx;
        by1_sym := Ph3Brick702CoordToSymCoord[by1].sym;
        i := 0;//find one bitwise coded symmetry
        while (by1_sym and (1 shl i)) = 0 do
          Inc(i);
        by1_sym := i;

        bxt := Ph3RLFBCentCoordSymTransform[bx1, by1_sym];
        cyt := Ph3RLFBCentCoordSymTransform[cy1, by1_sym];

        bxcy_distmod3 := get_bycx_depth3(by1_class, 4900 * bxt + cyt);
        dy1 := distance[3 * dy + bxcy_distmod3];
        if dy1 >= togo then
          continue;

        fxymoves[mvIdx] := mv;
        Inc(mvIdx);
        SearchPh3Cent702(bx1, by1, cx1, cy1, dx1, dy1, togo - 1);

        if found then
          // return without changing mvIdx
          Exit;
        Dec(mvIdx);
      end;
    end;

  end;

end;


function faceletCube.MakePh3XCross(x: integer): boolean;
var
  idx, togo: integer;
begin
  togo := 0;
  found := False;
  for idx := Low(fxymoves) to High(fxymoves) do
    fxymoves[idx] := InitMove;
  while found = False do
  begin
    mvIdx := 0; // 1. empty position in fxymoves
    SearchPh3XCross(Ph3Brick702Coord(x), Ph3RLFBCenterCoord(x, x), togo);
    Inc(togo);
  end;
  Result := True;
end;

procedure faceletCube.SearchPh3XCross(bx, cx, togo: integer);
var
  mv: moves;
  bx1, cx1: UInt16;
begin
  if (Ph3RLFBXCrossPrun[4900 * bx + cx] > togo) then
    Exit;
  if togo = 0 then
  begin
    found := True;
  end
  else
  begin
    mv := fD3;//no U, D moves//InitMove;
    while True do
    begin
      mv := Succ(mv);
      while not Phase3Allowed[Ord(mv)] do
        mv := Succ(mv);
      if mv = NoMove then
      begin
        Exit;
      end
      else
      begin
        if mv < yU1 then
        begin
          bx1 := Ph3RLFBCenterMove[bx, Ord(mv)];
          cx1 := Ph3RLFBXCrossMove[cx, Ord(mv)];
        end
        else
          continue; // no y-moves

        fxymoves[mvIdx] := mv;
        Inc(mvIdx);
        SearchPh3XCross(bx1, cx1, togo - 1);

        if found then
          // return without changing mvIdx
          Exit;
        Dec(mvIdx);
      end;
    end;

  end;
end;

///////////////////////// phase 4 //////////////////////////////////////////////

function faceletCube.nextMovePh4(idx: integer; currMove: moves): moves;
var
  pm: moves;
begin
  if currMove = yB3 then // done
    Exit(NoMove);
  if idx = 0 then
  begin
    while not Phase4Allowed[Ord(Succ(currMove))] do
      Inc(currMove);
    Exit(Succ(currMove));
  end
  else
  begin
    pm := fxymoves[idx - 1]; // predecessor move
    while True do
    begin
      while not Phase4Allowed[Ord(Succ(currMove))] do
        Inc(currMove);
      currMove := Succ(currMove);
      if currMove = NoMove then
        Exit(NoMove);

      if Ord(pm) < Ord(xU1) then //previous move is face move
      begin
        if (Ord(currMove) <= Ord(pm)) then
          //all face moves commute restricted to the centers
          continue;
        if Ord(currMove) >= Ord(xU1) then //face move followed by slice move
          // always valid
          Exit(currMove)
        else // pm<currmove<xU1
        begin
          if Ord(pm) div 3 = Ord(currMove) div 3 then
            // same face
            continue
          else
            Exit(currMove);
        end;
      end;

      //Ord(pm) >= Ord(xU1), previous move is slice move

      if (Ord(pm) div 18 = Ord(currmove) div 18) and (Ord(currMove) <= Ord(pm)) then
        continue;
      // all x-slice moves commute for  (x,y)-orbit with x<>y becasue we ignore (x,x) centers
      // all y-slice moves commute for  (x,y)-orbit with x<>y

      if (Ord(pm) div 6) mod 3 <> (Ord(currMove) div 6) mod 3 then
        Exit(currMove);
      // pm and currMove are on different axes and hence do not commute


      // both moves are on the same axis and commute
      // we can force an order
      if Ord(currMove) <= Ord(pm) then
        continue;

      // if the  prefixes f,x,y are different for both moves, currmove is valid
      if Ord(currMove) div 18 <> Ord(pm) div 18 then
        Exit(currMove);

      // we have the same prefix and the same axis
      if (Ord(currMove) mod 6) div 3 <> (Ord(pm) mod 6) div 3 then
        // moves are on different slices of the axis
        Exit(currMove)
      else
        continue;
    end;
  end;
end;


procedure faceletCube.InvPhase4RLFBBrickCoord(cc, x, y: integer);
var
  m, i: integer;
begin

  m := size div 2;
  for i := 0 to 23 do
  begin
    setClusterColorIndex(x, m, i, NoCol);
    setClusterColorIndex(y, m, i, NoCol);
  end;


  if Odd(cc) then
    faceCols[Ord(b), m, y] := FCol
  else
    faceCols[Ord(F), m, size - 1 - y] := FCol;
  cc := cc div 2;
  if Odd(cc) then
    faceCols[Ord(b), m, size - 1 - y] := FCol
  else
    faceCols[Ord(F), m, y] := FCol;
  cc := cc div 2;

  if Odd(cc) then
    faceCols[Ord(L), m, y] := RCol
  else
    faceCols[Ord(R), m, size - 1 - y] := RCol;
  cc := cc div 2;
  if Odd(cc) then
    faceCols[Ord(L), m, size - 1 - y] := RCol
  else
    faceCols[Ord(R), m, y] := RCol;
  cc := cc div 2;

  if Odd(cc) then
    faceCols[Ord(b), m, x] := FCol
  else
    faceCols[Ord(F), m, size - 1 - x] := FCol;
  cc := cc div 2;
  if Odd(cc) then
    faceCols[Ord(b), m, size - 1 - x] := FCol
  else
    faceCols[Ord(F), m, x] := FCol;
  cc := cc div 2;

  if Odd(cc) then
    faceCols[Ord(L), m, x] := RCol
  else
    faceCols[Ord(R), m, size - 1 - x] := RCol;
  cc := cc div 2;
  if Odd(cc) then
    faceCols[Ord(L), m, size - 1 - x] := RCol
  else
    faceCols[Ord(R), m, x] := RCol;

end;


function faceletCube.Phase4RLFBBrickCoord(x, y: integer): integer;
  //0<=cc<256. if x-bricks are solved 0<=cc<16
var
  m: integer;
begin
  m := size div 2;
  Result := 0;
  Result := 2 * Result;
  if faceCols[Ord(R), m, x] <> RCol then
    Inc(Result);
  Result := 2 * Result;
  if faceCols[Ord(R), m, size - 1 - x] <> RCol then
    Inc(Result);
  Result := 2 * Result;
  if faceCols[Ord(F), m, x] <> FCol then
    Inc(Result);
  Result := 2 * Result;
  if faceCols[Ord(F), m, size - 1 - x] <> FCol then
    Inc(Result);

  Result := 2 * Result;
  if faceCols[Ord(R), m, y] <> RCol then
    Inc(Result);
  Result := 2 * Result;
  if faceCols[Ord(R), m, size - 1 - y] <> RCol then
    Inc(Result);
  Result := 2 * Result;
  if faceCols[Ord(F), m, y] <> FCol then
    Inc(Result);
  Result := 2 * Result;
  if faceCols[Ord(F), m, size - 1 - y] <> FCol then
    Inc(Result);
end;



function faceletCube.MakePh4UDPlusCross(x: integer): boolean;
var
  idx, togo: integer;
begin
  togo := 0;
  found := False;
  for idx := Low(fxymoves) to High(fxymoves) do
    fxymoves[idx] := InitMove;
  while found = False do
  begin
    mvIdx := 0; // 1.freier Platz in fxymoves
    SearchPh4UDPlusCross(Phase3CenterCoord(x, size div 2, U), 0, togo);
    Inc(togo);
  end;
  Result := True;
end;


procedure faceletCube.SearchPh4UDPlusCross(c, b, togo: integer);
var
  mv: moves;
  b1, c1: UInt16;
begin
  if (Ph4UDPlusCrossPrun[16 * c + b] > togo) then
    Exit;
  if togo = 0 then
  begin
    found := True;
  end
  else
  begin
    mv := InitMove;
    while True do
    begin
      mv := Succ(mv);
      while not Phase4Allowed[Ord(mv)] do
        mv := Succ(mv);
      if mv > xB2 then //no y-moves
      begin
        Exit;
      end
      else
      begin

        case mv of
          fU1..fB3:
          begin
            b1 := b;//allowed face moves do not change this
            c1 := Phase4CenterMove[c, Ord(mv)];
          end;
          xU1..xB3:
          begin
            b1 := Phase4RLFBBrickMove[b, Ord(mv) + 18];
            // must use y-bricks internally for 0<=b<16
            c1 := Phase4CenterMove[c, Ord(mv)];
          end;
        end;

        fxymoves[mvIdx] := mv;
        Inc(mvIdx);
        SearchPh4UDPlusCross(c1, b1, togo - 1);
        if found then
          // return without changing mvIdx
          Exit;
        Dec(mvIdx);
      end;
    end;

  end;
end;

function faceletCube.Phase4UDBrickCoord(x, y: integer): integer;
begin
  Result := B_8_4 * Phase3CenterCoord(x, size div 2, U) +
    Phase3CenterCoord(y, size div 2, U);
end;

procedure faceletCube.InvPhase4UDBrickCoord(cc, x, y: integer);
begin
  InvPhase3CenterCoord(cc div B_8_4, x, size div 2, U);
  InvPhase3CenterCoord(cc mod B_8_4, y, size div 2, U);
end;



function faceletCube.MakePh4UDCenters(x, y: integer): boolean;
var
  idx, togo: integer;
begin
  togo := 0;
  found := False;
  for idx := Low(fxymoves) to High(fxymoves) do
    fxymoves[idx] := InitMove;
  //Phase3CenterCoord(x, y, R)
  while found = False do
  begin
    mvIdx := 0; // 1.freier Platz in fxymoves
    SearchPh4UDCenter(Phase3CenterCoord(x, y, U),
      Phase3CenterCoord(y, x, U), Phase4UDBrickCoord(x, y),
      Phase4RLFBBrickCoord(x, y), togo);
    Inc(togo);
  end;
  Result := True;
end;


procedure faceletCube.SearchPh4UDCenter(cx, cy, bxy, bo, togo: integer);
var
  mv: moves;
  cx1, cy1, bxy1, bo1: integer;

begin
  // Application.ProcessMessages removed
  if Ph4UDCentBrickPrun[B_8_4 * (B_8_4 * bxy + cx) + cy] > togo then
    Exit;
  if (togo = 0) and (bo = 0) then
  begin
    found := True;
  end
  else
  begin
    mv := InitMove;

    while True do
    begin

      if mvIdx = 0 then
        mv := nextMovePhase4Arr[NoMove, mv]
      else
        mv := nextMovePhase4Arr[fxymoves[mvIdx - 1], mv];

      if mv = NoMove then
      begin
        Exit;
      end
      else
      begin

        cx1 := Phase4CenterMove[cx, Ord(mv)];
        bxy1 := Phase4UDBrickMove[bxy, Ord(mv)];
        case mv of
          fU1..fB3:
          begin
            cy1 := Phase4CenterMove[cy, Ord(mv)];
            bo1 := bo;
          end;
          xU1..xB3:
          begin
            bo1 := Phase4RLFBBrickMove[bo, Ord(mv)];
            cy1 := Phase4CenterMove[cy, Ord(mv) + 18];
          end;
          yU1..yB3:
          begin
            bo1 := Phase4RLFBBrickMove[bo, Ord(mv)];
            cy1 := Phase4CenterMove[cy, Ord(mv) - 18];
          end;
        end;

        fxymoves[mvIdx] := mv;
        Inc(mvIdx);
        SearchPh4UDCenter(cx1, cy1, bxy1, bo1, togo - 1);

        if found then
          // kehre zurück, ohne mvIdx zu verändern
          Exit;
        Dec(mvIdx);
      end;
    end;

  end;
end;


function faceletCube.MakePh4XCross(x: integer): boolean;
var
  idx, togo: integer;
begin
  togo := 0;
  found := False;
  for idx := Low(fxymoves) to High(fxymoves) do
    fxymoves[idx] := InitMove;

  while found = False do
  begin
    mvIdx := 0;
    SearchPh4XCross(Phase3CenterCoord(x, x, U),
      Phase4UDBrickCoord(x, x) mod B_8_4,
      { get only y-part}
      Phase4RLFBBrickCoord(x, x) and $F { get only y-part}, togo);
    Inc(togo);
  end;
  Result := True;
end;

procedure faceletCube.SearchPh4XCross(cx, b, bo, togo: integer);
var
  mv: moves;
  cx1, b1, bo1: UInt16;

begin
  // Application.ProcessMessages removed

  if Ph4UDXCrossPrun[B_8_4 * (B_8_4 * bo + b) + cx] > togo then
    Exit;
  if (togo = 0) then
  begin
    found := True;
  end
  else
  begin
    mv := InitMove;

    while True do
    begin
      if mvIdx = 0 then
        mv := nextMovePhase4Arr[NoMove, mv]
      else
        mv := nextMovePhase4Arr[fxymoves[mvIdx - 1], mv];

      if mv > xB2 then
      begin
        Exit;
      end
      else
      begin
        cx1 := Phase4UDXCrossMove[cx, Ord(mv)];
        case mv of
          fU1..fB3:
          begin
            b1 := Phase4UDBrickMove[b, Ord(mv)];
            bo1 := bo;
          end;
          xU1..xB3:
          begin
            b1 := Phase4UDBrickMove[b, Ord(mv) + 18];
            bo1 := Phase4RLFBBrickMove[bo, Ord(mv) + 18];
          end;

        end;


        fxymoves[mvIdx] := mv;
        Inc(mvIdx);
        SearchPh4XCross(cx1, b1, bo1, togo - 1);

        if found then
          // kehre zurück, ohne mvIdx zu verändern
          Exit;
        Dec(mvIdx);
      end;
    end;

  end;
end;

//phase 5
procedure faceletcube.edgemove(x: integer; mv: Moves);
//ecls,edgemx must be initialized!
var
  i, tmp: integer;
begin
  case mv of
    fU1:
      for i := 1 to edgemx do
      begin
        tmp := ecls[i, 0];
        ecls[i, 0] := ecls[i, 3];
        ecls[i, 3] := ecls[i, 2];
        ecls[i, 2] := ecls[i, 1];
        ecls[i, 1] := tmp;
        tmp := ecls[i, 12];
        ecls[i, 12] := ecls[i, 22];
        ecls[i, 22] := ecls[i, 16];
        ecls[i, 16] := ecls[i, 20];
        ecls[i, 20] := tmp;
      end;
    fD1:
      for i := 1 to edgemx do
      begin
        tmp := ecls[i, 4];
        ecls[i, 4] := ecls[i, 7];
        ecls[i, 7] := ecls[i, 6];
        ecls[i, 6] := ecls[i, 5];
        ecls[i, 5] := tmp;
        tmp := ecls[i, 21];
        ecls[i, 21] := ecls[i, 18];
        ecls[i, 18] := ecls[i, 23];
        ecls[i, 23] := ecls[i, 14];
        ecls[i, 14] := tmp;
      end;
    fR1:
      for i := 1 to edgemx do
      begin
        tmp := ecls[i, 12];
        ecls[i, 12] := ecls[i, 15];
        ecls[i, 15] := ecls[i, 14];
        ecls[i, 14] := ecls[i, 13];
        ecls[i, 13] := tmp;
        tmp := ecls[i, 8];
        ecls[i, 8] := ecls[i, 5];
        ecls[i, 5] := ecls[i, 11];
        ecls[i, 11] := ecls[i, 1];
        ecls[i, 1] := tmp;
      end;
    fL1:
      for i := 1 to edgemx do
      begin
        tmp := ecls[i, 16];
        ecls[i, 16] := ecls[i, 19];
        ecls[i, 19] := ecls[i, 18];
        ecls[i, 18] := ecls[i, 17];
        ecls[i, 17] := tmp;
        tmp := ecls[i, 3];
        ecls[i, 3] := ecls[i, 10];
        ecls[i, 10] := ecls[i, 7];
        ecls[i, 7] := ecls[i, 9];
        ecls[i, 9] := tmp;
      end;
    fF1:
      for i := 1 to edgemx do
      begin
        tmp := ecls[i, 20];
        ecls[i, 20] := ecls[i, 9];
        ecls[i, 9] := ecls[i, 21];
        ecls[i, 21] := ecls[i, 8];
        ecls[i, 8] := tmp;
        tmp := ecls[i, 2];
        ecls[i, 2] := ecls[i, 17];
        ecls[i, 17] := ecls[i, 4];
        ecls[i, 4] := ecls[i, 15];
        ecls[i, 15] := tmp;
      end;
    fB1:
      for i := 1 to edgemx do
      begin
        tmp := ecls[i, 22];
        ecls[i, 22] := ecls[i, 11];
        ecls[i, 11] := ecls[i, 23];
        ecls[i, 23] := ecls[i, 10];
        ecls[i, 10] := tmp;
        tmp := ecls[i, 0];
        ecls[i, 0] := ecls[i, 13];
        ecls[i, 13] := ecls[i, 6];
        ecls[i, 6] := ecls[i, 19];
        ecls[i, 19] := tmp;
      end;

    // Direct double face moves (2-swap pairs, avoids 2x loop overhead)
    fU2:
      for i := 1 to edgemx do
      begin
        tmp := ecls[i, 0]; ecls[i, 0] := ecls[i, 2]; ecls[i, 2] := tmp;
        tmp := ecls[i, 1]; ecls[i, 1] := ecls[i, 3]; ecls[i, 3] := tmp;
        tmp := ecls[i, 12]; ecls[i, 12] := ecls[i, 16]; ecls[i, 16] := tmp;
        tmp := ecls[i, 22]; ecls[i, 22] := ecls[i, 20]; ecls[i, 20] := tmp;
      end;
    fD2:
      for i := 1 to edgemx do
      begin
        tmp := ecls[i, 4]; ecls[i, 4] := ecls[i, 6]; ecls[i, 6] := tmp;
        tmp := ecls[i, 5]; ecls[i, 5] := ecls[i, 7]; ecls[i, 7] := tmp;
        tmp := ecls[i, 21]; ecls[i, 21] := ecls[i, 23]; ecls[i, 23] := tmp;
        tmp := ecls[i, 18]; ecls[i, 18] := ecls[i, 14]; ecls[i, 14] := tmp;
      end;
    fR2:
      for i := 1 to edgemx do
      begin
        tmp := ecls[i, 12]; ecls[i, 12] := ecls[i, 14]; ecls[i, 14] := tmp;
        tmp := ecls[i, 13]; ecls[i, 13] := ecls[i, 15]; ecls[i, 15] := tmp;
        tmp := ecls[i, 8]; ecls[i, 8] := ecls[i, 11]; ecls[i, 11] := tmp;
        tmp := ecls[i, 5]; ecls[i, 5] := ecls[i, 1]; ecls[i, 1] := tmp;
      end;
    fL2:
      for i := 1 to edgemx do
      begin
        tmp := ecls[i, 16]; ecls[i, 16] := ecls[i, 18]; ecls[i, 18] := tmp;
        tmp := ecls[i, 17]; ecls[i, 17] := ecls[i, 19]; ecls[i, 19] := tmp;
        tmp := ecls[i, 3]; ecls[i, 3] := ecls[i, 7]; ecls[i, 7] := tmp;
        tmp := ecls[i, 10]; ecls[i, 10] := ecls[i, 9]; ecls[i, 9] := tmp;
      end;
    fF2:
      for i := 1 to edgemx do
      begin
        tmp := ecls[i, 20]; ecls[i, 20] := ecls[i, 21]; ecls[i, 21] := tmp;
        tmp := ecls[i, 9]; ecls[i, 9] := ecls[i, 8]; ecls[i, 8] := tmp;
        tmp := ecls[i, 2]; ecls[i, 2] := ecls[i, 4]; ecls[i, 4] := tmp;
        tmp := ecls[i, 17]; ecls[i, 17] := ecls[i, 15]; ecls[i, 15] := tmp;
      end;
    fB2:
      for i := 1 to edgemx do
      begin
        tmp := ecls[i, 22]; ecls[i, 22] := ecls[i, 23]; ecls[i, 23] := tmp;
        tmp := ecls[i, 11]; ecls[i, 11] := ecls[i, 10]; ecls[i, 10] := tmp;
        tmp := ecls[i, 0]; ecls[i, 0] := ecls[i, 6]; ecls[i, 6] := tmp;
        tmp := ecls[i, 13]; ecls[i, 13] := ecls[i, 19]; ecls[i, 19] := tmp;
      end;
    // Direct inverse face moves (reverse 4-cycle: 0←1←2←3←0)
    fU3:
      for i := 1 to edgemx do
      begin
        tmp := ecls[i, 0];
        ecls[i, 0] := ecls[i, 1];
        ecls[i, 1] := ecls[i, 2];
        ecls[i, 2] := ecls[i, 3];
        ecls[i, 3] := tmp;
        tmp := ecls[i, 12];
        ecls[i, 12] := ecls[i, 20];
        ecls[i, 20] := ecls[i, 16];
        ecls[i, 16] := ecls[i, 22];
        ecls[i, 22] := tmp;
      end;
    fD3:
      for i := 1 to edgemx do
      begin
        tmp := ecls[i, 4];
        ecls[i, 4] := ecls[i, 5];
        ecls[i, 5] := ecls[i, 6];
        ecls[i, 6] := ecls[i, 7];
        ecls[i, 7] := tmp;
        tmp := ecls[i, 21];
        ecls[i, 21] := ecls[i, 14];
        ecls[i, 14] := ecls[i, 23];
        ecls[i, 23] := ecls[i, 18];
        ecls[i, 18] := tmp;
      end;
    fR3:
      for i := 1 to edgemx do
      begin
        tmp := ecls[i, 12];
        ecls[i, 12] := ecls[i, 13];
        ecls[i, 13] := ecls[i, 14];
        ecls[i, 14] := ecls[i, 15];
        ecls[i, 15] := tmp;
        tmp := ecls[i, 8];
        ecls[i, 8] := ecls[i, 1];
        ecls[i, 1] := ecls[i, 11];
        ecls[i, 11] := ecls[i, 5];
        ecls[i, 5] := tmp;
      end;
    fL3:
      for i := 1 to edgemx do
      begin
        tmp := ecls[i, 16];
        ecls[i, 16] := ecls[i, 17];
        ecls[i, 17] := ecls[i, 18];
        ecls[i, 18] := ecls[i, 19];
        ecls[i, 19] := tmp;
        tmp := ecls[i, 3];
        ecls[i, 3] := ecls[i, 9];
        ecls[i, 9] := ecls[i, 7];
        ecls[i, 7] := ecls[i, 10];
        ecls[i, 10] := tmp;
      end;
    fF3:
      for i := 1 to edgemx do
      begin
        tmp := ecls[i, 20];
        ecls[i, 20] := ecls[i, 8];
        ecls[i, 8] := ecls[i, 21];
        ecls[i, 21] := ecls[i, 9];
        ecls[i, 9] := tmp;
        tmp := ecls[i, 2];
        ecls[i, 2] := ecls[i, 15];
        ecls[i, 15] := ecls[i, 4];
        ecls[i, 4] := ecls[i, 17];
        ecls[i, 17] := tmp;
      end;
    fB3:
      for i := 1 to edgemx do
      begin
        tmp := ecls[i, 22];
        ecls[i, 22] := ecls[i, 10];
        ecls[i, 10] := ecls[i, 23];
        ecls[i, 23] := ecls[i, 11];
        ecls[i, 11] := tmp;
        tmp := ecls[i, 0];
        ecls[i, 0] := ecls[i, 19];
        ecls[i, 19] := ecls[i, 6];
        ecls[i, 6] := ecls[i, 13];
        ecls[i, 13] := tmp;
      end;

    xU1:
    begin
      tmp := ecls[x, 8];
      ecls[x, 8] := ecls[x, 13];
      ecls[x, 13] := ecls[x, 10];
      ecls[x, 10] := ecls[x, 17];
      ecls[x, 17] := tmp;
    end;
    xD1:
    begin
      tmp := ecls[x, 9];
      ecls[x, 9] := ecls[x, 19];
      ecls[x, 19] := ecls[x, 11];
      ecls[x, 11] := ecls[x, 15];
      ecls[x, 15] := tmp;
    end;
    xR1:
    begin
      tmp := ecls[x, 2];
      ecls[x, 2] := ecls[x, 21];
      ecls[x, 21] := ecls[x, 6];
      ecls[x, 6] := ecls[x, 22];
      ecls[x, 22] := tmp;
    end;
    xL1:
    begin
      tmp := ecls[x, 20];
      ecls[x, 20] := ecls[x, 0];
      ecls[x, 0] := ecls[x, 23];
      ecls[x, 23] := ecls[x, 4];
      ecls[x, 4] := tmp;
    end;
    xF1:
    begin
      tmp := ecls[x, 3];
      ecls[x, 3] := ecls[x, 18];
      ecls[x, 18] := ecls[x, 5];
      ecls[x, 5] := ecls[x, 12];
      ecls[x, 12] := tmp;
    end;
    xB1:
    begin
      tmp := ecls[x, 1];
      ecls[x, 1] := ecls[x, 14];
      ecls[x, 14] := ecls[x, 7];
      ecls[x, 7] := ecls[x, 16];
      ecls[x, 16] := tmp;
    end;

    // Direct double inner slice moves (2-swap pairs)
    xU2:
    begin
      tmp := ecls[x, 8]; ecls[x, 8] := ecls[x, 10]; ecls[x, 10] := tmp;
      tmp := ecls[x, 13]; ecls[x, 13] := ecls[x, 17]; ecls[x, 17] := tmp;
    end;
    xD2:
    begin
      tmp := ecls[x, 9]; ecls[x, 9] := ecls[x, 11]; ecls[x, 11] := tmp;
      tmp := ecls[x, 19]; ecls[x, 19] := ecls[x, 15]; ecls[x, 15] := tmp;
    end;
    xR2:
    begin
      tmp := ecls[x, 2]; ecls[x, 2] := ecls[x, 6]; ecls[x, 6] := tmp;
      tmp := ecls[x, 21]; ecls[x, 21] := ecls[x, 22]; ecls[x, 22] := tmp;
    end;
    xL2:
    begin
      tmp := ecls[x, 20]; ecls[x, 20] := ecls[x, 23]; ecls[x, 23] := tmp;
      tmp := ecls[x, 0]; ecls[x, 0] := ecls[x, 4]; ecls[x, 4] := tmp;
    end;
    xF2:
    begin
      tmp := ecls[x, 3]; ecls[x, 3] := ecls[x, 5]; ecls[x, 5] := tmp;
      tmp := ecls[x, 18]; ecls[x, 18] := ecls[x, 12]; ecls[x, 12] := tmp;
    end;
    xB2:
    begin
      tmp := ecls[x, 1]; ecls[x, 1] := ecls[x, 7]; ecls[x, 7] := tmp;
      tmp := ecls[x, 14]; ecls[x, 14] := ecls[x, 16]; ecls[x, 16] := tmp;
    end;

    // Direct inverse inner slice moves (reverse 4-cycle)
    xU3:
    begin
      tmp := ecls[x, 8]; ecls[x, 8] := ecls[x, 17]; ecls[x, 17] := ecls[x, 10]; ecls[x, 10] := ecls[x, 13]; ecls[x, 13] := tmp;
    end;
    xD3:
    begin
      tmp := ecls[x, 9]; ecls[x, 9] := ecls[x, 15]; ecls[x, 15] := ecls[x, 11]; ecls[x, 11] := ecls[x, 19]; ecls[x, 19] := tmp;
    end;
    xR3:
    begin
      tmp := ecls[x, 2]; ecls[x, 2] := ecls[x, 22]; ecls[x, 22] := ecls[x, 6]; ecls[x, 6] := ecls[x, 21]; ecls[x, 21] := tmp;
    end;
    xL3:
    begin
      tmp := ecls[x, 20]; ecls[x, 20] := ecls[x, 4]; ecls[x, 4] := ecls[x, 23]; ecls[x, 23] := ecls[x, 0]; ecls[x, 0] := tmp;
    end;
    xF3:
    begin
      tmp := ecls[x, 3]; ecls[x, 3] := ecls[x, 12]; ecls[x, 12] := ecls[x, 5]; ecls[x, 5] := ecls[x, 18]; ecls[x, 18] := tmp;
    end;
    xB3:
    begin
      tmp := ecls[x, 1]; ecls[x, 1] := ecls[x, 16]; ecls[x, 16] := ecls[x, 7]; ecls[x, 7] := ecls[x, 14]; ecls[x, 14] := tmp;
    end;
  end;
end;


procedure faceletcube.store(ed: Edge);
var
  allowBuffer: boolean;
begin
  // Reserve UB as buffer slot: only store to UB when it's the last bad horizontal
  allowBuffer := not AnyBadHorizontalExcept(UB);
  case ed of
    FL:
    begin
      if badEdgeCnt(UR) <> 0 then
      begin
        addmove(fF1);
        addmove(fU1);
        addmove(fF3);
      end
      else if badEdgeCnt(UL) <> 0 then
      begin
        addmove(fF1);
        addmove(fU3);
        addmove(fF3);
      end
      else if badEdgeCnt(UF) <> 0 then
      begin
        addmove(fL3);
        addmove(fU1);
        addmove(fL1);
      end
      else if allowBuffer and (badEdgeCnt(UB) <> 0) then
      begin
        addmove(fL3);
        addmove(fU3);
        addmove(fL1);
      end

      else if badEdgeCnt(DR) <> 0 then
      begin
        addmove(fF3);
        addmove(fD3);
        addmove(fF1);
      end
      else if badEdgeCnt(DL) <> 0 then
      begin
        addmove(fF3);
        addmove(fD1);
        addmove(fF1);
      end
      else if badEdgeCnt(DF) <> 0 then
      begin
        addmove(fL1);
        addmove(fD3);
        addmove(fL3);
      end
      else if badEdgeCnt(DB) <> 0 then
      begin
        addmove(fL1);
        addmove(fD1);
        addmove(fL3);
      end
      // Buffer fallback: use UB if it's the only bad horizontal
      else if badEdgeCnt(UB) <> 0 then
      begin
        addmove(fL3);
        addmove(fU3);
        addmove(fL1);
      end;
    end;//FL
    BL:
    begin
      if badEdgeCnt(UF) <> 0 then
      begin
        addmove(fL1);
        addmove(fU1);
        addmove(fL3);
      end
      else if allowBuffer and (badEdgeCnt(UB) <> 0) then
      begin
        addmove(fL1);
        addmove(fU3);
        addmove(fL3);
      end
      else if badEdgeCnt(UL) <> 0 then
      begin
        addmove(fB3);
        addmove(fU1);
        addmove(fB1);
      end
      else if badEdgeCnt(UR) <> 0 then
      begin
        addmove(fB3);
        addmove(fU3);
        addmove(fB1);
      end

      else if badEdgeCnt(DF) <> 0 then
      begin
        addmove(fL3);
        addmove(fD3);
        addmove(fL1);
      end
      else if badEdgeCnt(DB) <> 0 then
      begin
        addmove(fL3);
        addmove(fD1);
        addmove(fL1);
      end
      else if badEdgeCnt(DL) <> 0 then
      begin
        addmove(fB1);
        addmove(fD3);
        addmove(fB3);
      end
      else if badEdgeCnt(DR) <> 0 then
      begin
        addmove(fB1);
        addmove(fD1);
        addmove(fB3);
      end
      // Buffer fallback: use UB if it's the only bad horizontal
      else if badEdgeCnt(UB) <> 0 then
      begin
        addmove(fL1);
        addmove(fU3);
        addmove(fL3);
      end;
    end;
    FR:
    begin
      if allowBuffer and (badEdgeCnt(UB) <> 0) then
      begin
        addmove(fR1);
        addmove(fU1);
        addmove(fR3);
      end
      else if badEdgeCnt(UF) <> 0 then
      begin
        addmove(fR1);
        addmove(fU3);
        addmove(fR3);
      end
      else if badEdgeCnt(UR) <> 0 then
      begin
        addmove(fF3);
        addmove(fU1);
        addmove(fF1);
      end
      else if badEdgeCnt(UL) <> 0 then
      begin
        addmove(fF3);
        addmove(fU3);
        addmove(fF1);
      end

      else if badEdgeCnt(DB) <> 0 then
      begin
        addmove(fR3);
        addmove(fD3);
        addmove(fR1);
      end
      else if badEdgeCnt(DF) <> 0 then
      begin
        addmove(fR3);
        addmove(fD1);
        addmove(fR1);
      end
      else if badEdgeCnt(DR) <> 0 then
      begin
        addmove(fF1);
        addmove(fD3);
        addmove(fF3);
      end
      else if badEdgeCnt(DL) <> 0 then
      begin
        addmove(fF1);
        addmove(fD1);
        addmove(fF3);
      end
      // Buffer fallback: use UB if it's the only bad horizontal
      else if badEdgeCnt(UB) <> 0 then
      begin
        addmove(fR1);
        addmove(fU1);
        addmove(fR3);
      end;
    end;
    BR:
    begin
      if badEdgeCnt(UL) <> 0 then
      begin
        addmove(fB1);
        addmove(fU1);
        addmove(fB3);
      end
      else if badEdgeCnt(UR) <> 0 then
      begin
        addmove(fB1);
        addmove(fU3);
        addmove(fB3);
      end
      else if allowBuffer and (badEdgeCnt(UB) <> 0) then
      begin
        addmove(fR3);
        addmove(fU1);
        addmove(fR1);
      end
      else if badEdgeCnt(UF) <> 0 then
      begin
        addmove(fR3);
        addmove(fU3);
        addmove(fR1);
      end

      else if badEdgeCnt(DL) <> 0 then
      begin
        addmove(fB3);
        addmove(fD3);
        addmove(fB1);
      end
      else if badEdgeCnt(DR) <> 0 then
      begin
        addmove(fB3);
        addmove(fD1);
        addmove(fB1);
      end
      else if badEdgeCnt(DB) <> 0 then
      begin
        addmove(fR1);
        addmove(fD3);
        addmove(fR3);
      end
      else if badEdgeCnt(DF) <> 0 then
      begin
        addmove(fR1);
        addmove(fD1);
        addmove(fR3);
      end
      // Buffer fallback: use UB if it's the only bad horizontal
      else if badEdgeCnt(UB) <> 0 then
      begin
        addmove(fR3);
        addmove(fU1);
        addmove(fR1);
      end;
    end;//BR
  end;
end;

function faceletCube.AnyBadHorizontalExcept(buf: Edge): boolean;
begin
  Result :=
    ((buf <> UR) and (badEdgeCnt(UR) <> 0)) or
    ((buf <> UF) and (badEdgeCnt(UF) <> 0)) or
    ((buf <> UL) and (badEdgeCnt(UL) <> 0)) or
    ((buf <> UB) and (badEdgeCnt(UB) <> 0)) or
    ((buf <> DR) and (badEdgeCnt(DR) <> 0)) or
    ((buf <> DF) and (badEdgeCnt(DF) <> 0)) or
    ((buf <> DL) and (badEdgeCnt(DL) <> 0)) or
    ((buf <> DB) and (badEdgeCnt(DB) <> 0));
end;

function faceletCube.badVerticalTotal(orb: integer): integer;
var
  e: Edge;
begin
  Result := 0;
  for e := FR to BR do
  begin
    if ecls[orb, edgeidx[e, 0]] <> ecls[edgemx, edgeidx[e, 0]] then
      Inc(Result);
    if ecls[orb, edgeidx[e, 1]] <> ecls[edgemx, edgeidx[e, 1]] then
      Inc(Result);
  end;
end;

function faceletCube.badAllTotal(orb: integer): integer;
var
  e: Edge;
begin
  Result := 0;
  for e := UR to BR do
  begin
    if ecls[orb, edgeidx[e, 0]] <> ecls[edgemx, edgeidx[e, 0]] then
      Inc(Result);
    if ecls[orb, edgeidx[e, 1]] <> ecls[edgemx, edgeidx[e, 1]] then
      Inc(Result);
  end;
end;

procedure faceletcube.addmove(m: Moves);
begin
  fxymoves[mvIDx] := m;
  Inc(mvIDx);
end;

function faceletcube.tryDirectImprove(orb, brick, badEdgesStart: integer): boolean;
// Direct enumeration of all valid improving sequences instead of IDA* search.
// Every useful improving sequence follows the pattern:
//   InnerMove1 + FaceOpen + U/D_middle(s) + FaceClose + InnerMove2
// where the two inner moves balance displacement (same axis, sum to 4 mod 4).
// This gives 288 combinations for depth 5, 1728 for depth 6, 10368 for depth 7.
// Compared to IDA* exploring millions of nodes, this is nearly instant.
const
  InnerPairs: array[0..5, 0..1] of Moves = (
    (xU1, xU3), (xU3, xU1), (xU2, xU2),
    (xD1, xD3), (xD3, xD1), (xD2, xD2)
  );
  FaceOpeners: array[0..11] of Moves = (
    fR1, fR2, fR3, fL1, fL2, fL3, fF1, fF2, fF3, fB1, fB2, fB3
  );
  UDMoves: array[0..5] of Moves = (
    fU1, fU2, fU3, fD1, fD2, fD3
  );
var
  ip, fi, mi, m2i, m3i, o, k: integer;
  i1, i2, f, m1, m2, m3: Moves;
  badAfter: integer;
  savedEcls: array[1..8, 0..23] of integer;
begin
  Result := False;

  // Save ecls state for fast restore
  for o := 1 to edgemx do
    for k := 0 to 23 do
      savedEcls[o, k] := ecls[o, k];

  // Depth 5: I + F + M + F' + I (288 combinations)
  for ip := 0 to 5 do
  begin
    i1 := InnerPairs[ip, 0];
    i2 := InnerPairs[ip, 1];
    for fi := 0 to 11 do
    begin
      f := FaceOpeners[fi];
      for mi := 0 to 5 do
      begin
        m1 := UDMoves[mi];
        // Apply: i1, f, m1, f', i2
        edgemove(orb, i1);
        edgemove(orb, f);
        edgemove(orb, m1);
        edgemove(orb, invMove[f]);
        edgemove(orb, i2);

        badAfter := badEdgeCntIdx(orb, brick);
        if badAfter < badEdgesStart then
        begin
          fxymoves[0] := i1;
          fxymoves[1] := f;
          fxymoves[2] := m1;
          fxymoves[3] := invMove[f];
          fxymoves[4] := i2;
          mvIdx := 5;
          found := True;
          Result := True;
          exit;
        end;

        // Restore state
        for o := 1 to edgemx do
          for k := 0 to 23 do
            ecls[o, k] := savedEcls[o, k];
      end;
    end;
  end;

  // Depth 6: I + F + M1 + M2 + F' + I (1728 combinations)
  for ip := 0 to 5 do
  begin
    i1 := InnerPairs[ip, 0];
    i2 := InnerPairs[ip, 1];
    for fi := 0 to 11 do
    begin
      f := FaceOpeners[fi];
      for mi := 0 to 5 do
      begin
        m1 := UDMoves[mi];
        for m2i := 0 to 5 do
        begin
          m2 := UDMoves[m2i];
          // Skip consecutive same-axis U/D moves
          if Ord(m1) div 3 = Ord(m2) div 3 then
            continue;

          edgemove(orb, i1);
          edgemove(orb, f);
          edgemove(orb, m1);
          edgemove(orb, m2);
          edgemove(orb, invMove[f]);
          edgemove(orb, i2);

          badAfter := badEdgeCntIdx(orb, brick);
          if badAfter < badEdgesStart then
          begin
            fxymoves[0] := i1;
            fxymoves[1] := f;
            fxymoves[2] := m1;
            fxymoves[3] := m2;
            fxymoves[4] := invMove[f];
            fxymoves[5] := i2;
            mvIdx := 6;
            found := True;
            Result := True;
            exit;
          end;

          for o := 1 to edgemx do
            for k := 0 to 23 do
              ecls[o, k] := savedEcls[o, k];
        end;
      end;
    end;
  end;

  // Depth 6 double-face: I + F1 + F2 + close1 + close2 + I
  // Two close orders: (F1' F2') and (F2' F1')
  // F1 and F2 must use different conjugation tracks:
  //   R-track (rturn): fR1..fR3
  //   L-track (lturn): fL1..fL3
  //   FB-track (fturn): fF1..fF3, fB1..fB3
  for ip := 0 to 5 do
  begin
    i1 := InnerPairs[ip, 0];
    i2 := InnerPairs[ip, 1];
    for fi := 0 to 11 do
    begin
      f := FaceOpeners[fi];
      for m2i := 0 to 11 do
      begin
        m1 := FaceOpeners[m2i]; // second face opener
        // Must use different conjugation track
        // FaceOpeners: 0-2=R, 3-5=L, 6-8=F, 9-11=B
        if (fi div 3 = m2i div 3) then
          continue; // same axis
        if ((fi div 3 >= 2) and (m2i div 3 >= 2)) then
          continue; // F and B share fturn track

        // Close order 1: F1' then F2'
        edgemove(orb, i1);
        edgemove(orb, f);
        edgemove(orb, m1);
        edgemove(orb, invMove[f]);
        edgemove(orb, invMove[m1]);
        edgemove(orb, i2);

        badAfter := badEdgeCntIdx(orb, brick);
        if badAfter < badEdgesStart then
        begin
          fxymoves[0] := i1;
          fxymoves[1] := f;
          fxymoves[2] := m1;
          fxymoves[3] := invMove[f];
          fxymoves[4] := invMove[m1];
          fxymoves[5] := i2;
          mvIdx := 6;
          found := True;
          Result := True;
          exit;
        end;

        for o := 1 to edgemx do
          for k := 0 to 23 do
            ecls[o, k] := savedEcls[o, k];

        // Close order 2: F2' then F1'
        edgemove(orb, i1);
        edgemove(orb, f);
        edgemove(orb, m1);
        edgemove(orb, invMove[m1]);
        edgemove(orb, invMove[f]);
        edgemove(orb, i2);

        badAfter := badEdgeCntIdx(orb, brick);
        if badAfter < badEdgesStart then
        begin
          fxymoves[0] := i1;
          fxymoves[1] := f;
          fxymoves[2] := m1;
          fxymoves[3] := invMove[m1];
          fxymoves[4] := invMove[f];
          fxymoves[5] := i2;
          mvIdx := 6;
          found := True;
          Result := True;
          exit;
        end;

        for o := 1 to edgemx do
          for k := 0 to 23 do
            ecls[o, k] := savedEcls[o, k];
      end;
    end;
  end;

  // Depth 7 double-face: I + F1 + F2 + M + close1 + close2 + I
  // Also try: I + F1 + M + F2 + close1 + close2 + I (U/D move between openers)
  // U/D moves are allowed even when R/F/L/B faces are open.
  for ip := 0 to 5 do
  begin
    i1 := InnerPairs[ip, 0];
    i2 := InnerPairs[ip, 1];
    for fi := 0 to 11 do
    begin
      f := FaceOpeners[fi];
      for m2i := 0 to 11 do
      begin
        m1 := FaceOpeners[m2i]; // second face opener
        if (fi div 3 = m2i div 3) then
          continue;
        if ((fi div 3 >= 2) and (m2i div 3 >= 2)) then
          continue;

        for mi := 0 to 5 do
        begin
          m2 := UDMoves[mi]; // middle U/D move

          // Pattern A: I + F1 + F2 + M + F1' + F2' + I
          edgemove(orb, i1);
          edgemove(orb, f);
          edgemove(orb, m1);
          edgemove(orb, m2);
          edgemove(orb, invMove[f]);
          edgemove(orb, invMove[m1]);
          edgemove(orb, i2);

          badAfter := badEdgeCntIdx(orb, brick);
          if badAfter < badEdgesStart then
          begin
            fxymoves[0] := i1; fxymoves[1] := f; fxymoves[2] := m1;
            fxymoves[3] := m2; fxymoves[4] := invMove[f]; fxymoves[5] := invMove[m1];
            fxymoves[6] := i2;
            mvIdx := 7; found := True; Result := True; exit;
          end;
          for o := 1 to edgemx do
            for k := 0 to 23 do ecls[o, k] := savedEcls[o, k];

          // Pattern B: I + F1 + F2 + M + F2' + F1' + I (reverse close)
          edgemove(orb, i1);
          edgemove(orb, f);
          edgemove(orb, m1);
          edgemove(orb, m2);
          edgemove(orb, invMove[m1]);
          edgemove(orb, invMove[f]);
          edgemove(orb, i2);

          badAfter := badEdgeCntIdx(orb, brick);
          if badAfter < badEdgesStart then
          begin
            fxymoves[0] := i1; fxymoves[1] := f; fxymoves[2] := m1;
            fxymoves[3] := m2; fxymoves[4] := invMove[m1]; fxymoves[5] := invMove[f];
            fxymoves[6] := i2;
            mvIdx := 7; found := True; Result := True; exit;
          end;
          for o := 1 to edgemx do
            for k := 0 to 23 do ecls[o, k] := savedEcls[o, k];

          // Pattern C: I + F1 + M + F2 + F1' + F2' + I (U/D between openers)
          edgemove(orb, i1);
          edgemove(orb, f);
          edgemove(orb, m2);
          edgemove(orb, m1);
          edgemove(orb, invMove[f]);
          edgemove(orb, invMove[m1]);
          edgemove(orb, i2);

          badAfter := badEdgeCntIdx(orb, brick);
          if badAfter < badEdgesStart then
          begin
            fxymoves[0] := i1; fxymoves[1] := f; fxymoves[2] := m2;
            fxymoves[3] := m1; fxymoves[4] := invMove[f]; fxymoves[5] := invMove[m1];
            fxymoves[6] := i2;
            mvIdx := 7; found := True; Result := True; exit;
          end;
          for o := 1 to edgemx do
            for k := 0 to 23 do ecls[o, k] := savedEcls[o, k];

          // Pattern D: I + F1 + M + F2 + F2' + F1' + I (reverse close)
          edgemove(orb, i1);
          edgemove(orb, f);
          edgemove(orb, m2);
          edgemove(orb, m1);
          edgemove(orb, invMove[m1]);
          edgemove(orb, invMove[f]);
          edgemove(orb, i2);

          badAfter := badEdgeCntIdx(orb, brick);
          if badAfter < badEdgesStart then
          begin
            fxymoves[0] := i1; fxymoves[1] := f; fxymoves[2] := m2;
            fxymoves[3] := m1; fxymoves[4] := invMove[m1]; fxymoves[5] := invMove[f];
            fxymoves[6] := i2;
            mvIdx := 7; found := True; Result := True; exit;
          end;
          for o := 1 to edgemx do
            for k := 0 to 23 do ecls[o, k] := savedEcls[o, k];

          // Pattern E: I + F1 + F2 + F1' + M + F2' + I (U/D between closes)
          edgemove(orb, i1);
          edgemove(orb, f);
          edgemove(orb, m1);
          edgemove(orb, invMove[f]);
          edgemove(orb, m2);
          edgemove(orb, invMove[m1]);
          edgemove(orb, i2);

          badAfter := badEdgeCntIdx(orb, brick);
          if badAfter < badEdgesStart then
          begin
            fxymoves[0] := i1; fxymoves[1] := f; fxymoves[2] := m1;
            fxymoves[3] := invMove[f]; fxymoves[4] := m2; fxymoves[5] := invMove[m1];
            fxymoves[6] := i2;
            mvIdx := 7; found := True; Result := True; exit;
          end;
          for o := 1 to edgemx do
            for k := 0 to 23 do ecls[o, k] := savedEcls[o, k];

          // Pattern F: I + F1 + F2 + F2' + M + F1' + I (reverse close, U/D between)
          edgemove(orb, i1);
          edgemove(orb, f);
          edgemove(orb, m1);
          edgemove(orb, invMove[m1]);
          edgemove(orb, m2);
          edgemove(orb, invMove[f]);
          edgemove(orb, i2);

          badAfter := badEdgeCntIdx(orb, brick);
          if badAfter < badEdgesStart then
          begin
            fxymoves[0] := i1; fxymoves[1] := f; fxymoves[2] := m1;
            fxymoves[3] := invMove[m1]; fxymoves[4] := m2; fxymoves[5] := invMove[f];
            fxymoves[6] := i2;
            mvIdx := 7; found := True; Result := True; exit;
          end;
          for o := 1 to edgemx do
            for k := 0 to 23 do ecls[o, k] := savedEcls[o, k];
        end;
      end;
    end;
  end;

  // Depth 7: I + F + M1 + M2 + M3 + F' + I (up to 10368 combinations)
  for ip := 0 to 5 do
  begin
    i1 := InnerPairs[ip, 0];
    i2 := InnerPairs[ip, 1];
    for fi := 0 to 11 do
    begin
      f := FaceOpeners[fi];
      for mi := 0 to 5 do
      begin
        m1 := UDMoves[mi];
        for m2i := 0 to 5 do
        begin
          m2 := UDMoves[m2i];
          if Ord(m1) div 3 = Ord(m2) div 3 then
            continue;
          for m3i := 0 to 5 do
          begin
            m3 := UDMoves[m3i];
            if Ord(m2) div 3 = Ord(m3) div 3 then
              continue;

            edgemove(orb, i1);
            edgemove(orb, f);
            edgemove(orb, m1);
            edgemove(orb, m2);
            edgemove(orb, m3);
            edgemove(orb, invMove[f]);
            edgemove(orb, i2);

            badAfter := badEdgeCntIdx(orb, brick);
            if badAfter < badEdgesStart then
            begin
              fxymoves[0] := i1;
              fxymoves[1] := f;
              fxymoves[2] := m1;
              fxymoves[3] := m2;
              fxymoves[4] := m3;
              fxymoves[5] := invMove[f];
              fxymoves[6] := i2;
              mvIdx := 7;
              found := True;
              Result := True;
              exit;
            end;

            for o := 1 to edgemx do
              for k := 0 to 23 do
                ecls[o, k] := savedEcls[o, k];
          end;
        end;
      end;
    end;
  end;
end;

function faceletcube.improvePosition(orb, brick, badedgesStart, badEdgesLast: integer;
  rturn, fturn, lturn, bturn: Moves; dist, togo: integer): boolean;
// R/F/L/B face moves must be conjugated (paired with their inverse).
// This ensures inner slice moves are always sandwiched between face move
// pairs, preventing net center displacement. U/D face moves are unrestricted
// since they only affect outer rows (0 and n-1), not inner slice rows.
var
  badEdgesNew, distnew: integer;
  mv, pm, rturnnew, fturnnew, lturnnew, bturnnew: Moves;
begin
  if dist > togo then
    exit;
  if found = True then
    exit;

  if (togo = 0) then
  begin
    if (rturn = NoMove) and (fturn = NoMove) and (lturn = NoMove) and
      (bturn = NoMove) and (badEdgesLast < badedgesStart) and
      (dispU mod 4 = 0) and (dispD mod 4 = 0) and
      (badAllTotal(improveOrb) < badAllStart) then
      found := True;
    exit;
  end;

  for mv := fU1 to xD3 do
  begin
    rturnnew := rturn;
    fturnnew := fturn;
    lturnnew := lturn;
    bturnnew := bturn;
    distnew := dist;

    case mv of
      fR1..fR3:
      begin
        if rturn <> NoMove then
        begin
          if rturn = invMove[mv] then
          begin
            distnew := dist - 1;
            rturnnew := NoMove;
          end
          else
            continue;
        end
        else
        begin
          rturnnew := mv;
          distnew := dist + 1;
        end;
      end;
      fF1..fF3:
      begin
        if fturn <> NoMove then
        begin
          if fturn = invMove[mv] then
          begin
            distnew := dist - 1;
            fturnnew := NoMove;
          end
          else
            continue;
        end
        else
        begin
          fturnnew := mv;
          distnew := dist + 1;
        end;
      end;
      fL1..fL3:
      begin
        if lturn <> NoMove then
        begin
          if lturn = invMove[mv] then
          begin
            distnew := dist - 1;
            lturnnew := NoMove;
          end
          else
            continue;
        end
        else
        begin
          lturnnew := mv;
          distnew := dist + 1;
        end;
      end;
      fB1..fB3:
      begin
        // Original Kociemba: F and B share fturn tracking (intentional).
        if fturn <> NoMove then
        begin
          if fturn = invMove[mv] then
          begin
            distnew := dist - 1;
            fturnnew := NoMove;
          end
          else
            continue;
        end
        else
        begin
          fturnnew := mv;
          distnew := dist + 1;
        end;
      end;
    end;

    // No inner slice moves while any R/F/L/B face is open (unconjugated)
    if ((rturn <> NoMove) or (fturn <> NoMove) or (lturn <> NoMove) or
      (bturn <> NoMove)) and (mv > fB3) then
      continue;

    // Move-ordering: no consecutive same-axis moves
    if mvidx > 0 then
    begin
      pm := fxymoves[mvidx - 1];
      if Ord(pm) div 3 = Ord(mv) div 3 then
        continue;

      // Canonical ordering for commuting axes
      if (Ord(mv) mod 18) div 6 = (Ord(pm) mod 18) div 6 then
      begin
        if Ord(mv) < Ord(pm) then
          continue;
      end;
    end;

    edgemove(orb, mv);
    if mv > fB3 then
      badEdgesNew := badEdgeCntIdx(orb, brick)
    else
      badEdgesNew := badEdgesLast;

    if (mv > fB3) and (badEdgesNew > badedgesStart) then
    begin
      edgemove(orb, invMove[mv]);
      continue;
    end;

    case mv of
      xU1: Inc(dispU);
      xU2: Inc(dispU, 2);
      xU3: Inc(dispU, 3);
      xD1: Inc(dispD);
      xD2: Inc(dispD, 2);
      xD3: Inc(dispD, 3);
    end;

    fxymoves[mvIdx] := mv;
    Inc(mvIdx);
    improvePosition(orb, brick, badedgesStart, badEdgesNew, rturnnew,
      fturnnew, lturnnew, bturnnew, distnew, togo - 1);
    if found then
      exit;

    case mv of
      xU1: Dec(dispU);
      xU2: Dec(dispU, 2);
      xU3: Dec(dispU, 3);
      xD1: Dec(dispD);
      xD2: Dec(dispD, 2);
      xD3: Dec(dispD, 3);
    end;

    edgemove(orb, invMove[mv]);
    Dec(mvIdx);
  end;

end;

// Like improvePosition but tracks total bad facelets across ALL 4 vertical edges.
// Uses the same conjugate constraint as improvePosition.
function faceletcube.improveVertical(orb, badStart, badLast: integer;
  rturn, fturn, lturn, bturn: Moves; dist, togo: integer;
  countAll: boolean): boolean;
const
  MoveOrder: array[0..23] of Moves = (
    xU1, xU2, xU3, xD1, xD2, xD3,
    fR1, fR2, fR3, fL1, fL2, fL3,
    fF1, fF2, fF3, fB1, fB2, fB3,
    fU1, fU2, fU3, fD1, fD2, fD3
  );
var
  badNew, distnew, mi: integer;
  mv, pm, rturnnew, fturnnew, lturnnew, bturnnew: Moves;
begin
  if dist > togo then
    exit;
  if found = True then
    exit;

  if (togo = 0) then
  begin
    if (rturn = NoMove) and (fturn = NoMove) and (lturn = NoMove) and
      (bturn = NoMove) and (badLast < badStart) and
      (dispU mod 4 = 0) and (dispD mod 4 = 0) then
      found := True;
    exit;
  end;

  for mi := 0 to 23 do
  begin
    mv := MoveOrder[mi];
    rturnnew := rturn;
    fturnnew := fturn;
    lturnnew := lturn;
    bturnnew := bturn;
    distnew := dist;

    case mv of
      fR1..fR3:
      begin
        if rturn <> NoMove then
        begin
          if rturn = invMove[mv] then
          begin distnew := dist - 1; rturnnew := NoMove; end
          else continue;
        end
        else
        begin rturnnew := mv; distnew := dist + 1; end;
      end;
      fF1..fF3:
      begin
        if fturn <> NoMove then
        begin
          if fturn = invMove[mv] then
          begin distnew := dist - 1; fturnnew := NoMove; end
          else continue;
        end
        else
        begin fturnnew := mv; distnew := dist + 1; end;
      end;
      fL1..fL3:
      begin
        if lturn <> NoMove then
        begin
          if lturn = invMove[mv] then
          begin distnew := dist - 1; lturnnew := NoMove; end
          else continue;
        end
        else
        begin lturnnew := mv; distnew := dist + 1; end;
      end;
      fB1..fB3:
      begin
        if bturn <> NoMove then
        begin
          if bturn = invMove[mv] then
          begin distnew := dist - 1; bturnnew := NoMove; end
          else continue;
        end
        else
        begin bturnnew := mv; distnew := dist + 1; end;
      end;
    end;

    // No inner slice moves while any R/F/L/B face is open
    if ((rturn <> NoMove) or (fturn <> NoMove) or (lturn <> NoMove) or
      (bturn <> NoMove)) and (mv > fB3) then
      continue;

    // Move-ordering: no consecutive same-axis moves
    if mvidx > 0 then
    begin
      pm := fxymoves[mvidx - 1];
      if Ord(pm) div 3 = Ord(mv) div 3 then
        continue;
      if (Ord(mv) mod 18) div 6 = (Ord(pm) mod 18) div 6 then
      begin
        if Ord(mv) < Ord(pm) then
          continue;
      end;
    end;

    edgemove(orb, mv);
    if mv > fB3 then
    begin
      if countAll then
        badNew := badAllTotal(orb)
      else
        badNew := badVerticalTotal(orb);
    end
    else
      badNew := badLast;

    if (mv > fB3) and (badNew > badStart + 4) then
    begin
      edgemove(orb, invMove[mv]);
      continue;
    end;

    case mv of
      xU1: Inc(dispU);
      xU2: Inc(dispU, 2);
      xU3: Inc(dispU, 3);
      xD1: Inc(dispD);
      xD2: Inc(dispD, 2);
      xD3: Inc(dispD, 3);
    end;

    fxymoves[mvIdx] := mv;
    Inc(mvIdx);
    improveVertical(orb, badStart, badNew, rturnnew,
      fturnnew, lturnnew, bturnnew, distnew, togo - 1, countAll);
    if found then
      exit;

    case mv of
      xU1: Dec(dispU);
      xU2: Dec(dispU, 2);
      xU3: Dec(dispU, 3);
      xD1: Dec(dispD);
      xD2: Dec(dispD, 2);
      xD3: Dec(dispD, 3);
    end;

    edgemove(orb, invMove[mv]);
    Dec(mvIdx);
  end;

end;

procedure faceletCube.ReportPh5Progress(togo: integer; const context: string);
var
  nowTick, elapsedMs: QWord;
  currentBad, paired: integer;
  i: integer;
  e: Edge;
  pctDone: double;
begin
  nowTick := GetTickCount64;
  if (nowTick - ph5LastTick) < 5000 then
    Exit;
  ph5LastTick := nowTick;
  elapsedMs := nowTick - ph5StartTick;

  // Count current bad edges
  currentBad := 0;
  for i := 1 to edgemx - 1 do
    for e := UR to BR do
    begin
      if ecls[i, edgeidx[e, 0]] <> ecls[edgemx, edgeidx[e, 0]] then
        Inc(currentBad);
      if ecls[i, edgeidx[e, 1]] <> ecls[edgemx, edgeidx[e, 1]] then
        Inc(currentBad);
    end;

  paired := ph5TotalSlots - currentBad;
  if ph5InitialBad > 0 then
    pctDone := (ph5InitialBad - currentBad) / ph5InitialBad * 100
  else
    pctDone := 100;

  TSWriteLn(Format('Phase 5: %d/%d edge slots paired (%.0f%%), %d moves, %s elapsed [%s, depth %d]',
    [paired, ph5TotalSlots, pctDone, ns, FormatMs(elapsedMs), context, togo]));
  TSWriteLn(Format('PROGRESS:Phase5:EdgePairing:%d:%d',
    [paired, ph5TotalSlots]));
end;

function faceletCube.MakeFLEdge: boolean;
  // fix the FL edge using freeslice method (matches original Kociemba algorithm)
var
  i, togo, baded, brick, orb: integer;
  ed: Edge;
  improved: boolean;
begin
  mvIdx := 0;
  baded := 0;

  for ed := UR to DB do  //only U and D face horizontal edges
    Inc(baded, badEdgeCnt(ed));

  if baded <> 0 then //some horizontal slot empty
  begin
    if badEdgeCnt(FL) = 0 then  //but if other edges are ok save them first
    begin
      store(FL);
      Inc(ns, mvIdx);
      applyEdgeMoves(-1);
      applyMoves(-1, -1); //only face moves
      printMoves(-1, -1);
      Result := True;
    end
    else if badEdgeCnt(FR) = 0 then
    begin
      store(FR);
      Inc(ns, mvIdx);
      applyEdgeMoves(-1);
      applyMoves(-1, -1);
      printMoves(-1, -1);
      Result := True;
    end
    else if badEdgeCnt(BR) = 0 then
    begin
      store(BR);
      Inc(ns, mvIdx);
      applyEdgeMoves(-1);
      applyMoves(-1, -1);
      printMoves(-1, -1);
      Result := True;
    end
    else if badEdgeCnt(BL) = 0 then
    begin
      store(BL);
      Inc(ns, mvIdx);
      applyEdgeMoves(-1);
      applyMoves(-1, -1);
      printMoves(-1, -1);
      Result := True;
    end

    else //fix part of FL edge
    begin
      togo := 1;
      brick := ecls[edgemx, edgeidx[FL, 0]];

      repeat
        improved := False;
        for i := 1 to edgemx - 1 do //the orbits
        begin
          ReportPh5Progress(togo, Format('horizontal FL orb %d/%d', [i, edgemx - 1]));
          mvIdx := 0;
          found := False;
          dispU := 0;
          dispD := 0;
          improveOrb := i;
          baded := badEdgeCntIdx(i, brick);
          badAllStart := badAllTotal(i);
          if baded > 0 then
            ImprovePosition(i, brick, baded, baded, NoMove, NoMove,
              NoMove, NoMove, 0, togo);
          if found = True then
          begin
            Inc(ns, mvIdx);
            applyMoves(i, -1);
            printMoves(i, -1);
            improved := True;
            togo := 1;
          end;
        end;
        //if edge complete, exit
        baded := 0;
        for i := 1 to edgemx - 1 do
          Inc(baded, badEdgeCntIdx(i, brick));
        if baded = 0 then
          exit(True);

        if not improved then
          Inc(togo);
      until False;
    end;

  end
  else//all horizontal edges paired - check vertical edges too
  begin
    // On genuine odd cubes, freeslice theorem guarantees vertical edges are
    // paired when horizontal edges are. On virtual padded cubes (even->odd),
    // this doesn't hold for the padding orbit. Check and fix if needed.
    baded := 0;
    for i := 1 to edgemx - 1 do
      Inc(baded, badVerticalTotal(i));

    if baded = 0 then
    begin
      Result := False; // truly done - all edges paired
      Exit;
    end;

    WriteLnVerbose(Format('Horizontal edges paired but %d vertical edge mismatches - fixing', [baded]));

    togo := 1;
    repeat
      improved := False;
      for i := 1 to edgemx - 1 do
      begin
        ReportPh5Progress(togo, Format('vertical orb %d/%d', [i, edgemx - 1]));
        mvIdx := 0;
        found := False;
        dispU := 0;
        dispD := 0;
        baded := badAllTotal(i);  // track ALL edges to avoid breaking horizontal
        if baded > 0 then
          improveVertical(i, baded, baded, NoMove, NoMove,
            NoMove, NoMove, 0, togo, True);  // countAll=True
        if found then
        begin
          Inc(ns, mvIdx);
          applyMoves(i, -1);
          printMoves(i, -1);
          improved := True;
          togo := 1;
        end;
      end;
      // Check if ALL edges (horizontal + vertical) are now paired
      baded := 0;
      for i := 1 to edgemx - 1 do
        Inc(baded, badAllTotal(i));
      if baded = 0 then
      begin
        Result := False; // done
        Exit;
      end;

      if not improved then
        Inc(togo);
    until False;
  end;

end;

function faceletCube.badEdgeCnt(e: Edge): integer;
var
  i: integer;
begin
  Result := 0;
  for i := 1 to edgemx - 1 do
  begin
    if ecls[i, edgeidx[e, 0]] <> ecls[edgemx, edgeidx[e, 0]] then
      Inc(Result);
    if ecls[i, edgeidx[e, 1]] <> ecls[edgemx, edgeidx[e, 1]] then
      Inc(Result);
  end;
end;

function faceletCube.badEdgeCntIdx(orb, idx: integer): integer;
  //bad edges for specific brick
var
  e, e1: Edge;
begin
  //find brick
  for e1 := UR to BR do
  begin
    if (ecls[edgemx, edgeidx[e1, 0]] = idx) or (ecls[edgemx, edgeidx[e1, 1]] = idx) then
    begin
      e := e1;
      break;
    end;
  end;

  Result := 0;
  if ecls[orb, edgeidx[e, 0]] <> ecls[edgemx, edgeidx[e, 0]] then
    Inc(Result);
  if ecls[orb, edgeidx[e, 1]] <> ecls[edgemx, edgeidx[e, 1]] then
    Inc(Result);
end;



//function faceletCube.badEdgeCntOrbit(orb: integer): integer;
//var
//  e: Edge;
//begin
//  Result := 0;

//  for e := FR to BR do  //four vertical edges

//  begin
//    if ecls[orb, edgeidx[e, 0]] <> ecls[edgemx, edgeidx[e, 0]] then
//      Inc(Result);
//    if ecls[orb, edgeidx[e, 1]] <> ecls[edgemx, edgeidx[e, 1]] then
//      Inc(Result);
//  end;
//end;




//function faceletCube.nextMovePh5(idx: integer; currMove: moves): moves;
//var
//  pm: moves;
//begin
//  if currMove = xB3 then // done
//    Exit(NoMove);
//  if idx = 0 then
//    Exit(Succ(currMove))
//  else
//  begin
//    pm := fxymoves[idx - 1]; // predecessor
//    while True do
//    begin
//      currMove := Succ(currMove);

//      if currMove = NoMove then
//        Exit(NoMove);

//      if Ord(pm) < Ord(xU1) then //previous move is face move
//      begin
//        if (Ord(currMove) <= Ord(pm)) then
//          //all face moves commute restricted to the centers
//          continue;
//        if Ord(currMove) >= Ord(xU1) then  //face move followed by slice move
//          // always valid
//          Exit(currMove)
//        else // pm<currMove<xU1
//        begin
//          if Ord(pm) div 3 = Ord(currMove) div 3 then
//            // same face
//            continue
//          else
//            Exit(currMove);
//        end;
//      end;

//      //Ord(pm) >= Ord(xU1), previous move is slice move
//      if (Ord(pm) div 6) mod 3 <> (Ord(currMove) div 6) mod 3 then
//        Exit(currMove);
//      // pm and currMove are on different axes and hence do not commute


//      // both moves are on the same axis and commute
//      // we can force an order
//      if Ord(currMove) <= Ord(pm) then
//        continue;

//      // if the  prefixes f,x,y are different for both moves, currmove is valid
//      if Ord(currMove) div 18 <> Ord(pm) div 18 then
//        Exit(currMove);

//      // we have the same prefix and the same axis
//      if (Ord(currMove) mod 6) div 3 <> (Ord(pm) mod 6) div 3 then
//        // moves are on different slices of the axis
//        Exit(currMove)
//      else
//        continue;
//    end;
//  end;
//end;

//function faceletCube.SearchUDCent(x, y: integer): boolean;
//var
//  idx, togo, d1, d2,d3: integer;
//  i: integer;
//begin
//  found := False;
//  for idx := Low(fxymoves) to High(fxymoves) do
//    fxymoves[idx] := InitMove;

//  d1 := getUDBrickXCentXDepth(x, y);
//  d2 := getUDBrickXCentXDepth(y, x);
//  d3 := getUDBrickXYDepth(x, y);
//  togo := max(max(d1, d2),d3);
//  while found = False do
//  begin
//    mvIdx := 0; // 1. empty place in  fxymoves
//    Form1.Memo1.Lines.Add(Format('Searching depth %d...', [togo]));

//    SearchUDCenter(Phase1BrickCoord(x), Phase1CenterCoord(x, y),
//      Phase1BrickCoord(y), Phase1CenterCoord(y, x),
//      d3, d1, d2, togo);
//    Inc(togo);
//  end;
//  if found then
//  begin
//    Result := True;

//  end
//  else
//    Result := False;
//end;


////restores centers of orbits(x,y) and (y,x).
//procedure faceletCube.SearchUDCenter(bx, cx, by, cy, bxby_dist,
//  bxcx_dist, bycy_dist, togo: integer);
//var
//  mv: moves;
//  sc1: SymCoord32;
//  syms: UInt8;
//  i, bx_class, bx_sym, by_class, by_sym, altcx, altcy, altby,
//  bxcx_distmod3, bycy_distmod3, bxby_distmod3, bxcx_dist1, bycy_dist1,
//  bxby_dist1: integer;
//  bx1, by1, cx1, cy1: integer;

//begin
//  // Application.ProcessMessages removed
//   //Form1.Memo1.Lines.Add(Format('bx: %d, togo: %d, mvidx: %d', [bx, togo, mvidx]));
//   //printmoves(1,2);
//  if stopProgram then
//    exit;

//  if togo = 0 then
//  begin
//    found := True;
//  end
//  else
//  begin
//    mv := InitMove;
//    while True do
//    begin
//      if mvIdx = 0 then
//        mv := nextMovePhase1[NoMove, mv]
//      else
//        mv := nextMovePhase1[fxymoves[mvIdx - 1], mv];

//      if mv = NoMove then
//      begin
//        Exit;
//      end
//      else
//      begin
//        case mv of
//          fU1..fB3:
//          begin//face moves move x-bricks and (x,y)-orbit centers
//            bx1 := UDCenterMove[bx, Ord(mv)];
//            cx1 := UDCenterMove[cx, Ord(mv)];
//            by1 := UDCenterMove[by, Ord(mv)];
//            cy1 := UDCenterMove[cy, Ord(mv)];
//          end;
//          xU1..xB3:
//          begin
//            bx1 := UDCenterMove[bx, Ord(mv)];
//            cx1 := UDCenterMove[cx, Ord(mv)];
//            by1 := by;
//            // for (y,x) Orbit the roles of x-moves and y-moves are swapped
//            cy1 := UDCenterMove[cy, Ord(mv) + 18];
//          end;
//          yU1..yB3:
//          begin
//            bx1 := bx;
//            cx1 := UDCenterMove[cx, Ord(mv)];
//            by1 := UDCenterMove[by, Ord(mv) - 18];
//            cy1 := UDCenterMove[cy, Ord(mv) - 18];
//          end;
//        end;

//        bx_class := UDBrickCoordToSymCoord[bx1].c_idx;
//        bx_sym := UDBrickCoordToSymCoord[bx1].sym;
//        i := 0;//find one symmetry
//        while (bx_sym and (1 shl i)) = 0 do
//          Inc(i);
//        altcx := UDBrickCoordSymTransform[cx1, i];
//        altby := UDBrickCoordSymTransform[by1, i]; //compute here
//        bxcx_distmod3 := get_bxcx_depth3(bx_class, altcx);
//        bxcx_dist1 := distance[3 * bxcx_dist + bxcx_distmod3];
//         if bxcx_dist1 >= togo then
//          continue;

//        by_class := UDBrickCoordToSymCoord[by1].c_idx;
//        by_sym := UDBrickCoordToSymCoord[by1].sym;
//        i := 0;//find one symmetry
//        while (by_sym and (1 shl i)) = 0 do
//          Inc(i);
//        altcy := UDBrickCoordSymTransform[cy1, i];
//        bycy_distmod3 := get_bxcx_depth3(by_class, altcy);
//        bycy_dist1 := distance[3 * bycy_dist + bycy_distmod3];
//        if bycy_dist1 >= togo then
//          continue;

//        bxby_distmod3 := get_bxby_depth3(bx_class, altby);
//        bxby_dist1 := distance[3 * bxby_dist + bxby_distmod3];
//        if bxby_dist1 >= togo then
//          continue;

//        fxymoves[mvIdx] := mv;
//        Inc(mvIdx);


//        SearchUDCenter(bx1, cx1, by1, cy1, bxby_dist1, bxcx_dist1, bycy_dist1, togo - 1);

//        if found then
//          // kehre zurück, ohne mvIdx zu verändern
//          Exit;
//        Dec(mvIdx);
//      end;
//    end;

//  end;

//end;




end.
