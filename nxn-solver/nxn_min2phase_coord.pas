unit nxn_min2phase_coord;
// Move tables, symmetry tables, pruning tables for the Two-Phase Algorithm
// Ported from Herbert Kociemba's CubeExplorer (CordCube.pas, Symmetries.pas)

{$mode objfpc}{$H+}

interface

uses SysUtils, nxn_min2phase_defs, nxn_min2phase_cubicube;

// --- Symmetry tables ---
var
  // Corner/edge permutations for all 48 symmetries (indexed 0..47)
  CornSym: array[0..47] of TCornerCubie;
  EdgeSym: array[0..47] of TEdgeCubie;
  // Inverse symmetry index: S(InvIdx[i]) * S(i) = Identity
  InvIdx: array[0..47] of integer;
  // Group multiplication table: SymMult[i,j] = index of S(i)*S(j)
  SymMult: array[0..47, 0..47] of integer;
  // Move conjugation: SymMove[sym, move] = conjugated move index (0..17)
  SymMoveTable: array[0..47, 0..17] of integer;

// --- Class-index tables ---
  // FlipUDSlice sym-coord: maps class index -> raw FlipUDSlice coordinate of representant
  FlipUDSliceToRaw: array[0..N_FLIPSLICE_CLASS - 1] of integer;
  // CornPerm sym-coord: maps class index -> raw CornPerm coordinate of representant
  CornPermSymToRaw: array[0..N_CORNPERM_CLASS - 1] of word;
  // Inverse: raw CornPerm -> sym-coord (classIdx shl 4 + sym)
  CornPermRawToSym: array[0..N_CORNERS_PERM - 1] of integer;

// --- Move tables ---
  TwistMove: array[0..N_TWIST - 1, 0..17] of word;
  FlipMove: array[0..N_FLIP - 1, 0..17] of word;
  UDSliceMove: array[0..N_SLICE - 1, 0..17] of word;
  CornPermMove: array[0..N_CORNERS_PERM - 1, 0..17] of word;
  UDSliceSortedMove: array[0..N_SLICE_SORTED - 1, 0..17] of word;
  Edge8PermMove: array[0..N_UD_EDGES_PERM - 1, 0..17] of word;
  // FlipSlice sym-coord move table (dynamic)
  FlipSliceMove: array of array of integer;

// --- Conjugation tables ---
  TwistConj: array[0..N_TWIST - 1, 0..15] of word;
  Edge8PermConj: array[0..N_UD_EDGES_PERM - 1, 0..15] of word;

// --- Pruning tables (full depth, 1 byte per entry) ---
  // Phase 1: FlipSlice_class × Twist, ~140 MB
  PruningPhase1: array of byte;
  // Phase 2: CornPermSym_class × Edge8Perm, ~112 MB
  PruningPhase2: array of byte;
  // Phase 2 pre-check: CornPerm alone
  PruningCornPerm: array[0..N_CORNERS_PERM - 1] of byte;

// --- Edge8Perm extraction table ---
  // GetEdge8Perm[RLSliceSorted, FBSliceSorted mod 24] -> Edge8Perm
  GetEdge8Perm: array[0..N_SLICE_SORTED - 1, 0..23] of word;

// --- Table generation status ---
  TablesReady: boolean;

// Initialize all tables (call once at startup)
procedure InitAllTables;

// Get pruning value from Phase 1 table (unpacked, 2 bits per entry)
function GetPrunPhase1(flipSliceClass, twist, sym: integer): integer;
// Get pruning value from Phase 2 table
function GetPrunPhase2(cornPermClass, edge8Perm, sym: integer): integer;

// Compute FlipUDSlice sym-coordinate for a cube
function ComputeFlipUDSliceSymCoord(const cc: TCubieCube): integer;
// Compute CornPerm sym-coordinate
function ComputeCornPermSymCoord(const cc: TCubieCube): integer;

// Table cache directory
function GetTableCacheDir: string;

// Low-level pruning table access (2-bit packed, 4 entries per byte)
// Values 0-2 = depth mod 3, 3 = unvisited
function GetPrun2Bit(const table: array of byte; index: int64): integer;

implementation

uses Classes, nxn_util;

const
  CACHE_VERSION = 4;  // v4: full-depth byte-per-entry pruning tables

function GetTableCacheDir: string;
begin
  // Use the same TableDir as the NxN solver (set by InitTableDir)
  if nxn_util.TableDir <> '' then
    Result := nxn_util.TableDir
  else
    Result := IncludeTrailingPathDelimiter(
      ExtractFilePath(ParamStr(0)) + 'data');
  if not DirectoryExists(Result) then
    ForceDirectories(Result);
end;

// ============================================================================
// Symmetry table generation
// ============================================================================

procedure CreateSymmetries;
var
  cc: TCubieCube;
  index, urf3, f2, u4, lr2, j, k: integer;
  c: TCornerCubie;
begin
  CubieIdentity(cc);
  index := 0;
  for urf3 := 0 to 2 do
  begin
    for f2 := 0 to 1 do
    begin
      for u4 := 0 to 3 do
      begin
        for lr2 := 0 to 1 do
        begin
          CornSym[index] := cc.corn;
          EdgeSym[index] := cc.edge;
          Inc(index);
          CubieSymMult(cc, 3); // S_LR2
        end;
        CubieSymMult(cc, 2); // S_U4
      end;
      CubieSymMult(cc, 1); // S_F2
    end;
    CubieSymMult(cc, 0); // S_URF3
  end;

  // Find inverse symmetries
  for j := 0 to 47 do
    for k := 0 to 47 do
    begin
      CornMult(CornSym[j], CornSym[k], c);
      if (c[0].c = 0) and (c[1].c = 1) and (c[2].c = 2) then
      begin
        InvIdx[j] := k;
        break;
      end;
    end;
end;

procedure CreateSymMultTable;
var
  i, j, k: integer;
  corn: TCornerCubie;
begin
  for i := 0 to 47 do
    for j := 0 to 47 do
    begin
      CornMult(CornSym[i], CornSym[j], corn);
      for k := 0 to 47 do
        if (CornSym[k, 0].c = corn[0].c) and (CornSym[k, 1].c = corn[1].c) and
           (CornSym[k, 2].c = corn[2].c) then
        begin
          SymMult[i, j] := k;
          break;
        end;
    end;
end;

function IsCornIdentity(const cc: TCubieCube): boolean;
var
  i: integer;
begin
  Result := true;
  for i := 0 to 7 do
    if (cc.corn[i].c <> i) or (cc.corn[i].o <> 0) then
    begin
      Result := false;
      exit;
    end;
end;

procedure CreateSymMoveTable;
var
  c1, c2: TCubieCube;
  prod: TCornerCubie;
  axis, power, s, m, n: integer;
begin
  CubieIdentity(c1);
  CubieIdentity(c2);

  for axis := 0 to 5 do  // U, R, F, D, L, B
  begin
    for power := 0 to 3 do
    begin
      CubieMove(c1, axis);
      if power <> 3 then
      begin
        for s := 0 to 47 do
        begin
          // Conjugate: S * Move * S^-1
          CornMult(CornSym[s], c1.corn, prod);
          CornMult(prod, CornSym[InvIdx[s]], c2.corn);
          // Find which move this corresponds to
          for m := 0 to 5 do
          begin
            for n := 0 to 3 do
            begin
              CubieMove(c2, m);
              if n <> 3 then
                if IsCornIdentity(c2) then
                  SymMoveTable[s, 3 * axis + power] := 3 * m + (2 - n);
            end;
          end;
        end;
      end;
    end;
  end;
end;

// ============================================================================
// Class-index / representant table generation
// ============================================================================

function FlipUDSliceRawClassIndex(n: integer): integer;
var
  lo, hi, mid: integer;
begin
  Result := -1;
  if n > FlipUDSliceToRaw[N_FLIPSLICE_CLASS - 1] then exit;
  lo := 0;
  hi := N_FLIPSLICE_CLASS;
  while lo < hi do
  begin
    mid := (lo + hi) div 2;
    if FlipUDSliceToRaw[mid] < n then
      lo := mid + 1
    else if FlipUDSliceToRaw[mid] > n then
      hi := mid
    else
    begin
      Result := mid;
      exit;
    end;
  end;
end;

procedure CreateFlipUDSliceClassTable;
var
  cc, cc2: TCubieCube;
  classCount, raw, k, coord: integer;
  prod: TEdgeCubie;
  isRep: boolean;
begin
  classCount := 0;
  CubieIdentity(cc);
  CubieIdentity(cc2);

  // Iterate through all raw FlipUDSlice coords and find representants
  // Raw coord = UDSlice * 2048 + Flip
  for raw := 0 to N_SLICE * N_FLIP - 1 do
  begin
    SetUDSliceCoord(cc, raw div N_FLIP);
    SetEdgeOriCoord(cc, raw mod N_FLIP);

    // Check if this is a representant (smallest in its class)
    isRep := true;
    for k := 0 to 15 do
    begin
      EdgeMult(EdgeSym[k], cc.edge, prod);
      EdgeMult(prod, EdgeSym[InvIdx[k]], cc2.edge);
      coord := GetUDSliceCoord(cc2) * N_FLIP + GetEdgeOriCoord(cc2);
      if coord < raw then
      begin
        isRep := false;
        break;
      end;
    end;

    if isRep then
    begin
      if classCount < N_FLIPSLICE_CLASS then
        FlipUDSliceToRaw[classCount] := raw;
      Inc(classCount);
    end;
  end;

  // classCount should be N_FLIPSLICE_CLASS = 64430
  // WriteLn('FlipUDSlice classes: ', classCount);
end;

function CornPermSymSearch(coord: integer): integer;
var
  lo, hi, mid: integer;
begin
  Result := -1;
  lo := 0; hi := N_CORNPERM_CLASS;
  while lo < hi do
  begin
    mid := (lo + hi) div 2;
    if CornPermSymToRaw[mid] < coord then lo := mid + 1
    else if CornPermSymToRaw[mid] > coord then hi := mid
    else begin Result := mid; exit; end;
  end;
end;

procedure CreateCornPermClassTable;
var
  cc, cc2: TCubieCube;
  classCount, raw, k, coord: integer;
  prod: TCornerCubie;
  isRep: boolean;
begin
  classCount := 0;
  CubieIdentity(cc);
  CubieIdentity(cc2);

  for raw := 0 to N_CORNERS_PERM - 1 do
  begin
    SetCornPermCoord(cc, raw);

    isRep := true;
    for k := 0 to 15 do
    begin
      CornMult(CornSym[k], cc.corn, prod);
      CornMult(prod, CornSym[InvIdx[k]], cc2.corn);
      coord := GetCornPermCoord(cc2);
      if coord < raw then
      begin
        isRep := false;
        break;
      end;
    end;

    if isRep then
    begin
      if classCount < N_CORNPERM_CLASS then
        CornPermSymToRaw[classCount] := raw;
      Inc(classCount);
    end;

    // Build reverse lookup
    CornPermRawToSym[raw] := -1; // will be filled below
  end;

  // Build CornPermRawToSym: for each raw coord, find its class + symmetry
  for raw := 0 to N_CORNERS_PERM - 1 do
  begin
    SetCornPermCoord(cc, raw);
    for k := 0 to 15 do
    begin
      CornMult(CornSym[k], cc.corn, prod);
      CornMult(prod, CornSym[InvIdx[k]], cc2.corn);
      coord := GetCornPermCoord(cc2);
      // Binary search in CornPermSymToRaw
      classCount := CornPermSymSearch(coord);
      if classCount >= 0 then
      begin
        CornPermRawToSym[raw] := classCount shl 4 + k;
        break;
      end;
    end;
  end;
end;

// ============================================================================
// Move table generation
// ============================================================================

procedure CreateTwistMoveTable;
var
  cc: TCubieCube;
  i, k, axis: integer;
begin
  CubieIdentity(cc);
  for i := 0 to N_TWIST - 1 do
  begin
    SetCornOriCoord(cc, i);
    for axis := 0 to 5 do
      for k := 0 to 3 do
      begin
        CubieMove(cc, axis);
        if k <> 3 then
          TwistMove[i, 3 * axis + k] := GetCornOriCoord(cc);
      end;
  end;
end;

procedure CreateFlipMoveTable;
var
  cc: TCubieCube;
  i, k, axis: integer;
begin
  CubieIdentity(cc);
  for i := 0 to N_FLIP - 1 do
  begin
    SetEdgeOriCoord(cc, i);
    for axis := 0 to 5 do
      for k := 0 to 3 do
      begin
        CubieMove(cc, axis);
        if k <> 3 then
          FlipMove[i, 3 * axis + k] := GetEdgeOriCoord(cc);
      end;
  end;
end;

procedure CreateUDSliceMoveTable;
var
  cc: TCubieCube;
  i, k, axis: integer;
begin
  CubieIdentity(cc);
  for i := 0 to N_SLICE - 1 do
  begin
    SetUDSliceCoord(cc, i);
    for axis := 0 to 5 do
      for k := 0 to 3 do
      begin
        CubieMove(cc, axis);
        if k <> 3 then
          UDSliceMove[i, 3 * axis + k] := GetUDSliceCoord(cc);
      end;
  end;
end;

procedure CreateCornPermMoveTable;
var
  cc: TCubieCube;
  i, k, axis: integer;
begin
  CubieIdentity(cc);
  for i := 0 to N_CORNERS_PERM - 1 do
  begin
    SetCornPermCoord(cc, i);
    for axis := 0 to 5 do
      for k := 0 to 3 do
      begin
        CubieMove(cc, axis);
        if k <> 3 then
          CornPermMove[i, 3 * axis + k] := GetCornPermCoord(cc);
      end;
  end;
end;

procedure CreateUDSliceSortedMoveTable;
var
  cc: TCubieCube;
  i, k, axis: integer;
begin
  CubieIdentity(cc);
  for i := 0 to N_SLICE_SORTED - 1 do
  begin
    SetUDSliceSortedCoord(cc, i);
    for axis := 0 to 5 do
      for k := 0 to 3 do
      begin
        CubieMove(cc, axis);
        if k <> 3 then
          UDSliceSortedMove[i, 3 * axis + k] := GetUDSliceSortedCoord(cc);
      end;
  end;
end;

procedure CreateEdge8PermMoveTable;
var
  cc: TCubieCube;
  i, k, axis, mv: integer;
begin
  // Initialize to a sentinel value
  FillByte(Edge8PermMove, SizeOf(Edge8PermMove), $FF);

  CubieIdentity(cc);
  for i := 0 to N_UD_EDGES_PERM - 1 do
  begin
    SetPhase2EdgePermCoord(cc, i);
    for axis := 0 to 5 do
      for k := 0 to 3 do
      begin
        CubieMove(cc, axis);
        mv := 3 * axis + k;
        // Phase 2 only allows: U,U2,U',D,D2,D',R2,F2,L2,B2
        case axis of
          0, 3: // U, D: all three powers
            if k < 3 then Edge8PermMove[i, mv] := GetPhase2EdgePermCoord(cc);
          1, 2, 4, 5: // R, F, L, B: only X2
            if k = 1 then Edge8PermMove[i, mv] := GetPhase2EdgePermCoord(cc);
        end;
      end;
  end;
end;

procedure CreateFlipSliceMoveTable;
var
  cc, cc2: TCubieCube;
  i, k, axis, raw, newRaw, coord, classIdx: integer;
  symK: integer;
  prod: TEdgeCubie;
begin
  SetLength(FlipSliceMove, N_FLIPSLICE_CLASS, N_MOVE);

  CubieIdentity(cc);
  CubieIdentity(cc2);

  for i := 0 to N_FLIPSLICE_CLASS - 1 do
  begin
    raw := FlipUDSliceToRaw[i];
    SetUDSliceCoord(cc, raw div N_FLIP);
    SetEdgeOriCoord(cc, raw mod N_FLIP);

    for axis := 0 to 5 do
      for k := 0 to 3 do
      begin
        CubieMove(cc, axis);
        if k <> 3 then
        begin
          // Compute the FlipUDSlice sym-coordinate of the result
          newRaw := GetUDSliceCoord(cc) * N_FLIP + GetEdgeOriCoord(cc);

          // Find the class by trying all 16 symmetries
          FlipSliceMove[i, 3 * axis + k] := -1;
          for symK := 0 to 15 do
          begin
            EdgeMult(EdgeSym[symK], cc.edge, prod);
            EdgeMult(prod, EdgeSym[InvIdx[symK]], cc2.edge);
            coord := GetUDSliceCoord(cc2) * N_FLIP + GetEdgeOriCoord(cc2);
            // Binary search for this coord in FlipUDSliceToRaw
            begin
              classIdx := FlipUDSliceRawClassIndex(coord);
              if classIdx >= 0 then
                FlipSliceMove[i, 3 * axis + k] := classIdx shl 4 + symK;
            end;
            if FlipSliceMove[i, 3 * axis + k] >= 0 then break;
          end;
        end;
      end;
  end;
end;

// ============================================================================
// Conjugation table generation
// ============================================================================

procedure CreateTwistConjTable;
var
  cc, cc2: TCubieCube;
  i, k: integer;
  prod: TCornerCubie;
begin
  CubieIdentity(cc);
  CubieIdentity(cc2);
  for i := 0 to N_TWIST - 1 do
  begin
    SetCornOriCoord(cc, i);
    for k := 0 to 15 do
    begin
      CornMult(CornSym[k], cc.corn, prod);
      CornMult(prod, CornSym[InvIdx[k]], cc2.corn);
      TwistConj[i, k] := GetCornOriCoord(cc2);
    end;
  end;
end;

procedure CreateEdge8PermConjTable;
var
  cc, cc2: TCubieCube;
  i, k: integer;
  prod: TEdgeCubie;
begin
  CubieIdentity(cc);
  CubieIdentity(cc2);
  for i := 0 to N_UD_EDGES_PERM - 1 do
  begin
    SetPhase2EdgePermCoord(cc, i);
    for k := 0 to 15 do
    begin
      EdgeMult(EdgeSym[k], cc.edge, prod);
      EdgeMult(prod, EdgeSym[InvIdx[k]], cc2.edge);
      Edge8PermConj[i, k] := GetPhase2EdgePermCoord(cc2);
    end;
  end;
end;

// ============================================================================
// Edge8Perm extraction table
// ============================================================================

procedure CreateGetEdge8PermTable;
var
  cc, cc2: TCubieCube;
  i, j, x, y: integer;
  prod: TEdgeCubie;
begin
  // Initialize with sentinel
  for i := 0 to N_SLICE_SORTED - 1 do
    for j := 0 to 23 do
      GetEdge8Perm[i, j] := 65535;

  CubieIdentity(cc);
  CubieIdentity(cc2);

  for i := 0 to N_UD_EDGES_PERM - 1 do
  begin
    SetPhase2EdgePermCoord(cc, i);

    // Apply S_URF3 conjugation to get RL-slice sorted coord
    EdgeMult(EdgeSym[16], cc.edge, prod);
    EdgeMult(prod, EdgeSym[InvIdx[16]], cc2.edge);
    x := GetUDSliceSortedCoord(cc2); // this is the RLSliceSorted

    // Apply again to get FB-slice sorted coord
    EdgeMult(EdgeSym[16], cc2.edge, prod);
    EdgeMult(prod, EdgeSym[InvIdx[16]], cc2.edge);
    y := GetUDSliceSortedCoord(cc2) mod 24; // FBSliceSorted mod 24

    if GetEdge8Perm[x, y] = 65535 then
      GetEdge8Perm[x, y] := i;
  end;
end;

// ============================================================================
// Pruning table generation
// ============================================================================

// Legacy 2-bit access (kept for API compatibility but no longer used for pruning)
procedure SetPrun2Bit(var table: array of byte; index: int64; value: integer);
begin
  table[index] := byte(value);
end;

function GetPrun2Bit(const table: array of byte; index: int64): integer;
begin
  Result := table[index];
end;

procedure CreatePruningPhase1Table;
var
  totalSize: int64;
  done, total: int64;
  depth, classIdx, twist, m, x: integer;
  newClassIdx, newSym, newTwist: integer;
  idx: int64;
  changed: boolean;
begin
  totalSize := Int64(N_FLIPSLICE_CLASS) * N_TWIST;
  total := totalSize;
  SetLength(PruningPhase1, totalSize);
  FillByte(PruningPhase1[0], totalSize, $FF); // 255 = unvisited

  // Set solved state: flipSlice class 0, twist 0
  PruningPhase1[0] := 0;
  done := 1;
  depth := 0;

  while done < total do
  begin
    changed := false;
    for classIdx := 0 to N_FLIPSLICE_CLASS - 1 do
    begin
      for twist := 0 to N_TWIST - 1 do
      begin
        idx := Int64(classIdx) * N_TWIST + twist;
        if PruningPhase1[idx] = depth then
        begin
          for m := 0 to 17 do
          begin
            x := FlipSliceMove[classIdx, m];
            newClassIdx := x shr 4;
            newSym := x and 15;
            newTwist := TwistConj[TwistMove[twist, m], newSym];
            idx := Int64(newClassIdx) * N_TWIST + newTwist;

            if PruningPhase1[idx] = 255 then
            begin
              PruningPhase1[idx] := depth + 1;
              Inc(done);
              changed := true;
            end;
          end;
        end;
      end;
    end;
    Inc(depth);
    Write(StdErr, #13'Phase 1 pruning depth ', depth, ': ', done, '/', total);
    if not changed then break;
  end;
  WriteLn(StdErr, '');
end;

procedure CreatePruningPhase2Table;
var
  totalSize: int64;
  done, total: int64;
  depth, classIdx, edge8, m, p2m, x: integer;
  newClassIdx, newSym, newEdge8, cornRaw, newCornRaw: integer;
  idx: int64;
  changed: boolean;
begin
  totalSize := Int64(N_CORNPERM_CLASS) * N_UD_EDGES_PERM;
  total := totalSize;
  SetLength(PruningPhase2, totalSize);
  FillByte(PruningPhase2[0], totalSize, $FF); // 255 = unvisited

  // Set solved state
  PruningPhase2[0] := 0;
  done := 1;
  depth := 0;

  while done < total do
  begin
    changed := false;
    for classIdx := 0 to N_CORNPERM_CLASS - 1 do
    begin
      cornRaw := CornPermSymToRaw[classIdx];
      for edge8 := 0 to N_UD_EDGES_PERM - 1 do
      begin
        idx := Int64(classIdx) * N_UD_EDGES_PERM + edge8;
        if PruningPhase2[idx] = depth then
        begin
          for p2m := 0 to N_MOVE_P2 - 1 do
          begin
            m := Phase2MoveIdx[p2m];

            newCornRaw := CornPermMove[cornRaw, m];
            x := CornPermRawToSym[newCornRaw];
            newClassIdx := x shr 4;
            newSym := x and 15;

            newEdge8 := Edge8PermConj[Edge8PermMove[edge8, m], newSym];
            idx := Int64(newClassIdx) * N_UD_EDGES_PERM + newEdge8;

            if PruningPhase2[idx] = 255 then
            begin
              PruningPhase2[idx] := depth + 1;
              Inc(done);
              changed := true;
            end;
          end;
        end;
      end;
    end;
    Inc(depth);
    Write(StdErr, #13'Phase 2 pruning depth ', depth, ': ', done, '/', total);
    if not changed then break;
  end;
  WriteLn(StdErr, '');
end;

procedure CreateCornPermPruningTable;
var
  cc: TCubieCube;
  done, depth, i, k, axis, newCP: integer;
  changed: boolean;
begin
  FillByte(PruningCornPerm, SizeOf(PruningCornPerm), $FF);
  PruningCornPerm[0] := 0;
  done := 1;
  depth := 0;

  while done < N_CORNERS_PERM do
  begin
    changed := false;
    for i := 0 to N_CORNERS_PERM - 1 do
    begin
      if PruningCornPerm[i] = depth then
      begin
        for axis := 0 to 5 do
          for k := 0 to 2 do
          begin
            newCP := CornPermMove[i, 3 * axis + k];
            if PruningCornPerm[newCP] = $FF then
            begin
              PruningCornPerm[newCP] := depth + 1;
              Inc(done);
              changed := true;
            end;
          end;
      end;
    end;
    Inc(depth);
    if not changed then break;
  end;
end;

// ============================================================================
// Sym-coordinate computation
// ============================================================================

function ComputeFlipUDSliceSymCoord(const cc: TCubieCube): integer;
var
  k, raw, coord: integer;
  prod: TEdgeCubie;
  cc2: TCubieCube;
begin
  CubieIdentity(cc2);
  Result := -1;
  for k := 0 to 15 do
  begin
    EdgeMult(EdgeSym[k], cc.edge, prod);
    EdgeMult(prod, EdgeSym[InvIdx[k]], cc2.edge);
    raw := GetUDSliceCoord(cc2) * N_FLIP + GetEdgeOriCoord(cc2);
    coord := FlipUDSliceRawClassIndex(raw);
    if coord >= 0 then
    begin
      Result := coord shl 4 + k;
      exit;
    end;
  end;
end;

function ComputeCornPermSymCoord(const cc: TCubieCube): integer;
begin
  Result := CornPermRawToSym[GetCornPermCoord(cc)];
end;

// ============================================================================
// Pruning value access
// ============================================================================

function GetPrunPhase1(flipSliceClass, twist, sym: integer): integer;
begin
  Result := PruningPhase1[Int64(flipSliceClass) * N_TWIST + TwistConj[twist, sym]];
end;

function GetPrunPhase2(cornPermClass, edge8Perm, sym: integer): integer;
begin
  Result := PruningPhase2[Int64(cornPermClass) * N_UD_EDGES_PERM + Edge8PermConj[edge8Perm, sym]];
end;

// ============================================================================
// Table caching
// ============================================================================

function TrySaveTable(const filename: string; const data; size: integer): boolean;
var
  fs: TFileStream;
  dir: string;
begin
  Result := false;
  dir := GetTableCacheDir;
  try
    if not DirectoryExists(dir) then
      ForceDirectories(dir);
    fs := TFileStream.Create(dir + filename, fmCreate);
    try
      fs.WriteBuffer(data, size);
      Result := true;
    finally
      fs.Free;
    end;
  except
  end;
end;

function TryLoadTable(const filename: string; var data; size: integer): boolean;
var
  fs: TFileStream;
  path: string;
begin
  Result := false;
  path := GetTableCacheDir + filename;
  if not FileExists(path) then exit;
  try
    fs := TFileStream.Create(path, fmOpenRead);
    try
      if fs.Size = size then
      begin
        fs.ReadBuffer(data, size);
        Result := true;
      end;
    finally
      fs.Free;
    end;
  except
  end;
end;

function TrySaveDynTable(const filename: string; var data: array of byte): boolean;
var
  fs: TFileStream;
  dir: string;
begin
  Result := false;
  dir := GetTableCacheDir;
  try
    if not DirectoryExists(dir) then
      ForceDirectories(dir);
    fs := TFileStream.Create(dir + filename, fmCreate);
    try
      fs.WriteBuffer(data[0], Length(data));
      Result := true;
    finally
      fs.Free;
    end;
  except
  end;
end;

function TryLoadDynTable(const filename: string; var data: array of byte; expectedSize: integer): boolean;
var
  fs: TFileStream;
  path: string;
begin
  Result := false;
  path := GetTableCacheDir + filename;
  if not FileExists(path) then
    exit;
  try
    fs := TFileStream.Create(path, fmOpenRead);
    try
      if fs.Size = expectedSize then
      begin
        fs.ReadBuffer(data[0], expectedSize);
        Result := true;
      end;
    finally
      fs.Free;
    end;
  except
    on E: Exception do
      WriteLn(StdErr, '  Load error: ', path, ' - ', E.Message);
  end;
end;

// ============================================================================
// Main initialization
// ============================================================================

procedure InitAllTables;
var
  prunSize1, prunSize2: int64;
begin
  if TablesReady then exit;

  WriteLn(StdErr, 'Initializing min2phase tables...');

  // Symmetries (always computed, fast)
  WriteLn(StdErr, '  Creating symmetry tables...');
  CreateSymmetries;
  CreateSymMultTable;
  CreateSymMoveTable;

  // Class-index tables (needed before move tables)
  WriteLn(StdErr, '  Creating FlipUDSlice class table...');
  CreateFlipUDSliceClassTable;
  WriteLn(StdErr, '  Creating CornPerm class table...');
  CreateCornPermClassTable;

  // Move tables
  WriteLn(StdErr, '  Creating move tables...');
  CreateTwistMoveTable;
  CreateFlipMoveTable;
  CreateUDSliceMoveTable;
  CreateCornPermMoveTable;
  CreateUDSliceSortedMoveTable;
  CreateEdge8PermMoveTable;
  CreateFlipSliceMoveTable;

  // Conjugation tables
  WriteLn(StdErr, '  Creating conjugation tables...');
  CreateTwistConjTable;
  CreateEdge8PermConjTable;

  // Edge8Perm extraction table
  WriteLn(StdErr, '  Creating Edge8Perm table...');
  CreateGetEdge8PermTable;

  // Pruning tables (full depth, 1 byte per entry)
  prunSize1 := Int64(N_FLIPSLICE_CLASS) * N_TWIST;
  prunSize2 := Int64(N_CORNPERM_CLASS) * N_UD_EDGES_PERM;

  WriteLn(StdErr, '  Phase 1 pruning table (', prunSize1 div 1024 div 1024, ' MB)...');
  SetLength(PruningPhase1, prunSize1);
  if not TryLoadDynTable('prun_phase1_v' + IntToStr(CACHE_VERSION) + '.dat',
                          PruningPhase1, prunSize1) then
  begin
    CreatePruningPhase1Table;
    TrySaveDynTable('prun_phase1_v' + IntToStr(CACHE_VERSION) + '.dat', PruningPhase1);
  end;

  WriteLn(StdErr, '  Phase 2 pruning table (', prunSize2 div 1024 div 1024, ' MB)...');
  SetLength(PruningPhase2, prunSize2);
  if not TryLoadDynTable('prun_phase2_v' + IntToStr(CACHE_VERSION) + '.dat',
                          PruningPhase2, prunSize2) then
  begin
    CreatePruningPhase2Table;
    TrySaveDynTable('prun_phase2_v' + IntToStr(CACHE_VERSION) + '.dat', PruningPhase2);
  end;

  // CornPerm pre-check table
  CreateCornPermPruningTable;

  TablesReady := true;
  WriteLn(StdErr, 'min2phase tables ready.');
end;

initialization
  TablesReady := false;

end.
