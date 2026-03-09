unit nxn_min2phase;
// Pure Pascal 3x3 Rubik's Cube solver using Kociemba's Two-Phase Algorithm
// Main interface unit - replaces external linux-2phase binary dependency

{$mode objfpc}{$H+}

interface

// Initialize the two-phase solver tables (call once before solving).
// This generates symmetry tables, move tables, and pruning tables.
// First call takes ~30-60 seconds; subsequent calls load from cache.
// Tables are cached in data/
procedure InitMin2Phase;

// Solve a 3x3 Rubik's Cube given as a 54-character URFDLB facelet string.
// Returns space-separated move string (e.g. 'U R2 F'' D2 L B')
// Returns '' for already-solved cube.
// Returns '!ERROR' if cube state is invalid (parity error, etc.)
// maxLength: maximum solution length to accept (default 23, lower = better but slower)
// timeLimitMs: time budget in ms (default 5000; solver continues improving within this time)
function SolveMin2Phase(const facelets: string;
  maxLength: integer = 23; timeLimitMs: integer = 5000): string;

// Check if the solver tables are initialized and ready
function Min2PhaseInitialized: boolean;

implementation

uses nxn_min2phase_coord, nxn_min2phase_search;

procedure InitMin2Phase;
begin
  InitAllTables;
end;

function SolveMin2Phase(const facelets: string;
  maxLength: integer; timeLimitMs: integer): string;
begin
  if not TablesReady then
    InitAllTables;
  Result := SolveSearch(facelets, maxLength, timeLimitMs);
end;

function Min2PhaseInitialized: boolean;
begin
  Result := TablesReady;
end;

end.
