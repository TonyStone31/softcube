# NxN Rubik's Cube Solver

A standalone command-line solver for Rubik's Cubes of any size (2x2 and up). Written in Free Pascal. No external dependencies.

This solver is designed to be used independently — from the command line, shell scripts, web backends, or as a subprocess in any application. It communicates entirely through stdin/stdout/stderr and exit codes.

## Credits

- **Herbert Kociemba** — The NxN reduction algorithm is reimplemented in Pascal from [Kociemba's RubikNxNxNSolver](https://github.com/hkociemba/RubikNxNxNSolver) (Python). The built-in 3x3 two-phase solver is reimplemented from Kociemba's two-phase algorithm originally written in Java for [CubeExplorer](http://kociemba.org/cube.htm).

## Building

Requires [Free Pascal Compiler](https://www.freepascal.org/) (FPC) 3.2+.

```bash
fpc -O3 -Sg nxn_solver.lpr
```

Or with a custom FPC installation:
```bash
/path/to/fpc -O3 -Sg -Fu/path/to/fpc/units/x86_64-linux/* nxn_solver.lpr
```

The build produces a single static binary (`nxn_solver`) with no runtime dependencies.

## Usage

```
nxn_solver <size> <facelet_string> [options]
```

### Arguments

- `size` — Cube dimension: 2, 3, 4, 5, 6, 7, etc.
- `facelet_string` — Cube state as `6 * N^2` characters using face letters `U R F D L B`

### Options

| Option | Description | Default |
|--------|-------------|---------|
| `--verbose` | Print detailed progress to stderr | off |
| `--table-dir <path>` | Directory for cached table files | `data/` next to binary |
| `--max-length <n>` | Target max moves for 3x3 phase | 23 |
| `--time-limit <ms>` | Time limit for 3x3 solver (ms) | 5000 |

### Commands

| Command | Description |
|---------|-------------|
| `--generate-tables <size>` | Pre-generate all tables for a cube size (run once) |
| `--check-tables <size>` | Check which tables exist and which are missing |
| `--selftest` | Run built-in verification tests |

## Output Protocol

### stdout (solution)

On success, exactly two lines:
```
<elapsed_ms> ms
<move_sequence>
```

Example:
```
4891 ms
R U R' F2 D 2U R2 F2 R2 F2 2U' L' D' F2 R' U B L' D
```

Line 1 is the total solve time in milliseconds. Line 2 is the solution as a space-separated move sequence.

### stderr (progress)

Progress and status messages are written to stderr in machine-parseable formats. All are optional — you can ignore stderr entirely if you only need the solution.

| Prefix | Format | Description |
|--------|--------|-------------|
| `PHASE:` | `PHASE: <n> <name> - <moves> moves in <ms> ms` | A solve phase completed |
| `SOLUTION:` | `SOLUTION: <moves> moves (target: <n>)` | 3x3 solution found |
| `SEARCH:` | `SEARCH: best so far <n> moves, searching depth <d>...` | Searching for shorter solution |
| `SEARCH:` | `SEARCH: Phase 5 edge pairing depth <d>...` | Edge pairing progress |
| `PROGRESS:` | `PROGRESS:<Phase>:<Table>:<done>:<total>` | Table generation progress |

**PHASE examples** (one per solve phase for 4x4+):
```
PHASE: 1 UD centers - 136 moves in 7989 ms
PHASE: 2 FB centers - 142 moves in 973 ms
PHASE: 3 RLFB centers - 162 moves in 6244 ms
PHASE: 4 UD centers - 117 moves in 227 ms
PHASE: 5 Edge pairing - 457 moves in 200462 ms
PHASE: 6 3x3 solve - 23 moves in 837 ms
```

**PROGRESS example** (during table generation):
```
PROGRESS:Phase1:Loading/Creating:0:0
PROGRESS:Phase2:UDCenterMove:50000:100000
```

### Exit Codes

| Code | Meaning |
|------|---------|
| 0 | Success — solution on stdout |
| 1 | Invalid arguments, bad facelet string, or solver error |
| 4 | Solution verification failed (internal error) |

## Facelet String Format

The facelet string describes the cube by listing the color of each sticker, face by face, in the order **U R F D L B**. Within each face, stickers are read left-to-right, top-to-bottom (as if looking directly at that face).

Each character represents a color using the letter of the face it belongs to on a solved cube:

| Letter | Color | Solved face |
|--------|-------|-------------|
| `U` | White | Up |
| `R` | Red | Right |
| `F` | Green | Front |
| `D` | Yellow | Down |
| `L` | Orange | Left |
| `B` | Blue | Back |

### 3x3 Example (54 characters)

Solved state:
```
UUUUUUUUURRRRRRRRRFFFFFFFFFDDDDDDDDDLLLLLLLLLBBBBBBBBB
```

Each face has 9 stickers (3x3), read in this order:
```
0 1 2
3 4 5
6 7 8
```

### 2x2 Example (24 characters)

Each face has 4 stickers (2x2):
```
0 1
2 3
```

### NxN Example

For an NxN cube, the string has `6 * N^2` characters. Each face has `N^2` stickers read left-to-right, top-to-bottom.

## Move Notation

The solution uses standard cube notation:

| Move | Meaning |
|------|---------|
| `R` | Right face 90° clockwise |
| `R'` | Right face 90° counter-clockwise |
| `R2` | Right face 180° |
| `2R` | Second-layer right slice (inner slice) |
| `2R'` | Second-layer right slice counter-clockwise |
| `2R2` | Second-layer right slice 180° |
| `3R` | Third-layer right slice (for 6x6+) |

Face letters: `U` (up), `D` (down), `R` (right), `L` (left), `F` (front), `B` (back).

## Examples

### Solve a 3x3
```bash
./nxn_solver 3 DRLUUBFBRBLURRLRUBLRDDFDLFUFUFFDBRDUBRUFLLFDDBFLUBLRBD
```

### Solve a 3x3 with shorter solution
```bash
./nxn_solver 3 DRLUUBFBRBLURRLRUBLRDDFDLFUFUFFDBRDUBRUFLLFDDBFLUBLRBD \
  --max-length 20 --time-limit 30000
```

### Solve a 2x2
```bash
./nxn_solver 2 UFRDURBDLFFLRUDBBLRDLU
```

### Solve a 5x5 with progress output
```bash
./nxn_solver 5 <150-char-facelet-string> --verbose --table-dir ~/.softcube/tables
```

### Pre-generate tables
```bash
# Generate once, reuse on all subsequent solves
./nxn_solver --generate-tables 5 --table-dir ~/.softcube/tables

# Check what's ready
./nxn_solver --check-tables 5 --table-dir ~/.softcube/tables
```

## Table Files

The solver generates pruning/move tables on first use and caches them to disk. Subsequent solves load tables from cache instantly.

| Cube Size | Table Size | Generation Time |
|-----------|-----------|-----------------|
| 2x2, 3x3 | ~240 MB | ~2 seconds |
| 4x4, 5x5 | ~3 GB | ~10 minutes |
| 6x6+ | ~7+ GB | Several hours |

Tables are stored in the directory specified by `--table-dir` (default: `data/` next to the binary). They only need to be generated once per cube size.

For 6x6+ cubes, use `--generate-tables` to pre-generate tables before solving. Without pre-generated tables, the first solve will block while tables are created.

## How It Works

### 2x2 and 3x3

Uses Kociemba's **two-phase algorithm** directly:
- Phase 1: Reduce to the G1 subgroup (orient edges/corners, position UD slice edges)
- Phase 2: Solve within G1 (permute all pieces using only U, D, R2, L2, F2, B2)

IDA* search with pruning tables finds near-optimal solutions (typically 19-23 moves).

### 4x4 and larger

Uses a **reduction approach** — solve centers, pair edges, then solve as 3x3:

| Phase | Goal | Moves |
|-------|------|-------|
| 1 | Place UD center pieces on U/D faces | ~100-200 |
| 2 | Place FB center pieces on F/B faces | ~100-200 |
| 3 | Fully solve all remaining centers | ~100-200 |
| 4 | Fix center parity | ~50-150 |
| 5 | Pair all edges (reduce to virtual 3x3) | ~200-500 |
| 6 | Solve the virtual 3x3 (two-phase) | ~20-23 |

Each phase uses IDA* search with precomputed pruning tables for efficient solving.

## Integration Guide

To use this solver as a backend for your application:

1. **Spawn** `nxn_solver` as a subprocess with size and facelet string as arguments
2. **Read stdout** for the solution (2 lines: timing + moves)
3. **Read stderr** for real-time progress updates (optional, useful for UI)
4. **Check exit code** — 0 means success
5. **Pre-generate tables** for the cube sizes you need (avoids delay on first solve)

### Python example

```python
import subprocess

result = subprocess.run(
    ['./nxn_solver', '3', 'DRLUUBFBRBLURRLRUBLRDDFDLFUFUFFDBRDUBRUFLLFDDBFLUBLRBD'],
    capture_output=True, text=True
)

if result.returncode == 0:
    lines = result.stdout.strip().split('\n')
    timing = lines[0]       # "1234 ms"
    solution = lines[1]     # "R U R' F2 D ..."
    print(f"Solved in {timing}: {solution}")
```

### Node.js example

```javascript
const { execFile } = require('child_process');

execFile('./nxn_solver', ['3', faceletString], (err, stdout, stderr) => {
    if (!err) {
        const [timing, solution] = stdout.trim().split('\n');
        console.log(`Solved in ${timing}: ${solution}`);
    }
});
```

### Web backend pattern

```
Client POST /solve { size: 5, facelets: "..." }
  → Server spawns nxn_solver subprocess
  → Server streams stderr PHASE/SEARCH lines as SSE events (optional)
  → Server returns stdout solution as JSON response
```

### Tips

- Use `--table-dir` to share a single table cache across instances
- For web servers, pre-generate tables at deploy time with `--generate-tables`
- The `--time-limit` option controls the 3x3 phase only; larger phases have their own timeouts
- Parse `PHASE:` lines from stderr for per-phase timing breakdown
- The solver is single-threaded and CPU-bound; parallelize by running multiple instances
