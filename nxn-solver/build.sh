#!/bin/bash
# Build nxn_solver with auto-incrementing build number
# Usage: ./build.sh [--debug]

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
cd "$SCRIPT_DIR"

# Use FPC from PATH, or set FPC env var to override
FPC="${FPC:-$(command -v fpc)}"
if [ -z "$FPC" ]; then
    echo "Error: fpc not found. Install Free Pascal or set FPC=/path/to/fpc"
    exit 1
fi
FPC_DIR="$(dirname "$(dirname "$FPC")")"
FPC_UNITS="${FPC_UNITS:-${FPC_DIR}/units/x86_64-linux/*}"
VERSION_FILE="version.inc"

# Read current build number
CURRENT_BUILD=$(grep 'SOLVER_BUILD' "$VERSION_FILE" | grep -o '[0-9]*')
CURRENT_VERSION=$(grep 'SOLVER_VERSION' "$VERSION_FILE" | grep -o "'[^']*'" | tr -d "'")

# Increment build number
NEW_BUILD=$((CURRENT_BUILD + 1))

# Update version.inc
cat > "$VERSION_FILE" << EOF
const
  SOLVER_VERSION = '${CURRENT_VERSION}';
  SOLVER_BUILD = ${NEW_BUILD};
EOF

echo "Building nxn_solver v${CURRENT_VERSION} build ${NEW_BUILD}..."

# Clean
rm -f *.o *.ppu

# Compile
if [ "$1" = "--debug" ]; then
    $FPC -g -Sg -Fu$FPC_UNITS nxn_solver.lpr 2>&1
else
    $FPC -O3 -Sg -CX -XX -Fu$FPC_UNITS nxn_solver.lpr 2>&1
fi

if [ $? -eq 0 ]; then
    echo "Build successful: nxn_solver v${CURRENT_VERSION} build ${NEW_BUILD}"
else
    echo "Build FAILED"
    # Revert build number on failure
    cat > "$VERSION_FILE" << EOF
const
  SOLVER_VERSION = '${CURRENT_VERSION}';
  SOLVER_BUILD = ${CURRENT_BUILD};
EOF
    exit 1
fi
