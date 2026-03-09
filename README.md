# SoftCube
Rubik's Cube simulator and solver written with Lazarus and Free Pascal. Supports 2x2 through 7x7+ cubes with 3D visualization, keyboard controls, and webcam scanning.

## Features
- Interactive 3D cube with mouse and keyboard controls for any size cube (2x2 and up)
- Solver support for 2x2 through 5x5 (larger sizes in progress)
- Tested with cubes up to 10x10
- Manual color entry and webcam-based color scanning
- Solve quality presets (Fast / Balanced / Optimal)
- BGRABitmap-based rendering with lighting effects

## Credits & Acknowledgments

### Herbert Kociemba
The NxN solver is based on [Herbert Kociemba's RubikNxNxNSolver](https://github.com/hkociemba/RubikNxNxNSolver) — a reduction-based approach that solves NxN cubes by reducing them to 3x3 and then applying the two-phase algorithm.

The built-in 3x3 two-phase solver is a Pascal port of Kociemba's two-phase algorithm, originally implemented in his [CubeExplorer](http://kociemba.org/cube.htm) software. His pioneering work on optimal and near-optimal Rubik's Cube solving algorithms is the foundation of the solver used here.

### Original 3D Rendering
The original 3D cube rendering code was obtained from [CodeS-SourceS.CommentCaMarche.net](https://codes-sources.commentcamarche.net/source/53132-rubik-s-cube) and has been substantially modified and extended to support NxN cubes.

## Building
Requires [Lazarus IDE](https://www.lazarus-ide.org/) with Free Pascal Compiler (FPC) and the BGRABitmap package.

```bash
lazbuild RubiksCube.lpi --build-all
```

The NxN solver CLI tool can be built separately:
```bash
cd nxn-solver
fpc nxn_solver.lpr
```
