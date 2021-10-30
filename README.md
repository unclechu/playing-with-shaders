# My GLSL shaders playground

![Mandelbrot set](artwork/mandelbrot-set-screenshot.png)

## How to run

Nix is a requirement.

Either you enter a Nix Shell and run `make` commands:

``` sh
nix-shell --run 'make run'
```

Or you can build the derivation and run it:

``` sh
nix-build
result/bin/app
```

You can pre-build Nix development environment
(to add a GC root and prevent it from being garbage
collected unless you remove the `result*` symlink):

``` sh
nix-build -A env -o result-env
```

## Author

Viacheslav Lotsmanov
