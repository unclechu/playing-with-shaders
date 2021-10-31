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

## Known issues

GNU/Make doesn’t recognize changes in `*.glsl` source files for some reason.
So if you just change a shader without cleaning the cache you actually get
previous result. I usually use `touch src/main.cpp && make run` as a hack to not
clean the whole cache. Or you can use this hack for automatically recompile and
run when you save any change in a shader file:

``` sh
while true; do inotifywait -e create src/shaders/** ; touch src/main.cpp && make run; done
```

## Author

Viacheslav Lotsmanov
