# 3110

Team 3: The HALK

- Alvin Lyuh (akl93)
- Leo Mazur (lgm59)
- Amanda Zhu (az565)
- Helena Zhang (hxz3)
- Kyle Fan (kf446)

## Run instructions

Currently, just run:

```sh
dune exec bin/main.exe
```

On wayland, you can run:

```sh
xwayland-satellite :1
DISPLAY=:1 dune exec bin/main.exe
```

The hiders win by surviving until the timer runs out (20 seconds by default).
To change it, pass `--time`:

```sh
dune exec bin/main.exe -- --time=30
```

## Coding Practices

Work on a new branch for each feature, named `date-feature` (ex: 2026-11-20-update-coding-practices).

For changes made with AI, start commit message with `[AI]`.
