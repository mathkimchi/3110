# 3110

Team 3: The HALK

- Alvin Lyuh (akl93)
- Leo Mazur (lgm59)
- Amanda Zhu (az565)
- Helena Zhang (hxz3)
- Kyle Fan (kf446)

## Run instructions

To play the game, run:

```sh
dune exec bin/main.exe
# with options: (default 20 sec)
dune exec bin/main.exe -- --time=30
```

To run the trainer, run:

```sh
dune exec bin/train.exe
# default rounds is 100
dune exec bin/train.exe -- --rounds=100 --time=30 --seed=1
```

By default both sides are trained with PPO (needs `opam install owl-base`).
Pick who plays each side, how often the networks update, and where to save
or load them:

```sh
# train only the hider, against the greedy hunter, updating every 10 rounds
dune exec bin/train.exe -- --hunter=greedy --batch=10 --out=nn.txt
# keep training from a saved file
dune exec bin/train.exe -- --in=nn.txt --out=nn.txt
```

Without `--out` nothing is saved. The file holds the seed and the networks of
the sides that were PPO. `--out` and `--in` need at least one PPO side.

On wayland, you might need to run:

```sh
xwayland-satellite :1
DISPLAY=:1 dune exec bin/main.exe
```

## Coding Practices

Work on a new branch for each feature, named `date-feature` (ex: 2026-11-20-update-coding-practices).

For changes made with AI, start commit message with `[AI]`.
