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
# default rounds is 100, default --time is 100 seconds per round
dune exec bin/train.exe -- --rounds=100 --time=100 --seed=1
```

The hider is rewarded for every step it stays alive, so its return is
`1 - 0.5^(seconds survived / 20)`. After each update the trainer prints the
average survival time of the batch and each network's losses.

The map looks the same mirrored left-right or top-bottom, so each update also
penalizes the networks for treating a position differently from its mirror
images (`Ppo.symmetry_coef`, 0 turns it off). The printed asymmetry is how
far they still are from symmetric; it should fall toward 0 as training goes.

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

The trainer shows each round in a window. `--speed` sets how many game
seconds pass per real second (default 1); `--speed=0` or `--speed=inf` runs
as fast as possible. While it runs, `>` and `<` in the window step the speed
through 1x, 5x, 20x and as fast as possible. Closing the window (or pressing
Q) quits right away without saving.

```sh
dune exec bin/train.exe -- --speed=5
dune exec bin/train.exe -- --speed=inf --rounds=1000 --out=nn.txt
```

On wayland, you might need to run:

```sh
xwayland-satellite :1
DISPLAY=:1 dune exec bin/main.exe
```

## Coding Practices

Work on a new branch for each feature, named `date-feature` (ex: 2026-11-20-update-coding-practices).

For changes made with AI, start commit message with `[AI]`.
