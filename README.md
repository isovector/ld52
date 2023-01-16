# ld52

## Controls

- Movement: arrow keys
- Jump: space, or x
- Throw warpball: hold z to charge

## Downloads

- [Linux x86_64](https://files.jam.host/uploads/$314776/chickenman-x86_64-linux.AppImage)

## Build Instructions

ld52 is a Haskell project, which means you're going to need a Haskell toolchain
to compile it. First, install [Stack](https://docs.haskellstack.org/en/stable/)
and then run

```bash
$ stack run
```

you'll need sdl2 and openal. On Fedora:

```bash
$ dnf install sdl2 openal-devel freealut-devel
```

which will kick off a long build and eventually start the game!


## Made By:

- [Andrew McKnight](https://github.com/amcknight/)
- [Sandy Maguire](https://github.com/isovector/)

