Roguelike game written in Haskell

Not much yet.

Build with stack and run:
```
stack build

stack exec rhogue
```


## Todo
need to standardize coordinates... UI uses (r,c), rest uses (x,y).
  would be best with a more opaque interface between the game and the UI

have an arbitrary number of Actors

time queue

NPCs

fix module exports/imports - currently the world's most polluted namespace

randomly generated levels

the part where it's a game


rewrite UI/use another library
