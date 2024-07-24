# NethackLogfiles

## About NethackLogfiles

Read logfiles of the most-ancient game of 'Nethack'.
The package provides functions for parsing logfiles
and xlogfiles. Abbreviations in those files (such as
character class and alignment) can be expanded.

              ----
             --...      ---
            ......     --.--             ----
            ....^--    |...|    .----    |..|
         --.......---- |...-    -...--- --..--
        -#*........|.----...--- |.....| |....|
       --.----.........--.....-- -....| --..--
       |<.|    -...............| |....-  |.--
       -.)|    -..............-- |.....|--.|
        .--    ...........^...|---.|.|.||..|
               ..............--....| |.||.--
           |................--..---- |.|-.----------
          --..[)..[.........^..--    |.|........d.).
        ---....[.-.-.-.....-----     |....---------.
        |....................--      ------
        |.......---------.....|
        |........|         ...|
        ---.>...--          ---
          --.----
           ---
    Enrico the Conjurer              St:25 Dx:13 Co:16 In:20 Wi:11 Ch:8 Chaotic
    Dlvl:4 $:70 HP:29(29) Pw:13(42) AC:7 Xp:4 Burdened



## Installation

The latest *development* version is available from
[https://enricoschumann.net/R/packages/NethackLogfiles/](https://enricoschumann.net/R/packages/NethackLogfiles/). You
can install either version directly from within R:

    install.packages('NethackLogfiles', ## development version
                     repos = c('https://enricoschumann.net/R',
                               getOption('repos')))


There is a publicly-available code repository at
[https://git.sr.ht/~enricoschumann/NethackLogfiles](https://git.sr.ht/~enricoschumann/NethackLogfiles)
