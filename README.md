## Geschwindigkeitsficken - Speedfuck

[Brainfuck](https://en.wikipedia.org/wiki/Brainfuck) is an esoteric (aka joke)
programming language invented by Urban Müller in 1993. This project aims at
compiling and more importantly optimizing code written in Brainfuck. If you
want to know more about optimizing brainfuck i recommend [Mats Linander](http://calmerthanyouare.org/2015/01/07/optimizing-brainfuck.html) blog
post about it. There is also a list of similar projects [on this page](https://github.com/lifthrasiir/esotope-bfc/wiki/Comparison).

### Name

The name originated in a skype chat many years ago. [@Webfreak001](https://github.com/WebFreak001) had the idea of
writing an optimizing brainfuck compiler calling it "Speedfuck" - a translator
bot automatically translated it to "Geschwindigkeitsficken" from the german word
for Speed (Geschwindigkeit) and the colloquial word for having Sex (ficken).

### Compiling

```sh
git clone https://github.com/M4GNV5/Geschwindigkeitsficken.git
cd Geschwindigkeitsficken

mkdir bin
ghc -outputdir bin -isrc -o bin/speedfuck -O2 src/main.hs
```

### Usage

There are three different usage modes
```sh
bin/speedfuck arg '+++++--[->+>++<<]' #passing code via the commandline
bin/speedfuck file myCode.b #reading code from a file
bin/speedfuck stdin #reading code from stdin
```

Currently the output is pseudo C-like code, but outputting actual assembly
and passing it to `as` is planned.
