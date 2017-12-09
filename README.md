## Geschwindigkeitsficken - Speedfuck

### Name

The name originated in a skype cheat many years ago. [@Webfreak001](https://github.com/WebFreak001) had the idea of
writing an optimizing brainfuck compiler calling it "Speedfuck" - a translator
bot automatically translated it to "Geschwindigkeitsficken" from the german words
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
bin/speedfuck file myCode.bf #reading code from a file
bin/speedfuck stdin #reading code from stdin
```

Currently the output is pseudo C-like code, but outputting actual assembly
and passing it to `as` is planned.
