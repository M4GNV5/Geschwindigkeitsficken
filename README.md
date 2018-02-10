## Geschwindigkeitsficken - Speedfuck

[Brainfuck](https://en.wikipedia.org/wiki/Brainfuck) is an esoteric (aka joke)
programming language invented by Urban Müller in 1993. This project aims at
compiling and more importantly optimizing code written in Brainfuck. If you
want to know more about optimizing brainfuck i recommend [Mats Linander](http://calmerthanyouare.org/2015/01/07/optimizing-brainfuck.html)s blog
post about it. There is also a list of similar projects [on this page](https://github.com/lifthrasiir/esotope-bfc/wiki/Comparison).

### Name

The name originated in a skype chat many years ago. [@Webfreak001](https://github.com/WebFreak001) had the idea of
writing an optimizing brainfuck compiler calling it "Speedfuck" - a translator
bot automatically translated it to "Geschwindigkeitsficken" from the german word
for Speed (Geschwindigkeit) and the colloquial word for having Sex (ficken).

### Speed

![](https://i.m4gnus.de/brainfuckcompilers1.png)

The graph shows the runtime of [Mandelbrot](examples/mandelbrot.bf)
compiled using different Brainfuck compilers. All compilers which output C code
appear twice, compiling the generated code with `gcc -O0` vs `gcc -O2`. Speedfuck
appears three times: Compiling generated C code with `-O0`, with `-O2` and
generating assembly code directly. The script for measuring execution speed and plotting
can be found [here](https://gist.github.com/M4GNV5/dbad0a612349d65b9f7140199de270d1).

Here is a list of the compilers in the graph above:
- [esotope](https://github.com/lifthrasiir/esotope-bfc) 2009 by Kang Seonghoon
- [bfoptimization](https://github.com/matslina/bfoptimization) 2015 by Mats Linander
- [bf2c.hs](http://esoteric.sange.fi/brainfuck/impl/compilers/bf2c.hs) 2002 by Bertram Felgenhauer
- [speedfuck](https://github.com/M4GNV5/Geschwindigkeitsficken) 2018 by Jakob Löw
- [yabfc](https://github.com/cameronswinoga/yabfc) 2017 by Cameron Swinoga
- [bfc](https://github.com/Wilfred/bfc) 2017 by Wilfred Hughes
- [bfdb](http://djm.cc/dmoews.html) 2006 by David Moews

### Compiling

```sh
git clone https://github.com/M4GNV5/Geschwindigkeitsficken.git
cd Geschwindigkeitsficken

make
```

### Examples

Compiling the popular Hello World! program
```b
++++++++++[>+++++++>++++++++++>+++>+<<<<-]
>++.>+.+++++++..+++.>++.<<+++++++++++++++.>.+++.------.--------.>+.>.
```
outputs
```sh
puts("Hello World!\n")
```

Well duh! But thats kind of boring so lets take simpler programs and disable constant folding:
```sh
$ bin/speedfuck -Oconstfold -Otrailing -code '+++++>>++<<--'
p[0] += 3
p[2] += 2

#Here you can already see one of the most common optimizations applied to
# brainfuck: grouping +, -, > and <. Note how the statements are reordered so
# that the last two - can be grouped with the first five + even though there is
# code in between.

$ bin/speedfuck -Oconstfold -Otrailing -code '++++>>++<<-[-]'
p[0] = 0
p[2] += 2

#Another common optimization is [-] which sets a cell to zero, this also turns
# all previous changes to p[0] to noops so they are removed.

$ bin/speedfuck -Oconstfold -Otrailing -code '++[->+++<]'
p[0] += 2
p[1] += p[0] * 3
p[0] = 0

#the above loop is called a copy loop as it adds three to p[1] and subtracts one
# from p[0] until p[0] is zero. Thus the loop can be optimized to adding
# 3 * p[0] to p[1] and setting p[0] to zero.

$ bin/speedfuck -Oconstfold -Otrailing -code '++[-->+++>++++<<]'
p[0] += 2
p[2] += p[0] * 2
p[1] += p[0] * 3 / 2
p[0] = 0

#copyloops still work with multiple multiplications, in fact the set to zero
# loop [-] is just a special copyloop without multiplications.

$ bin/speedfuck -Oconstfold -code '++>+++[->++++<]<++[->>+++<<]>>++.'
p[0] += 4
p[1] += 3
p[2] += p[1] * 4 + p[0] * 3 + 2
putchar(p[2])

#The above brainfuck code might look complicated at first but in fact it's just
# two copy loops which both add to p[2]. Note how the p[0] += 4 is combined
# from the two ++ before and after the first copyloop and how the two copyloops
# optimize to a single p[2] += ... statement.
```

### Command line options

#### Generic
Either `-i` or `-code` must be given.
- `-i <file>` specifies an input file
- `-o <file>` specifies the output file (default: `a.out`). If the output file ends
with `.c` C code will be outputted, `.S` makes it output amd64 assembly and `.dump` will
write the internal instruction layout. Everything else produces a native executable.
- `-code <code>` passes code through a command line option rather than reading it from a file

#### Optimization
By default all optimizations are enabled and can be disabled using the specific
`-O` switch. Using `-Onone` disables all optimizations and allows to enable
specific ones using the corresponding `-O` switch.

- `-Onone` Disables all optimizations by default
- `-Ogroup` groups censecutive `+`, `-` and `>`, `<` together.
- `-Ocopyloop` optimize `[->++<]` to `p[1] = p[0] * 2; p[0] = 0;`
- `-Oshifts` optimize `++>++` to `p[0] += 2; p[1] += 2;`
- `-Onoop` removes noops (e.g. `++--`)
- `-Ogroup2` reorders and groups statements. e.g. `++>++<++` turns into `p[0] += 4; p[1] += 2`
- `-Oconstfold` evaluate the program as much as possible
- `-Otrailing` remove all instructions after the last `,` or `.`
