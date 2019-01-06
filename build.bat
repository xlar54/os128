64tass -a ./src/test.asm -l ./target/test.lst -o ./target/test

c1541 -format "os128,sh" d64 ./target/os128.d64
c1541 -attach ./target/os128.d64 -write ./target/test test