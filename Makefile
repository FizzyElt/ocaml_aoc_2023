target = day1

build:
	dune build ./$(target)/main.exe

run:
	make build
	dune exec $(target)/main.exe

run-part1:
	make build
	dune exec $(target)/main.exe $(target)/input1.txt

run-part2:
	make build
	dune exec $(target)/main.exe $(target)/input2.txt

clean:
	rm -rf _build