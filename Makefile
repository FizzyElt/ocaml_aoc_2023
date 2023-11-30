target = day1
datafile = day1/test.txt

build:
	dune build ./$(target)/main.exe

run:
	make build
	dune exec $(target)/main.exe $(datafile)

clean:
	rm -rf _build