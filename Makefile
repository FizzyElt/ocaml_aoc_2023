target = day1

build:
	dune build ./$(target)/main.exe

run:
	make build
	dune exec $(target)/main.exe

clean:
	rm -rf _build