all: getInputFocus

getInputFocus: getInputFocus.c
	gcc -o getInputFocus getInputFocus.c `pkg-config --cflags --libs libwnck-1.0`

install: getInputFocus
	cp getInputFocus ~/bin

clean:
	rm getInputFocus
