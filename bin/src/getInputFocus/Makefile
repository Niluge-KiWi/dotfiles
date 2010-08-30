all: getInputFocus

getInputFocus: getInputFocus.c
	gcc -o getInputFocus getInputFocus.c -lX11

install: getInputFocus
	cp getInputFocus ~/bin
	cp getInputFocusName.sh ~/bin/getInputFocusName

clean:
	rm getInputFocus
