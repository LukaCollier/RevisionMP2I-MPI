labyrinthe : unionfind.o labyrinthe.c
	gcc -o labyrinthe unionfind.o labyrinthe.c -Wall -fsanitize=address

unionfind.o : unionfind.c
	gcc -c unionfind.c -Wall -fsanitize=address

clean :
	rm -f labyrinthe unionfind.o
