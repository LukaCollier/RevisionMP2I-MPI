labyrinthe : unionfind.o heap.o labyrinthe.c
	gcc -o labyrinthe unionfind.o heap.o labyrinthe.c -Wall -fsanitize=address

unionfind.o : unionfind.c
	gcc -c unionfind.c -Wall -fsanitize=address

heap.o : heap.c
	gcc -c heap.c -Wall -fsanitize=address

clean :
	rm -f labyrinthe unionfind.o heap.o
