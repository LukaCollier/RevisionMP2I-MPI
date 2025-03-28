id3 : decisiontree.o id3.c
	gcc -Wall -fsanitize=address -o id3 decisiontree.o id3.c -lm

decisiontree.o : decisiontree.c
	gcc -Wall -fsanitize=address -c decisiontree.c

clean :
	rm -rf id3 decisiontree.o
