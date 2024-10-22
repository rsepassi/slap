CFLAGS += -std=c99 -Wall -Werror

slap: Makefile slap.c
	make -C mir
	$(CC) -o $@ -Imir/include $(CFLAGS) slap.c mir/libmir.a -lc

libslap.a: Makefile slap.c
	$(CC) -c -Imir/include $(CFLAGS) slap.c
	ar rcs $@ slap.o

clean:
	rm -f slap slap.o libslap.a

clean-all: clean
	make -C mir clean
