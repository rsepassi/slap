CFLAGS := -std=c99 -Wall -Werror

slap: slap.c
	make -C mir
	$(CC) -o slap -Imir/include $(CFLAGS) slap.c mir/libmir.a -lc

clean:
	rm -f slap

clean-all: clean
	make -C mir clean
