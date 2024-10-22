CFLAGS += -O3 -g -Wno-abi -std=gnu11 -fsigned-char \
					-Iinclude -DMIR_NO_IO -DMIR_NO_SCAN -DNDEBUG

SRCS = Makefile \
			 $(wildcard src/*.c) $(wildcard src/*.h) $(wildcard include/*.h)
OBJS = mir.o mir-gen.o

libmir.a: $(OBJS)
	$(AR) rcs $@ $^

%.o: src/%.c $(SRCS)
	$(CC) $(CFLAGS) -c $<

clean:
	rm -f mir.o mir-gen.o libmir.a
