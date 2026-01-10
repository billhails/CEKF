CFILES = $(wildcard *.c)
BINS = $(patsubst %.c,bin/%,$(CFILES))

all: $(BINS)

bin/%: %.c | bin
	$(CC) $(CFLAGS) -o $@ $< $(LDFLAGS)

bin:
	mkdir -p bin