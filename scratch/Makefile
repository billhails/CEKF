.PHONY: all clean run

CFILES = $(wildcard *.c)
BINS = $(patsubst %.c,bin/%,$(CFILES))

all: $(BINS)
 
run: all
	for bin in $(BINS); do \
		$$bin; \
	done

bin/%: %.c | bin
	$(CC) $(CFLAGS) -o $@ $< $(LDFLAGS)

clean:
	rm -f $(BINS)

bin:
	mkdir -p bin