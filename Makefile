CC:=aarch64-linux-gnu-gcc
LFLAGS=-O2 -g 
CFLAGS=-I. -O2 -g -Wall
SFLAGS=-I. -O2 -g -Wall
DEPENDFLAGS=-I. -M

C_OBJS=test.o 
ASM_OBJS=sha.o 

OBJS=$(C_OBJS) $(ASM_OBJS)


test: $(OBJS)
	$(CC) $(LFLAGS) -o test $(OBJS)

.s.o:
	$(CC) $(SFLAGS) -c $<

.c.o:
	$(CC) $(CFLAGS) -c $<

clean: 
	rm -rf *.o *~test
depend:
	rm -rf .depend
	$(CC) $(DEPENDFLAGS) $(C_OBJS:.o=.c) > .depend

ifeq (.depend,$(wildcard .depend))
include .depend
endif
