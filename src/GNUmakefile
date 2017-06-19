SOURCES = $(shell ls *.cpp *.c *.f)

TARGETS = dispRity.so

RCPP_FLAG = $(shell Rscript -e "Rcpp:::CxxFlags()")
export PKG_CPPFLAGS = $(RCPP_FLAG)

all: $(TARGETS)

dispRity.so: $(SOURCES)
    R CMD SHLIB -o $@ $(SOURCES)

clean:
    rm -f *.o *.so

.PHONY: all clean