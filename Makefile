all: latc

latc: bnfc
	stack install latte:exe:latc --local-bin-path ./

.PHONY: bnfc
bnfc:
	make -C src/bnfc/

.PHONY: clean
clean:
	make -C src/bnfc/ distclean
	rm latc
