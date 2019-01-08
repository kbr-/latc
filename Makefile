latc: bnfc
	stack install --local-bin-path ./

.PHONY: bnfc
bnfc:
	make -C src/bnfc/

.PHONY: clean
clean:
	make -C src/bnfc/ distclean
	rm latc
	rm latc_x86
	stack clean
