# bnfc=$(HOME)/.cabal/bin/bnfc
bnfc=bnfc
example=examples/append.dk
# example=$(HOME)/bin/src/dedukti/examples/append.dk

default :
	$(bnfc) -o BNFC --haskell Dedukti.cf
	make -C BNFC
	BNFC/TestDedukti examples/ho_bug2.dk

#	$(shell for i in examples/*.dk; do BNFC/TestDedukti $$i; done)
# EOF
