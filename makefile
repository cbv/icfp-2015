default : testboard.exe

SOURCES=board-sig.sml board.sml testboard.sml testboard.mlb rng-sig.sml rng.sml forwardchain.sml forwardchain-sig.sml phrases.sml

testboard.exe : $(SOURCES)
	mlton -output $@ testboard.mlb

nj:
	ml-build testboard.cm TestBoard.smlnj_entry nj.img

.PHONY: sayei
sayei:
	mkdir -p bin
	mlton -output bin/sayei
	mv bin/sayei
