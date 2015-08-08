default : testboard.exe

SOURCES=board-sig.sml board.sml rng-sig.sml rng.sml forwardchain.sml forwardchain-sig.sml phrases.sml ansi.sml

testboard.exe : $(SOURCES) testboard.mlb testboard.sml testboard-main.sml
	mlton -output $@ testboard.mlb

getscore.exe : $(SOURCES) getscore.mlb getscore.sml
	mlton -output $@ getscore.mlb

nj:
	ml-build testboard.cm TestBoard.smlnj_entry nj.img

.PHONY: sayei
sayei:
	mkdir -p bin
	mlton -output bin/sayei
	mv bin/sayei
