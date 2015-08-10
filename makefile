default : testboard.exe

SOURCES=board-sig.sml board.sml rng-sig.sml rng.sml forwardchain.sml forwardchain-sig.sml phrases.sml ansi.sml excluded.sml power-util.sml power-util-sig.sml

testboard.exe : $(SOURCES) testboard.mlb testboard.sml testboard-main.sml
	mlton -output $@ testboard.mlb

getscore.exe : $(SOURCES) getscore.mlb getscore.sml
	mlton -output $@ getscore.mlb

powerwalk.exe : $(SOURCES) powerwalk.mlb powerwalk.sml
	mlton -output $@ powerwalk.mlb

powerball.exe : $(SOURCES) powerball.mlb powerball.sml
	mlton -output $@ powerball.mlb

driver.exe : $(SOURCES) driver.mlb driver.sml
	mlton -output $@ driver.mlb

bestscores.exe : $(SOURCES) bestscores.mlb bestscores.sml
	mlton -output $@ bestscores.mlb

nj:
	ml-build testboard.cm TestBoard.smlnj_entry nj.img

.PHONY: sayei
sayei:
	mkdir -p bin
	mlton -output bin/sayei
	mv bin/sayei
