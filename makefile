default : testboard.exe

testboard.exe : board-sig.sml board.sml testboard.sml testboard.mlb rng-sig.sml rng.sml forwardchain.sml forwardchain-sig.sml phrases.sml
	mlton -output $@ testboard.mlb

.PHONY: sayei
sayei:
	mkdir -p bin
	mlton -output bin/sayei
	mv bin/sayei 
