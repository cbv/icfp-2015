default : testboard.exe

testboard.exe : board-sig.sml board.sml testboard.sml testboard.mlb
	mlton -output $@ testboard.mlb
