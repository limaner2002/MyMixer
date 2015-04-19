all:
	ghc -o myMixer main.hs -i./ -O2 #-fllvm

clean:
	rm -f *.o *.hi myMixer
