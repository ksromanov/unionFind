CLM=env CLEANLIB=/usr/lib64/clean/exe/ clm -lat -lset -I /usr/lib64/clean/StdEnv/ 

all: quickFind quickUnion quickWeightedUnion

quickFind: quickFind.icl
	$(CLM) quickFind -o quickFind

quickUnion: quickUnion.icl
	$(CLM) quickUnion -o quickUnion

quickWeightedUnion: quickWeightedUnion.icl
	$(CLM) quickWeightedUnion -o quickWeightedUnion

clean:
	rm -f quickFind quickUnion quickWeightedUnion
	rm -rf "Clean System Files"
