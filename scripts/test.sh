#!/bin/bash
tests=$(find examples/model examples/atom -name '*.hs' -type f -exec grep -l '^main' {} +)

inputs=12
ret=0

if command -v stack; then
	build_hs="stack ghc --"
	run_comp="stack run forsyde-compiler-exe --"
else
	build_hs="cabal exec ghc --"
	run_comp="cabal exec forsyde-compiler-exe --"
fi

while [ -n "$1" ]; do
	case "$1" in
		"--stack")
			build_hs="stack ghc --"
			run_comp="stack run forsyde-compiler-exe --"
			shift
			;;
		"--cabal")
			build_hs="cabal exec ghc --"
			run_comp="cabal exec forsyde-compiler-exe --"
			shift
			;;
	esac
done

for t in $tests; do
	exe=${t%.hs}
	name=$(basename $exe)

	output_c=$(mktemp)
	output_hs=$(mktemp)

	$build_hs -main-is $name $t
	$run_comp $t
	gcc -I examples/implementation/platform_independent main.c

	echo "Running $name:"
	input=$( ( for i in $(seq $inputs); do echo -n " $(( RANDOM % 100 ))"; done; ) )
	echo "Input:$input"
	echo "$input" | ./$exe > "$output_hs"
	echo "$input" | ./a.out | { tr '\n' ' '; echo; } | sed -e 's/  */ /g' -e 's/ *$//' > "$output_c"

	if ! diff -Naur "$output_hs" "$output_c"; then
		echo "$name: FAILED"
		ret=1
	else
		echo "Output: $(cat $output_hs)"
		echo "$name: OK"
	fi
	rm $exe
	rm -f "$output_c"
	rm -f "$output_hs"
	echo
done
exit $ret
