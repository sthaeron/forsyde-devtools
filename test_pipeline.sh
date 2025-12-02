#!/usr/bin/env bash
set -e

nix run .#forsyde-compiler -- examples/model/SDF_example_008.hs -o ./examples/implementation/platform_independent/test_008.c --output-c
gcc examples/implementation/platform_independent/test_008.c -o test_008

expected_output=$(cat <<EOF
2
6
12
20
EOF
)
actual_output=$(printf "1 1\n2 2\n3 3\n4 4\n" | ./test_008)

if [ "$expected_output" != "$actual_output" ]; then
    echo "FAIL: output mismatch"
    echo "--- expected ---"
    echo "$expected_output"
    echo "--- actual ---"
    echo "$actual_output"
    exit 1
fi

echo "OK"

rm -f examples/implementation/platform_independent/test_008.c
rm -f test_008