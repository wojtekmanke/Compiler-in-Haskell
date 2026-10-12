#!/usr/bin/env bash
#
# Runs the test suite, or only the given tests:
#
#   tests/run.sh [FILE.lat ...]
#
# A program in a directory named "bad" has to be rejected by the compiler; if
# its first line is a comment of the form "// error: TEXT", the error message
# has to contain TEXT. Any other program has to compile and, reading FILE.input
# if there is one, print exactly the contents of FILE.output. Its exit status
# has to be the number in FILE.status, or 0 if there is no such file.

set -u

root=$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)
work=$(mktemp -d)
trap 'rm -rf "$work"' EXIT

passed=0
failed=0

fail() {
    printf 'FAIL %s: %s\n' "$1" "$2"
    failed=$((failed + 1))
}

# Compiles a copy of the program, so that nothing is written next to the test.
compile() {
    rm -f "$work"/test*
    cp "$1" "$work/test.lat"
    "$root/latc_x86_64" "$work/test.lat" > /dev/null 2> "$work/stderr"
}

check_good() {
    local source=$1 input=${1%.lat}.input status expected=0
    if ! compile "$source"; then
        fail "$source" "does not compile: $(sed -n 2p "$work/stderr")"
        return
    fi
    [[ -f $input ]] || input=/dev/null
    [[ -f ${source%.lat}.status ]] && expected=$(< "${source%.lat}.status")
    # The braces keep the shell's own report of a crash out of the output.
    { timeout 10 "$work/test" < "$input" > "$work/stdout"; } 2> /dev/null
    status=$?
    if [[ $status -eq 124 ]]; then
        fail "$source" "timed out"
    elif [[ $status -ne $expected ]]; then
        fail "$source" "exit status $status instead of $expected"
    elif ! cmp -s "$work/stdout" "${source%.lat}.output"; then
        fail "$source" "wrong output"
    else
        passed=$((passed + 1))
    fi
}

check_bad() {
    local source=$1 status expected
    compile "$source"
    status=$?
    expected=$(sed -n '1s|^// error: ||p' "$source")
    if [[ $status -eq 0 ]]; then
        fail "$source" "is accepted"
    elif [[ $status -ne 1 || $(head -n 1 "$work/stderr") != ERROR ]] \
            || ! grep -q ': error: ' "$work/stderr"; then
        fail "$source" "is rejected without a proper error message"
    elif ! grep -qF -- "$expected" "$work/stderr"; then
        fail "$source" "wrong error: $(sed -n 2p "$work/stderr")"
    else
        passed=$((passed + 1))
    fi
}

if [[ $# -gt 0 ]]; then
    tests=("$@")
else
    cd "$root" || exit 1
    tests=(tests/good/*.lat tests/extensions/arrays1/*.lat tests/extensions/struct/*.lat tests/bad/*.lat)
fi

for source in "${tests[@]}"; do
    # The kind of a test follows from the directory it is in, however the
    # path to it was written.
    case $(cd "$(dirname "$source")" && pwd) in
        */bad) check_bad "$source" ;;
        *)     check_good "$source" ;;
    esac
done

echo "$passed passed, $failed failed"
[[ $failed -eq 0 ]]
