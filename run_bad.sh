FILES="tests/bad/*.lat
tests/typecheck/bad/*.lat"

make
for f in $FILES
do
  echo "$f"
  ./generate_asm $f
done
