#!/bin/bash
FILES="tests/good/*.lat
tests/extensions/arrays1/*.lat
tests/extensions/struct/*.lat"

make

for f in $FILES
do
  echo "$f"
  inFile="${f%.lat}.input"
  outFile="${f%.lat}.output"
  execFile="${f%.lat}"
  ./latc_x86_64 "$f"
  if [ -f "$inFile" ]; then
   ./"$execFile" <"$inFile" >tmp.out
  else
    ./"$execFile" >tmp.out
  fi
  diff tmp.out "$outFile"
  rm "$execFile" tmp.out "${f%.lat}.o" "${f%.lat}.s"
done
