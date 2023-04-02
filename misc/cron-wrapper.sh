#!/bin/bash
set -eou pipefail

# Modify to suit
F2I=/home/bink/.cabal/bin/fronius-to-influx

# Look for *.inverter and *.powerflow files one level deep, process them with F2I
find . -maxdepth 2 -type f \( -name \*.inverter -or -name \*.powerflow \) -print0 | \
  xargs -r0 -- "${F2I}" --influx-protocol=http --processed processed/

# Look for folders inside of processed/ that haven't been touched for 1,2,3 days
# and compress them.
cd processed/
find . -maxdepth 1 -mindepth 1 -type d \( -mtime 1 -or -mtime 2 -or -mtime 3 \) -print0 | while read -d '' f; do
  if [[ $f =~ processed ]] || [[ $f =~ .*.tar.xz ]] || [ -r $f.tar.xz ] || [ ! -d $f ]; then
    continue
  fi
  tar -cJf $f.tar.xz $f && rm -rf $f
done