#! /usr/bin/env bash

set -e
set -u
set -o pipefail

while [[ $# -gt 0 ]]; do
  case $1 in 
    --day)
      DAY_NUMBER="$2"
      shift # past argument
      shift # past value
      ;;
    *)
      echo "unknown argument $1"
      exit 1
      ;;
  esac
done

if [ -z "${DAY_NUMBER+x}" ]; then
  echo "please supply the day"
  echo "  ex: new-day.bash --day 25"
  exit 1
fi

if [ -d "./day${DAY_NUMBER}" ]; then
  echo "there is already a directory called day${DAY_NUMBER}"
  exit 1
fi

cp -r "./template" "./day${DAY_NUMBER}"

# apply template dayX.cabal
mv "./day${DAY_NUMBER}/dayX.cabal" "./day${DAY_NUMBER}/day${DAY_NUMBER}.cabal"
sed -i -e "s/\([dD]\)ayX/\1ay${DAY_NUMBER}/g" "./day${DAY_NUMBER}/day${DAY_NUMBER}.cabal"

# apply template DayX.hs
mv "./day${DAY_NUMBER}/DayX.hs" "./day${DAY_NUMBER}/Day${DAY_NUMBER}.hs"
sed -i -e "s/\([dD]\)ayX/\1ay${DAY_NUMBER}/g" "./day${DAY_NUMBER}/Day${DAY_NUMBER}.hs"

