#! /bin/bash 
cd "$(dirname "$0")"

DAY=`date +%d`
MONTH=`date +%m`
YEAR=`date +%Y`
HOUR=`date +%H`
MINUTE=`date +%M`

DIR="SNAPSHOTS/$YEAR-$MONTH$DAY-$HOUR$MINUTE"
if [ ! -d "$DIR" ]; then
        mkdir -p $DIR
fi

mkdir $DIR/output
#mkdir $DIR/experiments
#mkdir $DIR/paramsearch
mkdir $DIR/actr6
mkdir $DIR/actr6/other-files

cp *.lisp $DIR/
cp *.R $DIR/
cp output/*.R $DIR/output/
#cp experiments/*.R $DIR/experiments/
#cp paramsearch/*.R $DIR/paramsearch/
cp ../actr6/other-files/*.lisp $DIR/actr6/other-files/

echo Created snapshot in $DIR