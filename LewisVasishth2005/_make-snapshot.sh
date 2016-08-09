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

mkdir $DIR/model
mkdir $DIR/model/output
mkdir $DIR/model/experiments
mkdir $DIR/model/paramsearch
mkdir $DIR/sp
mkdir $DIR/actr6
mkdir $DIR/actr6/other-files

cp *.lisp $DIR/model/
cp *.R $DIR/model/
cp *.sh $DIR/model/
cp -r data $DIR/model/
cp output/*.R $DIR/model/output/
cp experiments/*.R $DIR/model/experiments/
cp paramsearch/*.R $DIR/model/paramsearch/

cp ../sp/*.lisp $DIR/sp/
cp ../sp/*.R $DIR/sp/
cp ../sp/*.sh $DIR/sp/
cp ../actr6/other-files/*.lisp $DIR/actr6/other-files/

echo Snapshot created in $DIR