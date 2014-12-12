#! /bin/bash 
cd "$(dirname "$0")"

DAY=`date +%d`
MONTH=`date +%m`
YEAR=`date +%Y`
HOUR=`date +%H`
MINUTE=`date +%M`

DIR="PACKAGE-$YEAR-$MONTH$DAY-$HOUR$MINUTE"
if [ ! -d "$DIR" ]; then
        mkdir -p $DIR
fi

mkdir $DIR/model
mkdir $DIR/model/output
mkdir $DIR/model/experiments
mkdir $DIR/model/paramsearch
mkdir $DIR/sp

cp *.lisp $DIR/model/
cp *.R $DIR/model/
cp *.sh $DIR/model/
cp output/*.R $DIR/model/output/
cp experiments/*.R $DIR/model/experiments/
cp paramsearch/*.R $DIR/model/paramsearch/
cp -r data $DIR/model/data

cp ../sp/*.lisp $DIR/sp/
cp ../sp/*.R $DIR/sp/
cp ../sp/*.sh $DIR/sp/
cp -r ../sp/doc $DIR/sp/
cp -r ../sp/TOOLS $DIR/sp/

cp -r ../actr6 $DIR/

rm -r $DIR/actr6/docs
rm -r $DIR/actr6/tutorial
rm -r $DIR/actr6/extras
rm -r $DIR/actr6/examples

zip -r $DIR.zip $DIR
rm -r $DIR

echo Package created in $DIR.zip