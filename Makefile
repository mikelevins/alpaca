# ***********************************************************************
# FILE IDENTIFICATION
#
# Name:          Makefile
# Project:       Alpaca
# Purpose:       the Alpaca Makefile
# Author:        mikel evins
# Copyright:     2009 by mikel evins
#
# ***********************************************************************

# Which Clozure Common Lisp? (edit for your system)
CCL_DIR=/usr/local/src/ccl
LISP=${CCL_DIR}/dx86cl64


APPNAME=Alpaca
BUNDLE=${APPNAME}.app
XCODE=XCode/Alpaca/Alpaca

EXECUTABLE=${APPNAME}.image

all: bundle

clean:
	rm -rf ${BUNDLE}
	find . -name "*.dfsl" -delete
	find . -name "*~" -exec rm -rf {} \;
	find . -name "*.dx??fsl" -delete

tidy:
	find . -name "*.dfsl" -delete
	find . -name "*~" -exec rm -rf {} \;
	find . -name "*.dx??fsl" -delete

bundle: image
	mkdir -p ./${BUNDLE}/Contents/Resources/en.lproj/
	mkdir -p ./${BUNDLE}/Contents/MacOS
#	ibtool ./${XCODE}/en.lproj/MainMenu.xib --compile ./${BUNDLE}/Contents/Resources/en.lproj/MainMenu.nib
	cp ./${XCODE}/en.lproj/Credits.rtf ./${BUNDLE}/Contents/Resources/en.lproj/Credits.rtf
	cp ./${XCODE}/en.lproj/InfoPlist.strings ./${BUNDLE}/Contents/Resources/en.lproj/InfoPlist.strings
	cp ./${XCODE}/Alpaca-Info.plist ./${BUNDLE}/Contents/Info.plist
	cp ./XCode/Alpaca/alpaca.icns ./${BUNDLE}/Contents/Resources/alpaca.icns
	mv ./${EXECUTABLE} ./${BUNDLE}/Contents/MacOS/${APPNAME}
	find . -name "*.dfsl" -delete
	find . -name "*.dx??fsl" -delete

image: 
	${LISP} --batch -l "alpaca.asd" -e "(build-alpaca \"${EXECUTABLE}\")"


