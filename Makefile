default: bundle

bundle: zip
	mkdir -p Alpaca.app/Contents/MacOS
	mkdir -p Alpaca.app/Contents/Resources
	cp nw/node-webkit.app/Contents/MacOS/node-webkit Alpaca.app/Contents/MacOS/node-webkit
	cp -R /Users/mikel/Workshop/alpaca/nw/node-webkit.app/Contents/Frameworks Alpaca.app/Contents/Frameworks
	cp assets/bundle_resources/Alpaca-Info.plist Alpaca.app/Contents/Info.plist
	cp -R assets/bundle_resources/Alpaca.icns Alpaca.app/Contents/Resources/Alpaca.icns
	mv src/alpaca.nw Alpaca.app/Contents/Resources/app.nw

zip:
	cd src/; zip -r alpaca.nw *

clean:
	rm -rf Alpaca.app
	rm -f src/alpaca.nw
