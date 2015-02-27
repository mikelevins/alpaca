
default:
	rm -rf ./Alpaca.app
	ccl64 -l alpaca.asd -e "(build-alpaca)"

clean:
	rm -rf ./Alpaca.app
