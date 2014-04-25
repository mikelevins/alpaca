# Alpaca #

Alpaca was originally a programmable word processor written in Common Lisp. Its reason for existing was that I was working on a novel and I wanted a combination of features offered by WYSIWYG word processors and Emacs, and no existing editor offered me that combination of features.

It's suffered bit rot since 2004; most importantly, the Objective-C interop layer in Clozure Common Lisp changed sufficiently that the old code doesn't build anymore. Several times I've considered updating it, but lately another goal interposed itself.

I've been wanting to write an implementation of Bard that offers the same kind of live, mutable, all-in-one environment as old-fashioned Smalltalk and Lisp environments. I've also been wanting to write an "OS" that can live "in the cloud" and yet be completely private and safe. I was thinking about solving both of those problems by writing a Bard compiler that targets asm.js and therefore runs in browsers. 

Then it occurred to me that if I combined those ideas with a rewrite of Alpaca, I'd have a programmer's editor to use with the Bard compiler, making it easier to eat my own dogwood.

So that's this version of Alpaca: a programmable editor and word-processor written in Bard, where Bard is implemented as an incremental, interactive compiler that targets asm.js.