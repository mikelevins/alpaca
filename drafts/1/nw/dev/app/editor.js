// ----------------------------------------------------------------------
// globals
// ----------------------------------------------------------------------

var newButton, openButton, saveButton;
var editor;
var menu;
var fileEntry;
var hasWriteAccess;

var gui = require("nw.gui");
var fs = require("fs");
var clipboard = gui.Clipboard.get();

// ----------------------------------------------------------------------
// mode and theme 
// ----------------------------------------------------------------------

function handleDocumentChange(title) {

  var mode = "commonlisp";
  var modeName = "Common Lisp";

  if (title) {
    title = title.match(/[^/]+$/)[0];
    document.getElementById("title").innerHTML = title;
    document.title = title;

    if (title.match(/.apl$/)) {
      mode = {name: "apl", json: true};
      modeName = "APL";
    } else if (title.match(/.c$/)) {
      mode = "clike";
      modeName = "C";
    } else if (title.match(/.cpp$/)) {
      mode = "clike";
      modeName = "C++";
    } else if (title.match(/.cxx$/)) {
      mode = "clike";
      modeName = "C++";
    } else if (title.match(/.h$/)) {
      mode = "clike";
      modeName = "C";
    } else if (title.match(/.m$/)) {
      mode = "clike";
      modeName = "Objective-C";
    } else if (title.match(/.clj$/)) {
      mode = "clojure";
      modeName = "Clojure";
    } else if (title.match(/.coffee$/)) {
      mode = "coffeescript";
      modeName = "coffeescript";
    } else if (title.match(/.lisp$/)) {
      mode = "commonlisp";
      modeName = "Common Lisp";
    } else if (title.match(/.css$/)) {
      mode = "css";
      modeName = "CSS";
    } else if (title.match(/.erl$/)) {
      mode = "erlang";
      modeName = "Erlang";
    } else if (title.match(/.go$/)) {
      mode = "go";
      modeName = "Go";
    } else if (title.match(/.hs$/)) {
      mode = "haskell";
      modeName = "Haskell";
    } else if (title.match(/.html$/)) {
      mode = "htmlmixed";
      modeName = "HTML";
    } else if (title.match(/.json$/)) {
      mode = {name: "javascript", json: true};
      modeName = "JavaScript (JSON)";
    } else if (title.match(/.scm$/)) {
      mode = "scheme";
      modeName = "Scheme";
    }

  } else {
    document.getElementById("title").innerHTML = "[no document]";
  }
  editor.setOption("mode", mode);
  document.getElementById("mode").innerHTML = modeName;
}


// ----------------------------------------------------------------------
// file-handling
// ----------------------------------------------------------------------

function newFile() {
  fileEntry = null;
  hasWriteAccess = false;
  handleDocumentChange(null);
}

function setFile(theFileEntry, isWritable) {
  fileEntry = theFileEntry;
  hasWriteAccess = isWritable;
}

function readFileIntoEditor(theFileEntry) {
  fs.readFile(theFileEntry, function (err, data) {
    if (err) {
      console.log("Read failed: " + err);
    }

    handleDocumentChange(theFileEntry);
    editor.setValue(String(data));
  });
}

function writeEditorToFile(theFileEntry) {
  var str = editor.getValue();
  fs.writeFile(theFileEntry, editor.getValue(), function (err) {
    if (err) {
      console.log("Write failed: " + err);
      return;
    }

    handleDocumentChange(theFileEntry);
    console.log("Write completed.");
  });
}


// ----------------------------------------------------------------------
// event-handling
// ----------------------------------------------------------------------

var onChosenFileToOpen = function(theFileEntry) {
  setFile(theFileEntry, false);
  readFileIntoEditor(theFileEntry);
};

var onChosenFileToSave = function(theFileEntry) {
  setFile(theFileEntry, true);
  writeEditorToFile(theFileEntry);
};

function handleNewFile() {
  if (false) {
    newFile();
    editor.setValue("");
  } else {
    var x = window.screenX + 10;
    var y = window.screenY + 10;
    window.open('untitled.lisp', '_blank', 'screenX=' + x + ',screenY=' + y);
  }
}

function handleOpenFile() {
  $("#openFile").trigger("click");
}

function handleSaveFile() {
  if (fileEntry && hasWriteAccess) {
    writeEditorToFile(fileEntry);
  } else {
    $("#saveFile").trigger("click");
  }
}

// ----------------------------------------------------------------------
// updating views
// ----------------------------------------------------------------------

onresize = function() {
  var container = document.getElementById('editor');
  var containerWidth = container.offsetWidth;
  var containerHeight = container.offsetHeight;

  editor.setSize(containerWidth,containerHeight);

  var scrollerElement = editor.getScrollerElement();
  scrollerElement.style.width = containerWidth + 'px';
  scrollerElement.style.height = containerHeight + 'px';

  editor.refresh();
}

// ----------------------------------------------------------------------
// init
// ----------------------------------------------------------------------

function initMenubar() {
    var menubar = new gui.Menu({type: 'menubar'});

    var fileMenu = new gui.Menu();
    fileMenu.append(new gui.MenuItem({label: 'New', click: handleNewFile}));
    fileMenu.append(new gui.MenuItem({label: 'Open', click: handleOpenFile}));
    fileMenu.append(new gui.MenuItem({label: 'Close'}));
    fileMenu.append(new gui.MenuItem({type: 'separator'}));
    fileMenu.append(new gui.MenuItem({label: 'Save', click: handleSaveFile}));
    fileMenu.append(new gui.MenuItem({label: 'Save As...'}));

    var fileMenuItem = new gui.MenuItem({label: 'File',submenu: fileMenu});

    var viewMenu = new gui.Menu();
    viewMenu.append(new gui.MenuItem({label: 'Buffers'}));

    var viewMenuItem = new gui.MenuItem({label: 'View',submenu: viewMenu});

    gui.Window.get().menu = menubar;
    menubar.insert(fileMenuItem,1);
    menubar.insert(viewMenuItem,3);
}

onload = function() {
    // build the app menu
    initMenubar();

    // hook up event handlers
    $("#saveFile").change(function(evt) {
        onChosenFileToSave($(this).val());
    });
    $("#openFile").change(function(evt) {
        onChosenFileToOpen($(this).val());
    });

    // create the editor
    editor = CodeMirror(
        document.getElementById("editor"),
        {
            mode: {name: "javascript", json: true },
            lineNumbers: true,
            theme: "lesser-dark",
            keyMap: "emacs",
            extraKeys: {
                "Cmd-S": function(instance) { handleSaveButton() },
                "Ctrl-S": function(instance) { handleSaveButton() },
            }
        });

    // create the initial document and show the window
    newFile();
    onresize();

    gui.Window.get().show();
};

