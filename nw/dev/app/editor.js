var newButton, openButton, saveButton;
var editor;
var menu;
var fileEntry;
var hasWriteAccess;

var gui = require("nw.gui");
var fs = require("fs");
var clipboard = gui.Clipboard.get();

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
    } else if (title.match(/.clj$/)) {
      mode = "clojure";
      modeName = "Clojure";
    } else if (title.match(/.lisp$/)) {
      mode = "commonlisp";
      modeName = "Common Lisp";
    } else if (title.match(/.css$/)) {
      mode = "css";
      modeName = "CSS";
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
    document.getElementById("title").innerHTML = "[no document loaded]";
  }
  editor.setOption("mode", mode);
  document.getElementById("mode").innerHTML = modeName;

  var ed = document.getElementById('editor');

  document.getElementById("mode_width").innerHTML = ed.offsetWidth;
  document.getElementById("mode_height").innerHTML = ed.offsetHeight;
}

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

var onChosenFileToOpen = function(theFileEntry) {
  setFile(theFileEntry, false);
  readFileIntoEditor(theFileEntry);
};

var onChosenFileToSave = function(theFileEntry) {
  setFile(theFileEntry, true);
  writeEditorToFile(theFileEntry);
};

function handleCloseButton() {
    window.close();
}

function handleMinButton() {
}

function handleMaxButton() {
}

function initMenubar() {
    var menubar = new gui.Menu({type: 'menubar'});

    var fileMenu = new gui.Menu();
    fileMenu.append(new gui.MenuItem({label: 'New'}));
    fileMenu.append(new gui.MenuItem({label: 'Open'}));
    fileMenu.append(new gui.MenuItem({label: 'Close'}));
    fileMenu.append(new gui.MenuItem({type: 'separator'}));
    fileMenu.append(new gui.MenuItem({label: 'Save'}));
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
  initMenubar();

  closeButton = document.getElementById("closeButton");
  minButton = document.getElementById("minButton");
  maxButton = document.getElementById("maxButton");

  closeButton.addEventListener("click", handleCloseButton);
  minButton.addEventListener("click", handleMinButton);
  maxButton.addEventListener("click", handleMaxButton);

  $("#saveFile").change(function(evt) {
    onChosenFileToSave($(this).val());
  });
  $("#openFile").change(function(evt) {
    onChosenFileToOpen($(this).val());
  });

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

  newFile();
  onresize();

  gui.Window.get().show();
};

onresize = function() {
  var container = document.getElementById('editor');
  var containerWidth = container.offsetWidth;
  var containerHeight = container.offsetHeight;

  editor.setSize(containerWidth,containerHeight);

  var scrollerElement = editor.getScrollerElement();
  scrollerElement.style.width = containerWidth + 'px';
  scrollerElement.style.height = containerHeight + 'px';


  document.getElementById("mode_width").innerHTML = container.offsetWidth;
  document.getElementById("mode_height").innerHTML = container.offsetHeight;

  editor.refresh();
}
