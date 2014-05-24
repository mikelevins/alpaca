// CodeMirror init

// configuration
var cmConfig = {
    mode: 'markdown',
    theme: 'twilight',
    lineNumbers: true
};

var theTextArea = document.getElementById('theEditorTextArea');
var theCodeMirror = CodeMirror.fromTextArea(theTextArea,cmConfig);
