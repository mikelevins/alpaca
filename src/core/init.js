// CodeMirror init
var cmConfig = {
mode: 'markdown',
theme: 'twilight'
};
var theTextArea = document.getElementById('theEditorTextArea');
var theCodeMirror = CodeMirror.fromTextArea(theTextArea,cmConfig);
