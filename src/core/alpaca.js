// alpaca module

var ALPACA = (function (){
    var alpaca = {};

    // -------------------------
    // app object
    // -------------------------
    
    alpaca.gui = require('nw.gui');
    alpaca.win = alpaca.gui.Window.get();

    // -------------------------
    // menubar
    // -------------------------

    alpaca.menubar = new alpaca.gui.Menu({ type: 'menubar' });

    // -------------------------
    // File menu
    // -------------------------

    alpaca.fileMenu = new alpaca.gui.Menu();
    alpaca.fileMenu.append(new alpaca.gui.MenuItem({
        label: 'New',
        click: function () {alert('New file')}}));
    alpaca.fileMenu.append(new alpaca.gui.MenuItem({
        label: 'Open...',
        click: function () {alert('Open a file')}}));
    alpaca.fileMenu.append(new alpaca.gui.MenuItem({
        label: 'Directory',
        click: function () {alert('Dired')}}));
    alpaca.fileMenu.append(new alpaca.gui.MenuItem({
        label: 'Close',
        click: function () {alert('Close the file')}}));
    alpaca.fileMenu.append(new alpaca.gui.MenuItem({
        label: 'Save',
        click: function () {alert('Save the file')}}));
    alpaca.fileMenu.append(new alpaca.gui.MenuItem({
        label: 'Save As...',
        click: function () {alert('Save the file as...')}}));

    
    // -------------------------
    // Help menu
    // -------------------------

    alpaca.helpMenu = new alpaca.gui.Menu();

    // -------------------------
    // menubar setup
    // -------------------------

    alpaca.win.menu = alpaca.menubar;
    alpaca.win.menu.insert(new alpaca.gui.MenuItem({ label: 'File', submenu: alpaca.fileMenu}), 1);
    alpaca.win.menu.append(new alpaca.gui.MenuItem({ label: 'Help', submenu: alpaca.helpMenu}));

    return alpaca;
})();
