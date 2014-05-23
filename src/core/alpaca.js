// menu
var gui = require('nw.gui');
var menu = new gui.Menu();

menu.append(new gui.MenuItem({
    label: 'Alpaca Test',
    click: function () {alert('chose Alpaca Test')}}));

var menubar = new gui.Menu({type: 'menubar'});
menubar.append(new gui.MenuItem({label: 'Testing', submenu: menu}));

var win = gui.Window.get();
win.menu = menubar;
