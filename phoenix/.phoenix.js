/* globals api Window */

/* Window movement operations */

var pushLeft = function() {
  var win = Window.focusedWindow(),
      frame = win.frame(),
      screen = win.screen().frameWithoutDockOrMenu();

  frame.x = screen.x;
  frame.y = screen.y;

  frame.width = Math.round(screen.width / 2);
  frame.height = screen.height;

  win.setFrame(frame);
};

var pushRight = function() {
  var win = Window.focusedWindow(),
      frame = win.frame(),
      screen = win.screen().frameWithoutDockOrMenu();

  frame.x = Math.round(screen.width/2);
  frame.y = screen.y;

  frame.width = Math.round(screen.width / 2);
  frame.height = screen.height;

  win.setFrame(frame);
};

var fullscreen = function() {
  var win = Window.focusedWindow(),
      frame = win.frame(),
      screenFrame = win.screen().frameWithoutDockOrMenu();

  win.setFrame(screenFrame);
};

var centerWindow = function() {
  var win = Window.focusedWindow(),
      frame = win.frame(),
      screen = win.screen().frameWithoutDockOrMenu();

  frame.x = Math.round(screen.x + screen.width/4);
  frame.y = screen.y;

  frame.width = Math.round(screen.width / 2);
  frame.height = screen.height;

  win.setFrame(frame);
};


/* Music key mappings */
var anyplayer = function(cmd) {
  api.runCommand('~/bin/anyplayer', [cmd]);
};

var playMusic = function() {
  anyplayer('play');
};

var pauseMusic = function() {
  anyplayer('pause');
};

var nextSong = function() {
  anyplayer('next');
}

var prevSong = function() {
  anyplayer('prev');
}

var louder = function() {
  api.runCommand('/usr/bin/osascript', ['-e "set volume output volume (output volume of (get volume settings) + 10)"']);
};

/* Key combinations */
var mash  = ['ctrl','alt','cmd'];

/* Keybindings */
api.bind('left', mash, pushLeft);
api.bind('right', mash, pushRight);
api.bind('c', mash, centerWindow);
api.bind('m', mash, fullscreen);
api.bind('p', ['cmd', 'ctrl'], playMusic);
api.bind('m', ['cmd', 'ctrl'], pauseMusic);
api.bind('l', ['cmd', 'ctrl'], nextSong);
api.bind('h', ['cmd', 'ctrl'], prevSong);
api.bind('k', ['cmd', 'ctrl'], louder);
