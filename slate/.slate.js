/* global slate */

/* Window movement operations */

var pushRight = slate.operation('push', {
  'direction': 'right',
  'style': 'bar-resize:screenSizeX/2'
});

var pushLeft = slate.operation('push', {
  'direction': 'left',
  'style': 'bar-resize:screenSizeX/2'
});

var pushTop = slate.operation('push', {
  'direction': 'top',
  'style': 'bar-resize:screenSizeY/2'
});

var fullscreen = slate.operation('move', {
  'x': 'screenOriginX',
  'y': 'screenOriginY',
  'width': 'screenSizeX',
  'height': 'screenSizeY'
});

var center = slate.operation('move', {
  'x': 'screenOriginX+screenSizeX/4',
  'y': 'screenOriginY',
  'width': 'screenSizeX/2',
  'height': 'screenSizeY'
});

/* Key Bindings */

// Resize window to left half of screen
slate.bind('left:cmd,ctrl,alt', function(win) {
  win.doOperation(pushLeft);
});

// Resize window to right half of screen
slate.bind('right:cmd,ctrl,alt', function(win) {
  win.doOperation(pushRight);
});

// Make window half screen width, center on screen
slate.bind('c:cmd,ctrl,alt', function(win) {
  win.doOperation(center);
});

// Fullscreen window (maximize)
slate.bind('m:cmd,ctrl,alt', function(win) {
  win.doOperation(fullscreen);
});

// Hide current window (fixes broken Chrome keybinding?)
slate.bind('h:cmd', function(win) {
  win.doOperation(slate.operation('hide', {
    'app': ['current']
  }));
});

// Focus Bindings
// NOTE: some of these may *not* work if you have not removed the expose/spaces/mission control bindings
slate.bindAll({
  'l:cmd': slate.op('focus', { 'direction': 'right' }),
  'h:cmd': slate.op('focus', { 'direction': 'left' }),
  'k:cmd': slate.op('focus', { 'direction': 'up' }),
  'j:cmd': slate.op('focus', { 'direction': 'down' })
});

// relaunch slate to reload config
slate.bind('r:cmd,ctrl,shift', slate.operation('relaunch'));

// Experimental Slate app switcher:
//var switcher = slate.operation('switch');
//slate.bind('tab:cmd', switcher);