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

var hideCurrentApp = slate.operation('hide', {
  'app': ['current']
});

/* Volume ond media pause/play perations */

var volUp = slate.operation('shell', {
  'command': '~/bin/vol --up',
  'wait': true
});

var volDown = slate.operation('shell', {
  'command': '~/bin/vol --down',
  'wait': true
});

/* Media play, pause, next operations with Anyplayer:
*  https://github.com/sunny/anyplayer
*/

var playNext = slate.operation('shell', {
  'command': '~/bin/vol --next',
  'wait': true
});

var playPrev = slate.operation('shell', {
  'command': '~/bin/vol --prev',
  'wait': true
});

var playMusic = slate.operation('shell', {
  'command': '~/bin/vol --play',
  'wait': true
});

var pauseMusic = slate.operation('shell', {
  'command': '~/bin/vol --pause',
  'wait': true
});

var darkenDisplay = slate.operation('shell', {
  'command': '~/bin/vol --darker',
  'wait': true
});

var brightenDisplay = slate.operation('shell', {
  'command': '~/bin/vol --brighter',
  'wait': true
});

/* Key Bindings */

slate.bindAll({
  'left:cmd,ctrl,alt': function(win) { win.doOperation(pushLeft); },
  'right:cmd,ctrl,alt': function(win) { win.doOperation(pushRight); },
  'c:cmd,ctrl,alt': function(win) { win.doOperation(center); },
  'm:cmd,ctrl,alt': function(win) { win.doOperation(fullscreen); },
  'h:cmd': function(win) { win.doOperation(hideCurrentApp); }, // fixes Chrome bug
  'j:cmd,ctrl': volDown,
  'k:cmd,ctrl': volUp,
  'l:cmd,ctrl': playNext,
  'h:cmd,ctrl': playPrev,
  'm:cmd,ctrl': pauseMusic,
  'p:cmd,ctrl': playMusic,
  'r:alt': slate.operation('relaunch'),
  '1:cmd,ctrl': darkenDisplay,
  '2:cmd,ctrl': brightenDisplay
});


