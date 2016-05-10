//module Blessed
var blessed = require('blessed');

exports.screen = function(options) {
  return function(){
    return blessed.screen(options);
  };
};

exports.formImpl = function(options) {
  return function(){
    return blessed.form(options);
  };
};

exports.textboxImpl = function(options) {
  return function(){
    return blessed.textbox(options);
  };
};

exports.textImpl = function(options) {
  return function(){
    return blessed.text(options);
  };
};

exports.listImpl = function(options) {
  return function(){
    return blessed.list(options);
  };
};

exports.render = function(screen) {
  return function(){
    return screen.render();
  };
};

exports.key = function(screen) {
  return function(key){
    return function(cb){
      return function(){
        return screen.key(key, cb);
      };
    };
  };
};

exports.append = function(el1) {
  return function (el2){
    return function(){
      return el1.append(el2);
    };
  };
};

exports.hide = function(el) {
  return function(){
    return el.hide();
  };
};

exports.show = function(el) {
  return function(){
    return el.show();
  };
};

exports.focus = function(el) {
  return function(){
    return el.focus();
  };
};

exports.clearValue = function(el) {
  return function(){
    return el.clearValue();
  };
};

exports.setValue = function(el) {
  return function(s){
    return function(){
      return el.setValue(s);
    };
  };
};

exports.readInput = function(el) {
  return function(cb){
    return function(){
      el.focus();
      return el.readInput(function(){
        cb(el.value)();
      });
    };
  };
};
// // Create a screen object.
// var screen = blessed.screen({
//   smartCSR: true
// });


// screen.title = 'PS blessed';

// var logger = blessed.textbox({
//   parent: screen,
//   top: 1,
//   height: 2,
//   style: {
//     fg: 'red',
//     bg: 'blue'
//   }
// });

// var log = function(x) {
//   logger.setValue(x);
// };

// var form = blessed.form({
//   parent: screen,
//   name: 'form',
//   bottom: 1,
//   left: 0,
//   width: '100%',
//   height: 1,
//   label: 'Pursuit'
// });

// var input = blessed.textbox({
//   parent: form,
//   width: '100%',
//   top: '100%',
//   height: 1,
//   left: 0,
//   tags: true,
//   style: {
//     fg: 'black',
//     bg: 'grey'
//   }
// });

// form.hide();

// // Quit on Escape, q, or Control-C.
// screen.key('q', function(ch, key) {
//   return process.exit(0);
// });

// screen.key('p', function() {
//   form.show();
//   input.focus();
//   screen.render();
//   input.readInput(function(){
//     var x = input.value;
//     log("WAOW" + x);
//     input.clearValue();
//     form.hide();
//     screen.render();
//   });
// });

// // Render the screen.
// screen.render();
