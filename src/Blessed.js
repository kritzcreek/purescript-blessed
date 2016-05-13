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

exports.clearItems = function(list) {
  return function(){
    return list.clearItems();
  };
};

exports.setItems = function(list) {
  return function(items){
    return function(){
      return list.setItems(items);
    };
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

exports.onSelect = function(list) {
  return function(cb){
    return function(){
      return list.on("select", function(_, index){
        cb(index)();
      });
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

exports.remove = function(el1) {
  return function(el2) {
    return function(){
      return el1.remove(el2);
    };
  };
};

exports.clearChildren = function(el1) {
  return function(){
    return el1.children = [];
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

exports.setContent = function(el) {
  return function(s){
    return function(){
      return el.setContent(s);
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
