package scuff.js

import org.junit._
import org.junit.Assert._

import CoffeeScriptCompiler._

class TestCoffeeScriptCompiler {
  @Test
  def legacyStrict(): Unit = {
    val compiler = new CoffeeScriptCompiler(Config(version = Version.Legacy, useDirective = Use.Strict))
    val coffee = """
arr = [3,5,23,67,34]
[foo, bar] = arr
sqr = (a) -> a*2
boo = sqr(15)
"""
    val js = compiler.compile(coffee).replaceAll("\\s", "")
    val expected =
      """(function() {
  "use strict";
  var arr, bar, boo, foo, sqr;

  arr = [3, 5, 23, 67, 34];

  foo = arr[0], bar = arr[1];

  sqr = function(a) {
    return a * 2;
  };

  boo = sqr(15);

}).call(this);
""".replaceAll("\\s", "")
    assertEquals(expected, js)
  }

  @Test //@Ignore // Problem with current Iced compiler
  def icedStrict(): Unit = {
    val compiler = new CoffeeScriptCompiler(Config(useDirective = Use.Strict, version = Version.Iced))
    val coffee = """
arr = [3,5,23,67,34]
[foo, bar] = arr
sqr = (a) -> a*2
boo = sqr(15)
await foo defer bar
alert bar
"""
    val js = compiler.compile(coffee).replaceAll("\\s", "")
    val expected = """
(function(){
      "usestrict";
      var arr,bar,boo,foo,sqr,__iced_deferrals,__iced_k,__iced_k_noop,_this = this;
      __iced_k = __iced_k_noop=function(){};

      arr = [3,5,23,67,34];
      foo = arr[0], bar=arr[1];
      sqr = function(a) {
      return a*2;
      };
      boo = sqr(15);
      (function(__iced_k){
      __iced_deferrals = newiced.Deferrals(__iced_k,{});
      foo(__iced_deferrals.defer({
        assign_fn:(function(){
        return function(){
            return bar = arguments[0];
          };
        })(),lineno:6}));
      __iced_deferrals._fulfill();
     })(function(){
        return alert(bar);
     });
}).call(this);
""".replaceAll("\\s", "")
    assertEquals(expected, js)
  }

  @Test
  def legacyOther(): Unit = {
    val coffee = """
arr = [3,5,23,67,34]
[foo, bar] = arr
#sqr = (a) -> a*3
boo = sqr 15
"""
    val compiler = new CoffeeScriptCompiler(Config(version = Version.Legacy, options = Map('bare -> true)))
    val js = compiler.compile(coffee).replaceAll("\\s", "")
    val expected =
      """
var arr, bar, boo, foo;

arr = [3, 5, 23, 67, 34];

foo = arr[0], bar = arr[1];

boo = sqr(15);
""".replaceAll("\\s", "")
    assertEquals(expected, js)
  }

  @Test
  def cs2(): Unit = {
    val coffee = """
# Comment
arr = [3,5,23,67,34]
[foo, bar] = arr
sqr1 = (a) -> a*3
sqr2 = (a) => a*3
boo = sqr 15
"""
    val compiler = new CoffeeScriptCompiler(new Config(options = Map('bare -> true), version = Version.CS2))
    val js = compiler.compile(coffee).replaceAll("\\s", "")
    val expected =
      """
// Comment
var arr, bar, boo, foo, sqr1, sqr2;

arr = [3, 5, 23, 67, 34];

[foo, bar] = arr;

sqr1 = function(a) {
  return a*3;
};

sqr2 = (a) => {
  return a*3;
};

boo = sqr(15);
""".replaceAll("\\s", "")
    assertEquals(expected, js)
  }

}
