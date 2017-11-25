package scuff.js

import org.junit._
import org.junit.Assert._

import CoffeeScriptCompiler._

class TestCoffeeScriptCompiler {
  @Test
  def regularStrict() {
    val compiler = new CoffeeScriptCompiler(Config(useDirective = Use.Strict))
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
  def icedStrict() {
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
  def other() {
    val coffee = """
arr = [3,5,23,67,34]
[foo, bar] = arr
#sqr = (a) -> a*3
boo = sqr 15
"""
    val compiler = new CoffeeScriptCompiler(Config(options = Map('bare -> true)))
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
  def cs2() {
    val coffee = """
arr = [3,5,23,67,34]
[foo, bar] = arr
#sqr = (a) -> a*3
boo = sqr 15
"""
    val compiler = new CoffeeScriptCompiler(new Config(options = Map('bare -> true), version = Version.Coffeescript2))
    val js = compiler.compile(coffee).replaceAll("\\s", "")
    val expected =
      """
var arr, bar, boo, foo;

arr = [3, 5, 23, 67, 34];

[foo, bar] = arr;

//sqr = (a) -> a*3

boo = sqr(15);
""".replaceAll("\\s", "")
    assertEquals(expected, js)
  }

}
