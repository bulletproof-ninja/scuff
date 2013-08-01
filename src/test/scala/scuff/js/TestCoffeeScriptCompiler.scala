package scuff.js

import org.junit._
import org.junit.Assert._

import CoffeeScriptCompiler._

class TestCoffeeScriptCompiler {
  @Test
  def simple2() {
    val compiler = CoffeeScriptCompiler(Use.Strict)
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

  @Test
  def other() {
    val coffee = """
arr = [3,5,23,67,34]
[foo, bar] = arr
#sqr = (a) -> a*3
boo = sqr 15
"""
    val compiler = CoffeeScriptCompiler('bare -> true)
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
}