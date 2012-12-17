package scuff.js

import org.junit._
import org.junit.Assert._

import scuff.js._

class LESSCompilerTest {
  @Test
  def foo {
    val compiler = LESSCompiler()
    println(compiler.compile(".class { width: 1 + 1 }"))
  }
  @Test
  def bar {
    val compiler = LESSCompiler()
    val LESS = """
#header {
    color: black;
    
    .navigation {
        font-size: 12px;
    }
    .logo {
        width: 300px;
        &:hover { text-decoration: none }
    }
}
      """
    println(compiler.compile(LESS))
  }
}