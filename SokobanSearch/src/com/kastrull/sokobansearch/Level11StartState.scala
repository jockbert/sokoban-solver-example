package com.kastrull.sokobansearch

object Level11StartState {

  def apply(): Sokoban = {
    val map = """	
    		|--####-
    		|###-s#-
    		|#-Bbb##
    		|#-b---#
    		|#xb---#
    		|#x#-bx#
    		|#x-x###
    		|#####--""".stripMargin
    Sokoban(map)
  }
}