package com.kastrull.sokobansearch

sealed trait Element
case class Wall() extends Element
case class Empty() extends Element
case class Target() extends Element