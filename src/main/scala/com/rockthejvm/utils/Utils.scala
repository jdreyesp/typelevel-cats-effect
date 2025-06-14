package com.rockthejvm.utils

import cats.effect.IO

extension [A](io: IO[A]) {
  def debugIO: IO[A] = for {
    a <- io
    t = Thread.currentThread().getName()
    _ = println(s"[$t] $a")
  } yield a
}
