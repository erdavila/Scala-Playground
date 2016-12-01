package forcomprehensions

import scala.annotation.tailrec
import scala.Console

object Test extends App {

  test(
    for {
      a <- monadFor("a")()
    } yield expression(a)
    ,
    monadFor("a")() map { a => expression(a) }
    ,
    "monadFor(\"a\")() map { a => expression(a) }"
  )

  test(
    for {
      Pattern(a) <- monadFor("a")()
    } yield expression(a)
    ,
    monadFor("a")()
      .withFilter {
        case Pattern(a) => true
        case _ => false
      }
      .map { case Pattern(a) => expression(a) }
    ,
    "monadFor(\"a\")() withFilter { case Pattern(a) => true case _ => false } map { case Pattern(a) => expression(a) }"
  )

  test(
    for {
      a <- monadFor("a")()
      b <- monadFor("b")(a)
    }  yield expression(a, b)
    ,
    monadFor("a")()
      .flatMap { a =>
        monadFor("b")(a) map { b => expression(a, b) }
      }
    ,
    "monadFor(\"a\")() flatMap { a => monadFor(\"b\")(a) map { b => expression(a, b) } }"
  )

  test(
    for {
      a <- monadFor("a")()
      Pattern(b) <- monadFor("b")(a)
    }  yield expression(a, b)
    ,
    monadFor("a")()
      .flatMap { a =>
        monadFor("b")(a)
          .withFilter {
            case Pattern(b) => true
            case _ => false
          }
          .map { case Pattern(b) => expression(a, b) }
      }
    ,
    "monadFor(\"a\")() flatMap { a => monadFor(\"b\")(a) withFilter { case Pattern(b) => true case _ => false } map { case Pattern(b) => expression(a, b) } }"
  )

  test(
    for {
      a <- monadFor("a")()
      if condition(a)
    } yield expression(a)
    ,
    monadFor("a")()
      .withFilter { a => condition(a) }
      .map { a => expression(a) }
    ,
    "monadFor(\"a\")() withFilter { a => condition(a) } map { a => expression(a) }"
  )

  test(
    for {
      a <- monadFor("a")()
      b = valueFor("b")(a)
    } yield expression(a, b)
    ,
    monadFor("a")()
      .map { a =>
        val b = valueFor("b")(a)
        (a, b)
      }
      .map { case (a, b) => expression(a, b) }
    ,
    "monadFor(\"a\")() map { a => val b = valueFor(\"b\")(a); (a, b) } map { case (a, b) => expression(a, b) }"
  )

  test(
    for {
      Pattern(a) <- monadFor("a")()
      b <- monadFor("b")(a)
    } yield expression(a, b)
    ,
    monadFor("a")()
      .withFilter {
        case Pattern(a) => true
        case _ => false
      }
      .flatMap { case Pattern(a) =>
        monadFor("b")(a) map { b => expression(a, b) }
      }
    ,
    "monadFor(\"a\")() withFilter { case Pattern(a) => true case _ => false } flatMap { case Pattern(a) => monadFor(\"b\")(a) map { b => expression(a, b) } }"
  )

  test(
    for {
      Pattern(a) <- monadFor("a")()
      Pattern(b) <- monadFor("b")(a)
    } yield expression(a, b)
    ,
    monadFor("a")()
      .withFilter {
        case Pattern(a) => true
        case _ => false
      }
      .flatMap { case Pattern(a) =>
        monadFor("b")(a)
          .withFilter {
            case Pattern(b) => true
            case _ => false
          }
          .map { case Pattern(b) => expression(a, b) }
      }
    ,
    "monadFor(\"a\")() withFilter { case Pattern(a) => true case _ => false } flatMap { case Pattern(a) => monadFor(\"b\")(a) withFilter { case Pattern(b) => true case _ => false } map { case Pattern(b) => expression(a, b) } }"
  )

  test(
    for {
      Pattern(a) <- monadFor("a")()
      if condition(a)
    } yield expression(a)
    ,
    monadFor("a")()
      .withFilter {
        case Pattern(a) => true
        case _ => false
      }
      .withFilter { case Pattern(a) => condition(a) }
      .map { case Pattern(a) => expression(a) }
    ,
    "monadFor(\"a\")() withFilter { case Pattern(a) => true case _ => false } withFilter { case Pattern(a) => condition(a) } map { case Pattern(a) => expression(a) }"
  )

  test(
    for {
      Pattern(a) <- monadFor("a")()
      b = valueFor("b")(a)
    } yield expression(a, b)
    ,
    monadFor("a")()
      .withFilter {
        case Pattern(a) => true
        case _ => false
      }
      .map { case Pattern(a) =>
        val b = valueFor("b")(a)
        (Pattern(a), b)
      }
      .map { case (Pattern(a), b) => expression(a, b) }
    ,
    "monadFor(\"a\")() withFilter { case Pattern(a) => true case _ => false } map { case Pattern(a) => val b = valueFor(\"b\")(a); (Pattern(a), b) } map { case (Pattern(a), b) => expression(a, b) }"
  )

  test(
    for {
      a <- monadFor("a")()
      b <- monadFor("b")(a)
      c <- monadFor("c")(a, b)
    } yield expression(a, b, c)
    ,
    monadFor("a")()
      .flatMap { a =>
        monadFor("b")(a)
          .flatMap { b =>
            monadFor("c")(a, b) map { c => expression(a, b, c) }
          }
      }
    ,
    "monadFor(\"a\")() flatMap { a => monadFor(\"b\")(a) flatMap { b => monadFor(\"c\")(a, b) map { c => expression(a, b, c) } } }"
  )

  test(
    for {
      a <- monadFor("a")()
      b <- monadFor("b")(a)
      Pattern(c) <- monadFor("c")(a, b)
    } yield expression(a, b, c)
    ,
    monadFor("a")()
      .flatMap { a =>
        monadFor("b")(a)
          .flatMap { b =>
            monadFor("c")(a, b)
              .withFilter {
                case Pattern(c) => true
                case _ => false
              }
              .map { case Pattern(c) => expression(a, b, c) }
          }
      }
    ,
    "monadFor(\"a\")() flatMap { a => monadFor(\"b\")(a) flatMap { b => monadFor(\"c\")(a, b) withFilter { case Pattern(c) => true case _ => false } map { case Pattern(c) => expression(a, b, c) } } }"
  )

  test(
    for {
      a <- monadFor("a")()
      b <- monadFor("b")(a)
      if condition(a)
    } yield expression(a, b)
    ,
    monadFor("a")()
      .flatMap { a =>
        monadFor("b")(a)
          .withFilter { b => condition(a) }
          .map { b => expression(a, b) }
      }
    ,
    "monadFor(\"a\")() flatMap { a => monadFor(\"b\")(a) withFilter { b => condition(a) } map { b => expression(a, b) } }"
  )

  test(
    for {
      a <- monadFor("a")()
      b <- monadFor("b")(a)
      c = valueFor("c")(a, b)
    } yield expression(a, b, c)
    ,
    monadFor("a")()
      .flatMap { a =>
        monadFor("b")(a)
          .map { b =>
            val c = valueFor("c")(a, b)
            (b, c)
          }
          .map { case (b, c) => expression(a, b, c) }
      }
    ,
    "monadFor(\"a\")() flatMap { a => monadFor(\"b\")(a) map { b => val c = valueFor(\"c\")(a, b); (b, c) } map { case (b, c) => expression(a, b, c) } }"
  )

  test(
    for {
      a <- monadFor("a")()
      Pattern(b) <- monadFor("b")(a)
      c <- monadFor("c")(a, b)
    } yield expression(a, b, c)
    ,
    monadFor("a")()
      .flatMap { a =>
        monadFor("b")(a)
          .withFilter {
            case Pattern(b) => true
            case _ => false
          }
          .flatMap { case Pattern(b) =>
            monadFor("c")(a, b) map { c => expression(a, b, c) }
          }

      }
    ,
    "monadFor(\"a\")() flatMap { a => monadFor(\"b\")(a) withFilter { case Pattern(b) => true case _ => false } flatMap { case Pattern(b) => monadFor(\"c\")(a, b) map { c => expression(a, b, c) } } }"
  )

  test(
    for {
      a <- monadFor("a")()
      Pattern(b) <- monadFor("b")(a)
      Pattern(c) <- monadFor("c")(a, b)
    } yield expression(a, b, c)
    ,
    monadFor("a")()
      .flatMap { a =>
        monadFor("b")(a)
          .withFilter {
            case Pattern(b) => true
            case _ => false
          }
          .flatMap { case Pattern(b) =>
            monadFor("c")(a, b)
              .withFilter {
                case Pattern(c) => true
                case _ => false
              }
              .map { case Pattern(c) => expression(a, b, c) }
          }
      }
    ,
    "monadFor(\"a\")() flatMap { a => monadFor(\"b\")(a) withFilter { case Pattern(b) => true case _ => false } flatMap { case Pattern(b) => monadFor(\"c\")(a, b) withFilter { case Pattern(c) => true case _ => false } map { case Pattern(c) => expression(a, b, c) } } }"
  )

  test(
    for {
      a <- monadFor("a")()
      Pattern(b) <- monadFor("b")(a)
      if condition(a, b)
    } yield expression(a, b)
    ,
    monadFor("a")()
      .flatMap { a =>
        monadFor("b")(a)
          .withFilter {
            case Pattern(b) => true
            case _ => false
          }
          .withFilter { case Pattern(b) => condition(a, b) }
          .map { case Pattern(b) => expression(a, b) }
      }
    ,
    "monadFor(\"a\")() flatMap { a => monadFor(\"b\")(a) withFilter { case Pattern(b) => true case _ => false } withFilter { case Pattern(b) => condition(a, b) } map { case Pattern(b) => expression(a, b) } }"
  )

  test(
    for {
      a <- monadFor("a")()
      Pattern(b) <- monadFor("b")(a)
      c = valueFor("c")(a, b)
    } yield expression(a, b, c)
    ,
    monadFor("a")()
      .flatMap { a =>
        monadFor("b")(a)
          .withFilter {
            case Pattern(b) => true
            case _ => false
          }
          .map { case Pattern(b) =>
            val c = valueFor("c")(a, b)
            (Pattern(b), c)
          }
          .map { case (Pattern(b), c) => expression(a, b, c) }

      }
    ,
    "monadFor(\"a\")() flatMap { a => monadFor(\"b\")(a) withFilter { case Pattern(b) => true case _ => false } map { case Pattern(b) => val c = valueFor(\"c\")(a, b); (Pattern(b), c) } map { case (Pattern(b), c) => expression(a, b, c) } }"
  )

  test(
    for {
      a <- monadFor("a")()
      if condition(a)
      b <- monadFor("b")(a)
    } yield expression(a, b)
    ,
    monadFor("a")()
      .withFilter { a => condition(a) }
      .flatMap { a =>
        monadFor("b")(a) map { b => expression(a, b) }
      }
    ,
    "monadFor(\"a\")() withFilter { a => condition(a) } flatMap { a => monadFor(\"b\")(a) map { b => expression(a, b) } }"
  )

  test(
    for {
      a <- monadFor("a")()
      if condition(a)
      Pattern(b) <- monadFor("b")(a)
    } yield expression(a, b)
    ,
    monadFor("a")()
      .withFilter { a => condition(a) }
      .flatMap { a =>
        monadFor("b")(a)
          .withFilter {
            case Pattern(b) => true
            case _ => false
          }
          .map { case Pattern(b) => expression(a, b) }
      }
    ,
    "monadFor(\"a\")() withFilter { a => condition(a) } flatMap { a => monadFor(\"b\")(a) withFilter { case Pattern(b) => true case _ => false } map { case Pattern(b) => expression(a, b) } }"
  )

  test(
    for {
      a <- monadFor("a")()
      if condition(a)
      if condition(a)
    } yield expression(a)
    ,
    monadFor("a")()
      .withFilter { a => condition(a) }
      .withFilter { a => condition(a) }
      .map { a => expression(a) }
    ,
    "monadFor(\"a\")() withFilter { a => condition(a) } withFilter { a => condition(a) } map { a => expression(a) }"
  )

  test(
    for {
      a <- monadFor("a")()
      if condition(a)
      b = valueFor("b")(a)
    } yield expression(a, b)
    ,
    monadFor("a")()
      .withFilter { a => condition(a) }
      .map { a =>
        val b = valueFor("b")(a)
        (a, b)
      }
      .map { case (a, b) => expression(a, b) }
    ,
    "monadFor(\"a\")() withFilter { a => condition(a) } map { a => val b = valueFor(\"b\")(a); (a, b) } map { case (a, b) => expression(a, b) }"
  )

  test(
    for {
      a <- monadFor("a")()
      b = valueFor("b")(a)
      c <- monadFor("c")(a, b)
    } yield expression(a, b, c)
    ,
    monadFor("a")()
      .map { a =>
        val b = valueFor("b")(a)
        (a, b)
      }
      .flatMap { case (a, b) =>
        monadFor("c")(a, b) map { c => expression(a, b, c) }
      }
    ,
    "monadFor(\"a\")() map { a => val b = valueFor(\"b\")(a); (a, b) } flatMap { case (a, b) => monadFor(\"c\")(a, b) map { c => expression(a, b, c) } }"
  )

  test(
    for {
      a <- monadFor("a")()
      b = valueFor("b")(a)
      Pattern(c) <- monadFor("c")(a, b)
    } yield expression(a, b, c)
    ,
    monadFor("a")()
      .map { a =>
        val b = valueFor("b")(a)
        (a, b)
      }
      .flatMap { case (a, b) =>
        monadFor("c")(a, b)
          .withFilter {
            case Pattern(c) => true
            case _ => false
          }
          .map { case Pattern(c) => expression(a, b, c) }
      }
    ,
    "monadFor(\"a\")() map { a => val b = valueFor(\"b\")(a); (a, b) } flatMap { case (a, b) => monadFor(\"c\")(a, b) withFilter { case Pattern(c) => true case _ => false } map { case Pattern(c) => expression(a, b, c) } }"
  )

  test(
    for {
      a <- monadFor("a")()
      b = valueFor("b")(a)
      if condition(a, b)
    } yield expression(a, b)
    ,
    monadFor("a")()
      .map { a =>
        val b = valueFor("b")(a)
        (a, b)
      }
      .withFilter { case (a, b) => condition(a, b) }
      .map { case (a, b) => expression(a, b) }
    ,
    "monadFor(\"a\")() map { a => val b = valueFor(\"b\")(a); (a, b) } withFilter { case (a, b) => condition(a, b) } map { case (a, b) => expression(a, b) }"
  )

  test(
    for {
      a <- monadFor("a")()
      b = valueFor("b")(a)
      c = valueFor("c")(a, b)
    } yield expression(a, b, c)
    ,
    monadFor("a")()
      .map { a =>
        val b = valueFor("b")(a)
        val c = valueFor("c")(a, b)
        (a, b, c)
      }
      .map { case (a, b, c) => expression(a, b, c) }
    ,
    "monadFor(\"a\")() map { a => val b = valueFor(\"b\")(a); val c = valueFor(\"c\")(a, b); (a, b, c) } map { case (a, b, c) => expression(a, b, c) }"
  )

  test(
    for {
      Pattern(a) <- monadFor("a")()
      b <- monadFor("b")(a)
      c <- monadFor("c")(a, b)
    } yield expression(a, b, c)
    ,
    monadFor("a")()
      .withFilter {
        case Pattern(a) => true
        case _ => false
      }
      .flatMap { case Pattern(a) =>
        monadFor("b")(a)
          .flatMap { b =>
            monadFor("c")(a, b)
              .map { c => expression(a, b, c) }
          }
      }
    ,
    "monadFor(\"a\")() withFilter { case Pattern(a) => true case _ => false } flatMap { case Pattern(a) => monadFor(\"b\")(a) flatMap { b => monadFor(\"c\")(a, b) map { c => expression(a, b, c) } } }"
  )

  test(
    for {
      Pattern(a) <- monadFor("a")()
      b <- monadFor("b")(a)
      Pattern(c) <- monadFor("c")(a, b)
    } yield expression(a, b, c)
    ,
    monadFor("a")()
      .withFilter {
        case Pattern(a) => true
        case _ => false
      }
      .flatMap { case Pattern(a) =>
        monadFor("b")(a)
          .flatMap { b =>
            monadFor("c")(a, b)
              .withFilter {
                case Pattern(c) => true
                case _ => false
              }
              .map { case Pattern(c) => expression(a, b, c) }
          }
      }
    ,
    "monadFor(\"a\")() withFilter { case Pattern(a) => true case _ => false } flatMap { case Pattern(a) => monadFor(\"b\")(a) flatMap { b => monadFor(\"c\")(a, b) withFilter { case Pattern(c) => true case _ => false } map { case Pattern(c) => expression(a, b, c) } } }"
  )

  test(
    for {
      Pattern(a) <- monadFor("a")()
      b <- monadFor("b")(a)
      if condition(a, b)
    } yield expression(a, b)
    ,
    monadFor("a")()
      .withFilter {
        case Pattern(a) => true
        case _ => false
      }
      .flatMap { case Pattern(a) =>
        monadFor("b")(a)
          .withFilter { b => condition(a, b) }
          .map { b => expression(a, b) }
      }
    ,
    "monadFor(\"a\")() withFilter { case Pattern(a) => true case _ => false } flatMap { case Pattern(a) => monadFor(\"b\")(a) withFilter { b => condition(a, b) } map { b => expression(a, b) } }"
  )

  test(
    for {
      Pattern(a) <- monadFor("a")()
      b <- monadFor("b")(a)
      c = valueFor("c")(a, b)
    } yield expression(a, b, c)
    ,
    monadFor("a")()
      .withFilter {
        case Pattern(a) => true
        case _ => false
      }
      .flatMap { case Pattern(a) =>
        monadFor("b")(a)
          .map { b =>
            val c = valueFor("c")(a, b)
            (b, c)
          }
          .map { case (b, c) => expression(a, b, c) }
      }
    ,
    "monadFor(\"a\")() withFilter { case Pattern(a) => true case _ => false } flatMap { case Pattern(a) => monadFor(\"b\")(a) map { b => val c = valueFor(\"c\")(a, b); (b, c) } map { case (b, c) => expression(a, b, c) } }"
  )

  test(
    for {
      Pattern(a) <- monadFor("a")()
      if condition(a)
      b <- monadFor("b")(a)
    } yield expression(a, b)
    ,
    monadFor("a")()
      .withFilter {
        case Pattern(a) => true
        case _ => false
      }
      .withFilter { case Pattern(a) => condition(a) }
      .flatMap { case Pattern(a) =>
        monadFor("b")(a)
          .map { b => expression(a, b) }
      }
    ,
    "monadFor(\"a\")() withFilter { case Pattern(a) => true case _ => false } withFilter { case Pattern(a) => condition(a) } flatMap { case Pattern(a) => monadFor(\"b\")(a) map { b => expression(a, b) } }"
  )

  test(
    for {
      Pattern(a) <- monadFor("a")()
      if condition(a)
      Pattern(b) <- monadFor("b")(a)
    } yield expression(a, b)
    ,
    monadFor("a")()
      .withFilter {
        case Pattern(a) => true
        case _ => false
      }
      .withFilter { case Pattern(a) => condition(a) }
      .flatMap { case Pattern(a) =>
        monadFor("b")(a)
          .withFilter {
            case Pattern(b) => true
            case _ => false
          }
          .map { case Pattern(b) => expression(a, b) }
      }
    ,
    "monadFor(\"a\")() withFilter { case Pattern(a) => true case _ => false } withFilter { case Pattern(a) => condition(a) } flatMap { case Pattern(a) => monadFor(\"b\")(a) withFilter { case Pattern(b) => true case _ => false } map { case Pattern(b) => expression(a, b) } }"
  )

  test(
    for {
      Pattern(a) <- monadFor("a")()
      if condition(a)
      if condition(a)
    } yield expression(a)
    ,
    monadFor("a")()
      .withFilter {
        case Pattern(a) => true
        case _ => false
      }
      .withFilter { case Pattern(a) => condition(a) }
      .withFilter { case Pattern(a) => condition(a) }
      .map { case Pattern(a) => expression(a) }
    ,
    "monadFor(\"a\")() withFilter { case Pattern(a) => true case _ => false } withFilter { case Pattern(a) => condition(a) } withFilter { case Pattern(a) => condition(a) } map { case Pattern(a) => expression(a) }"
  )

  test(
    for {
      Pattern(a) <- monadFor("a")()
      if condition(a)
      b = valueFor("b")(a)
    } yield expression(a, b)
    ,
    monadFor("a")()
      .withFilter {
        case Pattern(a) => true
        case _ => false
      }
      .withFilter { case Pattern(a) => condition(a) }
      .map { case Pattern(a) =>
        val b = valueFor("b")(a)
        (Pattern(a), b)
      }
      .map { case (Pattern(a), b) => expression(a, b) }
    ,
    "monadFor(\"a\")() withFilter { case Pattern(a) => true case _ => false } withFilter { case Pattern(a) => condition(a) } map { case Pattern(a) => val b = valueFor(\"b\")(a); (Pattern(a), b) } map { case (Pattern(a), b) => expression(a, b) }"
  )

  test(
    for {
      Pattern(a) <- monadFor("a")()
      b = valueFor("b")(a)
      c <- monadFor("c")(a, b)
    } yield expression(a, b)
    ,
    monadFor("a")()
      .withFilter {
        case Pattern(a) => true
        case _ => false
      }
      .map { case Pattern(a) =>
        val b = valueFor("b")(a)
        (Pattern(a), b)
      }
      .flatMap { case (Pattern(a), b) =>
        monadFor("c")(a, b) map { c => expression(a, b) }
      }
    ,
    "monadFor(\"a\")() withFilter { case Pattern(a) => true case _ => false } map { case Pattern(a) => val b = valueFor(\"b\")(a); (Pattern(a), b) } flatMap { case (Pattern(a), b) => monadFor(\"c\")(a, b) map { c => expression(a, b) } }"
  )

  test(
    for {
      Pattern(a) <- monadFor("a")()
      b = valueFor("b")(a)
      Pattern(c) <- monadFor("c")(a, b)
    } yield expression(a, b)
    ,
    monadFor("a")()
      .withFilter {
        case Pattern(a) => true
        case _ => false
      }
      .map { case Pattern(a) =>
        val b = valueFor("b")(a)
        (Pattern(a), b)
      }
      .flatMap { case (Pattern(a), b) =>
        monadFor("c")(a, b)
          .withFilter {
            case Pattern(c) => true
            case _ => false
          }
          .map { case Pattern(c) => expression(a, b) }
      }
    ,
    "monadFor(\"a\")() withFilter { case Pattern(a) => true case _ => false } map { case Pattern(a) => val b = valueFor(\"b\")(a); (Pattern(a), b) } flatMap { case (Pattern(a), b) => monadFor(\"c\")(a, b) withFilter { case Pattern(c) => true case _ => false } map { case Pattern(c) => expression(a, b) } }"
  )

  test(
    for {
      Pattern(a) <- monadFor("a")()
      b = valueFor("b")(a)
      if condition(a, b)
    } yield expression(a, b)
    ,
    monadFor("a")()
      .withFilter {
        case Pattern(a) => true
        case _ => false
      }
      .map { case Pattern(a) =>
        val b = valueFor("b")(a)
        (Pattern(a), b)
      }
      .withFilter { case (Pattern(a), b) => condition(a, b) }
      .map { case (Pattern(a), b) => expression(a, b) }
    ,
    "monadFor(\"a\")() withFilter { case Pattern(a) => true case _ => false } map { case Pattern(a) => val b = valueFor(\"b\")(a); (Pattern(a), b) } withFilter { case (Pattern(a), b) => condition(a, b) } map { case (Pattern(a), b) => expression(a, b) }"
  )

  test(
    for {
      Pattern(a) <- monadFor("a")()
      b = valueFor("b")(a)
      c = valueFor("c")(a, b)
    } yield expression(a, b, c)
    ,
    monadFor("a")()
      .withFilter {
        case Pattern(a) => true
        case _ => false
      }
      .map { case Pattern(a) =>
        val b = valueFor("b")(a)
        val c = valueFor("c")(a, b)
        (Pattern(a), b, c)
      }
      .map { case (Pattern(a), b, c) => expression(a, b, c) }
    ,
    "monadFor(\"a\")() withFilter { case Pattern(a) => true case _ => false } map { case Pattern(a) => val b = valueFor(\"b\")(a); val c = valueFor(\"c\")(a, b); (Pattern(a), b, c) } map { case (Pattern(a), b, c) => expression(a, b, c) }"
  )

  /*
  test(
    for {
      a <- monadFor("a")()
      b = valueFor("b")(a)
      if condition(a, b)
      c = valueFor("c")(a, b)
      d = valueFor("d")(a, b, c)
    } yield expression(a, b, c, d)
    ,
    monadFor("a")()
      .map { a =>
        val b = valueFor("b")(a)
        (a, b)
      }
      .withFilter { case (a, b) => condition(a, b) }
      .map { case (a, b) =>
        val c = valueFor("c")(a, b)
        val d = valueFor("d")(a, b, c)
        ((a, b), c, d)
      }
      .map { case ((a, b), c, d) => expression(a, b, c, d) }
    ,
    ""
  )
  */

  /*
  test(
    for {
      a <- monadFor("a")()
      Pattern(b) <- monadFor("b")(a)
      c <- monadFor("c")(a, b)
      Pattern(d) <- monadFor("d")(a, b, c)
      Pattern(e) <- monadFor("e")(a, b, c, d)
      f <- monadFor("f")(a, b, c, d, e)
      g = valueFor("g")(a, b, c, d, e, f)
      Pattern(h) <- monadFor("h")(a, b, c, d, e, f, g)
      i <- monadFor("i")(a, b, c, d, e, f, g, h)
      if condition(a, b, c, d, e, f, g, h, i)
      Pattern(j) <- monadFor("j")(a, b, c, d, e, f, g, h, i)
      Pattern(k) <- monadFor("k")(a, b, c, d, e, f, g, h, i, j)
      l <- monadFor("l")(a, b, c, d, e, f, g, h, i, j, k)
      Pattern(m) <- monadFor("m")(a, b, c, d, e, f, g, h, i, j, k, l)
      if condition(a, b, c, d, e, f, g, h, i, j, k, l, m)
      n = valueFor("n")(a, b, c, d, e, f, g, h, i, j, k, l, m)
      o <- monadFor("o")(a, b, c, d, e, f, g, h, i, j, k, l, m, n)
      p <- monadFor("p")(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o)
      Pattern(q) <- monadFor("q")(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p)
      r <- monadFor("r")(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q)
    } yield expression(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r)
    ,
    null
    ,
    ""
  )
  */

  def test[T](m1: Monad[T], m2: Monad[T], expectedFuncString: String): Unit = {
    println(normalizeString(m1.toString()))
    //println(normalizeString(m2.toString()))
    println(m1.toFuncString())
    //println(m2.toFuncString())
    println()

    assert(m1 == m2, "m1 != m2")
    assert(m1.toFuncString() == expectedFuncString, differences(m1.toFuncString(), expectedFuncString))
    assert(m2.toFuncString() == expectedFuncString, differences(m2.toFuncString(), expectedFuncString))
    assert(m1 != { for (x <- monadFor("y")()) yield expression(x) })
  }

  def normalizeString(funcString: String) = funcString
    .replaceAll(",false\\)", ")")
    .replaceAll("\\bSimpleVar\\((.)", "SimpleVar(\"$1\"")
    .replaceAll("\\bAuxVar\\((.)", "AuxVar(\"$1\"")
    .replaceAll("\\b(List|ArrayBuffer|WrappedArray)\\(", "Seq(")
    .replaceAll(",(map|flatMap|withFilter),", ", \"$1\", ")
    .replaceAll(",([^\\s])", ", $1")

  def differences(str1: String, str2: String) = {
    val (commonPrefix, restStr1, restStr2) = splitCommonPrefix(str1, str2)
    val (commonSuffix, uncommon1, uncommon2) = splitCommonSuffix(restStr1, restStr2)

    commonPrefix +
      Console.RED + "[-" + uncommon1 + "-]" + Console.RESET +
      Console.GREEN + "[+" + uncommon2 + "+]" + Console.RESET +
      commonSuffix
  }

  def splitCommonSuffix(str1: String, str2: String) = {
    val (commonPrefix, restStr1, restStr2) = splitCommonPrefix(str1.reverse, str2.reverse)
    (commonPrefix.reverse, restStr1.reverse, restStr2.reverse)
  }

  def splitCommonPrefix(str1: String, str2: String) = {
    val prefixSize = this.commonPrefixSize(str1, str2)

    val prefix = str1.slice(0, prefixSize)
    val restStr1 = str1.slice(prefixSize, Int.MaxValue)
    val restStr2 = str2.slice(prefixSize, Int.MaxValue)

    (prefix, restStr1, restStr2)
  }

  @tailrec
  def commonPrefixSize(str1: String, str2: String, size: Int = 0): Int =
    if (str1.size <= size || str2.size <= size) size
    else if (str1(size) == str2(size)) commonPrefixSize(str1, str2, size + 1)
    else size

  println("OK")
}
