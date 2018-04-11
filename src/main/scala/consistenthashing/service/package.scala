package consistenthashing

import scala.util.Random

package object service {
  type Hash = Int

  object Hash {
    def generate(random: Random): Hash = random.nextInt()
  }

  type Token = Hash
  val Token = Hash
}
