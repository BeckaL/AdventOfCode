package AOC_2017

import shared.{DayChallenge, Helpers, TestData}

object DayTwenty extends DayChallenge[Int, Int] with Helpers {
  override def partOne(l: List[String]): Int = {
    val endParticles = (1 to 1000).toList.foldLeft(toParticles(l)){case (particles, _) => particles.map(updateParticle)}
    endParticles.minBy(_.position.manhattanDistanceToOrigin).id
  }

  def toParticles(l: List[String]) =
    l.zipWithIndex
      .map { case (str, id) =>
        str.split(", ").toList match {
          case p :: v :: a :: Nil => Particle(id, get3DCoord(p), get3DCoord(v), get3DCoord(a))
          case _ => throw new RuntimeException(s"something went wrong converting $str to particle")
        }
      }

  def get3DCoord(assignmentStr: String): Coord3D =
    getTwoFromSplit(assignmentStr, "=<")._2.stripSuffix(">").split(",").map(interpretNumberWithOptionalSign).toList match {
      case x :: y :: z :: Nil => Coord3D(x, y, z)
      case _ => throw new RuntimeException(s"something went wrong converting $assignmentStr to 3d coord")
    }

  def updateParticle(p: Particle) = {
    val newVelocity = Coord3D(p.velocity.x + p.acceleration.x, p.velocity.y + p.acceleration.y, p.velocity.z + p.acceleration.z)
    val newPosition = Coord3D(p.position.x + newVelocity.x, p.position.y + newVelocity.y, p.position.z + newVelocity.z)
    Particle(p.id, newPosition, newVelocity, p.acceleration)
  }

  case class Particle(id: Int, position: Coord3D, velocity: Coord3D, acceleration: Coord3D)

  case class Coord3D(x: Int, y: Int, z: Int) {
    def manhattanDistanceToOrigin = x.abs + y.abs + z.abs
  }

  override def partTwo(l: List[String]): Int =
    (1 to 1000).toList.foldLeft(toParticles(l)) { case (particles, _) =>
      removeParticlesWithCollisions(particles.map(updateParticle), 0)
    }.size

  def removeParticlesWithCollisions(particles: List[Particle], i: Int): List[Particle] =
    if (i == particles.size) {
      particles
    } else {
      val collides = particles.filter(p => p.position == particles(i).position)
      if (collides.size > 1) {
        val newParticles = particles.filterNot(collides contains _)
        removeParticlesWithCollisions(newParticles, i)
      } else {
        removeParticlesWithCollisions(particles, i + 1)
      }
    }
}

object DayTwentyData extends TestData[Int, Int] {
  override val testData: List[String] = List(
    "p=<3,0,0>, v=<2,0,0>, a=<-1,0,0>",
    "p=<4,0,0>, v=<0,0,0>, a=<-2,0,0>")
  override val expectedPartOne: Option[Int] = Some(0)
  override val expectedPartTwo: Option[Int] = Some(1)
  override val testData2: Option[List[String]] = Some(List(
    "p=<-6,0,0>, v=<3,0,0>, a=<0,0,0>",
      "p=<-4,0,0>, v=<2,0,0>, a=<0,0,0>",
  "p=<-2,0,0>, v=<1,0,0>, a=<0,0,0>",
  "p=<3,0,0>, v=<-1,0,0>, a=<0,0,0>"
  ))
}