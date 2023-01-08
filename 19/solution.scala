import scala.io.Source


type Blueprint = Map[String, Map[String, Int]]

val resourceTypes = Seq("ore", "clay", "obsidian", "geode")

object Main {
    val blueprints = init
    def main(args: Array[String]) = {
        println(problem1)
        println(problem2)
    }
    def problem1 = {
        blueprints
            .zipWithIndex
            .map((blueprint, id) =>
                (id + 1) * maxGeodes(blueprint, 24, Map("ore" -> 1), Map.empty)
            )
            .sum
    }
    def problem2 = {
        blueprints
            .take(3)
            .map(maxGeodes(_, 32, Map("ore" -> 1), Map.empty))
            .reduce(_ * _)
    }
    def init: Seq[Blueprint] = {
        val robotPattern = """Each (\w+) robot costs ([\w\s]+).""".r
        val quantityPattern = """(\d+) (\w+)""".r
        Source
            .fromFile("input")
            .getLines
            .map(line =>
                robotPattern
                    .findAllMatchIn(line)
                    .map(m1 =>
                        m1.group(1) ->
                            quantityPattern.findAllMatchIn(m1.group(2))
                                .map(m2 =>
                                    m2.group(2) -> m2.group(1).toInt
                                )
                                .toMap
                    )
                    .toMap
            )
            .toSeq
    }
    def maxGeodes(blueprint: Blueprint, time: Int, robots: Map[String, Int], resources: Map[String, Int], skipRobots: Seq[String] = Nil): Int = {
        if (time == 0) resources.getOrElse("geode", 0)
        else {
            val potentialRobots = resourceTypes
                .filterNot(skipRobots.contains)
                .filter(robot =>
                    robot == "geode" || blueprint.values.map(_.getOrElse(robot, 0)).max > robots.getOrElse(robot, 0)
                )
                .flatMap(robot =>
                    val potentialResources = resourceTypes
                        .map(resource =>
                            resource -> (resources.getOrElse(resource, 0) - blueprint(robot).getOrElse(resource, 0))
                        )
                        .toMap
                    Some((robot, potentialResources)).filter(_._2.values.forall(_ >= 0))
                )
            potentialRobots
                .find(_._1 == "geode")
                .map((_, newRessources) =>
                    maxGeodes(
                        blueprint,
                        time - 1,
                        robots + ("geode" -> (robots.getOrElse("geode", 0) + 1)),
                        resourceTypes
                        .map(resource =>
                            resource -> (newRessources.getOrElse(resource, 0) + robots.getOrElse(resource, 0))
                        )
                        .toMap
                    )
                )
                .getOrElse(
                    (potentialRobots.map((newRobot, newResources) =>
                        maxGeodes(
                            blueprint,
                            time - 1,
                            robots + (newRobot -> (robots.getOrElse(newRobot, 0) + 1)),
                            resourceTypes
                            .map(resource =>
                                resource -> (newResources.getOrElse(resource, 0) + robots.getOrElse(resource, 0))
                            )
                            .toMap
                        )
                    )
                    :+ maxGeodes(
                        blueprint,
                        time - 1,
                        robots,
                        resourceTypes
                        .map(resource =>
                            resource -> (resources.getOrElse(resource, 0) + robots.getOrElse(resource, 0))
                        )
                        .toMap,
                        potentialRobots.map(_._1) ++ skipRobots
                    ))
                    .max
                )
        }
    }
}
