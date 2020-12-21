package AOC_2020

import shared.{DayChallenge, Helpers}

import scala.collection.immutable

object DayTwentyOne extends DayChallenge[Long, String] with Helpers {
  override val expectedPartOne: Option[Long] = Some(5)
  override val expectedPartTwo: Option[String] = Some("mxmxvkd,sqjhc,fvjkl")
  override val testData: List[String] = List(
      "mxmxvkd kfcds sqjhc nhms (contains dairy, fish)",
      "trh fvjkl sbzzf mxmxvkd (contains dairy)",
      "sqjhc fvjkl (contains soy)",
      "sqjhc mxmxvkd sbzzf (contains fish)")

  override def partOne(l: List[String]): Long = {
    val allergenCandidates = getPossibles(getMap(l)).flatMap(_._2).toList
    val allIngredients = allIngredientsList(l)
    val nonPossibles = allIngredients.filter(e => !allergenCandidates.contains(e))
    allIngredients.count(nonPossibles contains _)
  }

  private def allIngredientsList(l: List[String]): List[String] = l.flatMap{ line =>
    getTwoFromSplit(line, " \\(contains ")._1.split(" ").toList
  }

  private def getPossibles(mapWithSets: Map[String, List[List[String]]]): Map[String, Set[String]] =
    mapWithSets.map{case (k, values) => k -> values.flatten.toSet.filter(ingredient => values.forall(_ contains ingredient))}

  private def getMap(l: List[String], i: Int = 0, currentM: Map[String, List[List[String]]] = Map()): Map[String, List[List[String]]] = {
    val newOverallM = getIngredientsAndAllergens(l(i)).foldLeft(currentM){case (m, pair) => addOrAppend(m, pair._1, pair._2)}
    if (i == l.indices.end - 1) newOverallM else getMap(l, i + 1, newOverallM)
  }

  private def addOrAppend(m: Map[String, List[List[String]]], allergen: String, ingredients: List[String]): Map[String, List[List[String]]] =
    if (m.get(allergen).isDefined) {
      m + (allergen -> (m(allergen) :+ ingredients))} else {
      m + (allergen -> List(ingredients))
    }

  private def getIngredientsAndAllergens(line: String): Map[String, List[String]] = {
    val (ingredientsString, containsString) = getTwoFromSplit(line, " \\(contains ")
    val allergens = containsString.replaceAll("\\)", "").split(", ").toList
    allergens.map(_ -> ingredientsString.split(" ").toList).toMap
  }

  private def resolve(map: Map[String, Set[String]], resolvedMap: Map[String, String]): Map[String, String] = {
    val (resolved, unresolved) = map.partition{case (_, v) => v.size == 1}
    val newResolved = resolved.map{case (k, v) => k -> v.head}
    if (unresolved.isEmpty) {
      resolvedMap ++ newResolved
    } else {
      val newMap = newResolved.foldLeft(unresolved) { case (newUnresolved, (_, v)) => removedResolveds(newUnresolved, v) }
      resolve(newMap, resolvedMap ++ newResolved)
    }
  }

  private def removedResolveds(m: Map[String, Set[String]], resolved: String) =
    m.map{case (allergen, ingredients) => (allergen -> ingredients.filter(_ != resolved))}

  override def partTwo(l: List[String]): String =
    resolve(getPossibles(getMap(l)), Map()).toSeq.sortBy(_._1).map(_._2).mkString(",")
}
