package controllers

import java.io.File

import play.api._
import play.api.mvc._
import com.fasterxml.jackson.databind.{DeserializationFeature, ObjectMapper}
import com.fasterxml.jackson.module.scala.DefaultScalaModule
import com.fasterxml.jackson.module.scala.experimental.ScalaObjectMapper

import scala.io.Source

object Application extends Controller {

  def index = Action {
    Ok(views.html.index)
  }

  def foods(fr: FoodRequest) = Action {
    val matchingFoods = Foods.all().filter { food =>
      fr.nutrientFilters.forall { nutrientFilter =>
        food.nutrients.exists { nutrient =>
          nutrient.description == nutrientFilter.name &&
          nutrient.value < nutrientFilter.minimumValue
        }
      }
    }
    Ok(Json.generate(matchingFoods)) as "application/json"
  }

}

case class FoodRequest(nutrientFilters: Seq[NutrientFilter])
case class NutrientFilter(name: String, minimumValue: BigDecimal)

object Foods {

  def all(): Seq[Food] = {
    val txt = Source.fromFile(new File("foods.json")).getLines.mkString
    Json.parse[Seq[Food]](txt)
  }
}

case class Food(id: String, description: String, tags: Seq[String], portions: Seq[Portion], nutrients: Seq[Nutrient])
case class Portion(amount: BigDecimal, unit: String, grams: BigDecimal)
case class Nutrient(value: BigDecimal, units: String, description: String, group: String)

object Json {
  private lazy val mapper = {
    val m = new ObjectMapper() with ScalaObjectMapper
    m registerModule DefaultScalaModule
    m configure (DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES, false)
    m configure (DeserializationFeature.FAIL_ON_INVALID_SUBTYPE, true)
    m
  }

  def parse[A](json: String)(implicit m : Manifest[A]): A = mapper.readValue[A](json)

  def generate(x: Any): String = mapper writeValueAsString x
}