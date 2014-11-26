package controllers

import java.io.File

import play.api._
import play.api.mvc._
import com.fasterxml.jackson.databind.{DeserializationFeature, ObjectMapper}
import com.fasterxml.jackson.module.scala.DefaultScalaModule
import com.fasterxml.jackson.module.scala.experimental.ScalaObjectMapper

import scala.io.Source

object Application extends Controller {
  val pageSize = 50

  def index = Action {
    Ok(views.html.index())
  }

  def foods(fr: FoodRequest) = Action {
    val matchingFoods = Foods.all.filter { food =>
      fr.nutrientFilters.forall { filter =>
        food.nutrients.exists { nutrient =>
          nutrient.description == filter.name &&
          nutrient.value < filter.maximumValue
        }
      }
    }
    val response = FoodsResponse(matchingFoods.drop(fr.page - 1 * pageSize).take(pageSize), matchingFoods.length)
    Ok(Json.generate(response)) as "application/json"
  }

  def find(id: String) = Action {
    Foods.all.find(_.id == id) map { food =>
      val json = Json.generate(food)
      Ok(json) as "application/json"
    } getOrElse NotFound("")
  }

  def search(term: String) = Action {
    val matches = Foods.all.filter { food =>
      food.description.contains(term) || food.tags.contains(term)
    }
    val json = Json.generate(matches)
    Ok(json) as "application/json"
  }

  def nutrients() = Action {
    val nutrients = Foods.all.map(_.nutrients.map(_.description)).flatten.distinct.sorted
    Ok(Json.generate(nutrients)) as "application/json"
  }

}


case class NutrientFilter(name: String, maximumValue: BigDecimal)

case class FoodsResponse(foods: Seq[Food], total: Int)

object Foods {
  import play.api.Play.current

  def all(): Seq[Food] = {
    val txt = Source.fromFile(new File(Play.application.classloader.getResource("foods.json").getFile)).getLines().mkString
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

case class FoodRequest(nutrientFilters: Seq[NutrientFilter], page: Int)

object FoodRequest {
  class FoodRequestBinder extends QueryStringBindable[FoodRequest] {
    override def bind(key: String, params: Map[String, Seq[String]]): Option[Either[String, FoodRequest]] = {
      val filters = params
        .get("filters")
        .get
        .head
        .split("___")
        .filter(_.contains(":"))
        .map { rawValue =>
        val parts = rawValue.split(":")
        val name = parts(0)
        val amount = BigDecimal(parts(1))
        NutrientFilter(name, amount)
      }
      val page = params.get("page").getOrElse(Seq("1")).head.toInt
      val request = FoodRequest(filters, page)
      Some(Right(request))
    }

    override def unbind(key: String, value: FoodRequest): String = ""
  }

  implicit val binder: FoodRequestBinder = new FoodRequestBinder
}