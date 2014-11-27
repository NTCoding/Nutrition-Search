package controllers

import java.io.File

import org.h2.util.IOUtils
import play.api._
import play.api.mvc._
import com.fasterxml.jackson.databind.{DeserializationFeature, ObjectMapper}
import com.fasterxml.jackson.module.scala.DefaultScalaModule
import com.fasterxml.jackson.module.scala.experimental.ScalaObjectMapper

import scala.io.Source

object Application extends Controller {
  val pageSize = 25

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
    } sortBy(_.description) map(ApiFood.apply)
    val numberOfPages = Math.round((matchingFoods.length.toDouble / pageSize) + 1)
    val toDrop = (fr.page - 1) * pageSize
    val response = FoodsResponse(matchingFoods.drop(toDrop).take(pageSize), matchingFoods.length, numberOfPages.toInt)
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
}


case class NutrientFilter(name: String, maximumValue: BigDecimal)

case class FoodsResponse(foods: Seq[ApiFood], total: Int, numberOfPages: Int)

object Foods {
  import org.apache.commons.io.IOUtils._
  import org.apache.commons.io.IOUtils
  import play.api.Play.current

  def all(): Seq[Food] = {
    val foodData = Play resourceAsStream "public/foods.json" get
    val json = IOUtils toString (foodData, "UTF-8")
    Json.parse[Seq[Food]](json)
  }
}

object ApiFood {
  val mineralSymbols = Seq(
    ", Ca",
    ", Fe",
    ", Mg",
    ", P",
    ", K",
    ", Na",
    ", Zn",
    ", Cu",
    ", Mn",
    ", Se",
    ", DFE"
  )
  def apply(food: Food) = {
    val vitamins = food.nutrients.filter(_.description.toLowerCase.contains("vitamin")) sortBy(_.description)
    val minerals = food.nutrients.filter(n => mineralSymbols.exists(n.description.contains)) sortBy(_.description)
    val otherNutrients = food.nutrients.filterNot(n => vitamins.contains(n) || minerals.contains(n)) sortBy(_.description)
    new ApiFood(food.id, food.description, food.tags, food.portions, Nutrients(vitamins, minerals, otherNutrients))
  }
}
case class ApiFood(id: String, description: String, tags: Seq[String], portions: Seq[Portion], nutrients: Nutrients)
case class Nutrients(vitamins: Seq[Nutrient], minerals: Seq[Nutrient], otherNutrients: Seq[Nutrient])

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