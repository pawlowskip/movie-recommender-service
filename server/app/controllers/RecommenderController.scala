package controllers

import api.MovieRecommenderApi
import play.api.mvc._


class RecommenderController(val movieRecommenderDao: MovieRecommenderApi)
  extends Controller {

  def index = Action {
    Ok(views.html.index(""))
  }

}
