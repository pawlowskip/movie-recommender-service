package loader

import akka.actor.ActorSystem
import api.MovieRecommenderApi
import com.softwaremill.macwire._
import controllers.Assets
import controllers.RecommenderController
import dao.RecommenderServiceDao
import play.api.ApplicationLoader.Context
import play.api._
import play.api.inject.DefaultApplicationLifecycle
import play.api.routing.Router
import play.modules.reactivemongo.{DefaultReactiveMongoApi, ReactiveMongoComponents}
import router.Routes

/**
  * Created by pp on 8/27/16.
  */
class AppLoader extends ApplicationLoader {
  def load(context: Context): Application = new Components(context).application
}

class Components(context: Context) extends BuiltInComponentsFromContext(context) {
  // set up logger
  LoggerConfigurator(context.environment.classLoader).foreach {
    _.configure(context.environment)
  }
  lazy val assets: Assets = wire[Assets]
  lazy val router: Router = wire[Routes] withPrefix "/"
}

trait AppModule
  extends ReactiveMongoComponents
{
  def configuration: Configuration
  def actorSystem: ActorSystem
  def applicationLifecycle: DefaultApplicationLifecycle

  lazy val reactiveMongoApi = wire[DefaultReactiveMongoApi]

  // Define services bindings
  lazy val recommenderServiceDao: MovieRecommenderApi = wire[RecommenderServiceDao]

  // Define controllers bindings
  lazy val appController: RecommenderController = wire[RecommenderController]
  //lazy val authController: AuthenticationController = wire[AuthenticationController]

}