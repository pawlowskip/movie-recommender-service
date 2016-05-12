package example

import components._
import components.framework.AwesomeIcons.Icon

import scala.scalajs.js
import org.scalajs.dom

import scalatags.JsDom.all._
import japgolly.scalajs.react.vdom.prefix_<^._
import japgolly.scalajs.react.{BackendScope, Callback, React, ReactComponentB, ReactDOM}
import components.framework.Bootstrap.button
import components.framework.Bootstrap.menu.Location
import components.framework.Bootstrap.panel
import components.framework.{AwesomeIcons, Bootstrap}
import components.webpage.MyMoviesPanel
import model.{Movie, Poster}

object ScalaJSExample extends js.JSApp {

  def main(): Unit = {

    val root = dom.document.getElementById("root")

    val movies = for (i <- 1 to 6) yield
      i.toLong -> Movie(i.toLong, "Film name" + i, 2000, Poster("http://placehold.it/200x300"), 6.6, Some(4), 10000, "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Nam viverra euismod odio, gravida pellentesque urna varius vitae.")

    val myMoviesPanel = MyMoviesPanel.component(MyMoviesPanel.State(movies.toMap, ""))

    val locations = Seq(
      Location(" My Movies", isActive = true, Icon.film),
      Location(" Recommendations", isActive = false, Icon.thumbsUp),
      Location(" Top 100", isActive = false, Icon.star),
      Location(" My Profile", isActive = false, Icon.user)
    )

    val menu = Bootstrap.menu(locations)

    import Bootstrap.menu.{Style, FixedPosition}
    val menuPanel =
      menu(Bootstrap.menu.Props(None, loc => Callback.empty, Style.inverse, Some(FixedPosition.top)),
        myMoviesPanel.withKey("my-movies-panel")(Unit),
        <.div(^.key := "div1"),
        <.div(^.key := "div2"),
        <.div(^.key := "div3")
      )

    ReactDOM.render(menuPanel, root)


  }
}
