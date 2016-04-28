package components

import components.AwesomeIcons.Icon
import components.Bootstrap.button
import japgolly.scalajs.react.vdom.prefix_<^._
import japgolly.scalajs.react.{BackendScope, _}
import model.Movie

/**
  * Created by pp on 4/26/16.
  */
object MoviesDashboard {

  def component(initialState: State) =
    ReactComponentB[Props]("MoviesDashboard")
      .initialState(initialState)
      .renderBackend[Backend]
      .build

  case class State()
  case class Props(movies: Seq[Movie])

  class Backend($: BackendScope[Props, State]) {

    def render(p: Props, s: State) = {
      <.div(
        p.movies.sliding(3).map( threeMovies =>
          Bootstrap.row("md", 4, 4, 4)(
            threeMovies.map(movie =>
              <.div(^.`class`:="movie-item",
                <.img(^.`class`:="img-responsive", ^.src := movie.poster.url, ^.alt:= movie.name),
                <.h3(
                  <.a(^.href:="#", movie.name),
                  <.p(movie.description)
                )
              )
            ): _*
          )
        )
      )
    }
  }

}
