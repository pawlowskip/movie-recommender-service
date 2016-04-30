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

  val component=
    ReactComponentB[Props]("movies-dashboard")
      .initialState(Unit)
      .renderBackend[Backend]
      .build

  case class Props(movies: Seq[Movie], onMovieChange: Movie => Callback)

  class Backend($: BackendScope[Props, Unit]) {

    def render(p: Props) = {
      <.div(
        p.movies.sliding(3).map( threeMovies =>
          Bootstrap.row("md", 4, 4, 4)(
            threeMovies.map(movie =>
              MoviePanel.component(
                MoviePanel.Props(movie, p.onMovieChange)
              )
            ): _*
          )
        )
      )
    }
  }

}
