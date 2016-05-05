package components.webpage

import components.framework.Bootstrap
import japgolly.scalajs.react.vdom.prefix_<^._
import japgolly.scalajs.react.{BackendScope, _}
import model.Movie

/**
  * Created by pp on 4/26/16.
  */
object MoviesDashboard {

  val component =
    ReactComponentB[Props]("movies-dashboard")
      .renderBackend[Backend]
      .build

  case class Props(movies: Seq[Movie], onMovieChange: Movie => Callback)

  class Backend($: BackendScope[Props, Unit]) {

    def render(p: Props) = {
      def renderMovies() =
        p.movies.sliding(3).map( threeMovies =>
          Bootstrap.row("md", 4, 4, 4)(
            threeMovies.map(movie =>
              MoviePanel.component.withKey(movie.id)(
                MoviePanel.Props(movie, p.onMovieChange)
              )
            ): _*
          )
        )

      def renderEmpty() = <.h3(^.`class` := "text-center", "No movies to display.")

      <.div(
        if (p.movies.isEmpty) renderEmpty() else renderMovies()
      )
    }
  }

}
