package components.webpage

import japgolly.scalajs.react.vdom.prefix_<^._
import japgolly.scalajs.react.{BackendScope, _}
import model.Movie

/**
  * Created by pp on 4/28/16.
  */
object MoviePanel {

  val component =
    ReactComponentB[Props]("movie-panel")
      .renderBackend[Backend]
      .build

  case class Props(movie: Movie, onRate: Movie => Callback)

  class Backend($: BackendScope[Props, Unit]) {

    def render(p: Props) = {
      <.div(^.`class`:="movie-item",
        <.img(^.`class`:="img-responsive", ^.src := p.movie.poster.url, ^.alt:= p.movie.name),
        <.h3(
          <.a(^.href:="#", p.movie.name)
        ),
        RatingControl.component(
          RatingControl.Props(
            p.movie.myRating,
            i => p.onRate(p.movie.copy(myRating = Some(i))),
            minRate = 0,
            maxRate = 9)
        ),
        <.p(p.movie.description)
      )
    }
  }

}
