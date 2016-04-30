package components

import japgolly.scalajs.react.BackendScope
import japgolly.scalajs.react.vdom.prefix_<^._
import japgolly.scalajs.react.Callback
import model.Movie

/**
  * Created by pp on 4/28/16.
  */
object MoviePanel {

  case class State()
  case class Props(movie: Movie, onRate: Movie => Callback)

  class Backend($: BackendScope[Props, State]) {

    def render(p: Props, s: State) = {
      <.div(^.`class`:="movie-item",
        <.img(^.`class`:="img-responsive", ^.src := p.movie.poster.url, ^.alt:= p.movie.name),
        <.h3(
          <.a(^.href:="#", p.movie.name)
        ),
        RatingControl.component()(RatingControl.Props(p.movie.myRating, i => p.onRate(p.movie.copy(myRating = Some(i))), 0, 9)),
        <.p(p.movie.description)
      )
    }
  }

}
