package components

import japgolly.scalajs.react.BackendScope
import japgolly.scalajs.react.vdom.prefix_<^._
import model.Movie

/**
  * Created by pp on 4/28/16.
  */
object MoviePanel {

  case class State(movie: Movie)

  class Backend($: BackendScope[Unit, State]) {
    def render(s: State) = {
//      <.div(^.`class`:="movie-item",
//        <.img(^.`class`:="img-responsive", ^.src := movie.poster.url, ^.alt:= movie.name),
//        <.h3(
//          <.a(^.href:="#", movie.name),
//          <.p(movie.description)
//        )
//      )
    }
  }

}
