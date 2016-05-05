package components.webpage

import components.framework.AwesomeIcons
import components.framework.AwesomeIcons.Icon
import japgolly.scalajs.react.vdom.prefix_<^._
import japgolly.scalajs.react.{BackendScope, _}

/**
  * Created by pp on 4/30/16.
  */
object RatingControl {

  val component =
    ReactComponentB[Props]("rating-control")
      .initialState(State(None))
      .renderBackend[Backend]
      .build

  case class State(hoveredRating: Option[Int])
  case class Props(rating: Option[Int], onRate: Int => Callback, minRate: Int, maxRate: Int)

  class Backend($: BackendScope[Props, State]) {

    def displayRate(starNr: Option[Int]): Callback =
      $.setState(State(starNr))

    def render(p: Props, s: State) = {
      val emptyStar = AwesomeIcons(Icon.star_o)
      val filledStar = AwesomeIcons(Icon.star)

      def star(nr: Int) = {
        <.div(^.`class` := "cursor-pointer float-left", ^.onClick --> p.onRate(nr),
              ^.onMouseOver --> displayRate(Some(nr)), ^.onMouseLeave --> displayRate(p.rating),
          (p.rating, s.hoveredRating) match {
            case (None, None) => emptyStar
            case (_, Some(h)) => if (nr > h) emptyStar else filledStar
            case (Some(r), _) => if (nr > r) emptyStar else filledStar
          }
        )
      }

      <.div(
        (p.minRate to p.maxRate).map(star),
        <.br(^.`class` := "clear-left")
      )
    }
  }

}
