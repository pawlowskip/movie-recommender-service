package components.webpage

import components.framework.AwesomeIcons.Icon
import components.framework.Bootstrap.button
import components.framework.{AwesomeIcons, Bootstrap}
import japgolly.scalajs.react.vdom.prefix_<^._
import japgolly.scalajs.react.{BackendScope, CallbackTo, _}


/**
  * Created by pp on 4/25/16.
  */
object SearchPanel {

  def component(init: State) =
    ReactComponentB[Props]("MoviesDashboard")
      .initialState(init)
      .renderBackend[Backend]
      .build

  case class Props(onButtonSearch: String => Callback,
                   onButtonSettings: Callback,
                   onTextChange: String => Callback)

  case class State(searchText: String)

  class Backend($: BackendScope[Props, State]) {

    def onTextChange(e: ReactEventI): CallbackTo[Unit] =
      $.modState(state => state.copy(searchText = e.target.value)) >>
      $.state.map{state =>
        $.props.flatMap(props => props.onTextChange(state.searchText))
      }


    def render(p: Props, s: State) = {
      <.div(
        Bootstrap.row("sm", 8, 1)(
          <.input(^.`type`:="text", ^.`class`:="form-control", /*^.style:="width: 100%;",*/
                  ^.placeholder:="Find by title ...", ^.value := s.searchText, ^.onChange ==> onTextChange),
          button(p.onButtonSearch(s.searchText))(
            AwesomeIcons(Icon.search),
            " Search")/*,
          button(p.onButtonSettings)(
            AwesomeIcons(Icon.wrench),
            " Settings")*/
        )
      )
    }
  }

}
