package components

import components.AwesomeIcons.Icon
import components.Bootstrap.{button, panel}
import japgolly.scalajs.react.vdom.prefix_<^._
import japgolly.scalajs.react.{BackendScope, Callback, ReactEventI}

import scalatags.JsDom.all._

/**
  * Created by pp on 4/25/16.
  */
object SearchPanel {

  case class State(searchText: String,
                   onButtonSearch: String => Callback,
                   onButtonSettings: Callback,
                   onTextChange: String => Callback)

  class Backend($: BackendScope[Unit, State]) {

    def onTextChange(e: ReactEventI) =
      $.modState(state => state.copy(searchText = e.target.value)) >>
      $.state.flatMap(state => state.onTextChange(state.searchText))

    def render(s: State) = {
      <.div(
        Bootstrap.row("sm", 8, 2, 2)(
          <.input(^.`type`:="text", ^.`class`:="form-control", ^.style:="width: 100%;",
                  ^.placeholder:="Find by title ...", ^.value := s.searchText, ^.onChange ==> onTextChange),
          button(button.Props(s.onButtonSearch(s.searchText)), AwesomeIcons(Icon(AwesomeIcons.Type.search)), " Search"),
          button(button.Props(s.onButtonSettings), AwesomeIcons(Icon(AwesomeIcons.Type.wrench)), " Settings")
        )
      )
    }
  }

}
