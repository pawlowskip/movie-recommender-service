package components

import components.AwesomeIcons.Icon
import components.Bootstrap.{button, panel, row}
import japgolly.scalajs.react.vdom.prefix_<^._
import japgolly.scalajs.react.{BackendScope, _}

/**
  * Created by pp on 4/26/16.
  */
object SearchSettingsPanel {

  case class State(searchText: String,
                   onButtonSearch: String => Callback,
                   onButtonSettings: Callback,
                   onTextChange: String => Callback)

  class Backend($: BackendScope[Unit, State]) {

    def onTextChange(e: ReactEventI) =
      $.modState(state => state.copy(searchText = e.target.value)) >>
        $.state.flatMap(state => state.onTextChange(state.searchText))

    def render(s: State) = {
      row("sm", 12)(
        <.div(^.`class`:="filter-panel", ^.id := "filter-panel",
          panel()(
            <.form(^.`class`:="form", ^.role:="form",
              <.div(^.`class`:="form-group",
                <.label(^.`class`:="filter-col", ^.style:="margin-right:10px;", ^.`for`:="pref-perpage",
                  "Rows per page:"),
                <.select(^.id:="pref-perpage", ^.`class`:="form-control",
                  <.option(^.value:="0", 0),
                  <.option(^.selected:="selected", ^.value:="1", 1)
                )
              ),
              <.div(^.`class`:="form-group",
                <.div(^.`class`:="checkbox", ^.style:="margin-left:10px; margin-right:10px;",
                  <.label(
                    <.input(^.`type`:="checkbox")
                  )("Remember parameters")
                ),
                button(onClick = Callback.log(""), additionalStyles = Seq("filter-col"))(
                  "Save Settings"
                )
              )
            )
          )
        )
      )
    }
  }

}
