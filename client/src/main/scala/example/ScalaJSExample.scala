package example

import components.AwesomeIcons.Icon
import components.{AwesomeIcons, Bootstrap}

import scala.scalajs.js
import org.scalajs.dom
import scalatags.JsDom.all._
import japgolly.scalajs.react.vdom.prefix_<^._
import japgolly.scalajs.react.{ReactComponentB, BackendScope, ReactDOM, Callback}
import components.Bootstrap.button
import components.Bootstrap.panel

object ScalaJSExample extends js.JSApp {

  def main(): Unit = {

    val root = dom.document.getElementById("root")

    case class State(size: AwesomeIcons.Icon.Size)

    class Backend($: BackendScope[Unit, State]) {
      val incSize =
        $.modState(state => state.copy(size = AwesomeIcons.Icon.Size.X5)) >>
        Callback.log(s"Size increased to!")

      def render(s: State) = {
        <.div(^.`class` := "container",
          panel(panel.Props(Some("heading")),
            <.p(s.size.toString),
            button(button.Props(incSize), "Size up"),
            AwesomeIcons(AwesomeIcons.Props(Icon(AwesomeIcons.Type.thumbsUp, s.size)))
          )
        )
      }
    }

    val Example = ReactComponentB[Unit]("Example")
      .initialState(State(AwesomeIcons.Icon.Size.X3))
      .renderBackend[Backend]
      .build


    val comp =
      <.div(^.`class` := "container",
        Example(Unit)
      )

    ReactDOM.render(comp, root)


    //---------------------------------------------------------

//    case class BootstrapNavElem(title: String, icon: String, isActive: Boolean) {
//      val cssClass = if (isActive) "active" else ""
//      val html =
//        li(`class`:=cssClass)(
//          a(href:="")(
//            i(`class`:=s"fa fa-${icon} fa-2x"),
//            title
//          )
//        )
//    }
//
//    case class BootstrapNav(pages: Seq[BootstrapNavElem]) {
//      val html = ul(`class`:="nav navbar-nav")(
//        pages.map(_.html)
//      )
//    }
//
//    val pages =
//      Seq(
//        BootstrapNavElem(" My Movies", "film",true),
//        BootstrapNavElem(" Recommendations", "thumbs-up", false),
//        BootstrapNavElem(" Top 100", "star",false),
//        BootstrapNavElem(" My Profile", "user", false)
//      )
//
//
//    val nav =
//      div(`class`:="navbar navbar-inverse navbar-fixed-top")(
//        BootstrapNav(pages).html
//      ).render
//
//
//    val myMoviesPanel =
//      div(
//        div(`class`:="row")(
//          div(`class`:="col-sm-8")(
//            input(`type`:="text", `class`:="form-control", style:="width: 100%;", placeholder:="Find by title ...")
//          ),
//          div(`class`:="col-sm-2")(
//            button(`class`:="btn btn-block")(
//              i(`class`:="fa fa-search"), " Search"
//            )
//          ),
//          div(`class`:="col-sm-2")(
//            button(`class`:="btn btn-block")(
//              i(`class`:="fa fa-wrench"), " Settings"
//            )
//          )
//        )
//      )
//
//    val dataToggle = "data-toggle".attr
//    val dataTarget = "data-target".attr
//
//    val advancedSearch =
//      div(`class`:="row")(
//        div(id:="filter-panel", `class`:="filter-panel")(
//          div(`class`:="panel panel-default")(
//            div(`class`:="panel-body")(
//              form(`class`:="form", role:="form")(
//                div(`class`:="form-group")(
//                  label(`class`:="filter-col", style:="margin-right:10px;", `for`:="pref-perpage")(
//                    "Rows per page:"
//                  ),
//                  select(id:="pref-perpage", `class`:="form-control")(
//                    option(value:="0")(0),
//                    option(selected:="selected", value:="1")(1)
//                  )
//                ),
//                div(`class`:="form-group")(
//                  label(`class`:="filter-col",style:="margin-right:10px;",`for`:="pref-search")(
//                    "Search"
//                  ),
//                  input(`type`:="text", `class`:="form-control input-sm", id:="pref-search")
//                ),
//                div(`class`:="form-group")(
//                  label(`class`:="filter-col", style:="margin-right:10px;", `for`:="pref-orderby")(
//                    "Order by:"
//                  ),
//                  select(id:="pref-orderby", `class`:="form-control")(
//                    option("Descendent")
//                  )
//                ),
//                div(`class`:="form-group")(
//                  div(`class`:="checkbox", style:="margin-left:10px; margin-right:10px;")(
//                    label(
//                      input(`type`:="checkbox")
//                    )("Remember parameters")
//                  ),
//                  button(`type`:="submit", `class`:="btn btn-default filter-col")(
//                    "Save Settings"
//                  )
//                )
//              )
//            )
//          )
//        )
//      )
//
//
//    val filmRow =
//      div(`class`:="row")(
//        div(`class`:="col-md-4 movie-item")(
//          img(`class`:="img-responsive", src:="http://placehold.it/200x300", alt:=""),
//          h3(
//            a(href:="#")("Film name"),
//            p("Lorem ipsum dolor sit amet, consectetur adipiscing elit. Nam viverra euismod odio, gravida pellentesque urna varius vitae.")
//          )
//        ),
//        div(`class`:="col-md-4 movie-item")(
//          img(`class`:="img-responsive", src:="http://placehold.it/200x300", alt:=""),
//          h3(
//            a(href:="#")("Film name"),
//            p("Lorem ipsum dolor sit amet, consectetur adipiscing elit. Glentesque urna varius vitae.")
//          )
//        ),
//        div(`class`:="col-md-4 movie-item")(
//          img(`class`:="img-responsive", src:="http://placehold.it/200x300", alt:=""),
//          h3(
//            a(href:="#")("Film name"),
//            p("Lorem ipsum dolor sit amet, consectetur adipiscing elit. Nam viverra euismod odio, gravida pellentesque urna varius vitae.")
//          )
//        )
//      )
//
//    val dashboard = div(id:="dashboard").render

//    root.appendChild(
//      nav
//    )
//
//    root.appendChild(
//      dashboard
//    )
//
//    root.appendChild(
//      div(`class`:="container root")(
//        myMoviesPanel,
//        advancedSearch,
//        filmRow,
//        filmRow,
//        filmRow
//      ).render
//    )

  }
}
