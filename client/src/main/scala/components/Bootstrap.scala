package components

import japgolly.scalajs.react.{ReactNode, ReactComponentB, Callback}
import japgolly.scalajs.react.vdom.prefix_<^._

/**
 * Created by pp on 4/4/16.
 */
object Bootstrap {

  type Style = Style.Value
  object Style extends Enumeration {
    val default, primary, success, info, warning, danger = Value
  }

  def additionalStyleHtmlClass(styles: Seq[String]) = styles.foldLeft("")((s1, s2) => s1 + " " + s2)

  object button {
    case class Props(onClick: Callback,
                     isBlock: Boolean = false,
                     style: Style = Style.default,
                     additionalStyles: Seq[String] = Seq.empty) {

      val isBlockClass = if (isBlock) "btn-block" else ""
      val styleClass = "btn-" + style.toString
      val htmlClass = s"btn $isBlockClass $styleClass ${additionalStyleHtmlClass(additionalStyles)}"
    }

    val component =
      ReactComponentB[Props]("btn")
        .renderPC((_, props, children) =>
          <.button(^.`class` := props.htmlClass, ^.`type` := "button", ^.onClick --> props.onClick, children)
        ).build

    def apply(props: Props, children: ReactNode*) = component(props, children: _*)
    def apply() = component
  }

  object panel {
    case class Props(heading: Option[String] = None,
                     footer: Option[String] = None,
                     style: Style = Style.default,
                     additionalStyles: Seq[String] = Seq.empty) {
      val htmlClass = s"panel panel-$style ${additionalStyleHtmlClass(additionalStyles)}"
    }

    val component =
      ReactComponentB[Props]("btn")
        .renderPC((_, props, children) =>
          <.div(^.`class` := props.htmlClass,
            props.heading.map( heading =>
              <.div(^.`class` := "panel-heading", heading)
            ),
            <.div(^.`class` := "panel-body", children),
            props.footer.map( footer =>
              <.div(^.`class` := "panel-footer", footer)
            )
          )
        ).build

    def apply() = component
    def apply(props: Props, body: ReactNode*) = component(props, body: _*)

  }

  object navbar {

    case class Location(address: String, isActive: Boolean)
    case class Menu(locations: Location*) {

    }
    // TODO Locations, in props list of locations


    object Style extends Enumeration {
      val default, inverse = Value
    }

    object Position extends Enumeration {
      val top, bottom = Value
    }

    case class Props(brand: Option[ReactNode],

                     style: Style = Style.default,
                     additionalStyles: Seq[String] = Seq.empty)
  }

}
