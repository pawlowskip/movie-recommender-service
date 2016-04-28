package components

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.prefix_<^._

/**
 * Created by pp on 4/4/16.
 */
object Bootstrap {

  def additionalStyleHtmlClass(styles: Seq[String]) = styles.foldLeft("")((s1, s2) => s1 + " " + s2)

  type Style = Style.Value
  object Style extends Enumeration {
    val default, primary, success, info, warning, danger = Value
  }

  object row {
    def apply(size: String, proportions: Int*)(body: ReactNode*) = {
      //require(proportions.size == body.size, "Incompatibile number of columns and proportions value")
      <.div(^.`class` := "row",
        (proportions zip body).map{ case (proportion, node) =>
          <.div(^.`class` := s"col-$size-$proportion",
            node
          )}
      )
    }
  }

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

    def apply(onClick: Callback,
              isBlock: Boolean = false,
              style: Style = Style.default,
              additionalStyles: Seq[String] = Seq.empty)
             (children: ReactNode*) = component(Props(onClick, isBlock, style, additionalStyles), children: _*)

    def apply() = component
  }

//  object textInput {
//    case class Props(onTextChange: String => Callback,
//                     value: String = "",
//                     placeholder: String = "",
//                     additionalStyles: Seq[String] = Seq.empty) {
//
//      val htmlClass = s"form-control ${additionalStyleHtmlClass(additionalStyles)}"
//    }
//
//    val component =
//      ReactComponentB[Props]("text-input")
//        .renderPC((_, props, children) =>
//          <.input(^.`type`:="text", ^.`class`:=props.htmlClass, ^.style:="width: 100%;",
//            ^.placeholder:=props.placeholder, ^.value := props.value, ^.onChange ==> )
//
//          <.button(^.`class` := props.htmlClass, ^.`type` := "text", ^.onClick --> props.onClick, children)
//        ).build
//
//    <.input(^.`type`:="text", ^.`class`:="form-control", ^.style:="width: 100%;",
//      ^.placeholder:="Find by title ...", ^.value := s.searchText, ^.onChange ==> onTextChange),
//  }

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

    def apply(heading: Option[String] = None,
              footer: Option[String] = None,
              style: Style = Style.default,
              additionalStyles: Seq[String] = Seq.empty)
             (body: ReactNode*) = component(Props(heading, footer, style, additionalStyles), body: _*)

  }

  object menu {

    def apply(brand: Option[ReactNode],
              menu: Menu,
              style: Style = Style.default,
              fixedPosition: Option[FixedPosition] = None,
              additionalStyles: Seq[String] = Seq.empty)
              (menuElems: ReactNode*) = {
      require(menu.locations.size == menuElems.size, "Incompatible number of locations and menu elements")
      component(Props(brand, menu, style, fixedPosition, additionalStyles), menuElems: _*)
    }

    case class Location(address: String, isActive: Boolean, icon: AwesomeIcons.Icon) {
      val isActiveClass = if (isActive) "active" else ""
    }

    case class Menu(locations: Location*)


    type Style = Style.Value
    object Style extends Enumeration {
      val default, inverse = Value
    }

    type FixedPosition = FixedPosition.Value
    object FixedPosition extends Enumeration {
      val top, bottom = Value
      def bootstrapClass(fixedPosition: Option[FixedPosition]): String = fixedPosition match {
        case None => ""
        case Some(fp) => s"navbar-fixed-$fp"
      }
    }

    case class Props(brand: Option[ReactNode],
                     menu: Menu,
                     style: Style = Style.default,
                     fixedPosition: Option[FixedPosition] = None,
                     additionalStyles: Seq[String] = Seq.empty)

    val component =
      ReactComponentB[Props]("menu")
        .renderPC((_, props, children) =>
          <.div(
            <.nav(^.`class` := s"navbar navbar-${props.style} ${FixedPosition.bootstrapClass(props.fixedPosition)}",
              <.div(^.`class` := "container-fluid",
                props.brand.map(br =>
                  <.div(^.`class` := "navbar-header",
                    <.a(^.`class` := "navbar-brand", ^.href := "#",
                      br)
                  )
                ),
                <.ul(^.`class` := "nav navbar-nav",
                  props.menu.locations.map(location =>
                      <.li(^.`class` := location.isActiveClass,
                        <.a(^.href := "#",
                          AwesomeIcons(location.icon),
                          location.address)
                      )
                    )
                )
              )
            ),
            props.menu.locations
              .zipWithIndex
              .filter{ case (location, index) => location.isActive }
              .map{ case (_, index) => React.Children.toArray(children)(index)}
          )
        ).build


  }

}
