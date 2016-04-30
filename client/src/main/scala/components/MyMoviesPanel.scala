package components

import japgolly.scalajs.react.vdom.prefix_<^._
import japgolly.scalajs.react.{BackendScope, ReactComponentB, Callback}
import model.Movie

/**
  * Created by pp on 4/28/16.
  */
object MyMoviesPanel {

  def component(initialState: State) =
    ReactComponentB[Unit]("MoviesDashboard")
      .initialState(initialState)
      .renderBackend[Backend]
      .build

  case class State(movies: Map[Long, Movie], searchTitle: String)

  class Backend($: BackendScope[Unit, State]) {

    def doSearch(s: String) =
      $.modState(state => state.copy(searchTitle = s))

    def searchByTitle(state: State): Seq[Movie] = state.searchTitle match {
      case "" => state.movies.values.toSeq
      case _ => state.movies.filter{ case (_, movie) => movie.name contains state.searchTitle}.values.toSeq
    }

    def onMovieChange(movie: Movie): Callback =
      $.modState(state => state.copy(movies = state.movies.updated(movie.id, movie)))

    val search = SearchPanel.component(SearchPanel.State(""))

    def render(s: State) = {
      <.div(^.`class` := "container",
        search(SearchPanel.Props(doSearch, Callback.empty, s => Callback.empty)),
        <.br,
        MoviesDashboard.component(
          MoviesDashboard.Props(
            searchByTitle(s),
            onMovieChange
          )
        )
      )
    }
  }

}
