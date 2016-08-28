package dao

/**
  * Created by pp on 8/26/16.
  */
object MongoDocumentsNaming {
  object Users {
    val collectionName = "users"
    val id = "id"
    val login = "login"
    val password = "password"
    val myRatings = "myRatings"
    val myRecommendations = "myRecommendations"
  }

  object Movies {
    val collectionName = "movies"
    val id = "id"
    val title = "title"
    val year = "year"
    val posterUrl = "posterUrl"
    val averageRating = "averageRating"
    val viewers = "viewers"
    val description = "description"
  }

  object Ratings {
    val movieId = "movieId"
    val rate = "rate"
  }
}
