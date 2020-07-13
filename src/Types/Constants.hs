module Types.Constants where


type Route = String
type VimWikiLink = String


categoriesRoute :: Route
categoriesRoute = "/_categories"


categoriesRoutePrefix :: Route
categoriesRoutePrefix = categoriesRoute <> "/"


tagsRoute :: Route
tagsRoute = "/_tags"


tagsRoutePrefix :: Route
tagsRoutePrefix = tagsRoute <> "/"
