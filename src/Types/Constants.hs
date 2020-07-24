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


-- / allow up to 2000k for the header
maxHeaderSize :: Int
maxHeaderSize = 100 * 20


wordsPerMinute :: Int
wordsPerMinute = 270
