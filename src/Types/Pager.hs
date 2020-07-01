{-
   Pager is a structure that is used to be able to provide the Pager context to
   the template so that it it can render the appropriate links for pages that
   make up an index set.

   If we have a base index.html, then when it's been 'paginated', additional
   indexes of index-2.html, index-3.html ... will be generated and placed on the
   render queue.  The Pager items will provide the context within those pages as
   they are rendered.
-}

module Types.Pager where


import qualified Data.HashMap.Strict as HashMap
import           Data.List           (intercalate)
import           Data.List.Split     (chunksOf, splitOn)


type RouteToPager = HashMap.HashMap String Pager


-- | allow paging of an item type.
data Pager = Pager
    { pagerRoute         :: !String   -- The route of this page.
    , pagerMaxSize       :: !Int      -- The maximum number of pages per Pager
    , pagerItemsThisPage :: !Int      -- Number of items in the pager
    , pagerTotalItems    :: !Int      -- the total number of items being paged
    , pagerNumPagers     :: !Int      -- the number of Pager
    , pagerThisPage      :: !Int      -- The Pager number 1..pagerNumPagers
    , pagerItemIndexes   :: ![Int]    -- List[n..n+num-items-this-page -1] -- the indexes
    } deriving (Show)


-- an index route will looking like 'thing/'.  We need to turn this into
-- 'thing-pager-2/, 'thing-pager-3/' ...
-- The '/' route, needs to go to '/', 'pager-2/


-- convert a route in to a list of the routes based on a pager.
-- i.e. "/" 3 -> ["/", "/pager-2/", "/pager-3/"]
routeToPagerRoutes :: String -> Int -> [String]
routeToPagerRoutes s n
  | n < 1  = []
  | n == 1 = [s]
  | n > 1  = s : [addPagerRoute s x | x <- [2..n]]


-- | add in -pager-n to the route, handling the special cases as needed.
addPagerRoute :: String -> Int -> String
addPagerRoute "" n = "pager-" <> show n
addPagerRoute "/" n = "/" <> addPagerRoute "" n <> "/"
addPagerRoute s n
  | last s /= '/' = case splitOn "/" s of
      []  -> error "This shouldn't be able to happen!"
      [x] -> x <> "-" <> addPagerRoute "" n
      xs  -> intercalate "/" (init xs) <> "/" <> addPagerRoute (last xs) n
  | otherwise     = addPagerRoute (init s) n <> "/"



-- | Create a list of Pager records for a route and a list of pages.
-- Note that if the records fit into one page then only one Pager is returned.
-- Templates will need to decide what to show if there is no need for a pager
-- (i.e. only one Pager is returned)
makePagerList :: String -> Int -> Int -> [Pager]
makePagerList route numItems pageSize =
    let numPagers = (numItems `quot` pageSize)
                  + (if numItems `rem` pageSize /=0 then 1 else 0)
     in case routeToPagerRoutes route numPagers of
        [] -> []
        xs -> zipWith (makePager pageSize numItems numPagers) xs [1..]


-- | use to take a pager and put it in a HashMap using fromList
pagerListToTuples :: [Pager] -> [(String, Pager)]
pagerListToTuples = map (\p -> (pagerRoute p, p))


makePager :: Int -> Int -> Int -> String -> Int -> Pager
makePager pageSize numItems nPages route thisPageNum =
    let base = (thisPageNum-1)*pageSize
        size = minimum [pageSize, numItems - base]
     in Pager { pagerRoute=route
              , pagerMaxSize=pageSize
              , pagerItemsThisPage=size
              , pagerTotalItems=numItems
              , pagerNumPagers=nPages
              , pagerThisPage=thisPageNum
              , pagerItemIndexes=[base + x | x <- [0..size-1]]
              }
