module Experiments.SourceContext where


import           Data.Time.Clock (UTCTime)
import Data.Text as T

-- NOTE: not entirely sure whether to keep this or not; seems to be a duplicate
-- of SourcePageContext which might be better named as SourceContext??

{- |

The SourceContext record wraps up a single source file with the information from
that page.  This means that it holds the following information from the header:


--- sitegen
title: The page <title> value
template: default  # the template file (minus .extension) to render this page with
style: style.css   # the style sheet to apply to this page.
tags: [tag1, tag2] # tags to apply to the page
category: cat1     # The SINGLE category to apply to the page.
date: 2019-10-11   # The Date/time to apply to the page
updated: 2019-10-12 # Date/time the page was updated.
slug: code/slug-name  # The permanent slug to use for this page.
authors: [tinwood]  # Authors of this page.
draft: true        # The default is true; set to false to actually publish it.
site: <site-identifier> # the site that this belongs to. 'default' is the
                        # default site identifier.
<maybe more>
---                # this indicates the end of the header.

-}

data RawSourceContext = RawSourceContext
    { rsTitle  :: !String
    , rsTemplate :: !String
    , rsStyle :: !String
    , rsTags :: ![String]
    , rsCategory :: !String
    , rsDate :: !String
    , rsUpdated :: !String
    , rsRoute :: !String
    , rsAuthors :: ![String]
    , rsDraft :: !Bool
    , rsSite :: !String
    } deriving Show


data SourceContext = SourceContext
    { sTitle :: !String
    , sTemplate :: !FilePath
    , sStyle :: !String
    , sTags :: ![String]
    , sCategory :: !String
    , sDate :: UTCTime
    , sUpdate :: UTCTime
    , sRoute :: !String
    , sAuthors :: ![String]
    , sDraft :: !Bool
    , sSite :: !String
    , sFileTime  :: !UTCTime
    , sFileSize :: !Integer
    , sContent :: !T.Text
    } deriving Show

