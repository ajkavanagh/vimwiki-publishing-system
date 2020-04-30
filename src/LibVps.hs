{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleContexts #-}


module LibVps
    {-( someFunc-}
    {-, someOtherFunc-}
    {-, runPanDoc-}
    {-)-}
      where

-- for Pandoc processing
import qualified Data.List as L
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import           Data.Text.Titlecase (titlecase)
import qualified Text.Pandoc as TP
import qualified Text.Pandoc.Error as TPE
import qualified Text.Pandoc.Walk as TPW
import qualified Text.Regex.PCRE.Heavy as PCRE
import qualified Qq as Q

-- for system environment, etc.
import System.Directory (doesFileExist, doesDirectoryExist)
import System.Environment (getProgName, getArgs, getEnvironment)
import System.FilePath (FilePath, (</>), (<.>), takeBaseName)

-- monad related stuff
import Control.Monad (when, unless, foldM, liftM2)
import Control.Monad.IO.Class (liftIO)

-- for optparse
import Data.Semigroup ((<>))
import Options.Applicative
import Options.Applicative.Types (ReadM, readerAsk)

-- local library
import Lib ( isMarkDownStr
           , isMarkDownFile
           , strToLower
           , validateWithTests
           , printIfDoesntExist
           , validateFileExists
           , isDebug
           )


vw2html :: IO ()
vw2html = vimwikiSingleFileCli =<< execParser opts


vimwikiSingleFileCli :: VimwikiSingleFileCliArgs -> IO ()
vimwikiSingleFileCli args = do
    debug <- isDebug
    when debug $ do
        putStrLn "-- Debug Trace On --"
        debugParams args
    argsOk <- validateArgs args
    when argsOk $ runPanDoc args


runPanDoc :: VimwikiSingleFileCliArgs -> IO ()
runPanDoc args = do
    -- read the source file
    text <- TIO.readFile $ inputFileArg args
    -- process it for wikilinks -- turn them into markdown links
    let pText = processForLinks text
    -- create the writer options, including the template
    writerOptions <- pandocHtmlArgs args
    -- parse the markdown and then convert to HTML5 document
    result <- TP.runIO $ do
        doc <- TP.readMarkdown pandocMarkdownArgs pText
        TP.writeHtml5String writerOptions doc
    rst <- TP.handleError result
    -- finally write out the HTML5 document
    TIO.writeFile (outputFilePath args) rst


collectElems :: TP.Pandoc -> [String]
collectElems = TPW.query elems
  where
      elems :: TP.Block -> [String]
      elems x = [show x]


-- this is done line-by-line as we can't have a broken across lines.  However,
-- they'll still get converted if in a code black.
processForLinks :: T.Text -> T.Text
processForLinks = T.unlines . replaceLinks . T.lines


-- replace [[xxx]] with [xxx](xxx.html)
replaceLinks :: [T.Text] -> [T.Text]
replaceLinks = map (replace1 . replace2)
  where
      -- Q.re is a ungreedy utf-8 PCRE matcher.
      -- replace [[xxx]] links
      replace1 :: T.Text -> T.Text
      replace1 =  PCRE.gsub
                    [Q.re|\[\[(.+)\]\]|]
                    (\(l:_) -> "[" ++ l ++ "](" ++ l ++ ".html)" :: String)

      -- replace [[link|description]] pages
      replace2 :: T.Text -> T.Text
      replace2 = PCRE.gsub
                   [Q.re|\[\[([^|\]]+)\|(.*)\]\]|]
                   (\(l:d:_) -> "[" ++ d ++ "](" ++ l ++ ".html)" :: String)


pandocHtmlArgs :: VimwikiSingleFileCliArgs -> IO TP.WriterOptions
pandocHtmlArgs args = do
    template <- TIO.readFile $ templateFilePath args
    let jt = Just $ T.unpack template
    return TP.def { TP.writerTemplate = jt
                  , TP.writerVariables =
                      [ ("css", cssFileArg args)
                      , ("pagetitle", pageTitleFromArgs args)
                      , ("include-before", homeLinkText args)
                      ]
                  }

pandocMarkdownArgs :: TP.ReaderOptions
pandocMarkdownArgs = TP.def { TP.readerExtensions = TP.extensionsFromList
                              [ TP.Ext_autolink_bare_uris
                              , TP.Ext_backtick_code_blocks
                              ]
                            }

homeLinkText :: VimwikiSingleFileCliArgs -> String
homeLinkText args = "<div><a href=\"" ++ index ++ "\">Index</a></div><hr/>"
  where
      root = rootPathArg args
      index = if root == "-" then "index.html" else root </> "index.html"


-- old, not using, but shows how the sandbox is done.
convertMarkdownToHtml :: T.Text -> TP.ReaderOptions -> TP.WriterOptions -> Either TPE.PandocError T.Text
convertMarkdownToHtml input readerOptions writerOptions = TP.runPure $ do
    doc <- TP.readMarkdown readerOptions input
    TP.writeHtml5String writerOptions doc


outputFilePath :: VimwikiSingleFileCliArgs -> FilePath
outputFilePath args = outputDirArg args
                  </> takeBaseName (inputFileArg args)
                  <.> "html"

pageTitleFromArgs :: VimwikiSingleFileCliArgs -> String
pageTitleFromArgs = titlecase . takeBaseName . inputFileArg


templateFilePath :: VimwikiSingleFileCliArgs -> FilePath
templateFilePath args = templatePathArg args
                    </> templateNameArg args
                    <.> templateExtensionArg args


-- OPTION parsing of the command line
--
-- This is called from VimWiki directly and just converts a single file.
--
-- we want to grab the following which is called from Vimwiki
-- <progname> <force> <syntax> <extension> <output_dir> <input_file> <css_file>
--            <template_path> <template_default> <template_ext> <root_path>
--            <custom_args>  (not used in this script)
--
--The following arguments, in this order, are passed to the script:

-- 1. force : [0/1] overwrite an existing file
-- 2. syntax : the syntax chosen for this wiki
-- 3. extension : the file extension for this wiki
-- 4. output_dir : the full path of the output directory
-- 5. input_file : the full path of the wiki page
-- 6. css_file : the full path of the css file for this wiki
-- 7. template_path : the full path to the wiki's templates
-- 8. template_default : the default template name
-- 9. template_ext : the extension of template files
-- 10. root_path : a count of ../ for pages buried in subdirs
--     For example, if you have wikilink [[dir1/dir2/dir3/my page in a subdir]]
--     then this argument is '../../../'.
-- 11. custom_args : custom arguments that will be passed to the conversion
--     (can be defined in g:vimwiki_list as 'custom_wiki2html_args' parameter,
--     see |vimwiki-option-custom_wiki2html_args|)
--     script.
--
-- Options 7-11 are experimental and may change in the future.  If any of these
-- parameters is empty, a hyphen "-" is passed to the script in its place.
--
-- For an example and further instructions, refer to the following script:

--  $VIMHOME/autoload/vimwiki/customwiki2html.sh

data VimwikiSingleFileCliArgs = VimwikiSingleFileCliArgs
    { forceArg :: !Bool
    , syntaxArg :: !String
    , extensionArg :: !String
    , outputDirArg :: !String
    , inputFileArg :: !String
    , cssFileArg :: !String
    , templatePathArg :: !String
    , templateNameArg :: !String
    , templateExtensionArg :: !String
    , rootPathArg :: !String
    , extraArgs :: ![String]
    }
    deriving Show


forceArgReader :: ReadM Bool
forceArgReader =
    readerAsk >>= \case
        "0" -> pure False
        "1" -> pure True
        _ -> fail "<force> arg must be 0 or 1"


argsOptions :: Parser VimwikiSingleFileCliArgs
argsOptions = VimwikiSingleFileCliArgs
    <$> argument forceArgReader
        ( metavar "FORCE"
       <> help "[0|1] - 1 = force/overwrite the output file" )
    <*> argument str
        ( metavar "SYNTAX"
       <> help "the syntax chosen for this wiki" )
    <*> argument str
        ( metavar "EXT"
       <> help "the file extension for this wiki" )
    <*> argument str
        ( metavar "O_DIR"
       <> help "the full path of the output directory" )
    <*> argument str
        ( metavar "I_FILE"
       <> help "the full path of the wiki page" )
    <*> argument str
        ( metavar "CSS"
       <> help "the full path of the CSS file for this wiki" )
    <*> argument str
        ( metavar "T_PATH"
       <> help "the full path to the wiki's templates" )
    <*> argument str
        ( metavar "T_DEFAULT"
       <> help "the default template name" )
    <*> argument str
        ( metavar "T_EXT"
       <> help "the extension of template files" )
    <*> argument str
        ( metavar "ROOT_PATH"
       <> help "a count of ../ for pages buried in subdirs" )
    <*> some (argument str
        ( metavar "EXTRA"
       <> help "Extra argments"))


opts :: ParserInfo VimwikiSingleFileCliArgs
opts = info
    ( argsOptions <**> helper )
    ( fullDesc
    <> progDesc "Transform a Pandoc markdown file into an HTML file"
    <> header "vw-html-converter - convert vimwiki/markdown to html"
    <> noIntersperse )


debugParams :: VimwikiSingleFileCliArgs -> IO ()
debugParams args = do
    putStrLn "Decoded args are:"
    putStrLn $ "Template path is: " ++ templatePathArg args
    putStrLn $ "Template name is " ++  templateNameArg args
    putStrLn $ "InputFile is " ++ inputFileArg args
    putStrLn $ "Template ext is " ++  templateExtensionArg args
    putStrLn $ "Template file is " ++ templateFilePath args
    putStrLn $ "CssFile is " ++ cssFileArg args


validateArgs :: VimwikiSingleFileCliArgs -> IO Bool
validateArgs args = validateWithTests args tests
  where
      tests = [ validateForceArg
              , validateSyntaxArg
              , validateExtensionArg
              , validateFileExists inputFileArg     "Input file "
              , validateFileExists templateFilePath "Template File "
              , validateFileExists cssFileArg       "css file "
              ]


validateForceArg :: VimwikiSingleFileCliArgs -> IO Bool
validateForceArg args = do
    let outputFileName = outputFilePath args
    exists <- doesFileExist outputFileName
    if not (forceArg args) && exists
      then do
          putStrLn $ "File '" ++ outputFileName ++ "' exists and FORCE is 0"
          return False
      else return True


validateSyntaxArg :: VimwikiSingleFileCliArgs -> IO Bool
validateSyntaxArg args = do
    let syn = strToLower $ syntaxArg args
    if syn /= "markdown"
      then do
          progName <- getProgName
          putStrLn $ progName ++ " only understands 'markdown' SYNTAX"
          return False
      else return True


validateExtensionArg :: VimwikiSingleFileCliArgs -> IO Bool
validateExtensionArg args = do
    let ext = extensionArg args
    if not (isMarkDownStr ext)
      then do
          putStrLn $ "Extension " ++ ext ++ " is not a understood "
                      ++ "as an EXTENSION."
          return False
      else return True
