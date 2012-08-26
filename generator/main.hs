{-# LANGUAGE OverloadedStrings #-}
import Control.Arrow ((>>>))
import Hakyll

config :: HakyllConfiguration
config = defaultHakyllConfiguration {
    destinationDirectory = "../",
    storeDirectory = "../_store"
  }

main :: IO ()
main = hakyllWith config $ do
  match "images/*" $ do
    route   idRoute
    compile copyFileCompiler

  match "doc/*" $ do
    route idRoute
    compile copyFileCompiler

  match "javascripts/*" $ do
    route idRoute
    compile copyFileCompiler

  match "stylesheets/*" $ do
    route idRoute
    compile compressCssCompiler

  match "templates/*" $ compile templateCompiler

  match (list ["index.markdown"]) $ do
    route $ setExtension "html"
    compile $ pageCompiler
      >>> applyTemplateCompiler "templates/default.html"
      >>> relativizeUrlsCompiler

