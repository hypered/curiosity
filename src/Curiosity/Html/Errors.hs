-- |
--Module: Curiosity.Html.Errors
--Description: Error pages.
module Curiosity.Html.Errors
  ( NotFoundPage (..)
  , ErrorPage (..)
  ) where

import Smart.Html.Dsl qualified as Dsl
import Smart.Html.Errors qualified as Errors
import Smart.Html.Render qualified as Render
import Text.Blaze.Html5 qualified as H

--------------------------------------------------------------------------------
data NotFoundPage = NotFoundPage

instance H.ToMarkup NotFoundPage where
  toMarkup NotFoundPage = do
    Render.renderCanvas $ Dsl.SingletonCanvas Errors.NotFound

--------------------------------------------------------------------------------
data ErrorPage = ErrorPage Int Text Text

instance H.ToMarkup ErrorPage where
  toMarkup (ErrorPage code title subtitle) = do
    Render.renderCanvas . Dsl.SingletonCanvas $ Errors.Error code title subtitle
