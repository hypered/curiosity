{-# LANGUAGE DataKinds #-}
{- |
Module: Curiosity.Html.Misc
Description: Helper functions to build HTML views.

TODO Move to smart-design-hs Misc.
-}
module Curiosity.Html.Misc
  ( containerMedium
  , containerLarge
  , keyValuePair
  , fullScroll
  , groupLayout
  , panel
  , panel'
  , panelStandard
  , header

  -- Form
  , title
  , title'
  , inputText
  , inputPassword
  , disabledText
  , submitButton
  , button
  , buttonLink
  , buttonAdd
  , buttonGroup
  , buttonBar
  , buttonPrimary
  , buttonPrimaryDisabled
  , buttonSecondary
  , buttonSecondaryDisabled

  -- View
  , editButton
  , iconError

  -- Links
  , linkifyAts
  , linkEmail
  , linkTwitter

  -- Keep here:
  , renderView
  , renderView'
  , renderForm
  , renderForm'
  , renderFormLarge
  , autoReload
  ) where

import qualified Curiosity.Data.User           as User
import           Curiosity.Html.Navbar          ( navbar
                                                , navbarWebsite
                                                )
import qualified Data.Text                     as T
import qualified Smart.Html.Dsl                as Dsl
import qualified Smart.Html.Render             as Render
import           Smart.Html.Shared.Html.Icons   ( OSvgIconDiv(..)
                                                , divIconAdd
                                                , divIconCheck
                                                , svgIconCircleError
                                                , svgIconEdit
                                                )
import qualified Text.Blaze.Html5              as H
import           Text.Blaze.Html5               ( (!)
                                                , Html
                                                )
import qualified Text.Blaze.Html5.Attributes   as A


--------------------------------------------------------------------------------
-- | This works well paired with a side menu.
-- TODO Probably move this directly to a "layout" function such as
-- withSideMenuFullScroll.
containerMedium content =
  H.div
    ! A.class_ "u-scroll-wrapper-body"
    $ H.div
    ! A.class_ "o-container o-container--large"
    $ H.div
    ! A.class_ "o-container-vertical"
    $ H.div
    ! A.class_ "o-container o-container--medium"
    $ content

-- | This works well when not paired with a side menu.
-- TODO Probably move this directly to a "layout" function such as fullScroll.
containerLarge content =
  H.div
    ! A.class_ "u-scroll-wrapper-body"
    $ H.div
    ! A.class_ "o-container o-container--large"
    $ H.div
    ! A.class_ "o-container-vertical"
    $ H.div
    ! A.class_ "u-spacer-bottom-xl"
    $ content

keyValuePair :: H.ToMarkup a => Text -> a -> Html
keyValuePair key value = H.div ! A.class_ "c-key-value-item" $ do
  H.dt ! A.class_ "c-key-value-item__key" $ H.toHtml key
  H.dd ! A.class_ "c-key-value-item__value" $ H.toHtml value

-- | The corresponding layout with a side menu is withSideMenuFullScroll.
-- TODO Move to Smart.Html.Layout.
fullScroll content = H.main ! A.class_ "u-maximize-width" $ do
  content

renderView' mprofile content =
  Render.renderCanvasFullScroll
    . Dsl.SingletonCanvas
    $ H.div
    ! A.class_ "c-app-layout u-scroll-vertical"
    $ do
        header mprofile
        fullScroll content

renderView content =
  Render.renderCanvasFullScroll
    . Dsl.SingletonCanvas
    $ H.div
    ! A.class_ "c-app-layout u-scroll-vertical"
    $ do
        H.toMarkup . navbar $ "TODO username"
        fullScroll content

header mprofile = case mprofile of
  Just profile ->
    H.toMarkup
      . navbar
      . User.unUserName
      . User._userCredsName
      $ User._userProfileCreds profile
  Nothing -> H.toMarkup navbarWebsite

renderForm' :: Maybe User.UserProfile -> Html -> Html
renderForm' mprofile content =
  Render.renderCanvasFullScroll
    . Dsl.SingletonCanvas
    $ H.div
    ! A.class_ "c-app-layout u-scroll-vertical"
    $ do
        header mprofile
        H.main ! A.class_ "u-maximize-width" $ containerMedium $ do
          H.form content

renderForm :: User.UserProfile -> Html -> Html
renderForm profile content =
  Render.renderCanvasFullScroll
    . Dsl.SingletonCanvas
    $ H.div
    ! A.class_ "c-app-layout u-scroll-vertical"
    $ do
        H.header $ navbar' profile
        H.main ! A.class_ "u-maximize-width" $ containerMedium $ do
          H.form content

renderFormLarge :: User.UserProfile -> Html -> Html
renderFormLarge profile content =
  Render.renderCanvasFullScroll
    . Dsl.SingletonCanvas
    $ H.div
    ! A.class_ "c-app-layout u-scroll-vertical"
    $ do
        H.header $ navbar' profile
        H.main ! A.class_ "u-maximize-width" $ containerLarge $ do
          H.form content

navbar' profile =
  H.toMarkup
    . navbar
    . User.unUserName
    . User._userCredsName
    $ User._userProfileCreds profile

panel s content = H.div ! A.class_ "c-panel u-spacer-bottom-l" $ do
  H.div ! A.class_ "c-panel__header" $ H.h2 ! A.class_ "c-panel__title" $ H.text
    s
  H.div ! A.class_ "c-panel__body" $ groupLayout content

panel' panelTitle body =
  H.div
    ! A.class_ "o-container o-container--large"
    $ H.div
    ! A.class_ "o-container-vertical"
    $ H.div
    ! A.class_ "u-padding-vertical-s"
    $ H.div
    ! A.class_ "c-panel"
    $ do
        H.div
          ! A.class_ "c-panel__header"
          $ H.div
          ! A.class_ "c-toolbar"
          $ H.div
          ! A.class_ "c-toolbar__left"
          $ H.h2
          ! A.class_ "c-panel__title"
          $ H.text panelTitle
        H.div ! A.class_ "c-panel__body" $ body

panelStandard s content = H.div ! A.class_ "c-panel u-spacer-bottom-l" $ do
  H.div ! A.class_ "c-panel__header" $ H.h2 ! A.class_ "c-panel__title" $ H.text
    s
  H.div ! A.class_ "c-panel__body" $ groupLayoutStandard content

groupLayout content =
  H.div
    ! A.class_ "o-form-group-layout o-form-group-layout--horizontal"
    $ content

groupLayoutStandard content =
  H.div ! A.class_ "o-form-group-layout o-form-group-layout--standard" $ content


--------------------------------------------------------------------------------
title :: Text -> Html
title s = title' s Nothing

title' :: Text -> Maybe H.AttributeValue -> Html
title' s mEditButton =
  H.div
    ! A.class_ "u-spacer-bottom-l"
    $ H.div
    ! A.class_ "c-navbar c-navbar--unpadded c-navbar--bordered-bottom"
    $ H.div
    ! A.class_ "c-toolbar"
    $ do
        H.div
          ! A.class_ "c-toolbar__left"
          $ H.h3
          ! A.class_ "c-h3 u-m-b-0"
          $ H.text s
        maybe mempty editButton mEditButton

disabledText
  :: Text -> H.AttributeValue -> Maybe H.AttributeValue -> Maybe Text -> Html
disabledText label name mvalue mhelp = inputText' label name mvalue mhelp True

inputText
  :: Text -> H.AttributeValue -> Maybe H.AttributeValue -> Maybe Text -> Html
inputText label name mvalue mhelp = inputText' label name mvalue mhelp False

inputText'
  :: Text
  -> H.AttributeValue
  -> Maybe H.AttributeValue
  -> Maybe Text
  -> Bool
  -> Html
inputText' label name mvalue mhelp disabled =
  H.div ! A.class_ "o-form-group" $ do
    H.label ! A.class_ "o-form-group__label" ! A.for name $ H.toHtml label
    H.div
      ! A.class_ "o-form-group__controls o-form-group__controls--full-width"
      $ do
          (if disabled then (! A.disabled "disabled") else identity)
            $ maybe identity (\value -> (! A.value value)) mvalue
            $ H.input
            ! A.class_ "c-input"
            ! A.id name
            ! A.name name
          maybe mempty ((H.p ! A.class_ "c-form-help-text") . H.text) mhelp

inputPassword = H.div ! A.class_ "o-form-group" $ do
  H.label ! A.class_ "o-form-group__label" ! A.for "password" $ "Password"
  H.div
    ! A.class_ "o-form-group__controls o-form-group__controls--full-width"
    $ H.input
    ! A.class_ "c-input"
    ! A.type_ "password"
    ! A.id "password"
    ! A.name "password"

submitButton :: H.AttributeValue -> Html -> Html
submitButton submitUrl label =
  H.div
    ! A.class_ "o-form-group"
    $ H.div
    ! A.class_ "u-spacer-left-auto"
    $ H.button
    ! A.class_ "c-button c-button--primary"
    ! A.formaction (H.toValue submitUrl)
    ! A.formmethod "POST"
    $ H.span
    ! A.class_ "c-button__content"
    $ H.span
    ! A.class_ "c-button__label"
    $ label

buttonGroup content =
  H.div
    ! A.class_ "o-form-group-layout o-form-group-layout--horizontal"
    $ H.div
    ! A.class_ "o-form-group"
    $ H.div
    ! A.class_ "u-spacer-left-auto u-spacer-top-l"
    $ content

buttonBar content =
  H.div
    ! A.class_ "c-toolbar"
    $ H.div
    ! A.class_ "c-toolbar__right"
    $ H.div
    ! A.class_ "c-toolbar__item"
    $ H.div
    ! A.class_ "c-button-toolbar"
    $ content

button submitUrl label = buttonGroup $ buttonPrimary submitUrl label

buttonLink url label =
  H.a
    ! A.class_ "c-button c-button--secondary"
    ! A.href url
    $ H.div
    ! A.class_ "c-button__content"
    $ H.div
    ! A.class_ "c-button__label"
    $ H.text label

buttonPrimary submitUrl label =
  H.button
    ! A.class_ "c-button c-button--primary"
    ! A.formaction submitUrl
    ! A.formmethod "POST"
    $ H.span
    ! A.class_ "c-button__content"
    $ do
        H.span ! A.class_ "c-button__label" $ H.text label
        divIconCheck

buttonPrimaryDisabled label =
  H.button
    ! A.class_ "c-button c-button--primary"
    ! A.disabled "disabled"
    $ H.span
    ! A.class_ "c-button__content"
    $ do
        H.span ! A.class_ "c-button__label" $ H.text label
        divIconCheck

buttonSecondary submitUrl label =
  H.button
    ! A.class_ "c-button c-button--secondary"
    ! A.formaction submitUrl
    ! A.formmethod "POST"
    $ H.span
    ! A.class_ "c-button__content"
    $ H.span
    ! A.class_ "c-button__label"
    $ H.text label

buttonSecondaryDisabled label =
  H.button
    ! A.class_ "c-button c-button--secondary"
    ! A.disabled "disabled"
    $ H.span
    ! A.class_ "c-button__content"
    $ H.span
    ! A.class_ "c-button__label"
    $ H.text label

buttonAdd submitUrl label =
  H.button
    ! A.class_ "c-button c-button--secondary"
    ! A.formaction submitUrl
    ! A.formmethod "POST"
    $ H.span
    ! A.class_ "c-button__content"
    $ do
        H.toMarkup divIconAdd
        H.span ! A.class_ "c-button__label" $ H.text label

--------------------------------------------------------------------------------
editButton :: H.AttributeValue -> Html
editButton lnk =
  H.div
    ! A.class_ "c-toolbar__right"
    $ H.a
    ! A.class_ "c-button c-button--secondary"
    ! A.href lnk
    $ H.span
    ! A.class_ "c-button__content"
    $ do
        H.div ! A.class_ "o-svg-icon o-svg-icon-edit" $ H.toHtml svgIconEdit
        H.span ! A.class_ "c-button__label" $ "Edit"


--------------------------------------------------------------------------------
-- | Turn \@mentions into links. This is used for texts appearing in Bios and
-- Descriptions of user and business unit profiles.
linkifyAts :: Text -> Html
linkifyAts = mapM_ f . T.words
 where
  f word | "@" `T.isPrefixOf` word = do
    let (word', rest) = T.breakOn "." word
    H.a ! A.href (H.toValue $ "/" <> T.tail word') $ H.text word'
    H.text rest
    " "
  f word = H.text word >> " "

linkEmail :: Text -> Html
linkEmail s = H.a ! A.href (H.toValue $ "mailto:" <> s) $ H.text s

linkTwitter :: Text -> Html
linkTwitter s =
  H.a ! A.href (H.toValue $ "https://twitter.com/" <> s) $ H.text ("@" <> s)


--------------------------------------------------------------------------------
iconError = Just $ OSvgIconDiv @"circle-error" svgIconCircleError


--------------------------------------------------------------------------------
-- | This is a script to connect to the backend using websocket, and reload the
-- page when the connection is lost (and then successfully re-created). You can
-- thus add this element temporarily to a page when you're hacking at it using
-- something like ghcid.
{-# DEPRECATED autoReload "Use this only during interactive development" #-}
autoReload =
  H.preEscapedText
    "<script>\n\
  \function connect(isInitialConnection) {\n\
  \  // Create WebSocket connection.\n\
  \  var ws = new WebSocket('ws://' + location.host + '/ws');\n\
  \\n\
  \  // Connection opened\n\
  \  ws.onopen = function() {\n\
  \    ws.send('Hello server.');\n\
  \    if (isInitialConnection) {\n\
  \      console.log('autoreload: Initial connection.');\n\
  \    } else {\n\
  \      console.log('autoreload: Reconnected.');\n\
  \      location.reload();\n\
  \    };\n\
  \  };\n\
  \\n\
  \  // Listen for messages.\n\
  \  ws.onmessage = function(ev) {\n\
  \    console.log('autoreload: Message from server: ', ev.data);\n\
  \  };\n\
  \\n\
  \  // Trying to reconnect when the socket is closed.\n\
  \  ws.onclose = function(ev) {\n\
  \    console.log('autoreload: Socket closed. Trying to reconnect in 0.5 second.');\n\
  \    setTimeout(function() { connect(false); }, 500);\n\
  \  };\n\
  \\n\
  \  // Close the socker upon error.\n\
  \  ws.onerror = function(err) {\n\
  \    console.log('autoreload: Socket errored. Closing socket.');\n\
  \    ws.close();\n\
  \  };\n\
  \}\n\
  \\n\
  \connect(true);\n\
  \</script>\n"
