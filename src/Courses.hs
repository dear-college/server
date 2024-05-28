{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}

module Courses
  ( API,
    server,
  )
where

import AppM
  ( HasConfiguration (..),
    HasJwtSettings (..),
    HasUser (..),
    MonadDB (..),
    getConfiguration,
    getJWK,
    getJwtSettings,
  )
import Auth
import Configuration
import Control.Lens
import Control.Monad.Except (MonadError)
import Control.Monad.Reader
import Crypto.JWT
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy.Char8 as CL8
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time.Clock (addUTCTime, getCurrentTime)
import Model
import Network.URI (parseURI, uriToString)
import Servant
import Servant.HTML.Blaze
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as HA
import qualified Text.Blaze.Htmx as HA
import User
import qualified Views.A11y as HA
import Views.Page (partialPage)
import Web.Internal.FormUrlEncoded (FromForm (..), parseUnique)

ifM :: (Monad m) => m Bool -> m a -> m a -> m a
ifM mb mt me = do
  b <- mb
  if b then mt else me

-- TODO: I am confused about whether URI's are really utf8 or...?
textToURI :: Text -> Maybe URI
textToURI = parseURI . Text.unpack

instance FromForm Slug where
  fromForm f = Slug <$> parseUnique "course" f

type API =
  "courses"
    :> ( (Get '[HTML] H.Html)
           :<|> (ReqBody '[FormUrlEncoded] Slug :> Post '[HTML] H.Html)
           :<|> ( Capture "slug" Slug
                    :> ( (Get '[HTML] H.Html)
                           :<|> CaptureAll "path" Text :> (Get '[HTML] H.Html)
                       )
                )
       )

server ::
  ( MonadIO m,
    MonadDB m,
    MonadReader r m,
    HasConfiguration r,
    MonadRandom m,
    MonadError ServerError m,
    HasJwtSettings r,
    HasUser r
  ) =>
  ServerT API m
server = listCourses :<|> postCourse :<|> (\slug -> viewCourse slug :<|> redirectToContent slug)

inviteToLogin :: (MonadError ServerError m, MonadRandom m, MonadIO m, MonadDB m, MonadReader r m, HasJwtSettings r, HasConfiguration r, HasUser r) => m H.Html
inviteToLogin = do
  partialPage "join" $ do
    H.div ! HA.class_ "px-4 py-5 my-5 text-center" $ do
      H.h1 ! HA.class_ "display-5 fw-bold text-body-emphasis" $ do
        "Please log in "
      H.div ! HA.class_ "col-lg-6 mx-auto" $ do
        H.p ! HA.class_ "lead mb-4" $ do
          "You have not yet logged in."
        H.div ! HA.class_ "d-grid gap-2 d-sm-flex justify-content-sm-center" $ do
          H.a ! HA.href "/login" ! HA.class_ "btn btn-primary btn-lg px-4 gap-3" $ "Login"

withUserId :: (MonadError ServerError m, MonadRandom m, MonadIO m, MonadDB m, MonadReader r m, HasJwtSettings r, HasConfiguration r, HasUser r) => m H.Html -> (Subscriber -> m H.Html) -> m H.Html
withUserId partial handler = do
  user <- asks getUser
  case user of
    Unauthenticated -> partial
    AuthenticatedUser s -> handler s

courseLink :: Slug -> H.Html
courseLink (Slug text) = do
  H.a ! HA.href ("/courses/" <> H.toValue text) ! HA.class_ "list-group-item list-group-item-action" $ H.toHtml text

listCourses :: (MonadError ServerError m, MonadRandom m, MonadIO m, MonadDB m, MonadReader r m, HasJwtSettings r, HasConfiguration r, HasUser r) => m H.Html
listCourses = withUserId inviteToLogin $ \id -> do
  cs <- coursesForUser (AuthenticatedUser id)

  partialPage "courses" $ do
    H.nav ! HA.ariaLabel "breadcrumb" $ do
      H.ol ! HA.class_ "breadcrumb" $ do
        H.li ! HA.class_ "breadcrumb-item active" ! HA.ariaCurrent "page" $ do
          H.a ! HA.href "/courses" $ "Courses"
    H.h1 $ do
      "Your Courses"
    H.div ! HA.class_ "card" $ do
      H.ul ! HA.class_ "list-group list-group-flush" $ do
        mconcat (courseLink <$> cs)
        H.form ! HA.class_ "input-group" ! HA.hxPost "/courses" ! HA.hxTrigger "submit" ! HA.hxTarget "this" ! HA.hxSwap "beforebegin" $ do
          H.input ! HA.class_ "form-control border-0" ! HA.placeholder "Add new course…" ! HA.type_ "text" ! HA.name "course" ! HA.required mempty ! HA.onkeypress "return event.charCode != 32"
          H.div ! HA.class_ "input-group-append" $ do
            H.button ! HA.class_ "btn btn-secondary" ! HA.style "border-top-left-radius: 0; border-bottom-left-radius: 0; border-top-right-radius: 0" ! HA.type_ "submit" $ do
              H.i ! HA.class_ "bi bi-plus" $ mempty

viewCourse :: (MonadError ServerError m, MonadRandom m, MonadIO m, MonadDB m, MonadReader r m, HasJwtSettings r, HasConfiguration r, HasUser r) => Slug -> m H.Html
viewCourse slug@(Slug text) = withUserId inviteToLogin $ \id -> do
  content <-
    ifM
      (isInstructor (AuthenticatedUser id) slug)
      (viewCourseAsInstructor (AuthenticatedUser id) slug)
      (viewCourseAsStudent (AuthenticatedUser id) slug)

  partialPage text $ do
    H.nav ! HA.ariaLabel "breadcrumb" $ do
      H.ol ! HA.class_ "breadcrumb" $ do
        H.li ! HA.class_ "breadcrumb-item" $ do
          H.a ! HA.href "/courses" $ "Courses"
        H.li ! HA.class_ "breadcrumb-item active" ! HA.ariaCurrent "page" $ do
          H.a ! HA.href ("/courses/" <> (H.toValue text)) $ H.toHtml text
    H.h1 $ do
      H.toHtml text
    content

viewCourseAsStudent :: (MonadError ServerError m, MonadRandom m, MonadIO m, MonadDB m, MonadReader r m, HasJwtSettings r, HasConfiguration r, HasUser r) => User -> Slug -> m H.Html
viewCourseAsStudent user slug = do
  pure $ do
    H.p "You are a student."

viewCourseAsInstructor :: (MonadError ServerError m, MonadRandom m, MonadIO m, MonadDB m, MonadReader r m, HasJwtSettings r, HasConfiguration r, HasUser r) => User -> Slug -> m H.Html
viewCourseAsInstructor user slug = do
  asStudent <- viewCourseAsStudent user slug

  pure $ do
    H.p "You are an instructor."
    H.hr
    asStudent

postCourse :: (MonadError ServerError m, MonadRandom m, MonadIO m, MonadDB m, MonadReader r m, HasJwtSettings r, HasConfiguration r, HasUser r) => Slug -> m H.Html
postCourse slug@(Slug text) = do
  user <- asks getUser
  case user of
    Unauthenticated -> throwError err401
    AuthenticatedUser s -> do
      ifM
        (courseExists slug)
        (throwError err409 {errBody = "Course already exists"})
        (addInstructor user slug)
      pure $ courseLink slug

redirectToContent :: (MonadError ServerError m, MonadRandom m, MonadIO m, MonadDB m, MonadReader r m, HasJwtSettings r, HasConfiguration r, HasUser r) => Slug -> [Text] -> m H.Html
redirectToContent slug pathPieces = do
  let maybeUri = textToURI $ Text.intercalate "/" pathPieces
  case maybeUri of
    Nothing -> throwError $ err404 {errBody = "Invalid URI"}
    Just uri -> do
      user <- asks getUser
      redirectToContentWithUser user slug uri

instance AsError ServerError where
  _Error = prism' embed match
    where
      embed :: Error -> ServerError
      embed _ = err500
      match :: ServerError -> Maybe Error
      match _ = Nothing

redirectToContentWithUser :: (MonadError ServerError m, MonadRandom m, MonadIO m, MonadDB m, MonadReader r m, HasJwtSettings r, HasConfiguration r, HasUser r) => User -> Slug -> URI -> m H.Html
redirectToContentWithUser Unauthenticated (Slug text) uri = do
  partialPage "join" $ do
    H.div ! HA.class_ "px-4 py-5 my-5 text-center" $ do
      H.h1 ! HA.class_ "display-5 fw-bold text-body-emphasis" $ do
        "Join "
        H.preEscapedString "&ldquo;"
        H.toHtml text
        H.preEscapedString "&rdquo;"
      H.div ! HA.class_ "col-lg-6 mx-auto" $ do
        H.p ! HA.class_ "lead mb-4" $ do
          "You have not yet logged in.  Please log in and then you will be redirected to "
          H.code (H.toHtml $ show uri)
        H.div ! HA.class_ "d-grid gap-2 d-sm-flex justify-content-sm-center" $ do
          H.a ! HA.href "/login" ! HA.class_ "btn btn-primary btn-lg px-4 gap-3" $ "Login"
redirectToContentWithUser (AuthenticatedUser user) slug uri = do
  -- TODO: When should the tokens expire?
  issuedAt <- liftIO getCurrentTime
  let expireAt = addUTCTime 86400 issuedAt
  liftIO $ print $ "creaing JWT expiring at " <> (show expireAt)

  jwtSettings <- asks getJwtSettings
  jwk <- asks getJWK

  audience <- asks (getRootURI . getConfiguration)
  let claims = toolClaims audience expireAt user uri
  bestAlg <- bestJWSAlg jwk
  let bestHeader = newJWSHeader ((), bestAlg)
  mJWT <- signJWT jwk bestHeader claims
  let bs = encodeCompact mJWT

  let uri' = uri {uriFragment = "#" <> CL8.unpack bs}
  let uri'' = uriToString id uri' $ ""
  throwError $ err302 {errHeaders = [("Location", C8.pack uri'')]}
