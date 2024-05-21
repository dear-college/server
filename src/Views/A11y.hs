{-# LANGUAGE OverloadedStrings #-}

module Views.A11y where

import Text.Blaze.Html5 (customAttribute)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as HA

ariaControls :: H.AttributeValue -> H.Attribute
ariaControls = customAttribute "aria-controls"

ariaExpanded :: H.AttributeValue -> H.Attribute
ariaExpanded = customAttribute "aria-expanded"

ariaLabel :: H.AttributeValue -> H.Attribute
ariaLabel = customAttribute "aria-label"

ariaCurrent :: H.AttributeValue -> H.Attribute
ariaCurrent = customAttribute "aria-current"

ariaDisabled :: H.AttributeValue -> H.Attribute
ariaDisabled = customAttribute "aria-disabled"
