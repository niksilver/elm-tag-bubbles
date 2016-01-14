module Constants
    ( Id, Idable
    , Tag, Tags, TagsResult, emptyTagsResult
    , maxBubbleOpacity, fadeDuration
    , springStrength, airDragFactor, minimumVelocity
    ) where

import Http exposing (Error)
import Time exposing (Time)
import Signal

type alias Id = String

type alias Idable a = { a | id : String }

type alias Tag = { id : Id, webTitle : String, sectionId : String }

type alias Tags = List Tag

type alias TagsResult = Result Http.Error (List Tags)

emptyTagsResult = Ok [[]]

maxBubbleOpacity : Float
maxBubbleOpacity = 0.75

fadeDuration : Time
fadeDuration = Time.second

springStrength : Float
springStrength = 30.0

airDragFactor : Float
airDragFactor = 0.02

minimumVelocity : Float
minimumVelocity = 0.1

