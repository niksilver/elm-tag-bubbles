module Constants
    ( Id
    , Tag, Tags, TagsResult, emptyTagsResult
    , colour1, colour2
    , bubbleOpacity
    , springStrength, airDragFactor, minimumVelocity
    ) where

import Http exposing (Error)
import Signal

type alias Id = String

type alias Tag = { id : Id, webTitle : String, sectionId : String }

type alias Tags = List Tag

type alias TagsResult = Result Http.Error (List Tags)

emptyTagsResult = Ok [[]]

colour1 : String
colour1 = "green"

colour2 : String
colour2 = "red"

bubbleOpacity : String
bubbleOpacity = "0.75"

springStrength : Float
springStrength = 40.0

airDragFactor : Float
airDragFactor = 0.02

minimumVelocity : Float
minimumVelocity = 0.1

