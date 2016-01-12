module Constants
    ( Id
    , Tag, Tags, TagsResult, emptyTagsResult
    , maxBubbleOpacity, fadeStep
    , springStrength, airDragFactor, minimumVelocity
    ) where

import Http exposing (Error)
import Signal

type alias Id = String

type alias Tag = { id : Id, webTitle : String, sectionId : String }

type alias Tags = List Tag

type alias TagsResult = Result Http.Error (List Tags)

emptyTagsResult = Ok [[]]

maxBubbleOpacity : Float
maxBubbleOpacity = 0.75

fadeStep : Float
fadeStep = 0.015

springStrength : Float
springStrength = 30.0

airDragFactor : Float
airDragFactor = 0.02

minimumVelocity : Float
minimumVelocity = 0.1

