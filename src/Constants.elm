module Constants exposing
    ( Id, Idable
    , Tag, Tags
    -- , TagsResult
    , maxBubbles, minBubbleSize, maxBubbleSize
    , minSpringLength, maxSpringLength
    , maxBubbleOpacity
    , transitionDuration
    , springStrength, airDragFactor, minimumVelocity
    , navHeight, statusBarHeight, sideBorderWidth, minWorldWidth
    )

-- import Http exposing (Error)
-- import Time exposing (Time)


type alias Id = String

type alias Idable a = { a | id : String }

type alias Tag = { id : Id, webTitle : String }

type alias Tags = List Tag

-- type alias TagsResult = Result Http.Error (List Tags)

-- Bubbles

maxBubbles : Int
maxBubbles = 10

minBubbleSize : Float
minBubbleSize = 40.0

maxBubbleSize : Float
maxBubbleSize = 120.0

maxBubbleOpacity : Float
maxBubbleOpacity = 0.75

transitionDuration : Float
transitionDuration = Debug.todo "One second"

-- Physics

minSpringLength : Float
minSpringLength = 170.0

maxSpringLength : Float
maxSpringLength = 360.0

springStrength : Float
springStrength = 25.0

airDragFactor : Float
airDragFactor = 0.04

minimumVelocity : Float
minimumVelocity = 0.1

-- UI

navHeight : Int
navHeight = 50

statusBarHeight : Int
statusBarHeight = 40

sideBorderWidth : Int
sideBorderWidth = 40

minWorldWidth : Int
minWorldWidth = 800

