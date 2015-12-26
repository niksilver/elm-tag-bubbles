module TagFetcher where

import Secrets exposing (apiKey)

import Json.Decode exposing (..)

url = "http://content.guardianapis.com/search?show-tags=keyword&page-size=10&api-key=" ++ apiKey

type alias Tag = { id : String, webTitle : String, sectionId : String }

tagToId : Decoder String
tagToId = ("id" := string)

tagToWebTitle : Decoder String
tagToWebTitle = ("webTitle" := string)

tagToSectionId : Decoder String
tagToSectionId = ("sectionId" := string)

tagToTag : Decoder Tag
tagToTag =
    object3 Tag
        tagToId
        tagToWebTitle
        tagToSectionId

tagsToTags : Decoder (List Tag)
tagsToTags =
    list tagToTag

resultToTags : Decoder (List Tag)
resultToTags =
    ("tags" := tagsToTags)

resultsToTags : Decoder (List (List Tag))
resultsToTags =
    list resultToTags

responseToTags : Decoder (List (List Tag))
responseToTags =
    at ["response", "results"] resultsToTags

