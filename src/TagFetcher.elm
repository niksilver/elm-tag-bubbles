module TagFetcher where

import Constants exposing (Tag, Tags, TagsResult)
import Secrets exposing (apiKey)

import Json.Decode exposing (Decoder, (:=), string, object3, list, at)
import Http exposing (url, get)
import Effects exposing (Effects, task)
import Task exposing (toMaybe)

url =
    Http.url "http://content.guardianapis.com/search"
    [ ("show-tags", "keyword")
    , ("page-size", "10")
    , ("api-key", apiKey)
    ]

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

tagsToTags : Decoder Tags
tagsToTags =
    list tagToTag

resultToTags : Decoder Tags
resultToTags =
    ("tags" := tagsToTags)

resultsToTags : Decoder (List Tags)
resultsToTags =
    list resultToTags

responseToTags : Decoder (List Tags)
responseToTags =
    at ["response", "results"] resultsToTags

getTags : Effects TagsResult
getTags =
    get responseToTags url
        |> Task.toResult
        |> Effects.task

