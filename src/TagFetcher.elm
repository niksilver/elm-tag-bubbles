module TagFetcher where

import Constants exposing (Tag, Tags, TagsResult)
import Secrets exposing (apiKey)

import Json.Decode exposing (Decoder, (:=), string, object2, list, at)
import Http exposing (url, get)
import Effects exposing (Effects, task)
import Task exposing (toMaybe)

url tag =
    Http.url "http://content.guardianapis.com/search"
    [ ("show-tags", "keyword")
    , ("tag", tag)
    , ("page-size", "10")
    , ("api-key", apiKey)
    ]

tagToId : Decoder String
tagToId = ("id" := string)

tagToWebTitle : Decoder String
tagToWebTitle = ("webTitle" := string)

tagToTag : Decoder Tag
tagToTag =
    object2 Tag
        tagToId
        tagToWebTitle

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

getTags : String -> Effects TagsResult
getTags tag =
    url tag
        |> get responseToTags
        |> Task.toResult
        |> Effects.task

