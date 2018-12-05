module TagFetcher exposing (..)


import Constants exposing (Tag, Tags, TagsResult(..))
import Secrets exposing (apiKey)

import Json.Decode exposing (Decoder, field, string, map2, list, at)
import Http
import Url
import Url.Builder


url : String -> String
url tag =
  Url.Builder.crossOrigin
    "https://content.guardianapis.com"
    [ "search" ]
    [ Url.Builder.string "show-tags" "keyword"
    , Url.Builder.string "tag" tag
    , Url.Builder.string "page-size" "20"
    , Url.Builder.string "api-key" apiKey
    ]


tagToId : Decoder String
tagToId =
  field "id" string


tagToWebTitle : Decoder String
tagToWebTitle =
  field "webTitle" string


tagToTag : Decoder Tag
tagToTag =
  map2 Tag
    tagToId
    tagToWebTitle


tagsToTags : Decoder Tags
tagsToTags =
  list tagToTag


resultToTags : Decoder Tags
resultToTags =
  field "tags" tagsToTags


resultsToTags : Decoder (List Tags)
resultsToTags =
  list resultToTags


responseToTags : Decoder (List Tags)
responseToTags =
  at ["response", "results"] resultsToTags


getTags : String -> Cmd TagsResult
getTags tag =
  Http.get
    { url = url tag
    , expect = Http.expectJson (\x -> TagsResult x) responseToTags
    }


