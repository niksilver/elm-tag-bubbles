module TagFetcherTest where

import TagFetcher exposing (..)

import Json.Decode exposing (decodeString)
import Result exposing (Result(Ok))
import ElmTest exposing (..)

tag1 = """
    { "id": "world/bali-nine"
    , "webTitle": "Bali Nine"
    , "type": "keyword"
    , "sectionId": "world"
    , "webUrl": "http://www.theguardian.com/world/bali-nine"
    , "apiUrl": "http://content.guardianapis.com/world/bali-nine"
    , "sectionName": "World news"
    }
"""

tag2 = """
    { "id": "australia-news/q-a"
    , "webTitle": "Q&amp;A"
    , "description": "The latest news and comment on the Australian television program"
    , "type": "keyword"
    , "sectionId": "australia-news"
    , "webUrl": "http://www.theguardian.com/australia-news/q-a"
    , "apiUrl": "http://content.guardianapis.com/australia-news/q-a"
    , "sectionName": "Australia news"
    }
"""

tag3 = """
    { "id": "environment/cecil-the-lion"
    , "webTitle": "Cecil the lion"
    , "type": "keyword"
    , "sectionId": "environment"
    , "webUrl": "http://www.theguardian.com/environment/cecil-the-lion"
    , "apiUrl": "http://content.guardianapis.com/environment/cecil-the-lion"
    , "sectionName": "Environment"
    }
"""

tag4 = """
    { "id": "world/zimbabwe"
    , "webTitle": "Zimbabwe"
    , "type": "keyword"
    , "sectionId": "world"
    , "webUrl": "http://www.theguardian.com/world/zimbabwe"
    , "apiUrl": "http://content.guardianapis.com/world/zimbabwe"
    , "sectionName": "World news"
}
"""

tag5 = """
    { "id": "lifeandstyle/shops-and-shopping"
    , "webTitle": "Shops and shopping"
    , "description": "News, comment and features about shops and shopping"
    , "type": "keyword"
    , "sectionId": "lifeandstyle"
    , "webUrl": "http://www.theguardian.com/lifeandstyle/shops-and-shopping"
    , "apiUrl": "http://content.guardianapis.com/lifeandstyle/shops-and-shopping"
    , "sectionName": "Life and style"

}
"""

tag6 = """
    { "id": "money/consumer-affairs"
    , "webTitle": "Consumer affairs"
    , "type": "keyword"
    , "sectionId": "money"
    , "webUrl": "http://www.theguardian.com/money/consumer-affairs"
    , "apiUrl": "http://content.guardianapis.com/money/consumer-affairs"
    , "sectionName": "Money"

}
"""

result1 = """
    { "type": "article"
    , "sectionId": "media"
    , "webTitle": "What you read, watched and shared this year"
    , "webPublicationDate": "2015-12-25T12:00:17Z"
    , "id": "media/2015/dec/25/guardian-us-most-read-watched-commented"
    , "webUrl": "http://www.theguardian.com/..."
    , "apiUrl": "http://content.guardianapis.com/..."
    , "sectionName": "Media"
    , "tags":
""" ++ "[" ++ tag1 ++ "," ++ tag2 ++ "," ++ tag3 ++ "]" ++ "}"

result2 = """
    { "type": "article"
    , "sectionId": "business"
    , "webTitle": "Boxing Day sales hope..."
    , "webPublicationDate": "2015-12-25T12:00:17Z"
    , "id": "business/2015/dec/25/boxing-day-sales-biggest-discounts-uk-retail"
    , "webUrl": "http://www.theguardian.com/business/..."
    , "apiUrl": "http://content.guardianapis.com/business/..."
    , "sectionName": "Business"
    , "tags":
""" ++ "[" ++ tag4 ++ "," ++ tag5 ++ "," ++ tag6 ++ "]" ++ "}"

json = """
    { "response":
        { "status": "ok"
        , "userTier": "developer"
        , "total": 1840812
        , "startIndex": 1
        , "pageSize": 100
        , "currentPage": 1
        , "pages": 18409
        , "orderBy": "newest"
        , "results":
""" ++ "[" ++ result1 ++ "," ++ result2 ++ "]" ++ "}}"

all : Test
all =
    suite "TagFetcherTest"

    [ test "Tag to webTitle"
      (assertEqual
          (decodeString tagToWebTitle tag1)
          (Ok "Bali Nine"))

    , test "Tag to id"
      (assertEqual
          (decodeString tagToId tag1)
          (Ok "world/bali-nine"))

    , test "Tag to sectionId"
      (assertEqual
          (decodeString tagToSectionId tag1)
          (Ok "world"))
    ]
