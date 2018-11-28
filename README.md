# Elm tag bubbles

Elm application to explore the relationship between the
many topics written about at [theguardian.com](http://theguardian.com).
See it in action at
[http://niksilver.github.io/elm-tag-bubbles/](http://niksilver.github.io/elm-tag-bubbles/).

## Build and run the application

Once you've cloned the source you'll need to
[get your own API key](http://open-platform.theguardian.com/access/).
Then put it in a new file at `src/Secrets.elm` that looks like this:
```
module Secrets exposing (..)

apiKey : String
apiKey = "YOUR-API-KEY-GOES-HERE"
```

Now you can build the application with this command:

```
sh build.sh
```

and then visit `out/index.html`.

## Running the tests

Follow the
[https://www.npmjs.com/package/elm-test](instructions for `elm-test` for Elm 0.19).
First install the test packages:
```
npm install -g elm-test
```

To run the tests

```
elm-test
```

