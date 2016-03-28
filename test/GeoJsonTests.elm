module GeoJsonTests where

import ElmTest exposing (..)
import GeoJson exposing (..)
import Json.Decode exposing (decodeString)
import Json.Encode as JE exposing (Value)

import String

-- TODO: If I fix properties, I need to fix this too (Value)

all : Test
all =
  suite "A Test Suite"
    [ parsePoint ()
    , parseLineString ()
    , parsePolygon ()
    , parseFeature ()
    , parseFeatureCollection ()
    ]


parsePoint : a -> Test
parsePoint _ =
  let
    json =
    """
    {
      "type": "Point",
      "coordinates": [-104.99404, 39.75621]
    }
    """

    geoJson = GeoJsonGeometry (Point { coordinates = Position -104.99404 39.75621 })
  in
  test "Point" <| assertEqual (Result.Ok geoJson) (decodeString decoder json)


parseLineString : a -> Test
parseLineString _ =
  let
    json =
    """
    {
      "type": "LineString",
      "coordinates": [[30, 10], [10, 30], [40, 40]]
    }
    """

    geoJson = GeoJsonGeometry (LineString { coordinates =
      [ Position 30 10
      , Position 10 30
      , Position 40 40
      ]})
  in
  test "LineString" <| assertEqual (Result.Ok geoJson) (decodeString decoder json)


parsePolygon : a -> Test
parsePolygon _ =
  let
    json =
    """
    {
      "type": "Polygon",
      "coordinates": [
        [[35, 10], [45, 45], [15, 40], [10, 20], [35, 10]], [[20, 30], [35, 35], [30, 20], [20, 30]]
      ]
    }
    """

    geoJson = GeoJsonGeometry (Polygon { coordinates =
      [
        [ Position 35 10, Position 45 45, Position 15 40, Position 10 20, Position 35 10 ]
      , [ Position 20 30, Position 35 35, Position 30 20, Position 20 30 ]
      ]
    })
 in
  test "Polygon" <| assertEqual (Result.Ok geoJson) (decodeString decoder json)


parseFeature : a -> Test
parseFeature _ =
  let
    json =
    """
    {
      "type": "Feature",
      "geometry": {"type": "Point", "coordinates": [102.0, 0.5]},
      "properties": {"prop0": "value0"}
    }
    """

    geoJson = GeoJsonFeature
      { geometry = Point { coordinates = Position 102.0 0.5 }
      , properties = [("prop0", JE.string "value0")]
      , id = Nothing
      }
 in
  test "Feature" <| assertEqual (Result.Ok geoJson) (decodeString decoder json)


parseFeatureCollection : a -> Test
parseFeatureCollection _ =
  let
    json =
    """
    {
      "type": "FeatureCollection",
      "features": [
        {
          "type": "Feature",
          "geometry": { "type": "Point", "coordinates": [102.0, 0.5] },
          "properties": { "prop0": "value0" }
        },
        {
          "type": "Feature",
          "geometry": {
            "type": "LineString",
            "coordinates": [[102.0, 0.0], [103.0, 1.0], [104.0, 0.0], [105.0, 1.0]]
          },
          "properties": {
            "prop0": "value0",
            "prop1": 0.0
          },
          "id": "aLineString"
        },
        {
          "type": "Feature",
          "geometry": {
            "type": "Point",
            "coordinates": [100.0, 0.0]
          },
          "properties": {
            "prop0": "value0",
            "prop1": "value1"
          }
        }
      ]
    }
    """

    geoJson = GeoJsonFeatureCollection
      { features =
        [ Feature
            (Point { coordinates = { x = 102.0, y = 0.5 }})
            [("prop0", JE.string "value0")]
            Nothing
        , Feature
            (LineString { coordinates = [ Position 102.0 0.0, Position 103.0 1.0, Position 104.0 0.0, Position 105.0 1.0 ] })
            [("prop1", JE.float 0.0), ("prop0", JE.string "value0")]
            (Just "aLineString")
        , Feature
            (Point { coordinates = Position 100.0 0.0 })
            [("prop1", JE.string "value1"), ("prop0", JE.string "value0")]
            Nothing
        ]
      }
 in
  test "FeatureCollection" <| assertEqual (Result.Ok geoJson) (decodeString decoder json)
