module GeoJson
  ( GeoJson(GeoJsonGeometry, GeoJsonFeature, GeoJsonFeatureCollection)
  , Geometry(Point, LineString, Polygon)
  , Feature
  , FeatureCollection
  , Position
  , findFeatureById, features, bearing, distance, formatDistance
  ) where

{-| Library for working with GeoJSON data in Elm.
-}

import Json.Encode as JE exposing (Value)

type GeoJson
  = GeoJsonGeometry Geometry
  | GeoJsonFeature Feature
  | GeoJsonFeatureCollection FeatureCollection

type Geometry
  = Point { coordinates : Position }
  -- | MultiPoint { coordinates : List Position }
  | LineString { coordinates : List Position }
  -- | MultiLineString { coordinates : List (List Position) }
  | Polygon { coordinates : List (List Position) }
  -- | MultiPolygon { coordinates : List (List Position) }
  -- | GeometryCollection { geometries : List Geometry }


type alias Feature =
  { geometry : Geometry
  , properties : Value
  , id : Maybe String
  }


type alias FeatureCollection =
  { features : List Feature
  }


type alias Position =
  { x : Float
  , y : Float
  }


{-| Find a feature by its id
-}
findFeatureById : String -> GeoJson -> Maybe Feature
findFeatureById id geoJson =
  case geoJson of
    GeoJsonFeatureCollection featureCollection ->
      (.features >> List.filter (\feature -> feature.id == Just id) >> List.head) featureCollection

    GeoJsonFeature feature ->
      if feature.id == Just id then Just feature else Nothing

    GeoJsonGeometry _ ->
      Nothing


{-| Get all features in some GeoJson
-}
features : GeoJson -> List Feature
features geoJson =
  case geoJson of
    GeoJsonFeatureCollection featureCollection ->
      featureCollection.features

    GeoJsonFeature feature ->
      [ feature ]

    GeoJsonGeometry _ ->
      []


{-| Get the bearing in degrees between two positions
-}
bearing : Position -> Position -> Float
bearing pos1Degrees pos2Degrees =
  let
    toRad pos = { x = degrees pos.x, y = degrees pos.y }
    pos1 = toRad pos1Degrees
    pos2 = toRad pos2Degrees

    y = sin (pos2.x - pos1.x) * cos pos2.y
    x = cos pos1.y * sin pos2.y -
        sin pos1.y * cos pos2.y * cos (pos2.x - pos1.x)

    radiansToDegrees x = x * (180/pi)
  in
  radiansToDegrees (atan2 y x)


{-| Get the distance in metres between two positions
-}
distance : Position -> Position -> Float
distance pos1 pos2 =
  let
    r = 6371000
    p1 = degrees pos1.y
    p2 = degrees pos2.y
    deltaP = degrees (pos2.y - pos1.y)
    deltaL = degrees (pos2.x - pos1.x)

    a = sin (deltaP / 2) * sin (deltaP / 2) +
        cos p1 * cos p2 *
        sin (deltaL / 2) * sin (deltaL / 2)

    c = 2 * (atan2 (sqrt a) (sqrt (1 - a)))
  in
  r * c


{-| Format a distance in metres as metres or kilometres
-}
formatDistance : Int -> String
formatDistance metres =
  if metres < 1000 then
    metres
      |> toString
      |> \m -> m ++ "m"
  else
    metres
      |> \m -> m / 1000
      |> round
      |> toString
      |> \m -> m ++ "km"




