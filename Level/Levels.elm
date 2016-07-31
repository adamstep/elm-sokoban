module Level.Levels exposing (..)

import String
import Assets
import Maybe exposing (withDefault, andThen)
import Level.Model as Model exposing (..)
import Matrix exposing (Location, loc, set)

type alias LevelFile =
    { name : String
    , text : List String
    , ornaments : List Ornament
    }

level1File =
    LevelFile 
        "Level 1"
        [ "xxxxxxxxx"
        , "xxxx####x"
        , "xxxx#  #x"
        , "xxxx#  #x"
        , "xxxx#$.#x"
        , "x####  #x"
        , "x#     #x"
        , "x#@$.  #x"
        , "x#######x"
        , "xxxxxxxxx"
        ]
        []

level1 =
    parseLevelFile level1File

level2File =
    LevelFile
        "Level 6"
        [ "###########"
        , "#        @#"
        , "# #$ $ $  #"
        , "# # $$$$  #"
        , "# #$ $ $  #"
        , "# #     $ #"
        , "# #$### ###"
        , "# # #.....#"
        , "# # #.   .#"
        , "#    .....#"
        , "###########"
        ]
        []

level2 =
    parseLevelFile level2File

level3File =
    LevelFile
        "Level 6"
        [ "xxxxxxxxxxxxx"
        , "xx#########xx"
        , "xx# @#   ##xx"
        , "xx# s  c ##xx"
        , "xx#$ #######x"
        , "x## p#     #x"
        , "x#. . .    #x"
        , "x#.  #######x"
        , "x#####xxxxxxx"
        , "xxxxxxxxxxxxx"
        ]
        [ Ornament (loc 5 1) 0 Assets.wallTopBottomLeft
        , Ornament (loc 6 1) 0 Assets.concrete
        , Ornament (loc 6 1) 0 Assets.glassRight
        , Ornament (loc 6 0) 0 Assets.concrete
        , Ornament (loc 7 1) 0 Assets.concrete
        , Ornament (loc 7 0) 0 Assets.concrete
        , Ornament (loc 7 1) 0 Assets.glassRight
        , Ornament (loc 8 1) 0 Assets.wallTopBottomLeft

        , Ornament (loc 7 6) 0 Assets.concrete
        , Ornament (loc 8 6) 0 Assets.concrete
        , Ornament (loc 9 6) 0 Assets.concrete
        , Ornament (loc 7 6) 0 Assets.doorTop
        , Ornament (loc 7 5) 0 Assets.wallTopRightLeft
        , Ornament (loc 7 7) 0 Assets.wallTopBottomLeft

        , Ornament (loc 8 7) 0 Assets.smallTree
        , Ornament (loc 9 5) 0 Assets.smallTree

        , Ornament (loc 1 9) 0 Assets.wallTopBottom
        , Ornament (loc 2 10) 0 Assets.wallRightLeft
        , Ornament (loc 3 10) 0 Assets.wallRightLeft
        , Ornament (loc 2 9) 90 Assets.kitchenRange
        , Ornament (loc 3 9) 90 Assets.kitchenSink
        , Ornament (loc 4 9) 0 Assets.wallTopBottom

        , Ornament (loc 5 10) 90 Assets.largeTv2
        , Ornament (loc 6 10) 90 Assets.largeTv1
        ]

level3 =
    let
        (position, grid, ornaments) = parseLevelFile level3File
        newGrid = grid
            |> set (loc 5 6) (Floor Empty Wood)
            |> set (loc 6 6) (Floor Empty Wood)
            |> set (loc 5 7) (Floor Empty Wood)
            |> set (loc 6 7) (Goal Empty Wood)
            |> set (loc 5 8) (Floor Empty Wood)
            |> set (loc 6 8) (Floor Empty Wood)
            |> set (loc 5 9) (Floor Empty Wood)
            |> set (loc 6 9) (Floor Empty Wood)
            |> set (loc 5 10) (Floor Empty Wood)
            |> set (loc 6 10) (Floor Empty Wood)
    in
        (position, newGrid, ornaments)

parseLevelFile : LevelFile -> (Location, Grid, List Ornament)
parseLevelFile file =
    let
        charMatrix = Matrix.fromList (List.map (\a -> String.toList a) file.text)
        charToCell = \c -> case c of
            '#' -> Wall
            '$' -> Floor (Package Box) Tile
            's' -> Floor (Package SmallBox) Tile
            'c' -> Floor (Package Chair) Tile
            'p' -> Floor (Package Plant) Tile
            '@' -> Floor Player Tile
            ' ' -> Floor Empty Tile
            '*' -> Goal (Package Box) Tile
            '+' -> Goal Player Tile
            '.' -> Goal Empty Tile
            _ -> None

        charToPlayerLoc = \loc c -> case c of
            '@' -> Just loc
            '+' -> Just loc
            _ -> Nothing
            
        grid = Matrix.map charToCell charMatrix


        position = Matrix.mapWithLocation charToPlayerLoc charMatrix
            |> Matrix.flatten
            |> List.filter (\c -> c /= Nothing)
            |> List.head
            |> withDefault (Just (loc 0 0))
            |> withDefault (loc 0 0)
    in
        (position, grid, file.ornaments)

