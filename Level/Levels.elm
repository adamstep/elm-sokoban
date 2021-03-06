module Level.Levels exposing (..)

import String
import Assets
import Maybe exposing (withDefault, andThen)
import Level.Model as Model exposing (..)
import Matrix exposing (Location, loc, set)

type AvailableLevels
    = Level1
    | Level2
    | Level3
    | Level4
    | Level5
    | Level6

allLevels =
    [Level1, Level2, Level3, Level4, Level5, Level6]

getLevel l =
    case l of
        Level1 -> level1
        Level2 -> level3
        Level3 -> level2
        Level4 -> level4
        Level5 -> level5
        Level6 -> level6

nextLevel l =
    case l of
        Level1 -> Level2
        Level2 -> Level3
        Level3 -> Level4
        Level4 -> Level5
        Level5 -> Level6
        Level6 -> Level1

type alias LevelFile =
    { name : String
    , text : List String
    , ornaments : List Ornament
    }

level1File =
    LevelFile 
        "Level 1"
        [ "xxxxxxxxxxxxxxx"
        , "xxxxxxxxxxxxxxx"
        , "xxxxxxx####xxxx"
        , "xxxxxxx#  #xxxx"
        , "xxxxxxx#  #xxxx"
        , "xxxxxxx#$.#xxxx"
        , "xxxx####  #xxxx"
        , "xxxx#     #xxxx"
        , "xxxx#@$.  #xxxx"
        , "xxxx#######xxxx"
        , "xxxxxxxxxxxxxxx"
        , "xxxxxxxxxxxxxxx"
        ]
        [ Ornament (loc 3 3) 0 Assets.smallTree
        , Ornament (loc 4 5) 0 Assets.smallTree

        , Ornament (loc 9 7) 0 Assets.concrete
        , Ornament (loc 10 7) 0 Assets.concrete
        , Ornament (loc 11 7) 0 Assets.concrete

        , Ornament (loc 9 6) 0 Assets.wallTopRightBottom
        , Ornament (loc 9 8) 0 Assets.wallTopBottomLeft
        , Ornament (loc 9 7) 0 Assets.doorTop

        , Ornament (loc 5 10) 0 Assets.concrete
        , Ornament (loc 5 11) 0 Assets.concrete
        , Ornament (loc 5 12) 0 Assets.concrete
        , Ornament (loc 5 13) 0 Assets.concrete
        , Ornament (loc 5 14) 0 Assets.concrete
        , Ornament (loc 5 10) 0 Assets.doorLeft

        , Ornament (loc 4 10) 0 Assets.wallBottomRightLeft
        , Ornament (loc 6 10) 0 Assets.wallTopRightLeft
        ]

level1 =
    let
        (position, grid, ornaments) = parseLevelFile level1File
        newGrid = grid
            |> set (loc 5 8) (Floor (Package Television) Tile)
            |> set (loc 8 6) (Floor (Package SmallBox) Tile)
    in
        (position, newGrid, ornaments)

level2File =
    LevelFile
        "Level 6"
        [ "xxxxxxxxxxxxxxx"
        , "xx###########xx"
        , "xx#        @#xx"
        , "xx# #$ $ $  #xx"
        , "xx# # $$$$  #xx"
        , "xx# #$ $ $  #xx"
        , "xx# #     $ #xx"
        , "xx# #$### ###xx"
        , "xx# # #.....#xx"
        , "xx# # #.   .#xx"
        , "xx#    .....#xx"
        , "xx###########xx"
        ]
        []

level2 =
    parseLevelFile level2File

level3File =
    LevelFile
        "Level 6"
        [ "xxxxxxxxxxxxxxx"
        , "xxxxxxxxxxxxxxx"
        , "xxx#########xxx"
        , "xxx# @#   ##xxx"
        , "xxx# s  c ##xxx"
        , "xxx#$ #######xx"
        , "xx## p#    ##xx"
        , "xx#. . .   ##xx"
        , "xx#.  #######xx"
        , "xx#####xxxxxxxx"
        , "xxxxxxxxxxxxxxx"
        , "xxxxxxxxxxxxxxx"
        ]
        [ Ornament (loc 6 2) 0 Assets.wallTopBottomLeft
        , Ornament (loc 7 2) 0 Assets.concrete
        , Ornament (loc 7 1) 0 Assets.concrete
        , Ornament (loc 8 2) 0 Assets.concrete
        , Ornament (loc 8 1) 0 Assets.concrete
        , Ornament (loc 7 2) 0 Assets.glassRight
        , Ornament (loc 8 2) 0 Assets.glassRight
        , Ornament (loc 9 2) 0 Assets.wallTopBottomLeft

        , Ornament (loc 8 7) 0 Assets.concrete
        , Ornament (loc 9 7) 0 Assets.concrete
        , Ornament (loc 10 7) 0 Assets.concrete
        , Ornament (loc 11 7) 0 Assets.concrete
        , Ornament (loc 8 7) 0 Assets.doorTop
        , Ornament (loc 8 6) 0 Assets.wallTopRightLeft
        , Ornament (loc 8 8) 0 Assets.wallTopBottomLeft

        , Ornament (loc 9 8) 0 Assets.smallTree
        , Ornament (loc 10 6) 0 Assets.smallTree

        , Ornament (loc 2 10) 0 Assets.wallTopBottom
        , Ornament (loc 3 11) 0 Assets.wallRightLeft
        , Ornament (loc 4 11) 0 Assets.wallRightLeft
        , Ornament (loc 3 10) 90 Assets.kitchenRange
        , Ornament (loc 4 10) 90 Assets.kitchenSink
        , Ornament (loc 5 10) 0 Assets.wallTopBottom

        , Ornament (loc 6 11) 90 Assets.largeTv2
        , Ornament (loc 7 11) 90 Assets.largeTv1
        ]

level3 =
    let
        (position, grid, ornaments) = parseLevelFile level3File
        newGrid = grid
            |> set (loc 6 7) (Floor Empty Wood)
            |> set (loc 7 7) (Floor Empty Wood)
            |> set (loc 6 8) (Floor Empty Wood)
            |> set (loc 7 8) (Goal Empty Wood)
            |> set (loc 6 9) (Floor Empty Wood)
            |> set (loc 7 9) (Floor Empty Wood)
            |> set (loc 6 10) (Floor Empty Wood)
            |> set (loc 7 10) (Floor Empty Wood)
            |> set (loc 6 11) (Floor Empty Wood)
            |> set (loc 7 11) (Floor Empty Wood)
    in
        (position, newGrid, ornaments)

level4File =
    LevelFile
        "Level 2"
        [ "########"
        , "#  #   #"
        , "#   $  #"
        , "#  #$$ #"
        , "## #   #"
        , "#.. $# #"
        , "#..   @#"
        , "########"
        ]
        []

level4 =
    parseLevelFile level4File

level5File =
    LevelFile
        "Level 3"
        [ "xxxxxxxxxxx"
        , "xx#######xx"
        , "xx#     #xx"
        , "xx# $ $ #xx"
        , "xx# $ $@#xx"
        , "xx# $$ ##xx"
        , "xx#  ...#xx"
        , "xx###...#xx"
        , "xxxx#####xx"
        , "xxxxxxxxxxx"
        ]
        []

level5 =
    parseLevelFile level5File

level6File =
    LevelFile
        "Level 13"
        [ "xxxxxxxxxxxxxxx"
        , "x#############x"
        , "x#           #x"
        , "x# $$ # # $$ #x"
        , "x#  $$...$$  #x"
        , "x#   ##.##   #x"
        , "x# .*.....*. #x"
        , "x#  ###.###  #x"
        , "x#### $.$ ####x"
        , "xxxx# $$$ #xxxx"
        , "xxxx#  @  #xxxx"
        , "xxxx#######xxxx"
        ]
        []

level6 =
    parseLevelFile level6File




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

