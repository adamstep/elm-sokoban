module Assets exposing
    (
    -- player
      playerHold
    , playerStand
    -- floor tiles
    , slate
    , wood
    , concrete
    -- objects
    , box
    , smallBox
    , television
    , chair
    , plant
    , largeTv1
    , largeTv2
    -- special
    , goal
    , none
    -- walls
    , wallTopLeft
    , wallTopRight
    , wallTopBottom
    , wallTop
    , wallBottom
    , wallTopRightBottom
    , wallBottomRightLeft
    , wallBottomLeft
    , wallBottomRight
    , wallRightLeft
    , wallLeft
    , wallRight
    , wallTopRightLeft
    , wallTopBottomLeft
    , kitchenSink
    , kitchenRange
    -- glass
    , glassRight
    -- door
    , doorTop
    , doorLeft
    -- trees
    , smallTree
    )

import Element exposing (image, Element)

tileDir =
    "/assets/topdown-shooter/PNG/Tiles/"

playerDir =
    "/assets/topdown-shooter/PNG/Woman Green/"

tileElement width height name =
    image width height (tileDir ++ name)

standardTileElement =
    tileElement 64 64

playerElement width height name =
    image width height (playerDir ++ name)

-- player
playerStand = playerElement 33 43 "womanGreen_stand.png"
playerHold = playerElement 33 43 "womanGreen_hold.png"

-- floor tiles
slate = standardTileElement "tile_11.png"
wood = standardTileElement "tile_42.png"
concrete = standardTileElement "tile_07.png"

-- objects
box = standardTileElement "tile_129.png"
smallBox = standardTileElement "tile_156.png"
television = standardTileElement "tile_536.png"
chair = standardTileElement "tile_531.png"
plant = standardTileElement "tile_134.png"
largeTv1 = standardTileElement "tile_532.png"
largeTv2 = standardTileElement "tile_533.png"


-- special
goal = standardTileElement "tile_132.png"
none = standardTileElement "tile_01.png"

-- walls
wallTopLeft = standardTileElement "tile_109.png"
wallTopRight = standardTileElement "tile_110.png"
wallTopBottom = standardTileElement "tile_111.png"
wallTop = standardTileElement "tile_112.png"
wallBottom = standardTileElement "tile_113.png"
wallTopRightBottom = standardTileElement "tile_114.png"
wallBottomRightLeft = standardTileElement "tile_115.png"

wallBottomLeft = standardTileElement "tile_136.png"
wallBottomRight = standardTileElement "tile_137.png"
wallRightLeft = standardTileElement "tile_138.png"
wallLeft = standardTileElement "tile_139.png"
wallRight = standardTileElement "tile_140.png"
wallTopRightLeft = standardTileElement "tile_141.png"
wallTopBottomLeft = standardTileElement "tile_142.png"

kitchenSink = standardTileElement "tile_323.png"
kitchenRange = standardTileElement "tile_324.png"

glassRight = standardTileElement "tile_488.png"

doorTop = standardTileElement "tile_470.png"
doorLeft = standardTileElement "tile_443.png"

-- trees
smallTree = standardTileElement "tile_183.png"
